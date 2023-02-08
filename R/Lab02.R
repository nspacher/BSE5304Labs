Sys.getenv()
Sys.getenv("HOME")
myhomedir=Sys.getenv("HOME")
# Next depends on where you set up your git, and what you called it locally, 
# but when you start a new git project, it will be the first directory you 
# are placed in
getwd()
mygitdir=getwd()
# In the future you might want to search around for this
# mygitdir=paste0(myhomedir,"/2023/BSE5304Lab02")
# Mostly, we want to know where we are putting our homework PDFs
mypdfdir=paste0(mygitdir,"/pdfs")
# 
# Packages we think we are going to need today
# https://cran.r-project.org/web/packages/elevatr/elevatr.pdf
# https://cran.r-project.org/web/packages/raster/raster.pdf
# https://cran.r-project.org/web/packages/soilDB/soilDB.pdf
# https://cran.r-project.org/web/packages/rgdal/rgdal.pdf
#We use the R packages "elevatr", which provides access to digital elevation data from various sources including the USGS, “raster” which provides a variety of raster analysis functions, “SoilDB”, which accesses a Web Feature Service (WFS) (https://en.wikipedia.org/wiki/Web_Feature_Service) to access soil maps. 

#
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,patchwork,rnoaa)
pacman::p_load(operators,topmodel,DEoptim,soilDB,sp,curl,httr,
                 rnoaa,raster,shapefiles,rgdal,elevatr,terra,progress,lubridate)
system("git config --global user.email 'nspacher@vt.edu' ") 
system("git config --global user.name 'Nick Bentelspacher' ")

# We will explore this deeper when we get onto the VT ARC Next Week
# You do not want to, often can't actually, store large datasets in your 
# GitHub repository. This class is about repeatable modelling, not storing 
# large publicdatasets, which is a topic in of itself. 
datadir=paste0(myhomedir,"/data")
dir.create(datadir,recursive = T)
srcdir=paste0(myhomedir,"/src")
dir.create(srcdir,recursive = T)


setwd(srcdir)
system("svn checkout svn://scm.r-forge.r-project.org/svnroot/ecohydrology/"); 
install.packages(c("ecohydrology/pkg/EcoHydRology/"),repos = NULL)
pacman::p_load(EcoHydRology)
setwd(datadir)

# Get some flow data from USGS 0205551460 `
myflowgage_id="0205551460" #lick run
myflowgage=get_usgs_gage(myflowgage_id,begin_date = "2015-01-01",
                           end_date = "2019-01-01")

# For most watershed modelling purposes we normalize Q in mm/day for basins
myflowgage$flowdata$Qmm = myflowgage$flowdata$flow/myflowgage$area/10^3

# In the Lab01, we introduced you to a way to quickly get your WX Data 
# for any location in the world way easier than traditional download and
# parsing methods most old people use.
#
# Sorry, but there is even an easier way!
?FillMissWX()
WXData=FillMissWX(declat=myflowgage$declat, declon=myflowgage$declon,
                    StnRadius=30,minstns=10,date_min="2010-01-01",
                    date_max="2023-02-01",targElev=1,
                    method = "IDEW",alfa=2)

BasinData=merge(WXData,myflowgage$flowdata,by.x="date",by.y="mdate")
# A few constants
MinTempCol <- "#0000ff"
MaxTempCol <- "#ff0000"
PCol <- "#000000"
QCol <- PCol

coeff=1
p1= ggplot(BasinData, aes(x=date)) +
  geom_line( aes(y=MaxTemp), linewidth=1, color=MaxTempCol) + 
  geom_line( aes(y=MinTemp), linewidth=1, color=MinTempCol) + 
  geom_line( aes(y=Qmm), linewidth=1, color=QCol) +
  scale_y_continuous(
    # Features of the first axis
    name = "Temp(C)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Depth(mm)")
  ) + 
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.title.y.right = element_text(color = QCol, size=13)
  ) +
  ggtitle(myflowgage$gagename)

p1
basestr=format(Sys.time(),"/%Y%m%d%H%M")
filename=paste0(mypdfdir,basestr,"graph01.pdf")
pdf(filename) 
plot(p1)
dev.off()
print("file size")
print(file.size(filename))
print("I finished!")

#Watershed Delineation
# We can determine our UTM zone and build our proj4 projection string
trunc((180+myflowgage$declon)/6+1)
proj4_utm = paste0("+proj=utm +zone=", trunc((180+myflowgage$declon)/6+1), " +datum=WGS84 +units=m +no_defs")
print(proj4_utm)

# Lat/Lon (_ll) is much easier!
proj4_ll = "+proj=longlat"

# Now we will build our proj4strings which define our “Coordinate 
# Reference Systems” or CRS in future geographic manipulations. 
crs_ll=CRS(proj4_ll)
crs_utm=CRS(proj4_utm)
print(crs_ll)
print(crs_utm)

myflowgage$area   # area in km2

latlon <- cbind(myflowgage$declon,myflowgage$declat)
myflowgage$gagepoint_ll <- SpatialPoints(latlon)
proj4string(myflowgage$gagepoint_ll)=proj4_ll
myflowgage$gagepoint_utm=spTransform(myflowgage$gagepoint_ll,crs_utm)

# Open up maps.google.com to guesstimate area/lengths
url=paste0("https://www.google.com/maps/@",
             myflowgage$declat,",",myflowgage$declon,",18z")
browseURL(url)

# We are going to over estimate our area
sqrt(myflowgage$area)   # guestimating square watershed

# For our search we are going to multiply the area by 6 and
# to get the distance
sqrt(myflowgage$area*8)
searchlength=sqrt(myflowgage$area*8)*1000 
pourpoint=SpatialPoints(myflowgage$gagepoint_utm@coords,proj4string = crs_utm)
bboxpts=myflowgage$gagepoint_utm@coords
bboxpts=rbind(bboxpts,bboxpts+searchlength)
bboxpts=rbind(bboxpts,bboxpts-searchlength)
bboxpts
bboxpts=rbind(bboxpts,c(min(bboxpts[,1]),max(bboxpts[,2])))
bboxpts=rbind(bboxpts,c(max(bboxpts[,1]),min(bboxpts[,2])))
bboxpts
bboxpts=SpatialPoints(bboxpts,proj4string = crs_utm)
# From Lab04, get your DEM
mydem=get_aws_terrain(locations=bboxpts@coords, 
                        z = 12, prj = proj4_utm,src ="aws",expand=1)
res(mydem)
plot(mydem)
plot(bboxpts,add=T)
plot(pourpoint,add=T,col="red")

# Write our raster to a geotiff file that can be used with
# OS level hydrological models 
writeRaster(mydem,filename = "mydem.tif",overwrite=T)
# Our quick intro to terminal where the cloud offerings are usually Linux
# ls; cd ~; pwd;  # Linux/Mac 
# dir; cd ; # Windows

zoom(mydem)
zoomext=myflowgage$gagepoint_utm@coords
zoomext=rbind(zoomext,zoomext+res(mydem)*100)
zoomext=rbind(zoomext,zoomext-res(mydem)*100)
zoomext=SpatialPoints(zoomext,proj4string = crs_utm)  
zoom(mydem,ext=zoomext)
plot(bboxpts,add=T)
plot(pourpoint,add=T,col="red")

#terminal commands
# cd ~/src/      # Set your directory to your home directory
#   git clone https://github.com/dtarb/TauDEM.git
# mkdir ~/src/TauDEM/bin
# cd ~/src/TauDEM/src
#sed -i -e 's/MPI_Type_struct/MPI_Type_create_struct/g' linklib.h
# yes, this next line is very small font, but it is one line so...
#sed -i -e 's/MPI_Type_extent(MPI_LONG, \&extent)/MPI_Aint lb\;MPI_Type_get_extent(MPI_LONG, \&lb, \&extent)/g' linklib.h
# Now let's try make again!
#make

rm("old_path")
old_path <- Sys.getenv("PATH")
old_path
if( ! grepl("~/src/TauDEM/bin",old_path)){
  Sys.setenv(PATH = paste(old_path,
                          paste0(Sys.getenv("HOME"),"/src/TauDEM/bin"), 
                          sep = ":"))
}


system("mpirun aread8")

# Set working directory to your location
setwd(datadir)

z=raster("mydem.tif")
plot(z)
zdf <- as.data.frame(z, xy=T)
str(zdf)

# Pitremove
system("mpiexec -n 2 pitremove -z mydem.tif -fel mydemfel.tif")
fel=raster("mydemfel.tif")
plot(fel)

diff <- fel-z
diff_df <- as.data.frame(diff, xy=T)

z_plot <- ggplot()+
  geom_raster(data=zdf,aes(x=x,y=y, fill=layer))+
  scale_fill_viridis_c(name="Elevation (ft)")
diff_plot <- ggplot()+
  geom_raster(data=diff_df,aes(x=x,y=y, fill=layer))+
  scale_fill_viridis_c(name="Elevation Difference (ft)", option="plasma")

z_plot+diff_plot

# D8 flow directions
system("mpiexec -n 2 d8flowdir -p mydemp.tif -sd8 mydemsd8.tif -fel mydemfel.tif",show.output.on.console=F,invisible=F)
p=raster("mydemp.tif")
plot(p)
sd8=raster("mydemsd8.tif")
plot(sd8)

# Contributing area
system("mpiexec -n 2 aread8 -p mydemp.tif -ad8 mydemad8.tif")
ad8=raster("mydemad8.tif")
plot(log(ad8))
zoom(log(ad8))


# Grid Network 
system("mpiexec -n 2 gridnet -p mydemp.tif -gord mydemgord.tif -plen mydemplen.tif -tlen mydemtlen.tif")
gord=raster("mydemgord.tif")
plot(gord)
zoom(gord)

# DInf flow directions
system("mpiexec -n 2 dinfflowdir -ang mydemang.tif -slp mydemslp.tif -fel mydemfel.tif",show.output.on.console=F,invisible=F)
ang=raster("mydemang.tif")
plot(ang)
slp=raster("mydemslp.tif")
plot(slp)


# Dinf contributing area
system("mpiexec -n 2 areadinf -ang mydemang.tif -sca mydemsca.tif")
sca=raster("mydemsca.tif")
plot(log(sca))
zoom(log(sca))

# Threshold
system("mpiexec -n 2 threshold -ssa mydemad8.tif -src mydemsrc.tif -thresh 100")
src=raster("mydemsrc.tif")
plot(src)
zoom(src)

outlet=SpatialPointsDataFrame(myflowgage$gagepoint_utm,
                                data.frame(Id=c(1),outlet=paste("outlet",1,sep="")))
writeOGR(outlet,dsn=".",layer="approxoutlets",
           driver="ESRI Shapefile", overwrite_layer=TRUE)


# Move Outlets
system("mpiexec -n 2 moveoutletstostrm -p mydemp.tif -src mydemsrc.tif -o approxoutlets.shp -om outlet.shp")

approxpt=readOGR("approxoutlets.shp")
plot(approxpt,add=T,col="blue")

#outpt=read.shp("outlet.shp")
#approxpt=read.shp("ApproxOutlets.shp")

plot(src)
points(outpt$shp[2],outpt$shp[3],pch=19,col=2)
points(approxpt$shp[2],approxpt$shp[3],pch=19,col=4)

zoom(src)

# Contributing area upstream of outlet
system("mpiexec -n 2 aread8 -p mydemp.tif -o outlet.shp -ad8 mydemssa.tif")
ssa=raster("mydemssa.tif")
plot(ssa)
zoom(ssa)


# Threshold
system("mpiexec -n 2 threshold -ssa mydemssa.tif -src mydemsrc1.tif -thresh 2000")
src1=raster("mydemsrc1.tif")
plot(src1)
zoom(src1)

# Stream Reach and Watershed
system("mpiexec -n 2 streamnet -fel mydemfel.tif -p mydemp.tif -ad8 mydemad8.tif -src mydemsrc1.tif -o outlet.shp -ord mydemord.tif -tree mydemtree.txt -coord mydemcoord.txt -net mydemnet.shp -w mydemw.tif")
plot(raster("mydemord.tif"))
zoom(raster("mydemord.tif"))
plot(raster("mydemw.tif"))
plot(approxpt,add=T,col="blue")
zoom(raster("mydemw.tif"),ext=zoomext)

#here
# Plot streams using stream order as width
snet=read.shapefile("mydemnet")
ns=length(snet$shp$shp)
for(i in 1:ns){
  lines(snet$shp$shp[[i]]$points,lwd=snet$dbf$dbf$Order[i])
}

# Peuker Douglas stream definition
system("mpiexec -n 2 peukerdouglas -fel mydemfel.tif -ss mydemss.tif")
ss=raster("mydemss.tif")
plot(ss)
zoom(ss)

#  Accumulating candidate stream source cells
system("mpiexec -n 2 aread8 -p mydemp.tif -o outlet.shp -ad8 mydemssa.tif -wg mydemss.tif")
ssa=raster("mydemssa.tif")
plot(ssa)

#  Drop Analysis
system("mpiexec -n 2 dropanalysis -p mydemp.tif -fel mydemfel.tif -ad8 mydemad8.tif -ssa mydemssa.tif -drp mydemdrp.txt -o outlet.shp -par 5 500 10 0")

# Deduce that the optimal threshold is 300 
# Stream raster by threshold
system("mpiexec -n 2 threshold -ssa mydemssa.tif -src mydemsrc2.tif -thresh 300")
plot(raster("mydemsrc2.tif"))
zoom(raster("mydemsrc2.tif"))
# Stream network
system("mpiexec -n 2 streamnet -fel mydemfel.tif -p mydemp.tif -ad8 mydemad8.tif -src mydemsrc2.tif -ord mydemord2.tif -tree mydemtree2.dat -coord mydemcoord2.dat -net mydemnet2.shp -w mydemw2.tif -o outlet.shp",show.output.on.console=F,invisible=F)

plot(raster("mydemw2.tif"))
snet=read.shapefile("mydemnet2")
ns=length(snet$shp$shp)
for(i in 1:ns){
  lines(snet$shp$shp[[i]]$points,lwd=snet$dbf$dbf$Order[i])
}

# Wetness Index
system("mpiexec -n 2 slopearearatio -slp mydemslp.tif -sca mydemsca.tif -sar mydemsar.tif", show.output.on.console=F, invisible=F)
sar=raster("mydemsar.tif")
wi=sar
wi[,]=-log(sar[,])
plot(wi)
zoom(wi)

# Distance Down
system("mpiexec -n 2 dinfdistdown -ang mydemang.tif -fel mydemfel.tif -src mydemsrc2.tif -m ave v -dd mydemdd.tif",show.output.on.console=F,invisible=F)
plot(raster("mydemdd.tif"))


#masking rasters for plots
#new extent for masked dems
extent <- extent(587500, 595000, 4125000, 4135000)
mask <- raster("mydemw.tif") 
fel_mask <- mask(fel,mask)
fel_mask <- crop(fel_mask,extent)
diff_mask <- mask(diff,mask)
diff_mask <- crop(diff_mask,extent)
plot(diff_mask)

fel_df_mask <- as.data.frame(fel_mask,xy=T)
diff_df_mask <- as.data.frame(diff_mask,xy=T)

fel_plot <- ggplot()+
  geom_raster(data=fel_df_mask,aes(x=x,y=y,fill=mydemfel))+
  scale_fill_viridis_c(name="Elevation (ft)")+
  labs(title="Lick Run Basin Filled DEM",x=element_blank(),y=element_blank())
diff_plot <- 
  ggplot()+
  geom_raster(data=diff_df_mask,aes(x=x,y=y,fill=layer))+
  scale_fill_viridis_c(name="Elevation Difference (ft)",option = "H")+
  labs(title="Difference between DEM and Filled DEM",x=element_blank(),y=element_blank())

fel_plot+diff_plot
