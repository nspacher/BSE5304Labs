#HW2
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,patchwork,rnoaa)
pacman::p_load(operators,topmodel,DEoptim,soilDB,sp,curl,httr,
               rnoaa,raster,shapefiles,rgdal,elevatr,terra,progress,lubridate)
system("git config --global user.email 'nspacher@vt.edu' ") 
system("git config --global user.name 'Nick Bentelspacher' ")

pacman::p_load(EcoHydRology)

#Bonhomme Creek near Clarkson Valley, MO; USGS 06935770

#gage point setup for pour pt 
latlon <- cbind(-90.6185,38.65844444)
gagepoint_ll <- SpatialPoints(latlon)
proj4_ll = "+proj=longlat"
proj4string(gagepoint_ll)=proj4_ll
proj4_utm = paste0("+proj=utm +zone=", trunc((180+-90.6185)/6+1), " +datum=WGS84 +units=m +no_defs")
crs_utm <- CRS(proj4_utm)
gagepoint_utm <- spTransform(gagepoint_ll,crs_utm)

#bbox for terrain search
flowgage_area <- 11.3
sqrt(flowgage_area*8)
searchdist=sqrt(flowgage_area*8)*1000 
pourpoint=SpatialPoints(gagepoint_utm@coords,proj4string = crs_utm)
bboxpts=gagepoint_utm@coords
bboxpts=rbind(bboxpts,bboxpts+searchdist)
bboxpts=rbind(bboxpts,bboxpts-searchdist)
bboxpts
bboxpts=rbind(bboxpts,c(min(bboxpts[,1]),max(bboxpts[,2])))
bboxpts=rbind(bboxpts,c(max(bboxpts[,1]),min(bboxpts[,2])))
bboxpts
bboxpts=SpatialPoints(bboxpts,proj4string = crs_utm)

#DEM from aws
bonhomme=get_aws_terrain(locations=bboxpts@coords, 
                      z = 12, prj = proj4_utm,src ="aws",expand=1)

setwd(datadir)
writeRaster(bonhomme,filename = "bonhomme.tif",overwrite=T)

#Watershed Delineation

z=raster("bonhomme.tif")
plot(z)

# Pitremove
system("mpiexec -n 2 pitremove -z bonhomme.tif -fel bonhommefel.tif")
fel=raster("bonhommefel.tif")
plot(fel)


# D8 flow directions
system("mpiexec -n 2 d8flowdir -p bonhommep.tif -sd8 bonhommesd8.tif -fel bonhommefel.tif",show.output.on.console=F,invisible=F)
p=raster("bonhommep.tif")
plot(p)
sd8=raster("bonhommesd8.tif")
plot(sd8)

# Contributing area
system("mpiexec -n 2 aread8 -p bonhommep.tif -ad8 bonhommead8.tif")
ad8=raster("bonhommead8.tif")
plot(log(ad8))
zoom(log(ad8))


# Grid Network 
system("mpiexec -n 2 gridnet -p bonhommep.tif -gord bonhommegord.tif -plen bonhommeplen.tif -tlen bonhommetlen.tif")
gord=raster("bonhommegord.tif")
plot(gord)
zoom(gord)

# DInf flow directions
system("mpiexec -n 2 dinfflowdir -ang bonhommeang.tif -slp bonhommeslp.tif -fel bonhommefel.tif",show.output.on.console=F,invisible=F)
ang=raster("bonhommeang.tif")
plot(ang)
slp=raster("bonhommeslp.tif")
plot(slp)


# Dinf contributing area
system("mpiexec -n 2 areadinf -ang bonhommeang.tif -sca bonhommesca.tif")
sca=raster("bonhommesca.tif")
plot(log(sca))
zoom(log(sca))

# Threshold
system("mpiexec -n 2 threshold -ssa bonhommead8.tif -src bonhommesrc.tif -thresh 100")
src=raster("bonhommesrc.tif")
plot(src)
zoom(src)

outlet=SpatialPointsDataFrame(gagepoint_utm,
                              data.frame(Id=c(1),outlet=paste("outlet",1,sep="")))
writeOGR(outlet,dsn=".",layer="approxoutlets",
         driver="ESRI Shapefile", overwrite_layer=TRUE)


# Move Outlets
system("mpiexec -n 2 moveoutletstostrm -p bonhommep.tif -src bonhommesrc.tif -o approxoutlets.shp -om outlet.shp")

approxpt=readOGR("approxoutlets.shp")
plot(approxpt,add=T,col="blue")

outpt=read.shp("outlet.shp")
approxpt=read.shp("approxoutlets.shp")

plot(src)
points(outpt$shp[2],outpt$shp[3],pch=19,col=2)
points(approxpt$shp[2],approxpt$shp[3],pch=19,col=4)

zoom(src)

# Contributing area upstream of outlet
system("mpiexec -n 2 aread8 -p bonhommep.tif -o outlet.shp -ad8 bonhommessa.tif")
ssa=raster("bonhommessa.tif")
plot(ssa)
zoom(ssa)


# Threshold
system("mpiexec -n 2 threshold -ssa bonhommessa.tif -src bonhommesrc1.tif -thresh 2000")
src1=raster("bonhommesrc1.tif")
plot(src1)
zoom(src1)

# Stream Reach and Watershed
system("mpiexec -n 2 streamnet -fel bonhommefel.tif -p bonhommep.tif -ad8 bonhommead8.tif -src bonhommesrc1.tif -o outlet.shp -ord bonhommeord.tif -tree bonhommetree.txt -coord bonhommecoord.txt -net bonhommenet.shp -w bonhommew.tif")
plot(raster("bonhommeord.tif"))
zoom(raster("bonhommeord.tif"))
plot(raster("bonhommew.tif"))
plot(approxpt,add=T,col="blue")
zoom(raster("bonhommew.tif"),ext=zoomext)


#masking rasters for plots
b_extent <- extent(700000, 709000, 4270000, 4282500) #new extent for masked dems
mask <- raster("bonhommew.tif") 
fel_mask <- mask(fel,mask)
fel_mask <- crop(fel_mask,b_extent)

diff <- fel-z
diff_df <- as.data.frame(diff, xy=T)
diff_mask <- mask(diff,mask)
diff_mask <- crop(diff_mask,b_extent)
plot(diff_mask)

fel_df_mask <- as.data.frame(fel_mask,xy=T)
diff_df_mask <- as.data.frame(diff_mask,xy=T)

fel_plot <- ggplot()+
  geom_raster(data=fel_df_mask,aes(x=x,y=y,fill=bonhommefel))+
  scale_fill_viridis_c(name="Elevation (ft)")+
  labs(title="Bonhomme Creek near Clarkson Valley, MO Watershed Filled DEM",x=element_blank(),y=element_blank())
diff_plot <- 
  ggplot()+
  geom_raster(data=diff_df_mask,aes(x=x,y=y,fill=layer))+
  scale_fill_viridis_c(name="Elevation Difference (ft)",option = "H")+
  labs(title="Difference between DEM and Filled DEM",x=element_blank(),y=element_blank())

fel_plot+diff_plot




