#
# Since everything depends on the libraries you install
# it is worthwhile loading them at the beginning
#
# A useful function to make sure you don't have anything residually attached
# from your last session.
search()
# Or to see what might be attached
intersect(search(), objects())
objects()  # This will list the objects you have.
rm(list=objects()) # Removes ALL the objects… so be careful here.

# What is going to change from use case to use case 
Sys.getenv('USER')
LabNo="/Lab09"
#
# What needs to be loaded
#
if (!require("pacman")) install.packages("pacman")
myhomedir=Sys.getenv("HOME")
datadir=paste0(myhomedir,"/data",LabNo)
dir.create(datadir,recursive = T)
srcdir=paste0(myhomedir,"/src")
dir.create(srcdir,recursive = T)
# Setting the directory for where the GitHub project exists. 
# This depends on where you set up your git, and what you called it locally, 
# but when you start a new git project, it will be the first directory you 
# are placed in... or if later in the project:
# 
mygitdir=rstudioapi::getActiveProject()
mypdfdir=paste0(mygitdir,"/pdfs",LabNo)
dir.create(mypdfdir)
# 
setwd(mygitdir)
system("git config --global user.email 'nspacher@vt.edu' ") 
system("git config --global user.name 'Nick Bentelspacher' ")
system("git config pull.rebase false")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr,EcoHydRology,curl,elevatr,raster,rgdal,
                 data.table,foreign,maptools,dataRetrieval,gdistance,tidyverse)
setwd(datadir)
#
url="https://help.waterdata.usgs.gov/parameter_cd?group_cd=%"
browseURL(url)

# Set the Manning Coefficient in the USGS Gage's Site Table
#
#url="https://www.google.com/search?q=manning%27s+n+for+stream"
#browseURL(url)
#url="https://www.fsl.orst.edu/geowater/FX3/help/8_Hydraulic_Reference/Mannings_n_Tables.htm"
#browseURL(url)
##############################################
# Function for retrieving USGS data and storing in a list
##############################################
make_usgs_gage_list <- function(
siteNo = "0205551460",
parameterCd = c("00060","00065"),
start.date = "2017-05-01",  # Not frozen to not frozen
end.date = "2017-11-01" )   # to still not frozen
{
  # For each gage location, let's keep the data organized as a 
  # list.
  USGS=list()   # Organize the data in a nice list as in previous labs
  USGS[["flowdata"]]<- readNWISuv(siteNumbers = siteNo,parameterCd = parameterCd,startDate = start.date,endDate = end.date)
  
  # And of course we want to work in SI units so:
  USGS$flowdata$depth_m=USGS$flowdata$X_00065_00000*0.3048
  # m/ft depth
  USGS$flowdata$cms=USGS$flowdata$X_00060_00000*.02832
  # m3/ft3 flow
  #
  # Let's add in the USGS gage site information to the list and inspect
  USGS[["site"]]=readNWISsite(siteNo)
  head(USGS$site)
  class(USGS$site$dec_lat_va)
  #
  
  USGS$site$man_n=.035/1.49
  #
  # Create a SpatialPointsDataFrame out of the site dataframe in the USGS list
  coordinates(USGS$site)=~dec_long_va+dec_lat_va
  return(USGS)
}

USGS02056000=make_usgs_gage_list(siteNo = "02056000") #Roanoke River at Niagra, bottom gage
USGS0205551460=make_usgs_gage_list(siteNo ="0205551460") #Lick run
USGS02055100=make_usgs_gage_list(siteNo ="02055100") #Tinker Creek
USGS02055000=make_usgs_gage_list(siteNo ="02055000") #Roanoke River at Roanoke
USGS02054530=make_usgs_gage_list(siteNo ="02054530") #Roanoke River at Glenvar

#graduate student hw gauges
USGS08279500 <- make_usgs_gage_list(siteNo = "08279500") #Rio Grande at Embudo, bottom gage
USGS08279000 <- make_usgs_gage_list(siteNo = "08279000") #Embudo Creek at Dixon
USGS08276500 <- make_usgs_gage_list(siteNo = "08276500") #Rio Grande below Taos Junction
USGS08276300 <- make_usgs_gage_list(siteNo = "08276300") #Rio Pueblo De Taos below Los Cordovas

###############
#DEM Data
###############
ab_ll=rbind(USGS08279500$site,
              USGS08279000$site,
              USGS08276500$site,
              USGS08276300$site)
class(ab_ll)
ab_ll@proj4string
proj4_utm = paste0("+proj=utm +zone=",
                     trunc((180+coordinates(USGS08279000$site)[1])/6+1), 
                     " +datum=WGS84 +units=m +no_defs")
print(proj4_utm)
# Lat/Lon (_ll) is much easier!
proj4_ll = "+proj=longlat"
crs_ll=CRS(proj4_ll)
crs_utm=CRS(proj4_utm)
proj4string(ab_ll)=proj4_ll
ab_utm=spTransform(ab_ll,crs_utm)
ab_utm@coords
mydem=get_aws_terrain(locations=ab_utm@coords, 
                        z = 12, prj = proj4_utm,expand=1)
#
# Lets plot the DEM and the gage locations so we can guess 
# what gages connect with what gages
#
plot(mydem)
plot(ab_utm,add=T)
text(ab_utm, labels=ab_utm@data$site_no, cex=0.6, font=2,pos=1)
# Streams as USGS sees them, I know I can get an overview of streams with the 
# USGS H
url="https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/Shape/NHD_H_03010101_HU8_Shape.zip"
curl_download(url,"NHD_H_03010101_HU8_Shape.zip")
unzip("NHD_H_03010101_HU8_Shape.zip",exdir="03010101")
streams=readOGR("03010101/Shape/NHDFlowline.dbf")
streams_utm=spTransform(streams,crs_utm)
plot(streams_utm,col="blue",add=T)


##################
# Breaking things for educational purposes
View(USGS02056000$flowdata)
USGS02056000$flowdata=USGS02056000$flowdata[,c(1,2,3,4,5,8,10)]
View(USGS02056000$flowdata)
# Oh Noooooo!!!! This gage for some reason doesn't have "Gage height"
# 00065! What can we do!?!? OK, no worries, we do have "Discharge" 00060	

#rating curve for gage
USGS02056000[["rating"]]=readNWISrating(USGS02056000$site$site_no)
plot(USGS02056000$rating$DEP,USGS02056000$rating$INDEP,xlab="DEP",ylab="INDEP")

# Note that this is very similar to what we saw in the previous gage's results
# and as it turns out, we can use it to estimate a 00065 measurement as 
# we did for the previous gage.
USGS02056000$flowdata$X_00065_00000=approx(USGS02056000$rating$DEP,
                                             USGS02056000$rating$INDEP, xout = USGS02056000$flowdata$X_00060_00000, ties = min)$y
points(USGS02056000$flowdata$X_00060_00000,USGS02056000$flowdata$X_00065_00000,
         col="red")
#
USGS02056000$flowdata$depth_m=USGS02056000$flowdata$X_00065_00000*0.3048 # m/ft depth


###############################################
#####   HW1
###############################################
#fixing rating curve for lick run gage
USGS0205551460[["rating"]]=readNWISrating(USGS0205551460$site$site_no)
USGS0205551460$flowdata$depth_ft=approx(USGS0205551460$rating$DEP,
                                           USGS0205551460$rating$INDEP, xout = USGS0205551460$flowdata$X_00060_00000, ties = min)$y

View(USGS0205551460$flowdata)
USGS0205551460$flowdata$depth_m=USGS0205551460$flowdata$depth_ft*0.3048 # m/ft depth
plot(USGS0205551460$flowdata$cms, USGS0205551460$flowdata$depth_m)


#####################################
#from gdistance package
# A quick readthrough of the Example 1: Hiking around Maunga Whau
# in the package vignette. 
# vignette("Overview", package = "gdistance")
# Set the starting and ending locations
# determine the river reach length and slope using the gdistance package.
#
A=SpatialPoints(USGS08276300$site)# Up gradient 
B=SpatialPoints(USGS08279500$site) # Down gradient site 
proj4string(A)=proj4_ll
proj4string(B)=proj4_ll
A_utm=spTransform(A,crs_utm)
B_utm=spTransform(B,crs_utm)
# Cut the DEM down to a more manageable size
cropmydem=crop(mydem,extend(extent(ab_utm),600))
cropmydem=trim(cropmydem)
cropmydem=cropmydem*1000.0
plot(cropmydem)
plot(ab_utm,add=T)
# Set up the weighting functions
altDiff <- function(x){x[2] - x[1]}
hd <- transition(cropmydem, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd)
adj <- adjacent(cropmydem, cells=1:ncell(cropmydem), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)
# Find and plot the flow path
AtoB <- shortestPath(Conductance, A_utm, B_utm, output="SpatialLines")
plot(AtoB,add=T)
plot(streams_utm,col="blue",add=T)
plot(AtoB,add=T)
SpatialLinesLengths(AtoB)
USGS08276300$site$L=SpatialLinesLengths(AtoB) # km to m
USGS08276300$site$L # reach length in m
#
#
# Getting slope, we will extract the slope for points A and B from the DEM and # divide the difference by the length in m, this gives us a much better 
# estimate of slope than taking the point slopes at the gage site
#
USGS08276300$site$slope=(raster::extract(mydem,A_utm)-
                               raster::extract(mydem,B_utm))/USGS08276300$site$L
USGS08276300$site$slope

# ck
USGS08276300$flowdata$ck = 5/3*(sqrt(USGS08276300$site$slope)/USGS08276300$site$man_n)*
  USGS08276300$flowdata$depth_m^(2/3)
  # ANS
  mean(USGS08276300$flowdata$ck,na.rm=T)
# [1] 2.547238 for this example, confirm this result
USGS08276300$flowdata$dt = USGS08276300$site$L/USGS08276300$flowdata$ck
  mean(USGS08276300$flowdata$dt,na.rm=T)
# [1] 6328.655  for this example, confirm this result

plot(USGS08276300$flowdata$dateTime,USGS08276300$flowdata$dt)
USGS08276300$flowdata$outTime=USGS08276300$flowdata$dateTime+USGS08276300$flowdata$dt

plot(USGS02055100$flowdata$dateTime,USGS02055100$flowdata$dt,main="tinker") #tinker
plot(USGS02054530$flowdata$dateTime,USGS02054530$flowdata$dt,main="glenvar") #glenvar
plot(USGS02055000$flowdata$dateTime,USGS02055000$flowdata$dt,main="roanoke") #roanoke
#test celerity function
#test_celerity <- celerity(Up_gage = USGS0205551460, Down_gage = USGS02056000)



# Find the beginning of  Waves assuming a new wave starts at 110% of prior 
# flow. This might need to change for your homework
WaveStartDecPercent=1.1
USGS08276300$flowdata$newwave=
  USGS08276300$flowdata$cms *WaveStartDecPercent < data.table::shift(USGS08276300$flowdata$cms)
summary(USGS08276300$flowdata$newwave)
# Add plot of the point found
len=length(USGS08276300$flowdata$newwave)
USGS08276300$flowdata$newwave[is.na(USGS08276300$flowdata$newwave)]=F
# Removes repeated finds by going through loop backwards
for (i in seq(len,2)){
  print(i)
  if(USGS08276300$flowdata$newwave[i]==T &
     USGS08276300$flowdata$newwave[i-1]==T){
    USGS08276300$flowdata$newwave[i]=F
  }
}
plot(USGS08276300$flowdata$dateTime,USGS08276300$flowdata$cms,type="l")
points(USGS08276300$flowdata$dateTime[USGS08276300$flowdata$newwave],
         USGS08276300$flowdata$cms[USGS08276300$flowdata$newwave],col=2)

# Find the time locations where waves begin
which(USGS08276300$flowdata$newwave == TRUE)

#Tinker Creek plot
plot(USGS02055100$flowdata$dateTime,USGS02055100$flowdata$cms,
       type="l",xlim=c(USGS02055100$flowdata$dateTime[400],
                       USGS02055100$flowdata$dateTime[414+50]),ylab="Discharge (m^3/s)",xlab="Date Time",main="USGS 02055100 TINKER CREEK NEAR DALEVILLE, VA")
lines(USGS02055100$flowdata$outTime,USGS02055100$flowdata$cms,col=2)

#Glenvar plot
plot(USGS02054530$flowdata$dateTime,USGS02054530$flowdata$cms,
     type="l",xlim=c(USGS02054530$flowdata$dateTime[420],
                     USGS02054530$flowdata$dateTime[467+200]),ylab="Discharge (m^3/s)",xlab="Date Time",main="USGS 02054530 ROANOKE RIVER AT GLENVAR, VA")
lines(USGS02054530$flowdata$outTime,USGS02054530$flowdata$cms,col=2)

#Roanoke at Roanoke plot
plot(USGS02055000$flowdata$dateTime,USGS02055000$flowdata$cms,
     type="l",xlim=c(USGS02055000$flowdata$dateTime[16934],
                     USGS02055000$flowdata$dateTime[16934+75]),ylab="Discharge (m^3/s)",xlab="Date Time",main="USGS 02055000 ROANOKE RIVER AT ROANOKE, VA")
lines(USGS02055000$flowdata$outTime,USGS02055000$flowdata$cms,col=2)


#lengths
USGS02055100$site$L #tinker
USGS02054530$site$L #glenvar
USGS02055000$site$L #roanoke

#slope
USGS02055100$site$slope #tinker
USGS02054530$site$slope #glenvar
USGS02055000$site$slope #roanoke

#dt 
mean(USGS02055100$flowdata$dt) #tinker
mean(USGS02054530$flowdata$dt) #glenvar
mean(USGS02055000$flowdata$dt,na.rm = T) #roanoke

#Ck
mean(USGS02055100$flowdata$ck,na.rm = T) #tinker
mean(USGS02054530$flowdata$ck) #glenvar
mean(USGS02055000$flowdata$ck,na.rm = T) #roanoke


#GS hw plots
#Embudo Ck at Dixon
plot(USGS08279000$flowdata$dateTime,USGS08279000$flowdata$cms,
     type="l",xlim=c(USGS08279000$flowdata$dateTime[12820],
                     USGS08279000$flowdata$dateTime[12820+50]),
     ylab="Discharge (m^3/s)",xlab="Date Time",main="USGS 08279000 Embudo Creek at Dixon, NM",
     ylim=c(0,4))
lines(USGS08279000$flowdata$outTime,USGS08279000$flowdata$cms,col=2)

#Rio Grande below Taos Junction
plot(USGS08276500$flowdata$dateTime,USGS08276500$flowdata$cms,
     type="l",xlim=c(USGS08276500$flowdata$dateTime[11595],
                     USGS08276500$flowdata$dateTime[11600+40]),
     ylab="Discharge (m^3/s)",xlab="Date Time",main="USGS 08276500 Rio Grande Below Taos Junction, NM",
     ylim=c(0,30))
lines(USGS08276500$flowdata$outTime,USGS08276500$flowdata$cms,col=2)

#Rio Pueblo De Taos below Los Cordovas,NM 08276300
plot(USGS08276300$flowdata$dateTime,USGS08276300$flowdata$cms,
     type="l",xlim=c(USGS08276300$flowdata$dateTime[14550],
                     USGS08276300$flowdata$dateTime[14550+100]),
     ylab="Discharge (m^3/s)",xlab="Date Time",main="USGS 08276300 Rio Pueblo De Taos Below Los Cordovas, NM",
     ylim=c(0,4))
lines(USGS08276300$flowdata$outTime,USGS08276300$flowdata$cms,col=2)

#lengths
USGS08279000$site$L #embudo
USGS08276500$site$L #taos
USGS08276300$site$L #cordovas

#slope
USGS08279000$site$slope #embudo
USGS08276500$site$slope #taos
USGS08276300$site$slope #cordovas

#Ck
mean(USGS08279000$flowdata$ck,na.rm = T)
mean(USGS08276500$flowdata$ck)
mean(USGS08276300$flowdata$ck,na.rm = T) 

#dt
mean(USGS08279000$flowdata$dt,na.rm = T)
mean(USGS08276500$flowdata$dt)
mean(USGS08276300$flowdata$dt,na.rm = T) 
