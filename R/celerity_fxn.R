#kinematic wave function, not working
celerity <- function(Up_gage=USGS0205551460,
                     Down_gage=USGS02056000,
                     mydem=mydem){
#####################################
#from gdistance package
# A quick readthrough of the Example 1: Hiking around Maunga Whau
# in the package vignette. 
# vignette("Overview", package = "gdistance")
# Set the starting and ending locations
# determine the river reach length and slope using the gdistance package.
#
A=SpatialPoints(Up_gage$site)# Up gradient site Lick Run
B=SpatialPoints(Down_gage$site) # Down gradient site ROA River atNiagara
proj4string(A)=proj4_ll
proj4string(B)=proj4_ll
A_utm=spTransform(A,crs_utm)
B_utm=spTransform(B,crs_utm)
# Cut the DEM down to a more manageable size
#cropmydem=crop(mydem,extend(extent(ab_utm),600))
#cropmydem=trim(cropmydem)
#cropmydem=cropmydem*1000.0
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
Up_gage$site$L=SpatialLinesLengths(AtoB) # km to m
Up_gage$site$L # reach length in m
#
#
# Getting slope, we will extract the slope for points A and B from the DEM and # divide the difference by the length in m, this gives us a much better 
# estimate of slope than taking the point slopes at the gage site
#
Up_gage$site$slope=(extract(mydem,A_utm)-
                             extract(mydem,B_utm))/Up_gage$site$L
Up_gage$site$slope

# ck
Up_gage$flowdata$ck = 5/3*(sqrt(Up_gage$site$slope)/Up_gage$site$man_n)*
  Up_gage$flowdata$depth_m^(2/3)
# ANS
mean(Up_gage$flowdata$ck,na.rm=T)
# [1] 2.547238 for this example, confirm this result
Up_gage$flowdata$dt = Up_gage$site$L/Up_gage$flowdata$ck
mean(Up_gage$flowdata$dt,na.rm=T)
# [1] 6328.655  for this example, confirm this result

plot(Up_gage$flowdata$dateTime,Up_gage$flowdata$dt)
Up_gage$flowdata$outTime=Up_gage$flowdata$dateTime+Up_gage$flowdata$dt

return(Up_gage)
}
