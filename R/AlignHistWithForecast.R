if (!require("pacman")) install.packages("pacman")
pacman::p_load(meteoForecast)

gfsvarstemp <- grepVar('temp', service='gfs', complete = T)
testDay <- Sys.Date() - 1

library(lattice)

# url="https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/Lab04.R"
# download.file(url,"Lab04_10.R")
# file.edit("Lab04_10.R")

today=Sys.Date()
myflowgage_id="0205551460"  # Old Friendly Gage
myflowgage=get_usgs_gage(myflowgage_id,begin_date = "2015-01-01",
                         end_date = today)


## Multiple variables
Precipvars <- getPoint(c(myflowgage$declon, myflowgage$declat), 
                 vars = "Total_precipitation_surface_Mixed_intervals_Accumulation", 
                 day = today, service='gfs')
GFSPrecip <- aggregate(Precipvars, as.Date(time(Precipvars)), sum) #in mm 

Tempvars <- getPoint(c(myflowgage$declon, myflowgage$declat), 
                       vars = "Temperature_surface", 
                       day = today, service='gfs')
GFSMaxTemp <- aggregate(Tempvars, as.Date(time(Tempvars)), max) #temp in kelvin 
GFSMinTemp <- aggregate(Tempvars, as.Date(time(Tempvars)), min)


plot(precip)

plot(GFSMinTemp,ylim=c(276,300))
lines(GFSMaxTemp)

myflowgage$flowdata$Qmm = myflowgage$flowdata$flow/myflowgage$area/10^3
source("https://raw.githubusercontent.com/Rojakaveh/FillMissWX/main/FillMissWX.R")
WXData=FillMissWX(declat=myflowgage$declat, declon=myflowgage$declon,
                  StnRadius=30,minstns=10,date_min="2010-01-01",
                  date_max=today,targElev=myflowgage$elev,
                  method = "IDW",alfa=2)
#
# Add Forecast data to WXdata
#
BasinData=merge(WXData,myflowgage$flowdata,by.x="date",by.y="mdate")


dir.create("~/pngs")
setwd("~/pngs")
graphdir="~/pngs"
png(paste0(graphdir,"/TempForecast.png"))
plot(GFSMinTemp,ylim=c(276,300))
lines(GFSMaxTemp)
dev.off()


#for calibrating CN model look at lab05 example using DEoptim