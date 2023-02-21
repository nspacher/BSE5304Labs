#Lab04 HW 

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,patchwork,rnoaa)
pacman::p_load(operators,topmodel,DEoptim,soilDB,sp,curl,httr,
               rnoaa,raster,shapefiles,rgdal,elevatr,terra,progress,lubridate)
pacman::p_load(EcoHydRology)

#directories
myhomedir=Sys.getenv("HOME")
datadir=paste0(myhomedir,"/data",LabNo)
dir.create(datadir,recursive = T)
srcdir=paste0(myhomedir,"/src")
dir.create(srcdir,recursive = T)

mygitdir=rstudioapi::getActiveProject()
mypdfdir=paste0(mygitdir,"/pdfs",LabNo)
dir.create(mypdfdir)
# 
setwd(mygitdir)
system("git config --global user.email 'nspacher@vt.edu' ") 
system("git config --global user.name 'Nick Bentelspacher' ")
system("git config pull.rebase false")

setwd(datadir)

#Basin Data gathering 
myflowgage_id="14138870" #FIR CREEK NEAR BRIGHTWOOD, OR
myflowgage=get_usgs_gage(myflowgage_id,begin_date = "2015-01-01",
                         end_date = "2019-01-01")

#normalize Q in mm/day
myflowgage$flowdata$Qmm = myflowgage$flowdata$flow/myflowgage$area/10^3

#weather data
WXData=FillMissWX(declat=myflowgage$declat, declon=myflowgage$declon,
                  StnRadius=30,minstns=10,date_min="2010-01-01",
                  date_max="2023-02-01",targElev=1,
                  method = "IDEW",alfa=2)

BasinData=merge(WXData,myflowgage$flowdata,by.x="date",by.y="mdate")

TMWB <- BasinData

source("https://raw.githubusercontent.com/nspacher/BSE5304Labs/main/R/TMWBFuncs.R") #soil wetting functions
source("https://raw.githubusercontent.com/nspacher/BSE5304Labs/main/R/TISnow.R") #Temperature Index snow model function
source("https://raw.githubusercontent.com/nspacher/BSE5304Labs/main/R/TMWBmodel.R") #TMWB model function 

#Modeling snow
SNO_df=TISnow(TMWB,SFTmp=1,bmlt6=1.4,bmlt12=0.0,Tmlt=3,Tlag=1)
TMWB$SNO=SNO_df$SNO
TMWB$SNOmlt=SNO_df$SNOmlt
TMWB$SNOfall=SNO_df$SNOfall
TMWB$Tsno=SNO_df$Tsno

#TMWB modeling
TMWBmodel(TMWB, fc=0.45, wp=0.1, z=1000, fcres=0.3587)


