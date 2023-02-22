#Lab04 HW 

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,patchwork,rnoaa)
pacman::p_load(operators,topmodel,DEoptim,soilDB,sp,curl,httr,
               rnoaa,raster,shapefiles,rgdal,elevatr,terra,progress,lubridate)
pacman::p_load(EcoHydRology)
LabNo="/Lab04"
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
TMWB_df <- TMWBmodel(TMWB, fc=0.45, wp=0.1, z=1000, fcres=0.3587)
TMWB$AW=TMWB_df$AW
TMWB$Excess=TMWB_df$Excess
TMWB$dp=TMWB_df$dp
TMWB$ET=TMWB_df$ET
TMWB$S=TMWB_df$S
TMWB$Qpred=TMWB_df$Qpred

#test plots
Qplot <- ggplot(TMWB, aes(x=date))+
  geom_line(aes(y=Qmm,color="Observed"))+
  geom_line(aes(y=Qpred,color="Predicted"))+
  #geom_line(aes(y=Excess,color="Excess"))+
  scale_color_hue(name=element_blank())+
  labs(title = paste("TMWB Model; NSE =",signif(NSE(TMWB$Qmm,TMWB$Qpred),digits=4)),y="Streamflow (mm/day)",x=element_blank())
Qplot

P_plot <- ggplot(TMWB, aes(date))+
  geom_col(aes(y=Excess,fill="Excess"))+
  geom_col(aes(y=P, fill="Precipitation"))+
  geom_line(aes(y=ET,color="ET"))+
  scale_y_continuous(name="Depth of Water (mm)",
                     sec.axis=sec_axis(~.*(7), name="Available Soil Water (mm)")
  )+
  geom_line(aes(x=date,y=AW*(5/35),color="AW"))+
  scale_fill_manual(name=element_blank(), values=c("Precipitation"="blue", "Excess"="red"))+
  scale_color_manual(name=element_blank(),values=c("AW"="orange","ET"="darkgreen"))+
  labs(x=element_blank())
P_plot

Qplot/P_plot

#Storage Optimization for TMWB
#nse function
NSE <- function(Yobs,Ysim){
  return(1-sum((Yobs-Ysim)^2,na.rm=TRUE)/sum((Yobs-mean(Yobs, na.rm=TRUE))^2, na.rm=TRUE))
}

ggplot(TMWB, aes(dp, Qmm))+
  geom_point()

TMWB_S=TMWB[(month(TMWB$date) > 5 
                      & month(TMWB$date) < 11),]

attach(TMWB_S)
plot(dp,Qmm)
points(dp,dp^2/(dp+60),col="red")  #Best S estimate visually
points(dp,dp^2/(dp+45),col="blue")
NSE(Qmm,dp^2/(dp+60))

f <- function (x) {
  Sest=x
  NSE(Qmm,dp^2/(dp+Sest))
}
Sest <- optimize(f, c(30,60), tol = 0.0001,maximum = TRUE)$maximum
plot(dp,Qmm)
points(dp,dp^2/(dp+Sest),col="red") 
########
detach(TMWB_S)

#CN model
# CNavg = 75
# IaFrac = 0.05
# fnc_slope=0
# fnc_aspect=0
# func_DAWC=.3
# func_z=1000
# fnc_fcres=.3

source("https://raw.githubusercontent.com/nspacher/BSE5304Labs/main/R/CNModel.R") #CN model function
CN_JO <- CNModel(BasinData, CNavg = 75,IaFrac = 0.05,fnc_slope=0,fnc_aspect=0,func_DAWC=.3,func_z=1000,
                 fnc_fcres=.3)
# CN_JO$date <- BasinData$date
# CN_JO$Qmm <- BasinData$Qmm

CNQplot <- ggplot(CN_JO, aes(x=date))+
  geom_line(aes(y=Qmm,color="Observed"))+
  geom_line(aes(y=Qpred,color="Predicted"))+
  #geom_line(aes(y=Excess,color="Excess"))+
  scale_color_hue(name=element_blank())+
  labs(title = paste("CNModel; NSE =",signif(NSE(CN_JO$Qmm,CN_JO$Qpred),digits=4)),y="Streamflow (mm/day)",x=element_blank())
CNQplot

Qplot + CNQplot +plot_layout(guides='collect')


#x-y plots of predicted vs observed flow for both models
CNQvObs <- ggplot(CN_JO,aes(Qmm,Qpred))+
  geom_point()+
  geom_abline(intercept=0,slope=1,color="red")+
  labs(title="CN Model",x="Observed Streamflow (mm)",y="Predicted Streamflow (mm)")
TMWBQvObs <- ggplot(TMWB,aes(Qmm,Qpred))+
  geom_point()+
  geom_abline(intercept=0,slope=1,color="red")+
  labs(title="TMWB Model",x="Observed Streamflow (mm)",y="Predicted Streamflow (mm)")

TMWBQvObs+CNQvObs

#hydrographs 
ggplot(TMWB, aes(date))+
  geom_line(aes(y=Qpred,color="TMWB"))+
  geom_line(data=CN_JO,aes(y=Qpred, color="CN Model"))+
  scale_color_hue(name=element_blank())+
  labs(x=element_blank(),y="Predicted Daily Streamflow (mm)")


