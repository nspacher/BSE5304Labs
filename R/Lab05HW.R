#lab05 HW

# 
# Since everything depends on the libraries you install
# it is worthwhile loading them at the beginning
#
objects()  # This will list the objects you have.
rm(list=objects()) # Removes ALL the objects… so be careful here.
#
# What is going to change from use case to use case 
LabNo="/Lab05"
myflowgage_id="0205551460"
#
# What needs to be loaded
#
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,patchwork,rnoaa)
pacman::p_load(operators,topmodel,DEoptim,soilDB,sp,curl,httr,
               rnoaa,raster,shapefiles,rgdal,elevatr,terra,progress,lubridate)
#
# Getting our organization on for where we want to put
# Data, external programs, and our project files.
# Things are going to get messy if we don't start issolating
# our data files by Lab
#
myhomedir=Sys.getenv("HOME")
datadir=paste0(myhomedir,"/data",LabNo)
dir.create(datadir,recursive = T)
srcdir=paste0(myhomedir,"/src")
dir.create(srcdir,recursive = T)
# Setting the directory for where the GitHub project exists. 
# This depends on where you set up your git, and what you called it locally, 
# but when you start a new git project, it will be the first directory you 
# are placed in... or if later in the project:
# WOOO HOOO... took me a few hours to find this function!
# 
mygitdir=rstudioapi::getActiveProject()
mypdfdir=paste0(mygitdir,"/pdfs",LabNo)
dir.create(mypdfdir)
# 
setwd(mygitdir)
system("git config --global user.email 'nspacher@vt.edu' ") 
system("git config --global user.name 'Nick Bentelspacher' ")
system("git config pull.rebase false")
#
# This week, we discovered some "features" that make removing and 
# re-installing the EcoHydrology Library necessary.
#
setwd(srcdir)
#detach("package:EcoHydRology", unload = TRUE)
# remove.packages("EcoHydRology", lib="~/R/x86_64-pc-linux-gnu-library/4.2")
#system("svn checkout svn://scm.r-forge.r-project.org/svnroot/ecohydrology/"); 
#install.packages(c("ecohydrology/pkg/EcoHydRology/"),repos = NULL)
pacman::p_load(EcoHydRology)
#install.packages(c("ecohydrology/pkg/SWATmodel/"),repos = NULL)
pacman::p_load(SWATmodel)


setwd(datadir)
#
# Should we do a gage that is easy, or deal with some reality?
#
myflowgage=get_usgs_gage(myflowgage_id,begin_date = "2015-01-01",
                         end_date = "2022-03-01")

#
# This is where some folks had issues... they forgot to check their 
# watershed areas per the homework... though there were ways to fix
# it later with lower resolution DEM pull
#
print(paste0("reported Area ",myflowgage$area))
# For most watershed modelling purposes we normalize Q in mm/day for basins
myflowgage$flowdata$Qmm = myflowgage$flowdata$flow/myflowgage$area/10^3

# In the Lab02, we introduced you to a way to quickly get your WX Data 
# for any location in the world way easier than traditional download and
# parsing methods most old people use.
#
source("https://raw.githubusercontent.com/Rojakaveh/FillMissWX/main/FillMissWX.R")
# Remove this if it syncs with the EcoHydrology Version.
WXData=FillMissWX(declat=myflowgage$declat, declon=myflowgage$declon,
                  StnRadius=30,minstns=10,date_min="2010-01-01",
                  date_max="2023-02-01",targElev=myflowgage$elev,
                  method = "IDW",alfa=2)

BasinData=merge(WXData,myflowgage$flowdata,by.x="date",by.y="mdate")
TMWB=BasinData

#Load TMWB and CN functions
source("https://raw.githubusercontent.com/nspacher/BSE5304Labs/main/R/TMWBmodel.R")
source("https://raw.githubusercontent.com/nspacher/BSE5304Labs/main/R/CNModel.R")

TMWB_df <- TMWBmodel(TMWB, fc=0.45, wp=0.1, z=1000, fcres=0.3587, SFTmp = 1, bmlt6 = 1.4, bmlt12 = 0, Tmlt = 3, Tlag=1)
ggplot(TMWB_df, aes(date, Qpred, color = "Qpred"))+
  geom_line()+
  geom_line(aes(y=Qmm, color="Qmm"))+
  labs(title=NSE(Yobs = TMWB_df$Qmm, Ysim = TMWB_df$Qpred))


TMWBopt <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  x3 <- x[3]
  x4 <- x[4]
  x5 <- x[5]
  x6 <- x[6]
  x7 <- x[7]
  x8 <- x[8]
  x9 <- x[9]
  outTMWB <- TMWBmodel(TMWBdf=TMWB, fc= x1, wp=x2, z=x3, fcres=x4, SFTmp=x5, bmlt6=x6, bmlt12=x7, Tmlt=x8, Tlag=x9)
  return(1-NSE(Yobs = outTMWB$Qmm, Ysim = outTMWB$Qpred))
}

CNopt <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  x3 <- x[3]
  x4 <- x[4]
  x5 <- x[5]
  x6 <- x[6]
  x7 <- x[7]
  outCN <- CNModel(BasinData, CNavg=x1, IaFrac=x2, fnc_slope = x3, fnc_aspect = x4, func_DAWC = x5, func_z = x6, fnc_fcres = x7)
  return(1-NSE(Yobs = outCN$Qmm, Ysim = outCN$Qpred))
}

#TMWB lower and upper vectors
tlower <- c(0.09, 0.05, 300, 0.1, 1, 0.1, 0.1, 1, 0.1)
tupper <- c(0.45, 0.22, 3000, 0.95, 6, 5, 5, 3, 1)

outDEoptim <- DEoptim(TMWBopt, tlower, tupper, DEoptim.control(NP = 90,
                                                             itermax = 100, F = 1.2, CR = 0.7))


plot(outDEoptim$member$bestvalit,col="black")

plot(outDEoptim)
bestmemit <- as.data.frame(outDEoptim$member$bestmemit)
bestmemit$iteration = c(1:100)
bestmemit <- rename(bestmemit, "fc"=par1, "wp"=par2, "z"=par3, "fcres"=par4,"SFTmp"=par5,"bmlt6"=par6,"bmlt7"=par7,"Tmlt"=par8, "Tlag"=par9)
ggplot(bestmemit, aes(iteration, Tlag))+
  geom_point()

CNout <- CNModel(BasinData, CNavg=75, IaFrac=0.1, fnc_slope=0, fnc_aspect=0, func_DAWC=0.3, func_z=1000, fnc_fcres=0.3)
#CN lower and upper vecctors
CNlower <- c(70,0.05,0,0,0.2,1000,0.1)
CNupper <- c(80,0.2,1,360,0.4,3000,0.95)

CNoutDEoptim <- DEoptim(CNopt, CNlower, CNupper, DEoptim.control(NP = 70,
                                                               itermax = 100, F = 1.2, CR = 0.7))

plot(CNoutDEoptim$member$bestvalit,col="black")
plot(CNoutDEoptim)

CNpars <- as.data.frame(CNoutDEoptim$member$bestmemit)
CNpars <- rename(CNpars, "CNavg"=par1, "IaFrac"=par2, "slope"=par3, "aspect"=par4, "DAWC"=par5,"z"=par6,"fcres"=par7)
CNpars$iterations=c(1:100)
ggplot(CNpars, aes(iterations, fcres))+geom_point()
