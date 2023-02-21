#TMWB model
TMWBmodel <- function(TMWBdf, fc=0.45, wp=0.1, z=1000, fcres=0.3587){

#calculate PET
attach(TMWBdf)
TMWBdf$PET=PET_fromTemp(Jday=(1+as.POSIXlt(date)$yday),Tmax_C = MaxTemp,Tmin_C = MinTemp,
                      lat_radians = myflowgage$declat*pi/180) * 1000
detach(TMWBdf)


TMWBdf$AWC=(fc-wp)*z #
TMWBdf$dP = 0 # Initializing Net Precipitation
TMWBdf$ET = 0 # Initializing ET
TMWBdf$AW = 0 # Initializing AW
TMWBdf$Excess = 0 # Initializing Excess


# Loop to calculate AW and Excess
attach(TMWBdf)
for (t in 2:length(AW)){
  ET[t] = (AW[t-1]/AWC[t-1])*PET[t] # New Model
  dP[t] = P[t] - ET[t] + SNOmlt[t] - SNOfall[t] 
  if (dP[t]<=0) {
    values<-soildrying(AW[t-1],dP[t],AWC[t])
  } else if((dP[t]>0) & (AW[t-1]+dP[t])<=AWC[t]) {
    values<-soilwetting(AW[t-1],dP[t],AWC[t])
  } else {
    values<-soil_wetting_above_capacity(AW[t-1],dP[t],AWC[t])
  }
  AW[t]<-values[1]
  Excess[t]<-values[2]
  print(t)
}
TMWBdf$AW=AW
TMWBdf$Excess=Excess
TMWBdf$dP=dP
TMWBdf$ET=ET
rm(list=c("AW","dP","ET", "Excess"))
detach(TMWBdf) # IMPORTANT TO DETACH

# Calculate Watershed Storage and River Discharge, S and Qpred

TMWBdf$Qpred=NA
TMWBdf$Qpred[1]=0
TMWBdf$S=NA
TMWBdf$S[1]=0

attach(TMWBdf)
for (t in 2:length(date)){
  S[t]=S[t-1]+Excess[t]     
  Qpred[t]=fcres*S[t]
  S[t]=S[t]-Qpred[t]
}
TMWBdf$S=S
TMWBdf$Qpred=Qpred # UPDATE vector BEFORE DETACHING
plot(date,Qpred,type="l")
detach(TMWBdf) # IMPORTANT TO DETACH
rm(list=c("Qpred","S"))
return(data.frame(AW=TMWBdf$AW, Excess=TMWBdf$Excess, dp=TMWBdf$dP, ET=TMWBdf$ET, S=TMWBdf$S, Qpred=TMWBdf$Qpred))
}
