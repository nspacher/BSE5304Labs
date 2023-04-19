setwd("~")
objects()
rm(list=objects())
url="https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/MySetup.R"
download.file(url,"DanSetup.R")
file.edit("DanSetup.R")
# Open Bignette and then come back to start package install
# because it takes a while to install multisensi
#
url="https://cran.r-project.org/web/packages/multisensi/vignettes/multisensi-vignette.pdf"
browseURL(url)
pacman::p_load(data.table,multisensi)
# KEEP OPEN AS YOU WILL BE WALKING THROUGH IT FOR LAB	
vignette("multisensi-vignette")

verhulst <- function(K, Y0, a, t) {
  output <- K/(1 + (K/Y0 - 1) * exp(-a * t))
  return(output)
}

T <- seq(from = 5, to = 100, by = 5)
verhulst2 <- function(X, t = T) {
  out <- matrix(nrow = nrow(X), ncol = length(t), NA)
  for (i in 1:nrow(X)) {
    out[i, ] <- verhulst(X$K[i], X$Y0[i], X$a[i], t)
  }
  out <- as.data.frame(out)
  names(out) <- paste("t", t, sep = "")
  return(out)
}

n <- 10
set.seed(1234)
X <- data.frame(K = runif(n, min = 100, max = 1000), Y0 = runif(n, min = 1,
                                                                max = 40), a = runif(n, min = 0.05, max = 0.2))
Y <- verhulst2(X)
par(cex.axis = 0.7, cex.lab = 0.8)
plot(T, Y[1, ], type = "l", xlab = "Time", ylab = "Population size",
     ylim = c(0, 1000))
for (i in 2:n) {
  lines(T, Y[i, ], type = "l", col = i)
}


verhulst.seq <- multisensi(model=verhulst2, reduction=NULL, center=FALSE,
                           design.args = list( K=c(100,400,1000), Y0=c(1,20,40), a=c(0.05,0.1,0.2)))

# color palettes: rainbow, heat.colors, terrain.colors, topo.colors,
# cm.colors
plot(verhulst.seq, normalized = TRUE, color = terrain.colors, gsi.plot = FALSE)
title(xlab = "Time in half-decades.")
plot(verhulst.seq, normalized = FALSE, color = terrain.colors, gsi.plot = FALSE)
title(xlab = "Time in half-decades.")

print(verhulst.seq, digits = 2)

X <- expand.grid(K=c(100,400,1000), Y0=c(1,20,40), a=c(0.05,0.1,0.2))
Y <- verhulst2(X) ## this part can be performed outside R if necessary
verhulst.seq <- multisensi(design=X, model=Y, reduction=NULL, center=FALSE)


verhulst.pca <- multisensi(design=X, model=Y, reduction=basis.ACP, scale=FALSE)
summary(verhulst.pca, digits = 2)

plot(verhulst.pca, graph = 1)

plot(verhulst.pca, graph = 2)

verhulst.poly <- multisensi(design = X, model = Y, reduction = basis.poly,
                            dimension = 0.99, center = FALSE, scale = FALSE, cumul = FALSE,
                            basis.args = list(degree=6, x.coord=T), analysis = analysis.anoasg,
                            analysis.args = list(formula=2, keep.outputs=FALSE))
summary(verhulst.poly, digits=2)
plot(verhulst.poly, nb.comp = 3, graph = 1)

verhulst.bspl <- multisensi(design=X, model=Y, reduction=basis.bsplines,
                            dimension=NULL, center=FALSE, scale=FALSE,
                            basis.args=list(knots=10, mdegree=3), cumul=FALSE,
                            analysis=analysis.anoasg,
                            analysis.args=list(formula=2, keep.outputs=FALSE))
plot(verhulst.bspl, nb.comp = 5, graph = 1)

library(sensitivity)
m <- 10000
Xb <- data.frame(K = runif(m, min = 100, max = 1000), Y0 = runif(m, min = 1,
                                                                 max = 40), a = runif(m, min = 0.05, max = 0.2))
verhulst.seq.sobol <- multisensi(design = sobol2007, model = verhulst2,
                                 reduction = NULL, analysis = analysis.sensitivity, center = TRUE,
                                 design.args = list(X1 = Xb[1:(m/2), ], X2 = Xb[(1 + m/2):m, ], nboot = 100),
                                 analysis.args = list(keep.outputs = FALSE))
## [*] Design
## [*] Response simulation
## [*] Analysis + Sensitivity Indices
print(verhulst.seq.sobol, digits = 2)

plot(verhulst.seq.sobol, normalized = TRUE, color = terrain.colors, gsi.plot = FALSE)
title(xlab = "Time in half-decades")

verhulst.seq.fast <- multisensi(design = fast99, model = verhulst2,
                                center = FALSE, reduction = NULL, analysis = analysis.sensitivity,
                                design.args=list( factors=c("K","Y0","a"), n=1000, q = "qunif",
                                                  q.arg = list(list(min=100, max=1000), list(min=1, max=40),
                                                               list(min = 0.05, max = 0.2))),
                                analysis.args=list(keep.outputs=FALSE))
## [*] Design
## [*] Response simulation
## [*] Analysis + Sensitivity Indices
print(verhulst.seq.fast,digits=2)

?EcoHydRology::Solar

J <- seq(from = 1, to = 365, by = 5)
# Solar(lat, Jday, Tx, Tn, albedo=0.2, forest=0, slope=0, aspect = 0,
#      units="kJm2d")
# Note that the EcoHydRology::Solar() function is for specific days, 
# as such, we will want to create a function to loop through our period
# of interest:
Solar_Looped <- function(X, Jday = J) {
  out <- matrix(nrow = nrow(X), ncol = length(Jday), NA)
  for (i in 1:nrow(X)) {
    out[i, ] <- Solar(lat=X$lat[i],
                      Jday=Jday, Tx=X$Tx[i], 
                      Tn=(X$Tx[i]-X$Trange[i]), 
                      X$slope[i],X$aspect[i],units="Wm2")
  }
  out <- as.data.frame(out)
  names(out) <- paste("Jday", Jday, sep = "")
  return(out)
}

n <- 10
set.seed(1234)
X <- data.frame(Tx = runif(n, min = 5, max = 30), Trange = runif(n, min = 2,max = 16), 
                slope = runif(n, min = 0.0, max = 0.2),
                aspect = runif(n, min = 0.0, max = 0.2),
                lat=runif(n, min = 0.0, max = 1.1))  # 1.1 radians lat is where?

Y <- Solar_Looped(X,Jday = J)

par(cex.axis = 0.7, cex.lab = 0.8)
plot(J, Y[1, ], type = "l", xlab = "Day of Year", 
       ylab = "Surface Short Wave Rad(W/m^2)")
for (i in 2:n) {
  lines(J, Y[i, ], type = "l", col = i)
}

Solar_Looped.seq <- multisensi(model=Solar_Looped, reduction=NULL, center=FALSE,
                                 design.args = list( Tx = c(5,15,25), 
                                                     Trange = c(2,9,16), 
                                                     slope = c(0.1,0.2,0.3),
                                                     aspect = c(0.1,.5,1.0),
                                                     lat=c(0.1,.77,1.1)))

print(Solar_Looped.seq, digits = 2)
#
# 3.2 Graphical representation of sensitivity indices
#
dev.off() # Clean up previous par()
plot(Solar_Looped.seq, normalized = TRUE, color = terrain.colors, gsi.plot = FALSE)#normalized the upper subplot shows the extreme (tirets), #inter-quartile (grey) and median (bold line) output values
title(xlab = "Days of the Year.")
plot(Solar_Looped.seq, normalized = FALSE, color = terrain.colors, gsi.plot = FALSE)
title(xlab = "Days of the Year.")


# 4 Multivariate sensitivity analysis based on PCA
# read the vignette, though note we are using the multisensi() function
# to run our model (i.e. no “design” variable, and model=Solar_Looped)
Solar_Looped.pca <- multisensi(model=Solar_Looped, reduction=basis.ACP, scale=FALSE,
                                 design.args = list( Tx = c(5,15,25), 
                                                     Trange = c(2,9,16), 
                                                     slope = c(0.1,0.2,0.3),
                                                     aspect = c(0.1,.5,1.0),
                                                     lat=c(0.1,.77,1.1)))

summary(Solar_Looped.pca, digits = 2)
# 4.2 Graphical representation for PCA based analysis with 
# explanation in vignette. These graphs require the plot window to be larger
# and might give "Error in plot.new() : figure margins too large". 
# If so expand the plot window.
dev.off()
plot(Solar_Looped.pca, graph = 1)
plot(Solar_Looped.pca, graph = 2)
plot(Solar_Looped.pca, graph = 3)
#
# 5.1 Polynomial reduction of the multivariate output
# Skip 5.1 Polynomial reduction for now and move on to
# 6 Alternative methods of sensitivity analysis
# 6.1 With Sobol2007 implemented in the package sensitivity
# 
library(sensitivity)
m <- 10000
Xb <- data.frame(Tx = runif(m, min = 5, max = 30), 
                   Trange = runif(m, min = 2,max = 16), 
                   slope = runif(m, min = 0.0, max = 0.2),
                   aspect = runif(m, min = 0.0, max = 0.2),
                   lat=runif(m, min = 0.0, max = 1.1))

Solar_Looped.seq.sobol <- multisensi(design = sobol2007, model = Solar_Looped,
                                       reduction = NULL, analysis = analysis.sensitivity, center = TRUE,
                                       design.args = list(X1 = Xb[1:(m/2), ], X2 = Xb[(1 + m/2):m, ], nboot = 100),
                                       analysis.args = list(keep.outputs = FALSE))
#
# Note, this is a good time time get a drink of water and/or pee as 
# it is running the function m=10,000 times (a few minutes).
#
print(Solar_Looped.seq.sobol, digits = 2)
dev.off()
plot(Solar_Looped.seq.sobol, normalized = TRUE, color = terrain.colors)

Solar_Looped.seq.fast <- multisensi(design = fast99, model = Solar_Looped,
                                      center = FALSE, reduction = NULL, analysis = analysis.sensitivity,
                                      design.args=list( factors=c("Tx","Trange","slope","aspect","lat"), 
                                                        n=1000, q = "qunif",
                                                        q.arg = list(list(min=5, max=30), 
                                                                     list(min=2, max=16),
                                                                     list(min=0, max=.2),
                                                                     list(min=0, max=.2),
                                                                     list(min = 0.0, max = 1.1))),
                                      analysis.args=list(keep.outputs=FALSE))

print(Solar_Looped.seq.fast,digits=2)
plot(Solar_Looped.seq.fast, normalized = TRUE, color = terrain.colors)


####### HW1 ##########
?EcoHydRology::PET_fromTemp()
#forest ranges from 0 to 1. It should always set to zero for landscape-wide processes 
#regardless of the amount of forest present. Only change this if calculating PET under a canopy.

PET_fromTemp <- function (Jday, Tmax_C, Tmin_C, lat_radians, AvgT = (Tmax_C + Tmin_C)/2, albedo = 0.18, TerrestEmiss = 0.97, aspect = 0, slope = 0, forest = 0, PTconstant=1.26, AEparams=list(vp=NULL, opt="linear"))
{
  cloudiness <- EstCloudiness(Tmax_C, Tmin_C)
  DailyRad <- NetRad(lat_radians, Jday, Tmax_C, Tmin_C, albedo, forest, slope, aspect, AvgT, cloudiness, TerrestEmiss, AvgT, AEparams=AEparams)
  potentialET <- PTpet(DailyRad, AvgT, PTconstant)
  potentialET[which(potentialET < 0)] <- 0
  potentialET[which(Tmax_C == -999 | Tmin_C == -999)] <- (-999)
  return(potentialET)
}

PET_fromTemp(3,25,4,0.5,albedo=0.18,TerrestEmiss = 0.97, aspect = 0, slope = 0, forest = 0, PTconstant=1.26, AEparams=list(vp=NULL, opt="linear"))

J <- seq(from = 1, to = 365, by = 5) #julian day sequence
X <- data.frame(Tmax_C = runif(n, min = 5, max = 30), Trange = runif(n, min = 2,max = 16), 
                slope = runif(n, min = 0.0, max = 0.2),
                aspect = runif(n, min = 0.0, max = 0.2),
                lat_radians=runif(n, min = 0.0, max = 1.1))

PET_fromTemp_looped <- function(X, Jday = J) {
  out <- matrix(nrow = nrow(X), ncol = length(Jday), NA)
  for (i in 1:nrow(X)) {
    out[i, ] <- PET_fromTemp(Jday=Jday,Tmax_C=X$Tmax_C[i],
                             Tmin_C=(X$Tmax_C[i]-X$Trange[i]),
                             lat_radians=X$lat_radians[i],
                             albedo=0.18,
                             TerrestEmiss=0.97,
                             aspect=X$aspect[i],slope=X$slope[i],
                             forest=0,PTconstant=1.26,
                             AEparams=list(vp=NULL, opt="linear"))
  }
  out <- as.data.frame(out)
  names(out) <- paste("Jday", Jday, sep = "")
  return(out)
}

Y <- PET_fromTemp_looped(X,Jday = J)

par(cex.axis = 0.7, cex.lab = 0.8)
plot(J, Y[1, ], type = "l", xlab = "Day of Year", 
     ylab = "PET")
for (i in 2:n) {
  lines(J, Y[i, ], type = "l", col = i)
}

PET_fromTemp_Looped.seq <- multisensi(model=PET_fromTemp_looped, reduction=NULL, center=FALSE,
                               design.args = list( Tmax_C = c(5,15,25), 
                                                   Trange = c(2,9,16), 
                                                   slope = c(0.1,0.2,0.3),
                                                   aspect = c(0.1,.5,1.0),
                                                   lat_radians=c(0.1,.77,1.1)))
print(PET_fromTemp_Looped.seq,digits=2)

dev.off() # Clean up previous par()
plot(PET_fromTemp_Looped.seq, normalized = TRUE, color = terrain.colors, gsi.plot = FALSE)#normalized the upper subplot shows the extreme (tirets), #inter-quartile (grey) and median (bold line) output values
title(xlab = "Days of the Year.")
plot(PET_fromTemp_Looped.seq, normalized = FALSE, color = terrain.colors, gsi.plot = FALSE)
title(xlab = "Days of the Year.")

PET_fromTemp_Looped.pca <- multisensi(model=PET_fromTemp_looped, reduction=basis.ACP, scale=FALSE,
                               design.args = list( Tmax_C = c(5,15,25), 
                                                   Trange = c(2,9,16), 
                                                   slope = c(0.1,0.2,0.3),
                                                   aspect = c(0.1,.5,1.0),
                                                   lat_radians=c(0.1,.77,1.1)))

dev.off()
plot(PET_fromTemp_Looped.pca, graph = 1)
plot(PET_fromTemp_Looped.pca, graph = 2)
plot(PET_fromTemp_Looped.pca, graph = 3)

Xb <- data.frame(Tmax_C = runif(m, min = 5, max = 30), 
                 Trange = runif(m, min = 2,max = 16), 
                 slope = runif(m, min = 0.0, max = 0.2),
                 aspect = runif(m, min = 0.0, max = 0.2),
                 lat_radians=runif(m, min = 0.0, max = 1.1))

PET_fromTemp_Looped.seq.sobol <- multisensi(design = sobol2007, model = PET_fromTemp_looped,
                                     reduction = NULL, analysis = analysis.sensitivity, center = TRUE,
                                     design.args = list(X1 = Xb[1:(m/2), ], X2 = Xb[(1 + m/2):m, ], nboot = 100),
                                     analysis.args = list(keep.outputs = FALSE))
#
# Note, this is a good time time get a drink of water and/or pee as 
# it is running the function m=10,000 times (a few minutes).
#
print(Solar_Looped.seq.sobol, digits = 2)
dev.off()
plot(PET_fromTemp_Looped.seq.sobol, normalized = TRUE, color = terrain.colors)
#########
?EcoHydRology::NetRad()
NetRad_looped <- function(X, Jday = J) {
  out <- matrix(nrow = nrow(X), ncol = length(Jday), NA)
  for (i in 1:nrow(X)) {
    out[i, ] <- NetRad(lat=X$lat_radians[i],Jday=Jday,
                       Tx=X$Tmax_C[i],Tn=(X$Tmax_C[i]-X$Trange[i]),
                       albedo=0.18,forest=0,slope=X$slope[i],
                       aspect=X$aspect[i],
                       cloudiness = "Estimate",surfemissivity = 0.97, units="kJm2d",
                       AEparams=list(vp=NULL, opt="linear"))
  }
  out <- as.data.frame(out)
  names(out) <- paste("Jday", Jday, sep = "")
  return(out)
}

NetRad_looped.seq <- multisensi(model=NetRad_looped,reduction=NULL,center=F,
                                design.args=list(Tmax_C = c(5,15,25), 
                                                  Trange = c(2,9,16), 
                                                  slope = c(0.1,0.2,0.3),
                                                  aspect = c(0.1,.5,1.0),
                                                  lat_radians=c(0.1,.77,1.1)))

plot(NetRad_looped.seq,normalized=F, color = terrain.colors, gsi.plot = FALSE)
title(xlab = "Days of the Year.")


NetRad_looped.pca <- multisensi(model=NetRad_looped, reduction=basis.ACP, scale=FALSE,
                                design.args = list( Tmax_C = c(5,15,25), 
                                                    Trange = c(2,9,16), 
                                                    slope = c(0.1,0.2,0.3),
                                                    aspect = c(0.1,.5,1.0),
                                                    lat_radians=c(0.1,.77,1.1)))
dev.off()
plot(NetRad_looped.pca,graph=1)

NetRad_Looped.seq.sobol <- multisensi(design = sobol2007, model = NetRad_looped,
                                            reduction = NULL, analysis = analysis.sensitivity, center = TRUE,
                                            design.args = list(X1 = Xb[1:(m/2), ], X2 = Xb[(1 + m/2):m, ], nboot = 100),
                                            analysis.args = list(keep.outputs = FALSE))
#
# Note, this is a good time time get a drink of water and/or pee as 
# it is running the function m=10,000 times (a few minutes).
#
print(NetRad_Looped.seq.sobol, digits = 2)
dev.off()
plot(NetRad_Looped.seq.sobol, normalized = TRUE, color = terrain.colors)

####### SOIL STORAGE ##################
?EcoHydRology::SoilStorage()
CN <- c(30:100)
S_avg <- 1000/CN -10 #inches of storage
min(S_avg)
max(S_avg)
fc <- seq(0.09:0.4,by=0.01)#how could this be constrained to always be less than the porosity. 
porosity <- seq(0.3:0.5,by=0.01)
#data frame of input parameter combinations 
S <- data.frame(soil_water_content=c(0.01,0.2,0.3),
                field_capacity=c(0.09,0.2,0.3),
                porosity=c(0.3,0.35,0.4))

#SWC range of interest 
SWC <- seq(from=0, to=0.5, by=0.01)#how could this be constrained to always be less than the porosity. 
Savg <- (1000/CN -10+1)*25.4

SoilStorage_Looped <- function(S, S_avg=Savg){
  out <- matrix(nrow = nrow(S), ncol = length(S_avg), NA)
  for (i in 1:nrow(S)) {
    out[i, ] <- SoilStorage(S_avg=S_avg, field_capacity = S$fc[i],
                            soil_water_content = S$fc[i]-S$delta_wc[i],
                            porosity = S$delta_wc[i]+S$fc[i]+S$delta_p[i])
  }
  out <- as.data.frame(out)
  names(out) <- paste("Savg", S_avg, sep = "")
  return(out)
}
#tmp <- SoilStorage_Looped(S,Savg)
# SoilStorage(24,0.09,0.01,0.3)

# plot(tmp)
SoilStorage_looped.seq <- multisensi(model=SoilStorage_Looped,reduction=NULL,center=F,
                                design.args=list(delta_wc = c(0.1,0.06,0.11), 
                                                     fc = c(0.1,0.07,0.13), 
                                                           delta_p = c(0.1,0.15,0.2)))

dev.off() # Clean up previous par()
plot(SoilStorage_looped.seq, normalized = TRUE, color = terrain.colors, gsi.plot = FALSE)#normalized the upper subplot shows the extreme (tirets), #inter-quartile (grey) and median (bold line) output values
title(xlab = "Days of the Year.")
plot(SoilStorage_looped.seq, normalized = FALSE, color = terrain.colors, gsi.plot = FALSE)
title(xlab = "Days of the Year.")

SoilStorage_looped.pca <- multisensi(model=SoilStorage_Looped,reduction=basis.ACP,scale=F,
                                     design.args=list(delta_wc= c(0.1,0.06,0.11), 
                                                      fc = c(0.1,0.07,0.13), 
                                                      delta_p = c(0.1,0.15,0.2)))
plot(SoilStorage_looped.pca,graph=1)
