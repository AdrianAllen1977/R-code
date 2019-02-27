# -------------------------------------------------------------------------------------- +
# GBIF Ireland - SDM workshop                                                            |
# Day 1: Data preparation                                                                |
#                                                                                        |
# N.E. Zimmermann, WSL Birmensdorf  &  ETH Zürich                                        |
# -------------------------------------------------------------------------------------- +
setwd("Desktop/Species Distribution modelling course - October 2018/")
library(raster)

# Screening environmental data -----------------------------------------------------------
prcp07 <- raster("envData/raster/aar_81to10_jul.tif")
ndvi07 <- raster("envData/raster/NVDI_LTA99to11_jul_EPSG3035.tif")

prcp07
ndvi07

par(mfrow=c(1,2))
  plot(prcp07)
  plot(ndvi07)
par(mfrow=c(1,1))

# Importing and aggregating environmental data -------------------------------------------
env0<-stack("aar_81to10_jan.tif")
env0<-stack(env0,"aar_81to10_jul.tif")
env0<-stack(env0,"tmin_81to10_jan.tif")
env0<-stack(env0,"tmin_81to10_jul.tif")
env0<-stack(env0,"tmax_81to10_jan.tif")
env0<-stack(env0,"tmax_81to10_jul.tif")
env0<-stack(env0,"oceanicity.tif")
env0<-stack(env0,"g100_clc_wooded_propcov_aligned.tif")

# Generate an empty baseline raster and resample env to this baseline --------------------
## Just tidying the edges of the geographic projection and then applying it to the created stack of 8 layers
grd<-raster(nrows=441,ncols=344,xmn=24115.0,xmx=368115,ymn=19130,ymx=460130)
crs(grd) <- crs(env0)
env<-resample(env0,grd,method="ngb")

# Import raster of different spatial structure - the vegetation layers -------------------------------------------
tmp1<-raster("NVDI_LTA99to11_jan_EPSG3035.tif")
tmp2<-raster("NVDI_LTA99to11_jul_EPSG3035.tif")
tmp3<-raster("Ireland_mow_freq_mean_01_12_EPSG3035.tif")
tmp4<-raster("Ireland_grassld_manage_freq_EPSG3035.tif")
tmp5<-raster("Ireland_LSU_1km_EPSG3035.tif")
tmp6<-raster("g100_clc_V18_5_IrelandClip.tif")

# Reproject these rasters and stack - NDVI has a different projection, make it same as the aar projections ------------------------------------------------------
env<-stack(env,projectRaster(tmp3,env,method="bilinear"))
env<-stack(env,projectRaster(tmp4,env,method="bilinear"))
env<-stack(env,projectRaster(tmp5,env,method="ngb"))
env<-stack(env,projectRaster(tmp1,env,method="bilinear"))
env<-stack(env,projectRaster(tmp2,env,method="bilinear"))

# process last (tmp6) (CORINE) raster and do NOT stack - Corine landcover layers do not process quickly------------------------------------------
### Split the grd up into a greater resolution
grd1<-disaggregate(grd,fact=10)
#corine <- projectRaster(tmp6,grd1,method="ngb")

# Assign names in env and plot - make names of layers shorter for handiness ------------------------------------------------------------
names(env)<-c("prcp01","prcp07","tmin01","tmin07","tmax01","tmax07","oceani","wooded","MOWfrq","grassl","livest","ndvi01","ndvi07")
plot(env)

# Remove unrealistic values and convert NA's to 0 ----------------------------------------
env$ndvi01[env$ndvi01>100]<-NA
env$ndvi07[env$ndvi07>100]<-NA
for (i in 8:11){
  env[[i]][is.na(env[[i]])]<-0
  env[[i]]<-mask(env[[i]],env[[1]])
  }
## IN some layers, data points are not continuously distributed across the landscape
## So the loop converts the non collected NA regions to zero - but it also makes the ocean space zero too.
### So, the second half of the loop using a continuous layer, layer1, to determine that only the landmass is assigned a zero reading.

# Need to check which environmental variables in the stack are highly correlated.
### First need to make them comparable at a similar scale. Generate 5km point lattice -------------------------------------------------------------
library(ecospat)
ttt<-aggregate(env[[1]],fact=5)
tt1<-rasterToPoints(ttt)
pts5km<-data.frame(tt1[,1:2])
coordinates(pts5km)<-c("x","y")

# Extract env values at 5km lattice points and calculate correlations  -------------------
plot(env[[2]])
plot(pts5km,pch=16,cex=.5,add=T)

pts5km<-extract(env,pts5km,method="simple")
pts5km<-pts5km[complete.cases(pts5km),]
head(pts5km)

library(ecospat)
ecospat.cor.plot(pts5km[,1:7])

CorVars <- abs(cor(pts5km[,1:13], method="pearson"))
TreVars <- hclust(as.dist(1-CorVars), method="average")
plot(TreVars)
  abline(h=1 - 0.6, col="orange2", lty=1, lwd=.6)
  abline(h=1 - 0.7, col="red", lty=2, lwd=1.0)
  abline(h=1 - 0.8, col="firebrick3", lty=3, lwd=1.5)

# Importing and preparing species data ---------------------------------------------------
SAXspa<-read.delim("saxifragas_pathularis.csv",h=T)
head(SAXspa)

# Importing species files ----------------------------------------------------------------
EGRgar0<-read.delim("egretta_garzetta.csv",h=T)[,c(1,8:10,13,17:22,28)]
PYRpyr0<-read.delim("pyrrhocorax_pyrrhocorax.csv",h=T)[,c(1,8:10,13,17:22,28)]
SOMmol0<-read.delim("somateria_mollissima.csv",h=T)[,c(1,8:10,13,17:22,28)]
GONrha0<-read.delim("gonepteryx_rhamni.csv",h=T)[,c(1,8:10,13,17:22,28)]
POLcal0<-read.delim("polygonia_calbum.csv",h=T)[,c(1,8:10,13,17:22,28)]
PYRtit0<-read.delim("pyronia_tithonus.csv",h=T)[,c(1,8:10,13,17:22,28)]
SAXspa0<-read.delim("saxifragas_pathularis.csv",h=T)[,c(1,8:10,13,17:22,28)]
head(SAXspa0)

# Convert species files to spatial objects and re-project --------------------------------
coordinates(EGRgar0)<-c("decimallongitude","decimallatitude")
coordinates(PYRpyr0)<-c("decimallongitude","decimallatitude")
coordinates(SOMmol0)<-c("decimallongitude","decimallatitude")
coordinates(GONrha0)<-c("decimallongitude","decimallatitude")
coordinates(POLcal0)<-c("decimallongitude","decimallatitude")
coordinates(PYRtit0)<-c("decimallongitude","decimallatitude")
coordinates(SAXspa0)<-c("decimallongitude","decimallatitude")

crs(EGRgar0)<-crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
crs(PYRpyr0)<-crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
crs(SOMmol0)<-crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
crs(GONrha0)<-crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
crs(POLcal0)<-crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
crs(PYRtit0)<-crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
crs(SAXspa0)<-crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


## Species data GIS are in Lat and Lon - needs converted to the transverse mercator format.
## The Env data is already in that format, so just use spTransform with env set as the format standard
EGRgar<-spTransform(EGRgar0, crs(env))
PYRpyr<-spTransform(PYRpyr0, crs(env))
SOMmol<-spTransform(SOMmol0, crs(env))
GONrha<-spTransform(GONrha0, crs(env))
POLcal<-spTransform(POLcal0, crs(env))
PYRtit<-spTransform(PYRtit0, crs(env))
SAXspa<-spTransform(SAXspa0, crs(env))

spp<-c("EGRgar","PYRpyr","SOMmol","GONrha","POLcal","PYRtit","SAXspa")
par(mfrow=c(3,3), mar=c(1,1,1,1))
  for (i in 1:7){
    plot(env[[3]])
    plot(get(spp[i]),add=T)
    text(30000,430000,spp[i],font=2,cex=0.9,pos=4)
  }
par(mfrow=c(1,1))

# Analyse distance problems - avoiding pseudo-replication-------------------------------------------------------------
table(dist(coordinates(PYRpyr))<1000)
table(dist(coordinates(PYRpyr))<100)
table(dist(coordinates(PYRpyr))<10)

# Analyze coordinate uncertainties -------------------------------------------------------
for (i in 1:7){
  print(spp[i])
  print(table(as.vector(get(spp[i])@data[,6]),useNA="ifany"))
}

# Clean data for distances and coordinate uncertainties ----------------------------------
sp1<-c("EGga","PYpy","SOmo","GOrh","POca","PYti","SAsp")
mask<-env[[1]]>=0
for (i in 1:7){
  tt1 <- subset(get(spp[i]), is.na(coordinateuncertaintyinmeters) | coordinateuncertaintyinmeters<=2000)
  tt1@data$pres<-1
  tt2<-rasterize(tt1,mask,field=pres,fun=max,background=NA,mask=TRUE)
  tt3<-data.frame(rasterToPoints(tt2)[,-3])
  tt3$pres<-1
  assign(sp1[i],tt3)
}
plot(mask,col="grey70",legend=FALSE)
points(EGRgar,col=2,pch=16,add=TRUE) # ignore warning
points(EGga[,1:2],col=1,pch=16,add=TRUE) # ignore warning


# Generate random pseudoabsences --------------------------------------------------------
mask<-mask-1
pa<-data.frame(sampleRandom(mask,size=5000,xy=TRUE))
names(pa)[3]<-"pres"

# Combine presences and pseudo-absences and generate weights -----------------------------
for (i in 1:7){
  ttt<-rbind(get(sp1[i]),pa)
  ttt$weight<-table(ttt$pres)[2]/length(ttt$pres)
  ttt$weight[which(ttt$pres==1)]<-table(ttt$pres)[1]/length(ttt$pres)
  assign(sp1[i],ttt)
}

# attach environmental data to P/A files -------------------------------------------------
for (i in 1:7){
  tt1<-get(sp1[i])
  tt2<-extract(env,tt1[,1:2],method='bilinear')
  tt3<-cbind(tt1,tt2)
  assign(sp1[i],tt3)
}
head(SAsp)

# Clean up and save ----------------------------------------------------------------------
rm(list=ls(pattern="0"))
rm(list=ls(pattern="tt"))
rm(list=ls(pattern="tmp"))
rm(list=ls(pattern="07"))
rm(list=ls(pattern="Vars"))
rm(i)

save.image("GBIF_Ireland_day1.RData")
# DONE -----------------------------------------------------------------------------------
