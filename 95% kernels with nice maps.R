#############################################################################
# SET WORKING DIRECTORY
#############################################################################
setwd("your drive name")
#############################################################################
# LOAD PACKAGES
#############################################################################
library("adehabitatHR")
library("ggplot2")
library("maptools")
library("rgdal")
library("raster")
library("viridis")
#############################################################################
# DATA LOAD
#############################################################################
cl1<-read.table("TVR 263 Lineage sub-clade2_singles.txt", header=T)
# have a look at the data
str(cl1) #41 obs. of  3 variables
#############################################################################
# 1 plot the data using ggplot
#############################################################################
ggplot(cl1, aes (x = X
                ,y = Y)) + # ggplot makes nice graphs.  under aes (i.e. aesthetics) is where you specify x and y coordinates, colours etc
geom_point() + # geom point tells ggplot to plot points
  ggtitle("Badger locations") +
  theme_minimal() # theme minimal changes how the plot looks
########################################################################
# 2. plot the data using base R (just for comparison)
#############################################################################
plot(x = cl1$X, y = cl1$Y, main="Badger locations")
########################################################################
# 3. spatial points
##########################################################################
cl1_final<- cl1[,c(2,3)]
cl1_spatial<-SpatialPoints(cl1_final, proj4string=CRS("+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=1.000035 +x_0=200000 +y_0=250000 +datum=ire65 +units=m +no_defs +ellps=mod_airy +towgs84=482.530,-130.596,564.557,-1.042,-0.214,-0.631,8.15"))
##############################################################################
# 4. LSCV
#############################################################################
kud_cl1<-kernelUD(cl1_spatial, h="LSCV")  ### using the superior LSCV as'h'
image(kud_cl1) # YAY!
kud_cl1@h
plotLSCV(kud_cl1)
plot(kud_cl1)
##############################################################################
# 5. 95% is home range
#############################################################################
# this creates and plots the 95% envelope as a block
homerange_cl1<-getverticeshr(kud_cl1, percent=95)
plot(homerange_cl1, col="red")
##########################################################################
# 6. write as a shapefile
##########################################################################
writeOGR(homerange_cl1, 
         dsn = "clade4",
         driver="ESRI Shapefile", 
         layer="homerange_clade4")
##########################################################################
# 6. find the utilisaiton distribution (the contours)
##########################################################################
# utilisation distribution - this creates and plots the coutours
vol.kud_cl1 <- getvolumeUD(kud_cl1)
(vol.kud_cl1)
plot(vol.kud_cl1)
vol.kud_cl1.rst <- raster(vol.kud_cl1)
writeRaster(vol.kud_cl1.rst, "heatmap_raster.asc", overwrite=TRUE)
##########################################################################
# 7. plot a nice graph showing all the data thus far #####
##########################################################################
contour.image <- as.image.SpatialGridDataFrame(vol.kud_cl1) 
plot(vol.kud_cl1, col = rev(viridis(15))) # plot the utilisation distribution in colourblind friendly colours 
points(x = cl1$X, y = cl1$Y, main="Badger locations", # plot the badger points 
     cex = 0.5,
     col = "red",
     pch = 19)
contour(contour.image, add=TRUE) # finally plot the contour labels 

# write the plot to file at 300 dpi
png("Home_Range_Contours.png", width = 3000, height = 3000, res = 300)
 
dev.off()

##########################################################################
# 7. rasterise the shapefile at just the 95 % 
# if for example you wanted to work with rasters and not shapefiles to eg.extract values
# this isnt very elegent someone else may know better
##########################################################################
vol.kud_cl1.95 <- as.data.frame(vol.kud_cl1)[,1]
vol.kud_cl1.95 <- as.numeric(vol.kud_cl1.95 <= 95)
## Converts into a data frame
vol.kud_cl1.95 <- data.frame(vol.kud_cl1.95)
## Converts to a SpatialPixelsDataFrame
coordinates(vol.kud_cl1.95) <- coordinates(vol.kud_cl1)
gridded(vol.kud_cl1.95) <- TRUE
vol.kud_cl1.95.rs <- raster(vol.kud_cl1.95)
#### plot your raster and the data to see what it looks like #####
plot(vol.kud_cl1.95.rs)
points(x = cl1$X, y = cl1$Y, main="Badger locations", # plot the badger points 
       cex = 0.5,
       col = "red",
       pch = 19)
contour(contour.image, add=TRUE)
#### write it out if needed ###### 
writeRaster(vol.kud_cl1.95.rs, "clade_95_raster.asc", overwrite=TRUE)




########################################################################
# read everything in to plot it works
#########################################################################
# the headmap contours in raster form
tst.rst.heatmap <- raster("heatmap_raster.asc")
plot(tst.rst.heatmap, col = rev(viridis(15)))
plot(tst.rst.heatmap) # optional with original colour scheme

tst.rst.95 <- raster("clade_95_raster.asc")
plot(tst.rst.95)
