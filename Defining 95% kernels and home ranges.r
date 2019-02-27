#### Home range estimation by Kernel Density
## adehabitatHR package

### There are multiple methods for building home ranges - the Kernel density method is one such.

install.packages("adehabitatHR")

library(adehabitatHR)

### If you have spatial data in the form of X Y coords or lat / lon, you can use it in this package.  

###You must first convert it to the spatial points or spatial dataframe format.
## First read in data

setwd("Desktop/R stuff/TVR genomes/Shape files")

cl1<-read.table("Point locations/TVR 263 Lineage sub-clade1.txt", header=T)

### Take the X and Y columns from this dataframe

cl1_final<-cl1[,c(2,3)]

## Convert this file to a spatial points format.

cl1_spatial<-SpatialPoints(cl1_final, proj4string=CRS("+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=1.000035 +x_0=200000 +y_0=250000 +datum=ire65 +units=m +no_defs +ellps=mod_airy +towgs84=482.530,-130.596,564.557,-1.042,-0.214,-0.631,8.15"))

###  Now run the kernel density function.

## Unlike minimum convex polgon (MCP) home range methods, which calculate the area traversed by a species / bacterial type - kernels work out the probability density of the species - i.e. the probability of locating the species at any place according to co-ordinates.  Sometimes called a Utilisation Distribution (UD).

## The most commonly used kernel function is the bivariate normal kernel. All kernel density functions require a smoothing parameter which controls the width of the function placed over each spatial point.  The standard value of 'h' can be worked out from the standard deviations of the point locations.  For multi center species, this can over smooth the kernel - leads to high probability estimations for regions in which the species doesn't actually occur, so be careful!  The least square cross validation (LSCV) method is better to prevent over smoothing. 


kud_cl1<-kernelUD(cl1_spatial, h="href")  ##using the standard 'h' reference
kud_cl1<-kernelUD(cl1_spatial, h="LSCV")  ### using the superior LSCV as'h'

### To see a heat map image of the kernel produced, follow below:

image(kud_cl1)

## Check that the algorithm converges on a solution.  You want to see an optimal value for h being arrived at where coefficient of variance approaches zero. This makes sense - Where there is least variance between point locations which are closest together, the smoothing parameter finds an appropriate value.

kud_cl1@h

## Check from the output the convergence has occurred.

## Check plots to see relationship between CV and value of h.

plotLSCV(kud_cl1)

## If there is NO convergence - DON'T USE THE KERNEL ESTIMATE!
## Convergence can fail if there are multiple points from the exact same location.
## Remove duplicates if so or jitter the points

## cl_x<-jitter(cl_final$X, amount=x)  ## Jitter X co-ords
## cl_y<-jitter(cl_final$Y, amount=x)  ## Jitter X co-ords

## Amend amount of jitter until LSCV converges

## cl1_final2<-cbind(cl_x, cl_y)  ## make new combined, jittered dataframe of point locs.
## Rerun LSCV kernelUD command above.

####  Now, you can use the UD / probability density from this kernel density object to create a home range.

homerange_cl1<-getverticeshr(kud_cl1, percent=95) ### 95% is default, but added in here.
plot(homerange_cl1, col="red")

## Check the area of the home range

homerange_cl1

## You now have the home range defined as the smallest area in which you have a 95% chance of finding the species / type.

### This can be added to GIS plots with other layers. 
## Write the SpatialPolygonsDataFrame that is your homerange to a shape fle format.

library(maptools)
library(rgdal)

writeOGR(homerange_cl4, dsn = "clade4", driver="ESRI Shapefile", layer="homerange_clade4")

###You can then import these layers as with the GIS script

________________________________________________________________________________

### The latter was for a single species / type data set, you can do the same for multiple species.

### Create a multi species / multi type file with a column indicating species / type ID

## Convert this to a spatial points dataframe

data<-read.table("Point locations/TVR_263_clades.txt", header=T)

### Extract the xy coords

xy<-data[,c(3,4)]

## Extract the meta data (clade ID)
data2<-as.data.frame(data[,1])

### Create the spatial points dataframe using the iish grid projection

tvr_263_clades<-SpatialPointsDataFrame(coords = xy, data = data2, proj4string=CRS("+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=1.000035 +x_0=200000 +y_0=250000 +datum=ire65 +units=m +no_defs +ellps=mod_airy +towgs84=482.530,-130.596,564.557,-1.042,-0.214,-0.631,8.15"))

#### Run the kernel density of the dataframe - make sure you tell the algorithm to work by the levels in your data by specifying column 1.

kud<-kernelUD(tvr_263_clades[,1], h="href")

### Make a homerange

homeranges<-getverticeshr(kud, percent=95)

### Check the areas of the homeranges by level.

homeranges

### Plot the home ranges to see where they overlay
plot(homeranges, col=c("blue","green", "red", "purple"))


_________________________________________________________

## Minimum Convex Polygon (MCP) Methods

##An alternative to kernel density is to use MCP methods.

## MCPs define the smallest convex polygon enlcosing all locations of the species / type of interest.

## It is common to exclude / remove a small percentage of points that exhibit the greatest distance from the centroid of all points.

##Import the data
setwd("Desktop/R Stuff/TVR genomes/Shape files/")

clade1<-read.table("Point locations/TVR 263 Lineage sub-clade1.txt", header=T)

### Extract the XY coords
library(adehabitatHR)
cl1<-clade1[,c(2,3)]

### Make the XY data a spatial points object.
cl1_spatial<-SpatialPoints(cl1, proj4string=CRS("+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=1.000035 +x_0=200000 +y_0=250000 +datum=ire65 +units=m +no_defs +ellps=mod_airy +towgs84=482.530,-130.596,564.557,-1.042,-0.214,-0.631,8.15"))

### Run the MCP function and trim the 5% of the outliers.

cl1_mcp<-mcp(cl1_spatial, percent=95)

### Coerce the mcp object to a dataframe and you can find the area

as.data.frame(cl1_mcp)

#### Write the file to shape format so it can be used in GIS

library(maptools)
library(rgdal)

writeOGR(cl1_mcp, dsn = "mcp_clade1", driver="ESRI Shapefile", layer="homerange_clade1")

### Projection data is typoically expressed in metres.

### adehabitatHR outputs home range areas in hectares

## To convert to km^2 for any GIS shapefile, use the folowing script.

library(raster)
x<-shapefile("yourfile.shp")
crs(x)
x$area_sqkm <- area(x) / 1000000

### Simply run the following query to get the figure
x$area_sqkm