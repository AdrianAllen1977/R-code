######################################
### 1.140 strain type investigation ##
######################################

#############
## Packages #
#############

library("ggplot2")
library("gganimate")
library("rgdal")
library("sp")
library("raster")
library("ggthemes")
library("gifski")
library("ggsn")
library("dplyr")
library("ggdensity")
library("sf")

##########
# Script #
##########

setwd("Desktop/1.140_paper/")


# Read in NI map layer
NI<-readOGR(dsn=".", layer="NI_Outline") ## This layer uses the generala GPS WGS84 GRID!

# Convert layer to dataframe
NI2<-fortify(NI)

# Read in 1.140 sequence locations

x<-read.table("12_SNP_cutoff_locs.txt", header=T, sep="\t")

#OR

x<-read.csv("12_SNP_cutoff_locs.csv", header=T)

## check data structure / format
str(x)

## Change x and y variables to numeric format
x$x<-as.numeric(x$x)
x$y<-as.numeric(x$y)

# Filter sequences by cluster number
c5<-x %>% filter(cluster==5)

## ONLY DO THE BELOW IF YOU HAVE LOTS OF DATA POINTS - IF SMALL NUMBER USE DEFAULT h value
### Before you do any plotting - make sure you use the right kernel smoothing function h
## KDE estimators in R default to Gaussian methods - if you use these, they will oversmooth distributions that are not normally distributed, missing features of data
## Distribution of molecular types is likely not to be Gausssian.
## SO - use a non-parametric h smoothing function.
## The Sheather and Jones (SJ) method is good for getting h functions for multi modal distributions and avoiding oversmoothing
## Make a two column, lon and lat (in that order) vector for your locations

b<-cbind(c5$lon, c5$lat)

## The run the SJ algorithm to find h

bw.SJ(b)

## INsert this h value into your geom_hdr argument in ggplot

# Make map
# Base map first
map1<-ggplot(data=c5, aes(x=lon, y=lat)) +
  geom_polygon(data=NI2, aes(x=long, y=lat, group = group), colour="black", fill="white") +
  geom_hdr(data=c5, aes(x=lon, y=lat), method="kde", probs=c(0.95, 0.50), fill="blue") + ## add density KDE layer of c5 points
  geom_point(data=c5, aes(x=lon, y=lat), color="black", size=1) + ## add c5 points
  theme_map()

# geom_hdr is a more useful way of visualising the 50% etc KDE of points.  You can set different probabilities.
# And you can use different methods of calculating density - the default is KDE.

# Add density kernel using geom_density2d_filled - more visually appealing altenative but not as useful as geom_hdr
# map2<-map1 + geom_density2d_filled(data=c13, contour_var = "ndensity", bins=8, alpha=0.2, aes(x=x, y=y))

# Add scalebar
map2<-map1+ ggsn::scalebar( dist_unit="km", dist=20, x.min=-10, x.max=-8, y.min=55, y.max=55.5, transform=T, model="WGS84", st.bottom=T, st.dist=0.1, st.size=4) + theme_map()

# Add compass point
map3<-map2 + ggsn::north2(map2, x=0.1, y=0.9, scale=0.2)

#######################################
## Extracting polygons from KDE plots #
#######################################

# Remake the map of just the KDE layers and don't plot it.
## SET SCALE LIMITS OR ELSE EXPORT OF SHAPE FILES WILL BE TRUNCATED
map<-ggplot() + geom_hdr(data=c5, aes(x=lon, y=lat)) + scale_x_continuous(limits = c(-6.5, -5.0)) + scale_y_continuous(limits = c (54, 55.5))


# Extract the layer data using the command below
data<-layer_data(map, i=1)

str(data) ## Check structure of data extracted from layers


# Identify all unique layers and give them an id which you add to the dataframe
data$pol <- paste0(data$group, "_", data$subgroup)
ids <- unique(data$pol)

## Extract the 50% layers - if you have more than 1 50% kde layer separate them out by their pols id as created above

fifty_kde<-data %>% filter(order=="50%")

## Make the polygon from the x and y lat and long coords in the extracted layer data
polygon <- fifty_kde %>%
       st_as_sf(coords = c("x", "y"), crs = 4326) %>%
       summarise(geometry = st_combine(geometry)) %>%
       st_cast("POLYGON")

## Write the 50% KDE polygon to file
st_write(polygon, "50%KDE.shp")

## Check you've plotted correct polygon by reference to map

ggplot() + geom_hdr(data=c5, aes(x=lon, y=lat), fill="blue") + geom_sf(data=polygon, colour="red")

## Check the polygon of interest is the one outlined in red on the new map.

## THen calculate the area

sf_use_s2(FALSE) # switches off spherical geometry

st_area(polygon) # gives areas in metres squared

# to get kilometres squared, use the following:

area<-st_area(polygon) / 1000000

####################################################
## Mapping and extracting 50% KDEs from MLVA 1.140 #
####################################################

y<-read.table("MLVA_1.140_2000-2022_cattle&badger_first_isolate.txt", header=T, sep="\t")


## check data structure / format
str(x)

## Change x and y variables to numeric format
x$x<-as.numeric(x$x)
x$y<-as.numeric(x$y)

## Before you do any plotting - make sure you use the right kernel smoothing function h
## KDE estimators in R default to Gaussian methods - if you use these, they will oversmooth distributions that are not normally distributed, missing features of data
## Distribution of molecaulr types is likely not to be Gausssian.
## SO - use a non-parametric h smoothing function.
## The Sheather and Jones (SJ) method is good for getting h functions for multi modal distributions and avoiding oversmoothing
## Make a two column, lon and lat (in that order) vector for your locations

b<-cbind(x$lon, x$lat)

## The run the SJ algorithm to find h

bw.SJ(b)

## INsert this h value into your geom_hdr argument in ggplot

## PLot map

map4<-ggplot(data=y, aes(x=lon, y=lat)) +
  geom_polygon(data=NI2, aes(x=long, y=lat, group = group), colour="black", fill="white") +
  geom_hdr(data=y, aes(x=lon, y=lat), method="kde", probs=c(0.95, 0.50), h=0.117, fill="blue") + 
  geom_point(data=y, aes(x=lon, y=lat), color="red", size=0.3) + 
  theme_map()

# Add scalebar
map5<-map4+ ggsn::scalebar( dist_unit="km", dist=20, x.min=-10, x.max=-8, y.min=55, y.max=55.5, transform=T, model="WGS84", st.bottom=T, st.dist=0.1, st.size=2)

# Add compass point
map6<-map5 + ggsn::north2(map5, x=0.1, y=0.9, scale=0.2)

# Remake the map of just the KDE layers and don't plot it.
MLVA_map<-ggplot() + geom_hdr(data=y, aes(x=lon, y=lat), method="kde", probs=c(0.95, 0.50), h=0.117, fill="blue")

# Extract the layer data using the command below
data2<-layer_data(MLVA_map, i=1)

str(data2) ## Check structure of data extracted from layers


# Identify all unique layers and give them an id which you add to the dataframe
data2$pol <- paste0(data2$group, "_", data2$subgroup)
ids <- unique(data2$pol)

## Extract the 50% layers - if you have more than 1 50% kde layer separate them out by their pols id as created above

MLVA_fifty_kde<-data2 %>% filter(order=="50%")

## There are three 50% KDE layers this time, so we need to extract each separately
## We can do this using the ids we created above.

# First inspect the pol column of the fifty_kde2 dataframe

MLVA_fifty_kde$pol

## There are three layer names as expected - filter each out
MLVA_fifty_kde_1<- MLVA_fifty_kde %>% filter(pol=="-1-004_1")
MLVA_fifty_kde_2<- MLVA_fifty_kde %>% filter(pol=="-1-004_2")
MLVA_fifty_kde_3<- MLVA_fifty_kde %>% filter(pol=="-1-004_3")

## Now make polygons for each

polygon_1 <- MLVA_fifty_kde_1 %>%
    st_as_sf(coords = c("x", "y"), crs = 4326) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")

polygon_2 <- MLVA_fifty_kde_2 %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

polygon_3 <- MLVA_fifty_kde_3 %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

## PLot to make sure these are correct.

ggplot() + geom_hdr(data=y, aes(x=lon, y=lat)) +
  geom_sf(data=polygon_1, colour="red") +
  geom_sf(data=polygon_2, colour="green") + 
  geom_sf(data=polygon_3, colour="blue")

## Green is eastern range, blue is southern range, red is western range

## Write polygons as shape files.

st_write(polygon_1, "MLVA_1.140_50%KDE_western1.shp")
st_write(polygon_2, "MLVA_1.140_50%KDE_eastern.shp")
st_write(polygon_3, "MLVA_1.140_50%KDE_southern.shp")

## Determine areas

sf_use_s2(FALSE) # switches off spherical geometry

st_area(polygon_1) # gives areas in metres squared
st_area(polygon_2)
st_area(polygon_3)

# to get kilometres squared, use the following:

west_range_area<-st_area(polygon_1) / 1000000
east_range_area<-st_area(polygon_2) / 1000000
south_range_area<-st_area(polygon_3) / 1000000
