##########################
### Tidyverse like maps!##
##########################

## Install packages

library(tidyverse)
library(sf)
library(tmap)
library(dplyr)

#############################
## Quick and dirty mapping ##
#############################

## Map any part of the world - first import world map details and map them as below
## Use the %>% function to port coordinates into the ggplot function.

world_map<-map_data("world") %>%
ggplot(aes(x =long, y =lat, group=group)) + geom_polygon()

## Now, you can filter the world map coordinates to display the territory you are interested in.
## For example - UK

UK_map<-map_data("world") %>%
filter(region == 'UK') %>%
ggplot(aes(x=long, y=lat, group=group)) + geom_polygon()
 
 
## And you can go down to a sub region - eg Northern Ireland

NI_map<-map_data("world") %>%
filter(region == 'UK') %>%
filter(subregion == 'Northern Ireland') %>%
ggplot(aes(x=long, y=lat, group=group)) + geom_polygon(colour="black", fill="darkgreen")


## Then you can plot points on the map layer simply by adding in geom_point layers.
## Mapping of points *must* be in the lat and long format

#######################
## Mapping with tmap ##
#######################

# read in your shape file

NI<-st_read("location_and_filename")

## Plot map in tmap

map1<-tm_shape(NI) + tm_fill(col="darkgreen")

## read in your data file of points.

points<-read.csv("yourfilename", header=T, fileEncoding="latin1") ## if read error of csv occurs use fileEncoding argument

## Subset the data you need with dplyr function

cluster1<-dplyr::filter(points, cluster==1)
cluster2<-dplyr::filter(points, cluster==2)
cluster3<-dplyr::filter(points, cluster==3)
cluster4<-dplyr::filter(points, cluster==4)
cluster5<-dplyr::filter(points, cluster==5)
cluster6<-dplyr::filter(points, cluster==6)
cluster7<-dplyr::filter(points, cluster==7)
cluster8<-dplyr::filter(points, cluster==8)
cluster9<-dplyr::filter(points, cluster==9)
cluster10<-dplyr::filter(points, cluster==10)
cluster11<-dplyr::filter(points, cluster==11)
cluster12<-dplyr::filter(points, cluster==12)
cluster13<-dplyr::filter(points, cluster==13)
cluster14<-dplyr::filter(points, cluster==14)
cluster15<-dplyr::filter(points, cluster==15)
cluster16<-dplyr::filter(points, cluster==16)

######################################################################
## tmap interactive maps! Zoom in and out on features you build up. ##
######################################################################

tmap_mode("view")

## Then plot your map as above
