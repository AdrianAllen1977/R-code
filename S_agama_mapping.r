#####################################
### S. agama mapping investigation ##
#####################################

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

setwd("S_agama")

# Read in NI map layer
NI<-readOGR(dsn=".", layer="NI_Outline") ## This layer uses the general GPS WGS84 GRID!

# Convert layer to dataframe
NI2<-fortify(NI)

# Read in S.agama sequence locations

x<-read.table("S_agama_locs.txt", header=T, sep="\t")

## check data structure / format
str(x)

## Change x and y variables to numeric format
x$x<-as.numeric(x$x)
x$y<-as.numeric(x$y)

# Filter sequences by species
badgers<-x %>% filter(SPECIES=="Badger")

sheep<-x %>% filter(SPECIES=="Ovine")

cattle<-x %>% filter(SPECIES=="Bovine")

# Filter further by tree clade

clade_A_badgers<-badgers %>% filter(clade=="A")

clade_B_badgers<-badgers %>% filter(clade=="B")

clade_C_badgers<-badgers %>% filter(clade=="C")

clade_A_sheep<-sheep %>% filter(clade=="A")

clade_B_sheep<-sheep %>% filter(clade=="B")

# Make map
# Base map first
map1<-ggplot(data=x, aes(x=lon, y=lat)) +
  geom_polygon(data=NI2, aes(x=long, y=lat, group = group), colour="black", fill="white") +
  geom_point(data=clade_A_badgers, aes(x=lon, y=lat), shape = 19, color="red", size=0.5) +
  geom_point(data=clade_B_badgers, aes(x=lon, y=lat), shape = 19, color="blue", size=0.5) +
  geom_point(data=clade_C_badgers, aes(x=lon, y=lat), shape = 19, color="darkgreen", size=0.5) +
  geom_point(data=clade_A_sheep, aes(x=lon, y=lat), shape = 6, color="red", size=0.5) +
  geom_point(data=clade_B_sheep, aes(x=lon, y=lat), shape = 6, color="blue", size=0.5) +
  geom_point(data=cattle, aes(x=lon, y=lat), shape = 17, color="red", size=0.5) +	
  theme_map()

# Add scalebar
map2<-map1+ ggsn::scalebar( dist_unit="km", dist=20, x.min=-10, x.max=-8, y.min=55, y.max=55.5, transform=T, model="WGS84", st.bottom=T, st.dist=0.1, st.size=4)

# Add compass point
map3<-map2 + ggsn::north2(map2, x=0.1, y=0.9, scale=0.2)