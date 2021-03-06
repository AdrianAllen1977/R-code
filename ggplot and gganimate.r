#### ggplot for mapping and gganimate  for animating maps

library("ggplot2")
library("gganimate")
library("rgdal")
library("sp")
library("raster")
library("ggthemes")
library("gifski")

## Import the file of points you want to work with.

x<-read.table("filename", header=T)

### Import the shape file you want to plot points on.
### Ensure the projections are the same for both or else it won't work.

TVRzone<-readOGR(dsn=".", layer="TVR")

TVRbuff<-readOGR(dsn=".", layer="2km_Buffer")

## Check projection
proj4string(TVRzone)

### Make sure points use same projection.

## Before you can plot a shapefile in ggplot, you need to make it into a dataframe.
## Using the fortify command from ggplot.

TVRzone2<-fortify(TVRzone)

TVRbuff2<-fortify(TVRbuff)

## Now use ggplot - plot points after the polygon layers or they'll be hidden!
## Plot the polygon layers in the stacked order you wish them appear 

map1<-ggplot(data=x, aes(x=X, y=Y)) +
geom_polygon(data=TVRbuff2, aes(x=long, y=lat, group = group), colour="black", fill="white") +
geom_polygon(data=TVRzone2, aes(x=long, y=lat, group = group), colour="black", fill="white") +
geom_point(colour="blue") +
theme_map()

### Making an animated version

anim<-map1+transition_manual(frames = Year, cumulative=T)+labs(title="Year: {current_frame}") + shadow_mark(past=T)

### Save a copy of the animation as a .gif - 
### the deafult on this function saves the last animation made.

anim_save("Clade1.gif")

