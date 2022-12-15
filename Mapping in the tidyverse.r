#####################
### Tidyverse maps!##
#####################

## Install packages

library(tidyverse)


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

