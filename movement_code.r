library(ggplot2)
library(dplyr)
library(lubridate)
library(rgdal)
library(ggthemes)

setwd("Desktop/movement_recording")

moves<-read.csv("TEST.csv", header=T)

NI<-readOGR(dsn=".", layer="NI2_outline_small") ## This layer uses the general GPS WGS84 GRID!

# Convert layer to dataframe
NI_df<-fortify(NI)


# Convert Date column to a proper date format (assuming your date column is 'Date')
moves2 <- moves %>%
  mutate(date = dmy(date)) 


# Ensure moves data is sorted by individual and date
moves3 <- moves2 %>%
  arrange(Tag_no, date)  

# Add columns for the next location (Lon_next, Lat_next) for each individual
moves4 <- moves3 %>%
  group_by(Tag_no) %>%  # Group by individual if multiple animals exist
  mutate(Lon_next = lead(Lon), Lat_next = lead(Lat)) %>% 
  ungroup()

# Plot with polygons, points, and arrows
ggplot() +
  # Plot the shapefile layer
  geom_polygon(data = NI_df, aes(x = long, y = lat, group = group), 
               colour = "black", fill = "white") +
  
  # Plot the points with date labels
  geom_point(data = moves4, aes(x = Lon, y = Lat), color = "blue", size = 3) +
  
  geom_text(data = moves4, aes(x = Lon, y = Lat, label = date), size = 3, hjust=-0.2, vjust=-0.2, col="blue") +

geom_curve(data = moves4,
           aes(x = Lon, y = Lat, xend = Lon_next, yend = Lat_next),
           arrow = arrow(length = unit(0.5, "cm")),
           color = "orange", size = 1, curvature = 0.2) +
  
  # Set map theme
  theme_map()
  
  # Set map theme
  theme_map()

  