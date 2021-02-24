## Animating strain type data by freq, herds affected and year

library("ggplot2")
library("gganimate")
library("ggthemes")
library("gifski")

## Set theme
theme_set(theme_grey()+ theme(legend.box.background = element_rect(),legend.box.margin = margin(6, 6, 6, 6)) )

## Import data - you MUST tell R that strain type is categorical / a factor - not continuous

df<-read.table("VNTR visualisation.txt", header=T, colClasses = c("numeric", "factor", "numeric", "numeric"))

## plot the data

x<-ggplot(data=df, aes(Herds, Freq, colour=Strain_type, frame=Year))+geom_point(size=3.0)+labs(x="No. of herds affected by strain", y="Frequency of strain")

## Animate the plot

anim<-x+transition_time(frames = Year, cumulative=T)+labs(title="Year: {current_frame}") + shadow_mark(past=T)