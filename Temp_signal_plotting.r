######################
##Temporal sig plots##
######################

## PACKAGES

library(ggplot2)
library(ggpubr)

## Set drive

setwd("Desktop/1.140_paper/Clock_rates/")

#########################
## Tempest output plot ##
#########################

a<-read.table("1.140_temp_sig.txt", header=T)

head(a) # Check column and row names for plotting

## Plots points with linear model line through them

aplot<-ggplot(a, aes(x=date, y=distance)) + 
	geom_point(colour="orange") + 
	geom_smooth(method=lm) + 
	ylab("Root to tip dist") + 
	xlab("Year") + 
	theme_classic()
	
###################################	
## Strict clock model and randos ##
###################################

b<-read.table("strict_clock_rates_&_randos.txt", header=T)
b1<-as.data.frame(b)

head(b1) ## check column and row names for plotting

## Set order of X axis models

b1$Model<-factor(b1$Model, levels=c("Strict_constant", "Strict_skyline", "Strict_BDSky", "Strict_random1","Strict_random2", "Strict_random3", "Strict_random4", "Strict_random5", "Strict_random6", "Strict_random7", "Strict_random8", "Strict_random9", "Strict_random10" ))

## Make plot

bplot<-ggplot(b1, aes(x=Model, y=Sub_rate)) + 
	geom_point(size=4, colour="orange") + 
	geom_errorbar(aes(ymax=HPD_upper, ymin=HPD_lower), colour="blue") + 
	ylab("Sub rate") + 
	xlab("Model") + 
	theme_classic()

## Make model names vertical

bplot2<-bplot + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

####################################
## Relaxed clock model and randos ##
####################################

c<-read.table("relaxed_clock_rates_&_randos.txt", header=T)
c1<-as.data.frame(c)

head(c1) ## check column and row names for plotting

## Set order of X axis models

c1$Model<-factor(c$Model, levels=c("Relaxed_constant", "Relaxed_skyline", "Relaxed_BDSky", "Relaxed_random1","Relaxed_random2", "Relaxed_random3", "Relaxed_random4", "Relaxed_random5", "Relaxed_random6", "Relaxed_random7", "Relaxed_random8", "Relaxed_random9", "Relaxed_random10" ))

cplot<-ggplot(c1, aes(x=Model, y=Sub_rate)) + 
	geom_point(size=4, colour="orange") + 
	geom_errorbar(aes(ymax=HPD_upper, ymin=HPD_lower), colour="blue") + 
	ylab("Sub rate") + 
	xlab("Model") +
	theme_classic()
	
## Make model names vertical

cplot2<-cplot + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


###################################
#### Arrange into final figure ####
###################################

ggarrange(aplot, bplot2, cplot2, labels=c("A", "B", "C"), ncol=2, nrow=2)