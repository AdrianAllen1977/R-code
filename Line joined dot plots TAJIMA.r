####################################
## Plotting joined line dot plots ##
####################################

## Example, Tajima's D across M. bovis genomes

library(ggplot2)
library(ggpubr)
library(scales)


## Read in data

setwd("yourdrive")

x<-read.table("yourdatafilename.txt", header=T)

plot<-ggplot(x, aes(x=BIN_START, y=TajimaD)) +
	geom_point(color="darkslategray4") +
	geom_line(color="orange") +
	geom_hline(yintercept=0, color="turquoise") +
	ylab("Tajima's D") +
	xlab("Genome Sliding Window") +
	theme_classic()
	
	
## Remove scientific notation from X axis	

plot2<-plot + scale_x_continuous(labels = comma)
	

ggarrange(tenkbplot, hundredkbplot, twohundredkbplot, onemegabaseplot, labels = c ("A", "B", "C", "D"), ncol= 2, nrow =2)


## Check correlation and linear relatonships between pi and Tajima's D

a<-read.table("BTB_all_samples_pi_100kb.windowed.pi", header=T)
b<-read.table("BTB_all_samples_Tajima_100kb.Tajima.D", header=T) ## Read in same window size for both!

# make a new table of columns from each initial table

c<-cbind(b$BIN_START, b$TajimaD, a$PI)
colnames(c)<-c("start", "Tajima", "PI")

## Make a dataframe of what you just created

d<-as.data.frame(c)

##  Check correlation of variables

cor.test(d$PI, d$Tajima)

## Run linear model of relationship

model<-lm(PI~Tajima, data=d)
summary(model)

## Plot both variables with regression line

plot<-ggplot(d, aes(x=Tajima, y=PI)) +
  geom_point(colour="orange") +
  geom_smooth(method="lm") +
  ylab("Mean pairwise nucleotide diversity (pi)") +
  xlab("Tajima's D") +
  ggtitle("Tajima's D vs pi 100kb sliding windows") +
  theme_classic()
	