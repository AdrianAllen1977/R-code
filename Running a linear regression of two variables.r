###Running a linear regression of two variables. Namely map distance versus genetic differentiation.

## Set drive
setwd("Desktop/R stuff/TVR genomes")

##Read in data - use the header=T to ensure the column titles are the same as in the csv
a<-read.table("bovis gen dist vs badger gen dist.txt", header=T)

####Plot data to get a feel for how they look.  Include all labels
b<-plot(Genome_Snp_dist ~ Badger_STR_dist, data=a, pch=16, main="Bovis vs Badger Genetic distance", xlab="Pairwise Badger micro satellite genetic dist. b", ylab="Pairwise SNP difference between M. bovis genomes")

## Press b to view plot again - leave plot open for other manipulations.

### Model the relationship between variables.  Always assign the linear model a character / id so that you can view its outputs.

c<-lm(Bovis_dist ~ Badger_dist, data=a)

###press c to view intercept and slope data.  Intercept is 1st, slope 2nd

#### Further summarise data from regression to see goodness of fit and significance etc

summary(c)

###to plot the established regression line through the points in the graph.
abline(c)

