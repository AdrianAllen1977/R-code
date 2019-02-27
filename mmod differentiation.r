####Genetic differentiation tests in R

###Using mod and adegenet packages

##First set working drive

setwd("Desktop/R stuff/Irish badgers")

###Open mmod library (contains adegenet)
library(mmod)

###Import genepop file of interest

data<-read.genepop("irishsubpopgenepop.gen", ncode=3)

###Calculate Jost's D statistic for all subpopulation pairs

d<-pairwise_D(data)

### Write as a matrix
library(MASS)
x<-as.matrix(d)
write.table(x, file="Josts.txt")

