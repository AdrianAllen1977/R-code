#####Running a Mantel test

##First, prepare your distance matrices, both the geographic distance and the genetic distance between population clusters in excel and save as tab delimited files.

##Then do the following

setwd("Desktop/R Stuff/RTA badgers VNTR")

### install package "ade4"

install.packages("ade4")

### Open ade4 library

library(ade4)

##import map pairwise distance file.

map<-read.table("RTA Strains centroid distance matrix.txt")

####import VNTR genetic distance matrix


gen<-read.table("RTA badger VNTR dist matrix.txt")


###All these matrices need to be converted to dist objects.

mapd<-as.dist(map)

gend<-as.dist(gen)


### Then run the mantel test for both comparisons

mantel.rtest(mapd, gend, nrepet=9999)


###you can plot the results of the mantel test

plot(mantel.rtest(mapd, gend, nrepet=9999))


