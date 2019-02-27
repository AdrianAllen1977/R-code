### Producing a dendrogram in R
### First convert your VNTR data into a text file.  Rows with sample numbers and columns of allele data.

setwd("Desktop/R Stuff/RTA Badgers VNTR")

### Read this table into R - note the header must be false and rownames set 1 - this means the isolate id number is what is used on the dendroram - not an index!

vntr<-read.table("Badger RTA VNTR.txt", header=F, row.names=1)

#### Building a dendrogram with raw VNTR data can build spurious trees - some loci vary more than others and can effect branching patterns.
### Weighting the allele calls by 1-diveristy index helps (i.e. the probability of two alleles being the same)
### From MLVA surveys, we know the allelic DIs for all M. bovis loci
### 1-DI for all 8 loci Mv2163b=0.811 Mv4052=0.663 Mv2461=0.642 Mv1955=0.564 Mv1895=0.607 Mv2165=0.631 Mv2163a=0.532 mv3232=0.453 

### Extract the list of names of strain type as a vector from the VNTR table
x<-row.names(vntr)

###Transform allele calls by 1-DI and bind the strain type ID to them.
vntr2<-cbind(x,vntr$V2*0.811,vntr$V3*0.663,vntr$V4*0.642,vntr$V5*0.564,vntr$V6*0.607,vntr$V7*0.631,vntr$V8*0.532,vntr$V9*0.453)



### Write the new VNTR table to file
write.table(vntr2, "Weighted_VNTR.txt", col.names=F, row.names=F, quote=F)

### reimport the new weighted VNTR file
vntr3<-read.table("yourfilename", row.names=1)


#### Now, using the allelic data, construct a distance matrix of distance between each sample.  Use the euclidean distance method of the dist function. 
## The manhattan method produces absolute distances between two strain types
d<-dist(vntr3, method="manhattan")


#### You can write this genetic distance matrix to an txt file by doing the following - the sep function allows you to indicate the type of separation you want between data - in this case a tab is represented by “\t”

library(MASS)

write.matrix(d, "RTA badger VNTR dist matrix.txt", sep = "\t")

### Now, using the hierarchical clustering function, hclust, plot the distance matrix.  Average method is UPGMA.

hc<-hclust(d, method="average")

## Then simply plot the clustering output.

plot(hc, hang=-1, main="Brucella VNTR Dendrogram", ylab=NULL, cex=0.6, yaxis=NULL, xaxis=NULL)

### Alternatively, plot the hc data using the ape package to make a more traditional phylogram.

# load package ape
library(ape)

# plot basic tree
plot(as.phylo(hc), cex = 0.9, label.offset = 1)