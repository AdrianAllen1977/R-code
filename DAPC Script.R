#### Discriminant Analysis of Principal Components Analysis (DAPC)

### STRUCTURE and other Bayesian clustering algorithms can infer clusters that have no biological meaning other than they are caused by isolation by distance - ie the results of a cline in distance.  It can therefore be useful to use a different approach that doesn't take account of linkage disequilibrium or Hardy Weinberg equilibrium to form clusters.  A PCA based approach can achieve this. DAPC can detect whether inferred clusters exhibit clinal variation or distinct non clinal 'island' effects.

### Open package and set working drive.

library(adegenet)
setwd("Desktop/R stuff/Irish badgers/DAPC")

### Import genepop file (genind object).
data<-read.genepop("all_ireland_badgers_dapc.gen", ncode=3)

### When using DAPC from first principles, it is best to get the package to infer how many likely clusters there are in the data.  This is achieved using the find.clusters command.  Set a maximum upper ceiling for this.

grp<-find.clusters(data, max.n.clust=40)

###You should now get a graph of how each eigen value in the PCA of the data accounts for accumulated variance in the dataset.  You can specify to retain whatever number of PCs.  Computational time for small nos of clusters is trivial, so opt to retain all (just put in an upper number equivalent to that on the graph).

### Having chosen the number of PCs to retain, you will now get a graph that shows decreasing Bayesian Information Criterion - an elbow curve.  The point of the curve at which it begins to plateau to when it is at its lowest, is generally the best estimate of K.  In a continuously distributed species, there may be no single value of K which is best due to subtle clinal variation.  For this dataset, K of 7-10 could be possible.  We'll try 7.

## Now to run the DAPC itself.

dapc1<-dapc(data, grp$grp)

### You now get a graph the same as the first one on PCs retained in find.clusters.  This time, to prevent over fitting of data and instability in probabilities assigned to individuals in group clusters, choose the number of components which account for most of the variance - the point at which the graph begins to plateau.  In this case, its around 30 PCs.

### Next you are asked how many discriminant functions you wish to retain.  For small numbers of clusters, this doesn't matter so much.  So, retain them all (6 in this case).  For tens of clusters, you can usually just retain the first few.

### Now, draw a scatterplot of the DAPC analysis.

scatter(dapc1)

### To tweak appearance of the scatterplot.  Move the da plot to an area where it doesn't impact on the plot, set the background to white and the point size and shape.
scatter(dapc1, posi.da="bottomleft", bg="white", pch=17.22)

### Use different colours by applying the 'myCol' function.
myCol<-c("black", "darkblue", "purple", "orange", "red", "blue", "green")

scatter(dapc1, posi.da="topleft", bg="white", pch=20, cstar=0, col=myCol, cex=3, clab=0, leg=TRUE, txt.leg=paste("Cluster", 1:7))

#### To get a table of which sample belongs to which group, do the following.
x<-dapc1$assign
write.table(x, "DAPC group memberships.txt")

### For a table of the posterior probabilities of belonging to a group.
y<-dapc1$posterior
write.table(y, "DAPC group membership probability.txt")

####To determine on your scatterplot, how much variance the linear discriminant axes explain, first quantify the total variance accounted for by all retained linear discriminant factors.
x<-sum(dapc1$eig)

###Then determine, for each linear discriminant factor, what proportion of total variance they account for.
varLD1<-(dapc1$eig[1]/x)*100
varLD2<-(dapc1$eig[2]/x)*100
varLD3<-(dapc1$eig[3]/x)*100
varLD4<-(dapc1$eig[4]/x)*100
varLD5<-(dapc1$eig[5]/x)*100
varLD6<-(dapc1$eig[6]/x)*100