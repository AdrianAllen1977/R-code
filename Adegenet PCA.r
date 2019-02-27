####PCA analysis of microsatellite data in R

###Using adegenet package

##First set working drive

setwd("Desktop/R stuff/Irish badgers/TVR years 1&2")

###Open mmod library (contains adegenet)
library(mmod)

###Import genepop file of interest

data<-read.genepop("Geneland Pops.gen", ncode=3)

### adegenet doesn't like to work with missing genotype data (NAs).  So we need to replace them with the population mean genotype score.

### First find and count  the missing NAs

sum(is.na(data$tab))

### Then, using the scalene function, replace them with the mean.

 X<-scaleGen(data, NA.method = c("mean"))

###This rejigs the whole genotype data matrix for further analysis.  You can now do a PCA using the dudi.pca function.  By removing scale features you can more properly visualise the data.

pca1<-dudi.pca(X, cent=F, scale=F, scannf=F, nf=3)

###So, this analysis determines all the eigenvalues and principal components from the genotype data.  Now plot the principle components.

s.label(pca1$li)

####This graph is quite crude - but we can modify it using the s.class function - leave the graph page open in R and add in extra commands as below.

s.class(pca1$li, pop(data),xax=1, yax=2, sub="PCA 1-3", csub=2)

###Set the palette of colours to rainbow shades

col<-rainbow(length(levels(pop(data))))

###increase transparency so you can observe stacking / clustering of individuals

s.class(pca1$li, pop(data), xax=1, yax=2, col=col,.7, axesell=F, cstar=0,cpoint=4, grid=F, label=NULL)


#### to determine the variance explained by axes, first find the column of the eigenvalues

x<-pca1$eig

write.table(x, "PCA eigenvalues.txt")

###Then simply work out what the total variance explained by all eigen values is by summing them.  Then the proportion the first two axes explain.




