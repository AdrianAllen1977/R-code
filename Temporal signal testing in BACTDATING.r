##Temporal signal testing in BACTDATING 
## See Didelot et al 2018 - Nucl Acid Res46(22) e134

## Install BactDating from github

devtools::install_github("xavierdidelot/BactDating")

### Packages
library(BactDating)
library(ape)
library(coda)

## Before we can use BactDating, we have to import the ML tree we have constructed in RAXML using the 'ape' package

tree<-read.tree("your_tree_name")

## If your tree is being imported from RaxML, you may find branch lengths are being expressed as substitutions per site.  So, you need to change these to actual lengths.

## Check the format of branch lengths by working out the sum of all branch lengths for the tree.

sum(tree$edge.length)

## If the sum of lengths equals 1 or just over 1, then you likely have the substitutions per site format.
## Change the format using the script below - L is the number of sites from the fasta that made the tree:

tree$edge.length<-tree$edge.length*L


## You can export this tree with the new lengths added.

write.tree(phy=tree, file="RaxML bipartitions tree actual lengths")

## Now that you have your tree imported, we need to get the tip dates exported.  Dependent of course on you having labelled the tips in the first place in the fasta!

labels<-tree$tip.label

## This should give you the format bovine_1993 etc for all tips.

### Now split the label and take only the dates / years.
### First make an empty vector called tipdates
tipdates<-c() 

### Now, populate the vector using a 'for' loop script
for(index in 1:length(tree$tip.label)){tipdates[index] <-strsplit(tree$tip.label[index], split="_")}

### Make the vector into a matrix with columns.
tipdates2<-matrix(unlist(tipdates), ncol=2, byrow=TRUE)

## Now extract the second column which has the dates in it.
dates<-tipdates2[,2]

## Export as a table with no quotes
write.table(dates, file="263_clade_dates.txt", quote=FALSE, row.names=FALSE, col.names=FALSE)

## Reinput the dates as an object d

d<-read.table("yourname.txt", header=F)

### Now, you can take the tree file, and the dates data and run BACTDATING to look for temporal signal.
## First, BACT DATING only take decimal dates. So add the same decimal on to each date
d2<-d+0.5

## The dates are currently in a data frame and need to be a vector. Make them a vector using 'unlist'
d3<-unlist(d2, use.names = F)

### Now, make a rooted tree for use in BactDating
rootedtree<-initRoot(tree, d3)

## Rooting shouldn't change the order of the labels, but check
labels2<-rootedtree$tip.label

## Compare the unrooted label order to the rooted
labelcompare<-cbind(labels, labels2)

## You can export this table and check it in Excel using a logical "IF" test
write.table(labelcompare, file="rooted vs unrooted labels.txt", col.names=c("unrooted", "rooted"))

### Or you can just do the logical test in R
X<-labelcompare[,1]==labelcompare[,2]
summary(X)

## If all columns are identical you'll see a "TRUE" return for whatever the n number was

## Now, test the temporal signal
tempsig<-roottotip(rootedtree, d3)

## A plot will be made with regression data.
## Save a copy of the plot

## Now, to test if the temporal signal you've got is robust
## Compare to the same tree, but make the tips all have only one date
## The latest date in your dataset will do.

## First make a vector for all tips with same date

d4<-rep(2017.5, n=493)

### Now run the BactDate function to create a Bayesian time stamped phyologeny for your first tree with actual dates.
## You need to make sure the MCMC converges, so make nbIts large enough to make 'hairy caterpillars'!
## NOTE - if you ahve some idea form literature or Tempest or otherm you can make the MCMC convergence easier
## By inputting initMu - the initial mutation rate - effectively a prior. 
model1<-bactdate(rootedtree, d3, initMu=0.3, nbIts=5e6, showProgress=T)

## check for convergence and hairy caterpillars!
plot(model1, 'trace')

## Now do the same for the rooted tree, but using the tip dates which are all the same.
## For something you expect to have very little temporal signal, convergence can be a problem
## So, provide an initMu starting point that is low to help the MCMC converge
model2<-bactdate(rootedtree, d4, initMu=0.1, nbIts=5e7, showProgress=T)

## Note the show progress argument that allows you to see how far along the MCMC is

### The single date model may require more time and more iterations to converge

## As well as inspecting your traces, you can check the ESS per parameter

model1mcmc<-as.mcmc(model1)
effectiveSize(model1mcmc)

## All parameters with ESS above 100 is what you're looking for here.
## If not above 100, then run the MCMC chain for longer.

## If both models have converged well, and have good ESS values
## Do the model comparison

modelcompare(model1, model2)

## If the DIC is lower for one model than the other, then the lowest one has the best fit.



