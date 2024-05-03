#####################################
## Maximum parsimony tree building ##
#####################################

## Packages
library(ape)
library(phangorn)
library(phytools)

## Import fasta alignment

sequences<- read.dna("filename", format = "fasta")

## Make fast import a PhyDat object

sequencesPhyDat <- phyDat(sequences, type = "DNA", levels = NULL)

## Make a starter neighbour joining tree

distanceMatrix <- dist.dna(sequences, model="JC69")
njTree <- nj(distanceMatrix)

##  Run a bootstrap of the tree

## First create a function

fun <- function(x) nj(dist.ml(x))

## Then run the function to create a bootstrapped set of possible trees

bs_njTree <- bootstrap.phyDat(sequencesPhyDat,  fun)


## Now, make a maximum parsimony tree

tree_MP<-pratchet(sequencesPhyDat, trace=0, minit=200)
parsimony(tree_MP, sequencesPhyDat)

##  This tree isn't the finished article
## You need to assign edge lengths and nodes with acctran

tree_MP<-acctran(tree_MP, sequencesPhyDat)

## And remove any very small distances in edge length

tree_MP<-di2multi(tree_MP)

## Now, using the bootstrap trees you created earlier, plot your max parsimony tree with bootstrap values.

treeMP2<-plotBS(midpoint(tree_MP), bs_njTree, type="phylogram", cex=0.5)

## Check parsimony score of tree

parsimony(treeMP2, sequencesPhyDat)

write.tree(treeMP2, file="MP_tree.tree")

