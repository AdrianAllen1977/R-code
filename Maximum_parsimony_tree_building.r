####################################### Maximum parsimony tree building ######################################### Packageslibrary(ape)library(phangorn)library(phytools)## Import fasta alignmentsequences<- read.dna("filename",+                      format = "fasta")## Make fast import a PhyDat objectsequencesPhyDat <- phyDat(sequences, type = "DNA", levels = NULL)## Make a starter neighbour joining treedistanceMatrix <- dist.dna(sequences, model="JC69")njTree <- nj(distanceMatrix)## Now, make a maximum parsimony treetree_MP<-pratchet(sequencesPhyDat, start=njTree, minit=200)##  This tree isn't the finished article## You need to assign edge lengths and nodes with acctrantree_MP<-acctran(outbreak_tree_MP, sequencesPhyDat)## And remove any very small distances in edge lengthtree_MP<-di2multi(outbreak_tree_MP)## Write tree to file and explore in figtreewrite.tree(tree_MP, file="MP_tree.tree")