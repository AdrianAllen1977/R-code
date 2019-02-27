#### PACKAGES ####
library(ape)
library(phangorn)
library(phytools)


#### READ FASTA ####

# Read in the FASTA file
sequences<- read.dna("/Users/josephcrisp1/Desktop/HelpingAdrian_AFBNI/TVR_263_single_isolates.fasta",
                            format = "fasta", skip=1) # skip first line - I added this line into FASTA: nSequences length


#### CHOOSE SUBSTITUTION MODEL ####

# Convert the sequences from a DNAbin format into a PhyDat format
sequencesPhyDat <- phyDat(sequences, type = "DNA", levels = NULL)

# Run model testing to select appropriate substitution model
modelTestResults <- modelTest(sequencesPhyDat, model = c("JC", "HKY", "GTR"))

# Get best model
cat(paste("Best substitution model:", 
          modelTestResults$Model[which.min(modelTestResults$AIC)], "\n"))


#### BUILD PHYLOGENY ####

# Build the distance matrix
distanceMatrix <- dist.dna(sequences, model="JC69")

# Build a neighbour joining tree - an initial tree
njTree <- nj(distanceMatrix)

# Compute likelihood of the initial Neighbour Joining tree given sequences
likelihoodObject <- pml(njTree, sequencesPhyDat)

# Set the controls for the Maximum Likelihood algorithm
controls <- pml.control(maxit=100000, trace=0)

# Run maximum likelihood
fittingOutput <- optim.pml(likelihoodObject,
                           optNni = TRUE, # Optimise topology
                           optInv = TRUE, # Optimise proportion of variable sites
                           model = "JC", # Substitution model
                           rearrangement="NNI", # Nearest Neighbour Interchanges
                           control=controls)

# Bootstrap the result of maximum likelihood
bootstrapResults <- bootstrap.pml(
  fittingOutput, # Use Maximium Likelihood settings on bootstrapped sequences
  bs = 100, # Number times to bootstrap sequences
  optNni = TRUE, # Use Nearest Neighbour Interchanges in tree building
  jumble=TRUE) # Jumble bootstrapped sequences before building trees

# Get phylogenetic tree with bootstrap values - plotBS will return a tree that can be used
tree <- plotBS(
  fittingOutput$tree,
  bootstrapResults,
  p = 50, # Plot bootstrap values if node in >=50 bootstrap trees
  type="phylogram", cex=0.1) # Type of phylogenetic tree shape to plot

### To ensure you can get a SNP difference scale bar on subsequent phylogenies.
### You need to alter the branch lengths to SNPs and not proportions.
tree$edge.length<-tree$edge.length*nSitesInFASTA

###If you want to write the tree to the Newick format that can be read by Figtree, do the following.
write.tree(phy=tree, file="TVR genomes_phylo Aug18")


#### ANCESTRAL STATE RECONSTRUCTION ####

# Root the tree
rootedTree <- root(tree, 
                   outgroup="__Reference", # Set the isolate to root the tree on - can also use node
                   resolve.root=TRUE) # Ensures new root will be bifurcating

# Remove any non-dichotomous branching nodes (nodes with more than two daughters)
rootedTree <- multi2di(tree, random=TRUE)

# Make branches with lengths of 0 very very small
rootedTree$edge.length[rootedTree$edge.length <= 0] <- 0.000001

# Get the sequence states from their names
sequenceStates <- list()
for(name in names(sequencesPhyDat)){
  
  sequenceStates[[name]] <- strsplit(strsplit(name, split="_")[[1]][4], split="2")[[1]][1]
}


# Identify the states for the tips of the phylogeny
tipStates <- c()
for(index in 1:length(rootedTree$tip.label)){
  tipStates[index] <- sequenceStates[[rootedTree$tip.label[index]]]
}

# Generate some random states - AS AN EXAMPLE - YOU'LL NEED TO INSERT ACTUAL STATES AS A VECTOR HERE
#states <- sample(c("A", "B"), size=length(njTree$tip.label), replace=TRUE)

### The lead state is the "Reference" genome - it has been entered as an NA because the scripts above were only pulling out the label badger or bovine.
### So, relabel this using the following script.
tipStates[1]<-"Bovine"

# Estimate ancestral states under a ER model
ancestralStateFitting <- ace(tipStates, rootedTree, 
                             model="ARD", # "ER" Equal rates; "ARD" All rates different
                             type="discrete") # Discrete states


#### PLOT TREE WITH ANCESTRAL STATES ####

# Set the plotting margins: c(bottom, left, top, right)
par(mar=c(0, 0, 0, 0))

# Assign colours to the states
stateColours <- setNames(c("red", "blue"), c("Badger", "Bovine"))

### Plot phylogenetic tree without ancestral states
plot.phylo(rootedTree, cex=0.1)


# Add tip shapes and colours - for the states
tiplabels(pie=to.matrix(tipStates,c("Badger", "Bovine")), # Provide the state of each tip
          piecol=stateColours, # Provide the colour assigned to each state
          cex=0.1) # Set the size of the tip circle

### Plot phylogenetic tree with ancestral states
plot.phylo(rootedTree, type="phylogram", # Set the shape of the tree to be a fan
           show.tip.label=FALSE) # Don't plot the tip labels

### Add scale bar
add.scale.bar(30, 0, length=2, lcol="red", lwd=1, cex=0.5)


# Add pie charts at each internal node
nodelabels(node= 1:rootedTree$Nnode+Ntip(rootedTree), # Provide an array with all the internal node IDs (nodes numbered from nTips onwards...)
           pie=ancestralStateFitting$lik.anc, # Assign pie charts to each node - based on the outputs from ancestral state estimation
           piecol=stateColours, # Set the colours to be used in the pie charts
           cex=0.1) # Set the size of the pie charts

# Add circles to indicate the states of the tips
tiplabels(pie=to.matrix(tipStates,c("Badger", "Bovine")), # Provide the state of each tip
          piecol=stateColours, # Provide the colour assigned to each state
          cex=0.1) # Set the size of the tip circle

# Add a legend
legend("bottomright",
       legend=c("Badger", "Bovine"), # Name the labels used in the legend
       pch=19, # Set the shape of the points used in legend
       col=c("red", "blue"), # Set the colour of each label point
       bty="n") # Remove box surrounding legend

#####################
## Remove the reference sequence from the tree
rootedTreeWithoutRef <- drop.tip(rootedTree, tip="__Reference")
tipStates <- tipStates[-1]

### Plot phylogenetic tree without ancestral states
plot.phylo(rootedTreeWithoutRef, align.tip.label=FALSE, cex=0.1)

# Add tip shapes and colours - for the states
tiplabels(pie=to.matrix(tipStates,c("Badger", "Bovine")), # Provide the state of each tip
          piecol=stateColours, # Provide the colour assigned to each state
          cex=0.1) # Set the size of the tip circle

### Plot phylogenetic tree with ancestral states
plot.phylo(rootedTreeWithoutRef, type="fan", # Set the shape of the tree to be a fan
           show.tip.label=TRUE, cex=0.05) # Don't plot the tip labels

# Use nodelabels() function to find out the numbers assigned to each node

# Add pie charts at each internal node
nodelabels(node= 1:rootedTreeWithoutRef$Nnode+Ntip(rootedTree), # Provide an array with all the internal node IDs (nodes numbered from nTips onwards...)
           pie=ancestralStateFitting$lik.anc, # Assign pie charts to each node - based on the outputs from ancestral state estimation
           piecol=stateColours, # Set the colours to be used in the pie charts
           cex=0.05) # Set the size of the pie charts

# Add circles to indicate the states of the tips
tiplabels(pie=to.matrix(tipStates,c("Badger", "Bovine")), # Provide the state of each tip
          piecol=stateColours, # Provide the colour assigned to each state
          cex=0.05) # Set the size of the tip circle

### Add scale bar
add.scale.bar(length=2, lcol="black", lwd=3, cex=1)

# Add a legend
legend("bottomright",
       legend=c("Badger", "Bovine"), # Name the labels used in the legend
       pch=19, # Set the shape of the points used in legend
       col=c("red", "blue"), # Set the colour of each label point
       bty="n") # Remove box surrounding legend

# Reset plotting margins
par(mar=c(5.1, 4.1, 4.1, 2.1))


