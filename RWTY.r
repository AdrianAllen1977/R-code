##############################
### RWTY - R We there yet? ###
##############################

## R package to assess MCMC performance for Bayesian phylogenetic outputs
## Use this to import single combined runs or multiple independent runs before combining
## Assess overall convergence in combined trees and logs
## Assess how close independent runs are in terms of convergence / MCMC performance and outputs



#########################
## Single combined run ##
#########################

# Navigate to location where tree and log files from run are found.
## import trees and logs using command below:

my.beast.trees <- load.trees("path_to_tree_file", format = "beast",  trim = 5)

## Analyze the MCMC outputs with RWTY

analyze.rwty(my.beast.trees)

###############################
## Multiple independent runs ##
###############################


## Move all your independent tree and log files of the same dataset into a single directory.
## Do not navigate into that directory, rather point the import files command to the directory using a PATH
## the load.multi command parses the directory for tree and log files with similar names.

my.beast.trees <- load.multi("MASCOT_singles/", format = "beast")

## Analyze the MCMC outputs with RWTY

analyze.rwty(my.beast.trees)


## Variety of plots are produced