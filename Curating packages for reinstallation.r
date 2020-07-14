### Package Installation for Changing R Versions

### When Changing R versions, you lose previously installed packages.
## This script helps you keep track of what you had to aid re-installation.

## First populate vector with a list of your packages.


ip<-installed.packages()

## Save the object as a .rds file

saveRDS(ip, "CurrentPackages.rds")

## After updating base R, load the file and re-install

ip<-readRDS("CurrentPackages.rds")

install.packages(ip[,1])