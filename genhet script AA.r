### Using genhet to determine individual levels of heterozygosity

##Set working drive
setwd("Desktop/R stuff/Irish badgers/NI badger heterozygosity July 18")

### Import data file of genotypes - convert all unknown genotypes (0s) to NA
data<-read.table("NI_RTA_badger_genos.txt", sep="\t", header=T)

### Create and import list of locus names - create text file of list of loci, in same order as they appear in the genoype file. and simply import as below
locusname<-scan("Badger_STR_locus_list.txt", what="character", sep="\t")

### Open the gtools library
library(gtools)

## From the Genhet.r text file, paste in the full text of all the commands which will create a function in R.
### Then run the function using the following script.
het<-GENHET(dat=data,estimfreq="T",locname=locusname)

###Write table of heterozygosity metrics per sample.
write.table(het,"NI badger heterozygosity.txt",sep="\t",row.names=F,quote=F)