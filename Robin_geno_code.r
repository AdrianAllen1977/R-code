
#################################
## Robin VNTR and Spoligo code ##
#################################

library(ggplot2)
library(dplyr)
library(ggpubr)

spol<-read.table("spoligo.txt", header=T)

geno<-read.table("genotypes.txt", header=T)

## Check distributions of prediction data.

spol_dp<-ggplot(spol, aes(PRED)) + geom_density()
  
geno_dp<-ggplot(geno, aes(PRED)) + geom_density()

########################################################
## Plot probabilities and 95% CI on horizontal charts ##
########################################################

## Spoligo plot ##

# First get mean and median

spol_median<-spol %>% summarise (median=median(PRED, na.rm=TRUE)) %>% as.data.frame()

spol_mean<-spol %>% summarise (mean=mean(PRED, na.rm=TRUE)) %>% as.data.frame()

# Order spoligo predictions in order of increasing size

spol$Spoligotype<-factor(spol$Spoligotype, levels= c("130", "263", "129", "140", "142", "975", "273", "145", "1035"))


# change imported data to dataframe
spol1<-as.data.frame(spol)

# Make plot in ggplot

spol_plot<-ggplot(spol1, aes(x=PRED, y=Spoligotype)) + 
  geom_errorbarh(aes(xmin=LCI, xmax=UCI), color="darkslategray4") + ## If horizontal display you variable of interest is on X axis
  geom_point(size=1, color="orange") + 
  geom_vline(xintercept=spol_mean$mean, linetype=2, color="gray50") + ### Add mean vertical
  geom_vline(xintercept=spol_median$median, linetype=2, colour="turquoise") + ## Add median vertical
  ylab("Spoligotype") + 
  xlab("Prob. being a reactor") + 
  theme_classic()
  
## Genotype plot ##
  
# First get mean and median

geno_median<-geno %>% summarise (median=median(PRED, na.rm=TRUE)) %>% as.data.frame()

geno_mean<-geno %>% summarise (mean=mean(PRED, na.rm=TRUE)) %>% as.data.frame()

# Order Genotype predictions in order of increasing size

geno$Genotype<-factor(geno$Genotype, levels= c("14", "24", "73", "25", "19", "49", "122", "15", "5", "117", "6", "146", "7", "4", "13", "8", "2", "42", "3", "72", "27", "266", "10", "113", "1", "119", "297", "109", "18", "9", "87", "103", "11", "17", "44", "16", "53", "114", "23", "468", "387"))

# change imported data to dataframe
geno1<-as.data.frame(geno)

# Make plot in ggplot

geno_plot<-ggplot(geno1, aes(x=PRED, y=Genotype)) + 
  geom_errorbarh(aes(xmin=LCI, xmax=UCI), color="darkslategray4") + ## If horizontal display you variable of interest is on X axis
  geom_point(size=1, color="orange") + 
  geom_vline(xintercept=geno_mean$mean, linetype=2, color="gray50") + ### Add mean vertical
  geom_vline(xintercept=geno_median$median, linetype=2, colour="turquoise") + ## Add median vertical
  ylab("VNTR Genotype") + 
  xlab("Prob. being a reactor") + 
  theme(axis.text.y=element_text(size=5))
  
  
### Put all four plots on a single figure

ggarrange(spol_dp, spol_plot, geno_dp, geno_plot, labels = c ("A", "B", "C", "D"), ncol= 2, nrow =2)
