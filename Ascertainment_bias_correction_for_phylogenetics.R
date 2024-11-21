## Determining breakdown of invariant sites from a SNP alignment.

###### When building a Bayesian time stamped phylogenetic tree, if you are using just variant sites
###### You need to correct for ascertainment bias by also taking account of invariant sites.
###### If you don't, this can affect the timing of ancestral nodes as you don't account for other sites that could have mutated but have since reverted to the consensus reference call.
###### So, you need to alter the BEAST XML file to include invariant calls.

## So, find out the frequencies of As, Cs, Gs and Ts in the filtered variant SNP alignment your pipeline has produced.


## In the RedDog filtered csv of SNP sites, you can do this by simply filtering by the column for the ref sequence by each individual letter - which lets you know how many reference A, C, G and T nucleotides are present in the variant sites.
## OR you can use the script below

## First install the bioconductor installer

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

## Then us the installer to install Biostrings

BiocManager::install("Biostrings")

setwd("Desktop/1.140_paper/fastas")

library(Biostrings)

## Import your alignment sequence fasta file of variable sites

align<-readDNAStringSet("1.140_Dec22_filtered.fasta")

### Count the number of A, C, G and Ts in the Reference - IT MUST BE THE REFERENCE!

A<-letterFrequency(align$Ref, "A")
C<-letterFrequency(align$Ref, "C")
G<-letterFrequency(align$Ref, "G")
T<-letterFrequency(align$Ref, "T")

## Check that your indiviudal base counts add up to the length for your alignment

A+G+C+T

## Then, for the reference genome, you need to count up the total number of As, Cs, Gs and Ts across the whole genome.  You can do that with the Biostrings package in R

## Read in your reference genome fasta file

refseq<-readDNAStringSet("AF212297.fasta")

## Now, count the number of As, Cs, Gs and Ts using the functions below:

A2<-letterFrequency(refseq, "A")
C2<-letterFrequency(refseq, "C")
G2<-letterFrequency(refseq, "G")
T2<-letterFrequency(refseq, "T")

## Check that your individual base counts add up to the length for the reference genome

A2+G2+C2+T2

## Subtract to find invariant sites for each base

invA<-A2-A
invC<-C2-C
invG<-G2-G
invT<-T2-T

## Print off the invariants as a table / dataframe
invariant_site_frequencies<-data.frame((invA), (invC), (invG), (invT))

invariant_site_frequencies

## You can then use these figures to amend your xml file for BEAST.

## For details, go to https://www.beast2.org/2019/07/18/ascertainment-correction.html

#Amend your XML using the text below - insert after the </data> line that demarcates the end of the main variable site alignment.

</data>
  
  
  <data id="alignment"
spec="FilteredAlignment" filter="-"
data="@original-alignment"
constantSiteWeights="747065 1430273 1424447 747741"/>   
