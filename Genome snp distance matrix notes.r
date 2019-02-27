### Producing a SNP distance matrix from a sequence alignment.


setwd("Desktop/R Stuff/TVR genomes")

###Open Biostrings library

library(Biostrings)

### Import your .fasta file where the alignments are.

genomes=readDNAStringSet("29Jun17bestsnp.fasta")

####Run dist matrix

a = stringDist(genomes, method="hamming")

#### Write the dist object produced to text file matrix.  You need to use the library MASS for this.

library(MASS)
write.matrix(a, "TVR pos badger 263 genomes SNP dist matrix.txt", sep = "\t")
