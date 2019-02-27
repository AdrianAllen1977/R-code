##### Making a histogram to investigate distributions of data

### When manipulating data from first principles in R, very often data will be returned from a specific process / function as numeric vectors that can be operated on directly with the "hist" function.  However, if you are importing data from a pre-made table or text file, the data is imported as a dataframe, which the "hist" function cannot work on.

### A solution is to input the data as normal.

setwd("Desktop/R stuff/GB&IRE badger paper 2018")
data<-read.table("Percentage GB in Irish badgers.txt")

### The ouput should look like this

       V1
1   27.73
2    4.59
3   62.75
4   28.87
5    4.34
6    0.61
7    0.67
8    1.06
etc etc

### Should you try to work on the dataframe, you will get an error saying the data is not numeric.

#### So, from the data frame, you need to isolate the column of interest as a vector of numeric data.  So, use the $ signifier along with the V1 column header

b<-data$V1

### Then, simply run the "hist" function

hist(b, ylim = range(0:400), main="Distribution of Percentage GB Genetic Heritage in Irish badgers", ylab="Frequency", xlab="Percentage GB genetic heritage")