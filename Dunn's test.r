#### Dunn's test for differences between multiple medians

### Following a Kruskal Wallis test, you may find an overall significant difference in means, but the KW test doesn't tell you where the difference lies.

## The Dunn's test allows you to compare the medians from multiple groups to see if any are significantly different.

###  First assemble your data into a .csv file - must be csv as it is most dataframe like.
###  Have 1 column for each set of observations per 'treatment'.  

setwd("Desktop/R stuff/TVR yr 4/...........")
library(dunn.test)

#### Import the csv file you've made

a<-read.csv("yourfile.csv")

#### Next import the columns from the csv as separate vectors.

## Column 1, 2, 3,  etc etc

x<-a$X1


### Some treatments may have different nos of entries, so some may be NAs.  To remove these, import the column as before, but exclude the NAs as below.

y1<-a$X2
y<-na.omit(y1)

###Then, run the Dunn's test as below.

dunn.test(x=list(x,y,z))

### Note, the p vlaue required for signficance at the chosen level of alpha will be corrected for multiple comparisons a la Bonferroni.

