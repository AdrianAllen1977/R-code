#### 1 Way Anova Script
### For comparing the means of more than two variables - do not do multiple t-tests.

setwd("Desktop/R stuff/TVR yr 4/Recapture etc etc etc")

### Have a data set assembled - x numbers of columns of values each with a categorical group they belong to.
### Next, import the data. Its best to have the data in the form of .csv as this behaves like a dataframe.  So if you have an unequal number of records per category, they will be converted to NAs.  .txt files with unequal numbers per category won't be imported.

data<-read.csv("filename")

##Check the summary data to get means, quartiles etc,
summary(data)


#### To run an ANOVA we need to get all the data into two columns - so all observations stacked on top of each other, with the second column indicating which category they belong to.

data2<-stack(data)

### If you had differing number of observations per category, the NAs will now have been stacked into this combined set.  So, you need to remove them.

data3<-na.omit(data2)

### Check you have the correct no. of entries per category.
summary(data3)

### Rename the columns in the dataframe youve made.  ONly single words per column title!
names(data3)<-c("Dist", "Years")

### To run functions associated with the ANOVA - attach the data frame
attach(data3)

###For a parametric test like ANOVA, a major assumption is equality of variance across categories.  Check it.
tapply(Recapture distance, Between year comparisons, var)

### Do the variances look similar?  Test their equality using the Fligner Killeen test.  A non significant result, means equality of variance.
fligner.test(Dist~Years)

#### Next, do a boxplot of the categories.
plot(Dist~Years, col="blue")

###Run the One Way ANOVA
summary(aov(Dist~Years))

###check plots to assumptions of ANOVA have been met
par(mfrow=c(2,2)
plot(aov(Dist~Years))

### Top two plots are the important ones.

###################################################
## Kruskal Wallis - non parametric multi median comparison.
##If your data is skewed, and you haven't done a transformation, you can use the non parametric Krusal Wallis test.
### Just assemble your data into a dataframe using the script above, and then aply the test as so:

kruskal.test(Dist~Years)