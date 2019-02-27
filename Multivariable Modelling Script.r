#### Multivariable modelling

### First, assemble a data frame of your response variable, and predictor variables in columns all labelled with a column title.  Save as a tab delimited text file.

##Input your data - make the header 'True' as you will specify the model using column names.

a<-read.table("yourfilename.txt", header=T)

### Check that none of the predictor variables are strongly correlated with one another.  Include the response variable too.

library(psych)

corr.test(a[1:4])

###If any predictor variables are strongly correlated (r >0.7), then remove the one from the pair that is least correlated with the response variable.

#### Now, run your model. 

### On occasions, for some apparently continuous variables, it may be better to run them as categorical variables - e.g. 1 yr interval time difference data. Use the 'factor' function for this (see below)  

###By contrasting the r squared values for each model, you can see what gives the best fit.

### Contrast models with other variants that remove some variables to see if there is a better fit.

## Using the lm function, the y (response variable) is entered first.

model1<-lm(SNP_Dist ~ Map_dist + STR_Dist + Time_Dist, data=a)

model2<-lm(SNP_Dist ~ Map_dist + STR_Dist + factor(Time_Dist), data=a)

## The output of the categorical variable is represented by the different levels of the variable.  
##The default is that the variable is ordered smallest to largest if numeric / alphabetic in nature.  So the significance is each level vs the initial one.

### When you introduce a categorical / factor, you can distinguish between the different levels using a post hoc test to see if levels differ from one another.

## First build a contrast matrix from the variance/covariance table from the summary of the models.
## IN this matrix, you define what variables you want to be compared.

contrast.matrix<-rbind('Time1 vs Time2' = c(0,0,0,-1,1,0), 'Time1 vs Time3' = c(0,0,0,-1,0,1), 'Time2 vs Time3' = c(0,0,0,0,-1,1))

## Then run the post hoc test between variables using the package 'multcomp'

library(multcomp)

comps<-glht(model2, contrast.matrix)

### If there is no difference between some of the levels of the categorical variable, it may be best to combine them into smaller numbers of levels.
### This is a simpler / more parsimonious approach.

### first attach the original data the model is run from, and rename the categorical variable.
attach(a)
newTime_Dist<-factor(Time_Dist)

###Check levels of new variable
levels(newTime_Dist)

###Group non significantly different levels and leave significantly different level separate.

levels(newTime_Dist) [c(1)]<- "Zero"
levels(newTime_Dist) [c(2,3,4)]<-"non-Zero"

##Check new levels assignment
levels(newTime_Dist)

## Re-run the simplified model
model3<-lm(SNP_Dist ~ Map_dist + STR_Dist + newTime_Dist)
summary(model3)

### Detach your data
detach(a)

### QA the best fitting models by checking the assumptions of homoscedasticity (equality of variance) and normal distribution.

## Set the plot window in R to a 2x2 format, then run the plot of the diagnostic plots.

par(mfrow=c(2,2))

plot(model1)

### The residuals vs fitted values plot should show the fitted line with outliers clustered around it.  You want the cloud of residuals to lie evenly around the line with little evidence of clumping

### The QQ plot for assessing normality should show normalised residuals lying along the line for the range of quantiles of the normality rule is obeyed.

## Check the fit of the models using the Akaike Information Criterion (AIC). The lower the AIC, the better model.

AIC(model1, k=2)
