### Running a Poisson Regression.

## IF you are modelling a count response variable - i.e. a discrete variable with frequency data of each count unit, you need a poisson model.
### The distribution of count data are positively (right) skewed and assumptions around normal distribution of residuals may not hold.  Always check distributions before deciding on approaches.

### run your poisson model using the glm function.

setwd("your data location")

a<-read.table("your data file name.txt", header=T)

x<-glm(SNP_Dist~Map_Dist, data=a, family=poisson())

##The additional unfilled brackets in the model function are a default inclusion of the link log function.  Poisson models produce regression lines which are log linear in nature.

summary(a)

###  Note because the model is logn linear, you need to back transform the outputted parameters using the exponential function.
### The gradient output needs a different type of interpretation as it is log linear - after transform, if the parameter is great than 1 - say 1.05, then this means for every 1 unit increase in the predictor variable, the response variable increases by 5%.  

### If the figure is below 1 - say 0.96 - Then this means there is a 4% decrease in the response variable for every 1 unit increase in the predictor.

## You can't simply use the abline function to plot the poisson regression line on any plot.
## You need to backtransform and create the line for a set of points from your data using the script below.

## First determine the range of your predictor variable

range(a$Map_Dist)
[1]     0.00 14374.19

### generate a sequence of numbers within this range with a regular (1 or other) unit tick.

Map<-seq(0, 15000, 1)

### Now, backwards predict using your model (defined as x) using the following script.

pred<-predict(x, list(Map_Dist=Map), type="response")

### Then plot the line using this script.

lines(Map, pred, col="blue") 


### If your poisson model output indicates the residual deviance is greater than the no of df, the model is overdispersed and you may want to run the model again using quasipoisson instead of poisson in the family function.