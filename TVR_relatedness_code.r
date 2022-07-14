library(fitdistrplus)
library(GGally)
library(lme4)
library(plyr) 
library(dplyr)
library(ggpubr)
library(ggfortify)
library(vcd)
library(nlme)
library(lmerTest)
library(MuMIn)


setwd("Desktop/TVR Perturbation - relatedness and mark recapture/TVR relatedness perturbation/")
data <- read.csv("Relatedness_model_data.csv")

## Check names of columns
names(data)

### Check format of columns

str(data)

## make some columns categorical variables

data$Social_group<-as.factor(data$Social_group)
data$Year<-as.factor(data$Year)
data$Culled_prev_years<-as.factor(data$Culled_prev_years)
data$Simple_cumulative_cull_intensity<-as.factor(data$Simple_cumulative_cull_intensity)

## Recheck format of data in columns
str(data)

## Summarise outcome variable of interest
summary(data$Relatedness)

## Check the distribution of the Relatedness  variable
x<-data$Relatedness

fit_n<-fitdist(x, "norm", method="mle") ##testing normal distribution
summary(fit_n) ## Check AIC and compare to others.

## Plot a density graph of your data vs the fitted distribution.
plot.legend <- c("Normal")
denscomp(list(fit_n), legendtext = plot.legend)

### Check distribution of outcome variable with denisty plot and histogram
dp<-ggplot(data, aes(Relatedness)) +
  geom_density()

hist<-ggplot(data, aes(Relatedness)) +
  geom_histogram(bins = 40)

## Set plot window for two plots
ggarrange(dp, hist, labels = c ("A", "B"), ncol= 1, nrow =2)

##########################################################
## Get a feel for the data                              ##
## for each variable, plot the outcome with explanatory ##
##########################################################

names(data)
year_vs_relatedness<-ggplot(data, aes(x = Year, y = Relatedness)) +
  geom_boxplot() +
  xlab("Capture Year") +
  ylab("Social Group Relatedness")
  
Culled_prev_yrs_vs_relatedness<-ggplot(data, aes(x = Culled_prev_years, y = Relatedness)) +
geom_boxplot() +
xlab("Social group culled in previous years?") +
ylab("Social Group Relatedness")

Cumulative_cull_intens_vs_relatedness<-ggplot(data, aes(x = Simple_cumulative_cull_intensity, y = Relatedness)) +
geom_boxplot() +
xlab("Cumulative cull intensity") +
ylab("Social Group Relatedness")

ggarrange(dp, year_vs_relatedness,Cumulative_cull_intens_vs_relatedness, labels = c ("A", "B", "C"), ncol= 1, nrow =3)

#################################################################
### Extract summary statistics using dplyr functions and piping.#
#################################################################

### IF 'dplyr' is loaded before 'plyr', it masks dplyr functions! See above

## Mean
data %>% group_by(Year) %>% summarise (mean_dist=mean(Relatedness, na.rm=TRUE)) %>% as.data.frame()

#sd
data %>% group_by(Year) %>% summarise (sd=sd(Relatedness, na.rm=TRUE)) %>% as.data.frame()

#max
data %>% group_by(Year) %>% summarise (max=max(Relatedness, na.rm=TRUE)) %>% as.data.frame()

#min
data %>% group_by(Year) %>% summarise (min=min(Relatedness, na.rm=TRUE)) %>% as.data.frame()

#Q1
data%>% group_by(Year) %>% summarise (Q1=quantile(Relatedness, na.rm=TRUE)[2]) %>% as.data.frame()

#Q3
data%>% group_by(Year) %>% summarise (Q3=quantile(Relatedness, na.rm=TRUE)[4]) %>% as.data.frame()

#Median
data %>% group_by(Year) %>% summarise (median=median(Relatedness, na.rm=TRUE)) %>% as.data.frame()

#####################################################################
### Simple uni-variable models to find effect on distance (if any) ##
#####################################################################

### Running linear model as relatedness distribution is normally distributed. 

## Relatedness vs Capture year 

Gau_cap_yr<-glm(Relatedness~Year, family="gaussian", data=data)
summary(Gau_cap_yr)
autoplot(Gau_cap_yr)

## Relatedness vs culled in prev years
Gau_cull_prev<-glm(formula = Relatedness ~ Culled_prev_years, family = "gaussian", data = data)
summary(Gau_cull_prev)
autoplot(Gau_cull_prev)

## Relatedness vs cull intensity
Gau_cull_int<-glm(formula = Relatedness ~ Simple_cumulative_cull_intensity, family = "gaussian", data = data)
summary(Gau_cull_int)
autoplot(Gau_cull_int)

## Relatedness vs time since last cull
#Gau_cull_time<-glm(formula = Relatedness ~ Time_since_last_cull_.yrs., family = "gaussian", data = data)
#summary(Gau_cull_time)
#autoplot(Gau_cull_time)

##########################################
### Check for correlation of variables ###
##########################################

## Create cross tab / contingency table of variables pairs.
tab1<-xtabs(~Culled_prev_years + Simple_cumulative_cull_intensity, data=data)

tab2<-xtabs(~Culled_prev_years + Year, data=data)

tab3<-xtabs(~Simple_cumulative_cull_intensity + Year, data=data)

## Run vcd function assoctats to check Cramer's V for correlating categorical variables

assocstats(tab1)

assocstats(tab2)

assocstats(tab3)


###############################################################
## Multivariable model with random effect of social group######
###############################################################

multi1<-glm(Relatedness~Year+Simple_cumulative_cull_intensity,family="gaussian", data=data)

multi2<-lmer(Relatedness~Year+Simple_cumulative_cull_intensity+(1|Social_group), data=data)

multi2b<-lme(Relatedness~Year+Simple_cumulative_cull_intensity, random=~1|Social_group, data=data)

multi3<-lmer(Relatedness~Year+Simple_cumulative_cull_intensity+Year*Simple_cumulative_cull_intensity +(1|Social_group), data=data)

multi3b<-lme(Relatedness~Year+Simple_cumulative_cull_intensity+Year*Simple_cumulative_cull_intensity, random=~1|Social_group, data=data)

##Check AIC fits of both models
AIC(multi1)
AIC(multi2)
AIC(multi3)

confint(multi1)

confint(multi2)

confint(multi3)

### determining pseudo rsquared for mixed model

r.squaredGLMM(multi2)

### this produces a 3x2 matrix with three different methods of calculating 2 different types of pseudo r squared.
## Marginal R squared is for just the fixed effects
## Conditional R squared is for fixed AND random effects.
### THe delta method is suitable for all distributions and link functions.
