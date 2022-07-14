library(fitdistrplus)
library(GGally)
library(lme4)
library(plyr) 
library(dplyr)
library(ggpubr)
library(ggfortify)
library(vcd)
library(ggplot2)
library(scales)
library(MuMIn)


setwd("Desktop/TVR Perturbation - relatedness and mark recapture/Mark recapture in depth/3_day_rule_compliant/")
data <- read.csv("All_animals_NAs_removed.csv")

## Check names of columns
names(data)

### Check format of columns

str(data)

## make some columns categorical variables

data$Simple_animal_ID <-as.factor(data$Simple_animal_ID)
data$Capture_year <- as.factor(data$Capture_year)
data$Sex<-as.factor(data$Sex)
data$Cub<-as.factor(data$Cub)
data$Season_of_recapture<-as.factor(data$Season_of_recapture)
data$Social_Group<-as.factor(data$Social_Group)
data$Social_Group_no<-as.factor(data$Social_Group_no)
data$Recap_after_social_group_cull<-as.factor(data$Recap_after_social_group_cull)
data$No_culls_simple<-as.factor(data$No_culls_simple)
data$Cull_intensity_simple<-as.factor(data$Cull_intensity_simple)
data$Days_between_consec_captures_cat<-as.factor(data$Days_between_consec_captures_cat)

## Make sure month order is correct
data$Season_of_recapture <- factor(data$Season_of_recapture, levels = c("Summer", "Autumn"))

## Check format of data in columns
str(data)


## Summarise outcome variable of interest
summary(data$Dist_between_consec_captures)


### Check distribution of outcome variable of interest - distance between consec captures

x<-data$Dist_between_consec_captures



#######################################################################################
## Check fit of the variable to different distributions by maximising log likelihood.##
#######################################################################################

## Some distributions see '0' values as NAs- to test fit, change zeroes to a low value
x1<-replace(x, x==0, 0.1)

fit_n<-fitdist(x1, "norm", method="mle") ##testing normal distribution
summary(fit_n) ## Check AIC and compare to others.

fit_w<-fitdist(x1, "weibull", method="mle") ## testing Weibull distribution
summary(fit_w)

fit_g<-fitdist(x1, "gamma", lower = c(0, 0)) ## testing gamma distibution
summary(fit_g)

fit_ln<-fitdist(x1, "lnorm", method="mle") ## testing lognormal distribution
summary(fit_ln)

## Plot a density graph of your data vs the fitted distributions.
plot.legend <- c("Normal", "Weibull", "gamma", "lognormal")
denscomp(list(fit_n, fit_w, fit_g, fit_ln), legendtext = plot.legend)

## Compare all goodness of fit parameters
gofstat(list(fit_n, fit_w, fit_g, fit_ln))

### Check distribution of outcome variable with denisty plot and histogram
dp<-ggplot(data, aes(Dist_between_consec_captures)) +
  geom_density()

hist<-ggplot(data, aes(Dist_between_consec_captures)) +
  geom_histogram(bins = 40)



## Set plot window for two plots
ggarrange(dp, hist, labels = c ("A", "B"), ncol= 1, nrow =2)


##########################################################
## Get a feel for the data                              ##
## for each variable, plot the outcome with explanatory ##
##########################################################
names(data)
capyr_vs_dist<-ggplot(data, aes(x = Capture_year, y = Dist_between_consec_captures)) +
  geom_boxplot() +
  xlab("Capture year") +
  ylab("Dist between captures (m)")

sex_vs_dist<-ggplot(data, aes(x = Sex, y = Dist_between_consec_captures)) +
  geom_boxplot() +
  xlab("Sex") +
  ylab("Dist between captures (m)")

cub_vs_dist<-ggplot(data, aes(x = Cub, y = Dist_between_consec_captures)) +
  geom_boxplot() +
  xlab("Is animal a cub?") +
  ylab("Dist between captures (m)")

season_of_capture_vs_dist<-ggplot(data, aes(x = Season_of_recapture , y = Dist_between_consec_captures)) +
  geom_boxplot() +
  xlab("Season of recapture") +
  ylab("Dist between captures (m)") 

days_between_caps_vs_dist<-ggplot(data, aes(x= Days_between_consec_captures, y= Dist_between_consec_captures)) +
  geom_point() +
  xlab("Days between captures") +
  ylab("Dist between captures (m)")

No_culls_before_cap_vs_dist<-ggplot(data, aes(x= No_culls_simple, y= Dist_between_consec_captures)) +
  geom_boxplot() +
  xlab("No. of culls before capture") +
  ylab("Dist between captures (m)")

Cumulative_cull_intensity_vs_dist <- ggplot(data, aes( x = Cull_intensity_simple, y= Dist_between_consec_captures)) +
  geom_boxplot() +
  xlab("Cumulative intensity of cull") +
  ylab("Dist between captures (m)")

After_cull_or_not_vs_dist <- ggplot(data, aes(x= Recap_after_social_group_cull, y= Dist_between_consec_captures)) +
  geom_boxplot() +
  xlab("Capture recorded after cull?") +
  ylab("Dist between captures (m)")


## Set plot window for 4 plots
ggarrange(capyr_vs_dist, sex_vs_dist, cub_vs_dist, season_of_capture_vs_dist, labels = c ("A", "B", "C", "D"), ncol= 2, nrow =2)

ggarrange(days_between_caps_vs_dist, No_culls_before_cap_vs_dist, Cumulative_cull_intensity_vs_dist, After_cull_or_not_vs_dist, labels = c ("E", "F", "G", "H"), ncol= 2, nrow =2)


#################################################################
### Extract summary statistics using dplyr functions and piping.#
#################################################################

### IF 'dplyr' is loaded before 'plyr', it masks dplyr functions! See above

#Mean
data %>% group_by(Capture_year) %>% summarise (mean_dist=mean(Dist_between_consec_captures, na.rm=TRUE)) %>% as.data.frame()

#sd
data %>% group_by(Capture_year) %>% summarise (sd_dist=sd(Dist_between_consec_captures, na.rm=TRUE)) %>% as.data.frame()

#max
data%>% group_by(Capture_year) %>% summarise (max_dist=max(Dist_between_consec_captures, na.rm=TRUE)) %>% as.data.frame()

#min
data%>% group_by(Capture_year) %>% summarise (min_dist=min(Dist_between_consec_captures, na.rm=TRUE)) %>% as.data.frame()

#Q1
data%>% group_by(Capture_year) %>% summarise (Q1_dist=quantile(Dist_between_consec_captures, na.rm=TRUE)[2]) %>% as.data.frame()

#Q3
data%>% group_by(Capture_year) %>% summarise (Q3_dist=quantile(Dist_between_consec_captures, na.rm=TRUE)[4]) %>% as.data.frame()

#median
data%>% group_by(Capture_year) %>% summarise (median_dist=median(Dist_between_consec_captures, na.rm=TRUE)) %>% as.data.frame()


####################################################################
### Simple uni-variable models to find effect on distance (if any)##
####################################################################

### Running Gaussian and Gamma families of glm 
## Gamma will not work with zeroes - convert zeroes to small number - 0.1 metres
Dist_between_consec_captures_g<-replace(data$Dist_between_consec_captures, data$Dist_between_consec_captures==0, 0.1)
## Bind the new dist column to the original data
data1<-cbind(Dist_between_consec_captures_g, data)

## Gaussian family glm for Capture_year

Gau_cap_yr<-glm(Dist_between_consec_captures~Capture_year, family="gaussian", data=data)

summary(Gau_cap_yr) ## 2017 significantly different than 2014 p=0.00372

autoplot(Gau_cap_yr) ## Diagnostic plots of model

## Gamma family glm for Capture_year
Gam_cap_yr<-glm(Dist_between_consec_captures_g~Capture_year, family="Gamma"(link="log"), data=data1)

summary(Gam_cap_yr)

autoplot(Gam_cap_yr)


## Gaussian family glm for Sex

Gau_sex<-glm(Dist_between_consec_captures~Sex, family="gaussian", data=data)

summary(Gau_sex)

autoplot(Gau_sex)

### Gamma family glm for Sex

Gam_sex<-glm(Dist_between_consec_captures_g~Sex, family="Gamma"(link="log"), data=data1)

summary(Gam_sex)

autoplot(Gam_sex)

### Gaussian family glm for recapture month

Gau_month<-glm(Dist_between_consec_captures~Month_of_recapture, family="gaussian", data=data)

summary(Gau_month)

autoplot(Gau_month)


### Gamma family glm for recapture month

Gam_month<-glm(Dist_between_consec_captures_g~Month_of_recapture, family="Gamma"(link="log"), data=data1)

summary(Gam_month)

autoplot(Gam_month)

## Gaussian family glm for season of recapture

Gau_season<-glm(Dist_between_consec_captures~Season_of_recapture, family="gaussian", data=data)

summary(Gau_season)

autoplot(Gau_season)


## Gamma family glm for season of recapture

Gam_season<-glm(Dist_between_consec_captures_g~Season_of_recapture, family="Gamma"(link="log"), data=data1)

summary(Gam_season)

autoplot(Gam_season)


### Gaussian family glm for Cub status

Gau_cub<-glm(Dist_between_consec_captures~Cub, family="gaussian", data=data)

summary(Gau_cub)

autoplot(Gau_cub)


### Gamma family glm for Cub status

Gam_cub<-glm(Dist_between_consec_captures_g~Cub, family="Gamma"(link="log"), data=data1)

summary(Gam_cub)

autoplot(Gam_cub)

### Gaussian family glm for cull status

Gau_cull_stat<-glm(Dist_between_consec_captures~Recap_after_social_group_cull, family="gaussian", data=data)

summary(Gau_cull_stat)

autoplot(Gau_cull_stat)

### Gamma family glm for cull status
Gam_cull_stat<-glm(Dist_between_consec_captures_g~Recap_after_social_group_cull, family="Gamma"(link="log"), data=data1)

summary(Gam_cull_stat)

autoplot(Gam_cull_stat)

### Gaussian family glm for simplified no culls before recapture

Gau_No_culls<-glm(Dist_between_consec_captures~No_culls_simple, family="gaussian", data=data)

summary(Gau_No_culls)

autoplot(Gau_No_culls)

### Gamma family glm for simplified no culls before recapture

Gam_No_culls<-glm(Dist_between_consec_captures_g~No_culls_simple, family="Gamma"(link="log"), data=data1)

summary(Gam_No_culls)

autoplot(Gam_No_culls)

### Gaussian family glm for simplified cumulative culled animals in social group before recapture

Gau_Cumu_culled_anims<-glm(Dist_between_consec_captures~Cull_intensity_simple, family="gaussian", data=data)

summary(Gau_Cumu_culled_anims)

autoplot(Gau_Cumu_culled_anims)


### Gamma family glm for simplified cumulative culled animals in social group before recapture

Gam_Cumu_culled_anims<-glm(Dist_between_consec_captures_g~Cull_intensity_simple, family="Gamma"(link="log"), data=data1)

summary(Gam_Cumu_culled_anims)

autoplot(Gam_Cumu_culled_anims)


### Gaussian family glm for days bewteen consecutive captures

Gau_days_btw_caps<-glm(Dist_between_consec_captures~Days_between_consec_captures, family="gaussian", data=data)

summary(Gau_days_btw_caps)

autoplot(Gau_days_btw_caps)


### Gamma family glm for days bewteen consecutive captures

Gam_days_btw_caps<-glm(Dist_between_consec_captures_g~Days_between_consec_captures, family="Gamma"(link="log"), data=data1)

summary(Gam_days_btw_caps)

autoplot(Gam_days_btw_caps)


#############################################################
## Checking for correlation between explanatory variables ### 
#############################################################

tab1<-xtabs(~Capture_year + Sex, data=data)
assocstats(tab1)

tab2<-xtabs(~Capture_year + Cub, data=data)
assocstats(tab2)

tab3<-xtabs(~Capture_year + Season_of_recapture, data=data)
assocstats(tab3)

tab4<-xtabs(~Capture_year + Recap_after_social_group_cull, data=data)
assocstats(tab4)

tab5<-xtabs(~Capture_year + Cull_intensity_simple, data=data)
assocstats(tab5)

tab6<-xtabs(~Capture_year + No_culls_simple, data=data)
assocstats(tab6)

tab7<-xtabs(~Sex + Cub, data=data)
assocstats(tab7)

tab8<-xtabs(~Sex + Season_of_recapture, data=data)
assocstats(tab8)

tab9<-xtabs(~Sex + Recap_after_social_group_cull, data=data)
assocstats(tab9)

tab10<-xtabs(~Sex + Cull_intensity_simple, data=data)
assocstats(tab10)

tab11<-xtabs(~Sex + No_culls_simple, data=data)
assocstats(tab11)

tab12<-xtabs(~Cub + Season_of_recapture, data=data)
assocstats(tab12)

tab13<-xtabs(~Cub + Recap_after_social_group_cull, data=data)
assocstats(tab13)

tab14<-xtabs(~Cub + Cull_intensity_simple, data=data)
assocstats(tab14)

tab15<-xtabs(~Cub + No_culls_simple, data=data)
assocstats(tab15)

tab16<-xtabs(~Season_of_recapture + Recap_after_social_group_cull, data=data)
assocstats(tab16)

tab17<-xtabs(~Season_of_recapture + Cull_intensity_simple, data=data)
assocstats(tab17)

tab18<-xtabs(~Season_of_recapture + No_culls_simple, data=data)
assocstats(tab18)

tab19<-xtabs(~Recap_after_social_group_cull + Cull_intensity_simple, data=data)
assocstats(tab19)

tab20<-xtabs(~Recap_after_social_group_cull + No_culls_simple, data=data)
assocstats(tab20)

tab21<-xtabs(~Cull_intensity_simple + No_culls_simple, data=data)
assocstats(tab21)

tab22<-xtabs(~Days_between_consec_captures_cat + Capture_year, data=data)
assocstats(tab22)

tab23<-xtabs(~Days_between_consec_captures_cat + Sex, data=data)
assocstats(tab23)

tab24<-xtabs(~Days_between_consec_captures_cat + Cub, data=data)
assocstats(tab24)

tab25<-xtabs(~Days_between_consec_captures_cat + Season_of_recapture, data=data)
assocstats(tab25)

tab26<-xtabs(~Days_between_consec_captures_cat + Recap_after_social_group_cull, data=data)
assocstats(tab26)

tab27<-xtabs(~Days_between_consec_captures_cat + Cull_intensity_simple, data=data)
assocstats(tab27)

tab28<-xtabs(~Days_between_consec_captures_cat + No_culls_simple, data=data)
assocstats(tab28)

##############################
## Multi-variable gamma glm ##
##############################

## Simple model just fixed effects
multi1<-glm(Dist_between_consec_captures_g~Capture_year + Sex + Cub + Season_of_recapture + Cull_intensity_simple + Days_between_consec_captures, family="Gamma" (link="log"),  data=data1)
confint(multi1)

## Add in random / mixed effects

# Social group added alone first
multi2<-glmer(Dist_between_consec_captures_g~Capture_year + Sex + Cub + Season_of_recapture + Cull_intensity_simple + Days_between_consec_captures + (1|Social_Group), glmerControl(optimizer = "Nelder_Mead", tolPwrss = 1e-10, optCtrl = list()), family="Gamma" (link="log"),  data=data1)
confint(multi2, method="Wald") ## Gamma GLM mixed models, default confint parameter doesn't work

par(mfrow=c(1,2))

resids<-residuals(multi2)
fit<-fitted(multi2)

plot(resids~fit, main="Equality of variance plot")
abline(0,0)

qqnorm(resids)
qqline(resids)


# Animal ID added alone second - optimizer added
multi3<-glmer(Dist_between_consec_captures_g~Capture_year + Sex + Cub + Season_of_recapture + Cull_intensity_simple + Days_between_consec_captures + (1|Simple_animal_ID), glmerControl(optimizer = "Nelder_Mead", tolPwrss = 1e-10, optCtrl = list()),family="Gamma" (link="log"),  data=data1)
confint(multi3, method="Wald")
## Individual animal nested within social group as a random effect
multi4<-glmer(Dist_between_consec_captures_g~Capture_year + Sex + Cub + Season_of_recapture + Cull_intensity_simple + Days_between_consec_captures + (1|Simple_animal_ID:Social_Group), glmerControl(optimizer = "Nelder_Mead", tolPwrss = 1e-10, optCtrl = list()),family="Gamma" (link="log"),  data=data1)
confint(multi4, method="Wald")

###  Addressing out-lier points from Model 2

## First look at the outliers - anything odd about them?
data2<-dplyr::filter(data1, resids<=-3)

data2

### These lower 13 residuals are from animals which exhibited very little between consec capture moves.
##  Many zero metre moves recorded. NO move higher than 2 metres.

## Now subset the data again to have everything but these outliers

data3<-dplyr::filter(data1, resids>=-3)

## Then rerun model 2 using this data3 data source.


multi2_reduced<-glmer(Dist_between_consec_captures_g~Capture_year + Sex + Cub + Season_of_recapture + Cull_intensity_simple + Days_between_consec_captures + (1|Social_Group), glmerControl(optimizer = "Nelder_Mead", tolPwrss = 1e-10, optCtrl = list()), family="Gamma" (link="log"),  data=data3)
confint(multi2_reduced, method="Wald") 

## Check parameters from model - do they differ much from multi2?
summary(multi2_reduced)

## Check diagnostic residuals plots

resids_reduced<-residuals(multi2_reduced)
fit_reduced<-fitted(multi2_reduced)

plot(resids_reduced~fit_reduced, main="Equality of variance plot")
abline(0,0)

qqnorm(resids_reduced)
qqline(resids_reduced)

### PLotting social group moves by year
data3<-dplyr::filter(data, Capture_year==2014)

a<-ggplot(data3, aes(x=Social_Group_no, y=Dist_between_consec_captures, group=Social_Group)) + 
  geom_boxplot()+
  xlab("Social Group ID")+
  ylab("Dist. between consec captures (m)")+
  ylim(0, 7000)
  
  
a2<-a + theme(axis.text.x = element_blank())


### determining pseudo rsquared for GLMM

r.squaredGLMM(multi2)

### this produces a 3x2 matrix with three different methods of calculating 2 different types of pseudo r squared.
## Marginal R squared is for just the fixed effects
## Conditional R squared is for fixed AND random effects.
### THe delta method is suitable for all distributions and link functions.
