# -------------------------------------------------------------------------------------- +
# GBIF Ireland - SDM workshop                                                            |
# Day 1: Data preparation                                                                |
#                                                                                        |
# N.E. Zimmermann, WSL Birmensdorf  &  ETH Zürich                                        |
# -------------------------------------------------------------------------------------- +
setwd("Desktop/R Stuff/Species Distribution Modelling/day2")
library(raster)
library(ecospat)
library(dismo)
library(PresenceAbsence)
library(AUC)
library(gam)
library(tree)
library(RColorBrewer)
#library(polyclip) # in case ecospat did not install properly

load("GBIF_Ireland_day1.RData")

### Need to clean the NAs from the species data
SAsp<-SAsp[complete.cases(SAsp),]
summary(SAsp)

## Do this for other species too if you want to look at them.

# Calculate the individual variable predictive power -------------------------------------
pred.pow<-data.frame(matrix(ncol=13,nrow=7))
row.names(pred.pow)<-sp1
names(pred.pow)<-names(EGga)[5:17]
for (i in 1:7)
  {
  ttt<-get(sp1[i])
  pred.pow[i,]<-round(sapply(5:17, function(x) 
    ecospat.adj.D2.glm(glm(ttt[,3]~ttt[,x]+I(ttt[,x]^2),family='binomial')))*100,1)
}
pred.pow

# Plot the variable predictive power across all species ----------------------------------
col.r<-colorRampPalette(brewer.pal(11,'RdYlBu'))
boxplot(pred.pow,col=col.r(13),cex.axis=.9)

# Selected variables for two species sets ------------------------------------------------
vars1<-c("tmin01","tmax07","oceani","wooded","MOWfrq","livest","ndvi01") #all but
vars2<-c("tmin01","prcp01","oceani","wooded","MOWfrq","livest","ndvi01") #SAsp

# Fit GLM model for Saxifraga spathularis with the selection vars2 -----------------------
form.glm<-as.formula(paste('pres',paste(vars2,'+I(',vars2,'^2)',collapse='+'),sep ='~'))
glm.full<-glm(form.glm, data=SAsp, family='binomial')
summary(glm.full)
ecospat.adj.D2.glm(glm.full)

# Optimize GLM model for Saxifraga spathularis with the selection vars2 ------------------
glm.step<-step(glm.full,direction='both',trace=0)
summary(glm.step)
ecospat.adj.D2.glm(glm.step)

# Project and plot GLM model for Saxifraga spathularis -----------------------------------
prj.glm<-predict(env,glm.step,type="response")
col.r<-colorRampPalette(brewer.pal(9,'YlOrRd'))
col1<-col.r(100)
col1[1]<-"#D1D0CE"
plot(prj.glm,col=col1)
points(SAsp[which(SAsp$pres==1),1:2],pch=16,cex=.5)

# Fit GAM model for Saxifraga spathularis with the selection vars2 -----------------------
form.gam<-as.formula(paste('pres',paste('s(',vars2,',df = 5)',collapse ='+'),sep='~'))
gam.full<-gam(form.gam,data=SAsp,family='binomial')
ecospat.adj.D2.glm(gam.full)
summary(gam.full)

# Optimize GAM model for Saxifraga spathularis with the selection vars2 ------------------
scope.gam<-gam.scope(SAsp[,vars2], arg = 'df = 5')
gam.step<-step.Gam(gam.full,scope=scope.gam,direction='both',trace=0)
summary(gam.step)
ecospat.adj.D2.glm(gam.step)

# Project and plot GAM model for Saxifraga spathularis -----------------------------------
prj.gam<-predict(env,gam.step,type="response")
plot(prj.gam,col=col1)
points(SAsp[which(SAsp$pres==1),1:2],pch=16,cex=.5)

# Visualize the difference between the GAM and the GLM model -----------------------------
col2<-c("blue","lightblue","grey90","orange","red","firebrick4")
plot(prj.gam-prj.glm,col=col2,main="GAM-GLM model probabilities")

# Fit CART model for Saxifraga spathularis with the selection vars2 ----------------------
form.tree <- as.formula(paste('factor(pres,levels=c(1,0))', paste(vars2,collapse='+'),sep='~'))
tree.full<-tree(form.tree,minsize=2,mindev=0,data=SAsp)
plot(tree.full) # You can also plot as follows: >plot(tree.full,type='uniform')
text(tree.full,cex=0.4)

summary(tree.full)
tree.full

# Optimize CART model for Saxifraga spathularis with the selection vars2 -----------------
set.seed(123)
tree.xval<-cv.tree(tree.full, K = 5)
plot(tree.xval)


dev.mat<-data.frame(matrix(nrow=length(tree.xval$dev),ncol=99))
for (i in 1:99){
  .Random.seed
  ttt<-cv.tree(tree.full,FUN=prune.tree,K=5)
  dev.mat[,i]<-ttt$dev
}
dev.opt<-cbind(rev(tree.xval$size),rev(apply(dev.mat,1,mean)))
head(dev.opt,8)
plot(tree.xval,ylim=c(600,900))
lines(dev.opt, lwd=2,col="firebrick3")

tree.prun<-prune.tree(tree.full, best=6)
tree.prun
plot(tree.prun)
text(tree.prun)

# Project and plot CART model for Saxifraga spathularis ----------------------------------
prj.tree<-predict(env,tree.prun,type='vector')
plot(prj.tree,col=col1)
points(SAsp[which(SAsp$pres==1),1:2],pch=16,cex=.5)

## Resubstitution tests ------------------------------------------------------------------
# Prepare resubstitution test data -------------------------------------------------------
resub.test<-data.frame(ID = 1:nrow(SAsp),observed=SAsp$pres)
resub.test$GLM<-glm.step$fitted
resub.test$GAM<-gam.step$fitted
resub.test$CART<-predict(tree.full,SAsp,type='vector')[,1]
head(resub.test)

resub.thrs<-optimal.thresholds(resub.test,threshold=101,opt.methods=1:9)
resub.thrs

# Plot resubstitution optimized range maps -----------------------------------------------
prob.maps <-stack(prj.glm,prj.gam,prj.tree)
range.maps<-stack(prj.glm>resub.thrs[4,2],prj.gam>resub.thrs[4,3],prj.tree>resub.thrs[4,4])
names(prob.maps) <-c("GLM","GAM","CART")
names(range.maps)<-c("GLM","GAM","CART")
par(mfrow=c(1,3))
  for (i in 1:3){plot(range.maps[[i]],col=c("grey80","black"))}
par(mfrow=c(1,1))

# Calculate kappa from resubstitution test -----------------------------------------------
test.dat<-data.frame(matrix(ncol=4,nrow=3))
row.names(test.dat)<-c("GLM","GAM","CART")
names(test.dat)<-c("Kappa.res","Kappa.xval","AUC.res","AUC.xval")

test.dat[1,1]<-Kappa(cmx(resub.test[,c(1:2,3)],threshold=resub.thrs[4,2]))[1]
test.dat[2,1]<-Kappa(cmx(resub.test[,c(1:2,4)],threshold=resub.thrs[4,3]))[1]
test.dat[3,1]<-Kappa(cmx(resub.test[,c(1:2,5)],threshold=resub.thrs[4,4]))[1]
test.dat

# Calculate and plot AUC from resubstitution test ----------------------------------------
resub.roc.GLM<-roc(resub.test$GLM, as.factor(resub.test$observed))
resub.roc.GAM<-roc(resub.test$GAM, as.factor(resub.test$observed))
resub.roc.CART<-roc(resub.test$CART, as.factor(resub.test$observed))
str(resub.roc.GLM)

plot(resub.roc.GLM, col="slategray4",lwd=5, main="Comparing ROC curves for GLM, GAM & CART")
plot(resub.roc.GAM, col="firebrick4",lwd=3,add=TRUE)
plot(resub.roc.CART,col="seagreen3",lwd=3,add=TRUE)

test.dat[1,3]<-auc(resub.roc.GLM)
test.dat[2,3]<-auc(resub.roc.GAM)
test.dat[3,3]<-auc(resub.roc.CART)
test.dat

## Cross-validation tests ----------------------------------------------------------------
# Defining the cross-validation function -------------------------------------------------
cv.model <- function(model, K, data = model$data){
require(dismo)  # to ensure the dismo package is attached
ks <- kfold(model$data, k = K, by = model$data[,as.character(formula(model)[2])])
	cvpreds <- data.frame(row = row.names(data), cvpred = numeric(length = nrow(data)))
	for(i in 1:K){
		train <- data[ks!=i,]
		test <- data[ks==i,]
		modtmp <- update(model, data = train)
		cvpreds[which(ks==i),2] <- predict(modtmp, newdata = test, type = 'response')
	}
	cvpreds
}

# Performing 5-fold cross-validations ----------------------------------------------------
glm.xval<-cv.model(glm.step, K = 5)
gam.xval<-cv.model(gam.step, K = 5)
head(glm.xval)

# Preparing the test data structure ------------------------------------------------------
xval.test<-data.frame(ID = 1:nrow(SAsp),observed=SAsp$pres)
xval.test$GLM<-glm.xval$cvpred
xval.test$GAM<-gam.xval$cvpred
xval.test$CART<-predict(tree.prun,SAsp,type='vector')[,1]
head(xval.test)

# Calculating optimal thresholds ---------------------------------------------------------
xval.thrs<-optimal.thresholds(xval.test,threshold=101,opt.methods=1:9)
xval.thrs

# Plotting range maps from cross-validation optimized thresholds -------------------------
prob.maps <-stack(prj.glm,prj.gam,prj.tree)
range.maps<-stack(prj.glm>xval.thrs[4,2],prj.gam>xval.thrs[4,3],prj.tree>xval.thrs[4,4])
names(prob.maps) <-c("GLM","GAM","CART")
names(range.maps)<-c("GLM","GAM","CART")
par(mfrow=c(1,3))
 for (i in 1:3){plot(range.maps[[i]],col=c("grey80","black"),legend=FALSE)}
par(mfrow=c(1,1))

# Calculating Kappa ----------------------------------------------------------------------
test.dat[1,2]<-Kappa(cmx(xval.test[,c(1:2,3)],threshold=xval.thrs[4,2]))[1]
test.dat[2,2]<-Kappa(cmx(xval.test[,c(1:2,4)],threshold=xval.thrs[4,3]))[1]
test.dat[3,2]<-Kappa(cmx(xval.test[,c(1:2,5)],threshold=xval.thrs[4,4]))[1]
test.dat

# Calculating the area under the ROC curve -----------------------------------------------
xval.roc.GLM<- roc(xval.test$GLM, as.factor(xval.test$observed))
xval.roc.GAM<- roc(xval.test$GAM, as.factor(xval.test$observed))
xval.roc.CART<-roc(xval.test$CART,as.factor(xval.test$observed))
str(xval.roc.GLM)

plot(xval.roc.GLM, col="slategray4",lwd=5, main="Comparing ROC curves for GLM, GAM & CART")
plot(xval.roc.GAM, col="firebrick4",lwd=3,add=TRUE)
plot(xval.roc.CART,col="seagreen3",lwd=3,add=TRUE)

test.dat[1,4]<-auc(xval.roc.GLM)
test.dat[2,4]<-auc(xval.roc.GAM)
test.dat[3,4]<-auc(xval.roc.CART)
test.dat
