#############################################################
### BDSky tools - interpreting BDSkyline log file outputs ###
#############################################################

## Adapted from 'Taming the beast' BEAST Skyline Plots tutorial by Louis du Plessis.

# Install bdskytools from github (only needs to be done once)

library(devtools)

devtools::install_github("laduplessis/bdskytools")

# Attach bdskytools package

library(bdskytools)

# Navigate to directory with log file.

setwd("Desktop")

## Give the filename a name in R or give full pathlength

fname<-"BDSkyline_relaxed_1.140.log"

# Read in the logfile and set 10% burnin

lf<-readLogfile(fname, burnin=0.1)

# Extract HPDs for the R0 and becoming uninfectious rates (delta)

Re_sky<-getSkylineSubset(lf, "reproductiveNumber")
Re_hpd<-getMatrixHPD(Re_sky)
delta_hpd<-getHPD(lf$becomeUninfectiousRate)

## Plot raw HPD intervals for R0 - make sure the dimension matches the  column dimensions of the hpd matrix

dim(Re_hpd)

plotSkyline(1:5, Re_hpd, type='step', ylab="Reproduction number")

## To plot a smoother version of this curve, marginalise the Re estimates on a regular timegrid
## Calculating the HPD at each gridpoint
## By using a grid with more dimensions than the dimension of R - try 6x the number
## Have the timegrid equal the number of years back to the outbreaks origin estimated by the BDSky model

timegrid<-seq(0,32, length.out=50)
Re_gridded<-gridSkyline(Re_sky, lf$origin, timegrid)
Re_gridded_hpd<-getMatrixHPD(Re_gridded)

## Set the dimension of the plot
## The scale of the time will be the latest year of a sampled isolated attached to the timegrid

times<-2022-timegrid

## Plot the smooth skyline

plotSkyline(times, Re_gridded_hpd, type='smooth', xlab="Time", ylab="Reproductive number")

## Plot the delta through time

plotSkyline(range(times), as.matrix(delta_hpd), type='step', axispadding=0.0, col=pal.dark(cblue), fill=pal.dark(cblue, 0.5), col.axis=pal.dark(cblue), ylab=expression(delta), side=4, yline=2, ylims=c(0,10), x.axis=FALSE)

## Plot the Skyline and delta values together on the same graph

## Do the delta graph first

# Set the plotting space

par(mar=c(5,4,4,4)+0.1)

## make sure the ylims are set to capture the HPD of your delta estimate

plotSkylinePretty(range(times), as.matrix(delta_hpd), type='step', axispadding=0.0, col=pal.dark(cblue), fill=pal.dark(cblue, 0.5), col.axis=pal.dark(cblue), ylab=expression(delta), side=4, yline=2, ylims=c(0,12), xaxis=FALSE)


## Add in the R skyline

plotSkylinePretty(times, Re_gridded_hpd, type='smooth', axispadding=0.0, col=pal.dark(corange), fill=pal.dark(cblue, 0.5), col.axis=pal.dark(cblack), xlab="Time", ylab=expression('R' [e]), side=2, yline=2.5, xline=2, xgrid=TRUE, ygrid=TRUE, gridcol=pal.dark(cgray), ylimsc(0, 3), new=TRUE, add=TRUE)

