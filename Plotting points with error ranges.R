### GGPLOT of points with error bars

## Packages

library(ggplot2)

## Read in your table from an EXCEL tab separated txt file.
x<-read.table("Temporal signal test.txt", header=T)

## Ensure all columns have a name - see below for example.

Model Substitution_rate  HPD_lower  HPD_upper
1  MASCOT1        0.34170311 0.26680521 0.43361119
2  MASCOT2        0.33886027 0.26335319 0.42701955
3  MASCOT3        0.34715918 0.26643033 0.43008107
4      TRC        0.37706679 0.28333117 0.48387635
5      TR1        0.05062129 0.03072141 0.07484167
6      TR2        0.05026828 0.03193821 0.07286574
7      TR3        0.05482151 0.03416719 0.07916997
8      TR4        0.04660227 0.02949368 0.06468086
9      TR5        0.05509642 0.03439212 0.07730494
10     TR6        0.06926845 0.04665382 0.09516172
11     TR7        0.05580557 0.03677885 0.08004000
12     TR8        0.04879219 0.02842059 0.07186449
13     TR9        0.04279255 0.02638686 0.06315010
14    TR10        0.04134458 0.02421569 0.06200827

### If some of your x labels are numeric, you want them to be factors or they won't plot - plus youcan order them like so
x$Model<-factor(x$Model, levels= c("2014", "2015", "2016"))

## Save the table as a data frame
x1<-as.data.frame(x)

### Plot the graph
a<-ggplot(x1, aes(x=Model, y=Substitution_rate)) +
geom_point(size=4) +
geom_errorbar(aes(ymax=HPD_upper, ymin=HPD_lower)) +
  xlab("BEAST model") +
  ylab("Substitution Rate")

## If you need to make discrete label changes o remove underscores etc

a2<- a + scale_x_discrete(labels=c("yourname1", "yourname2"))

## If you wish to upsacle the font size for the whole graph, do so with base size

a3<-a2+ theme_grey(base_size = 12) ## default size is 11

### To make your x labels vertical for space reasons, you can if needed do the following:
a+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

########################

## You can also make horizontal charts to display confidence intervals

a<-ggplot(x1, aes(x=PRED, y=Genotype)) + 
  geom_errorbarh(aes(xmin=LCI, xmax=UCI), color="darkslategray4") + ## If horizontal display you variable of interest is on X axis
  geom_point(size=1, color="orange") + 
  geom_vline(xintercept=0.7939, linetype=2, color="gray50") + ### Add a central line - maybe an average of the whole dataset
  ylab("VNTR Genotype") + 
  xlab("Prob. being a reactor") + 
  theme_classic()
