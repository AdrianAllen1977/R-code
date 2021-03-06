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

### Save the table as a data frame
x1<-as.data.frame(x)

### Plot the graph
ggplot(x1, aes(x=Model, y=Transition_rate)) +
geom_point(size=4) +
geom_errorbar(aes(ymax=HPD_upper, ymin=HPD_lower))

