####  Import dataframe in this long format
### Make sure group isn't just numeric!  Otherwise colour options won't work

Year	Group	Value						
2015	SG3	0.383			
2016	SG3	0.432			
2017	SG3	0.546			
2018	SG3	0.585						
2015	SG4	0.455			
2016	SG4	0.653			
2017	SG4	0.607			
2018	SG4	0.382			
2014	SG42	0.432			
2015	SG42	0.035			
2016	SG42	0.315			
2017	SG42	0.241			
2018	SG42	0.515			
2014	SG67	0.532			
2015	SG67	0.281						
2017	SG67	0.352			
2018	SG67	0.115			
2014	SG45	0.152						
2016	SG45	0.613			
2017	SG45	0.424			
2018	SG45	0.409						
2015	SG51	0.328			
2016	SG51	0.371			
2017	SG51	0.508			
2018	SG51	0.546		

## activate ggplot2
library(ggplot2)

### import data
x<-read.table("non_culled_groups.txt", header=T)
x1<-as.data.frame(x)

## Plot

a<-ggplot(x1, aes(x=Year, y=Value, group=Group)) +
    geom_line(aes(color=Group), size=1)+
    ylim(-0.3,1) +
    geom_point(size=1, shape=20) +
    ylab("Wang relatedness")