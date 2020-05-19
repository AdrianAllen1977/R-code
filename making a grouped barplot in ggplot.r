#### Importing the right data structure

 
x<-read.table("TVR263_n=303_tally.txt",header=T)

  
  ### Make sure data is in this structure
  
   Year Species Value
1  1986  Bovine     1
2  1986  Badger     0
3  1991  Bovine     1
4  1991  Badger     0
5  1992  Bovine     1
6  1992  Badger     0
7  1993  Bovine    10
8  1993  Badger     0
9  1996  Bovine     2
10 1996  Badger     0
11 1997  Bovine     1
12 1997  Badger     0
13 1998  Bovine     4
14 1998  Badger     0
15 1999  Bovine     4
16 1999  Badger     2
17 2000  Bovine     1
18 2000  Badger     0
19 2002  Bovine     1
20 2002  Badger     1
21 2003  Bovine     5
22 2003  Badger     0
23 2004  Bovine     7
24 2004  Badger     0
25 2005  Bovine     1
26 2005  Badger     0
27 2006  Bovine     6
28 2006  Badger     0
29 2007  Bovine    19
30 2007  Badger     0
31 2008  Bovine     9
32 2008  Badger     1
33 2009  Bovine    14
34 2009  Badger     0
35 2010  Bovine     8
36 2010  Badger     0
37 2011  Bovine    24
38 2011  Badger     1
39 2012  Bovine    20
40 2012  Badger     0
41 2013  Bovine    11
42 2013  Badger     1
43 2014  Bovine    41
44 2014  Badger    10
45 2015  Bovine    42
46 2015  Badger    23
47 2016  Bovine    14
48 2016  Badger    13
49 2017  Bovine     0
50 2017  Badger     4

### Use ggplot to make your graph
library(ggplot2)

ggplot(data=x, aes(fill=Species, y=Frequency, x=Year,)) + geom_bar(position="dodge", stat="identity") + scale_x_continuous(breaks = scales::pretty_breaks(n = 25))
