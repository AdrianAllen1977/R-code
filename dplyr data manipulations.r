#### Dplyr for data management, manipulation and explorationlibrary(dplyr)## Once you inport a table or csv, you can explore its dimensions with 'glimpse'## You'll get details on nos of cols and rows and names.x<-read.table("yourdata.txt", header=T)x<-read.csv("yourdata.csv")glimpse(x)##To get basic stats on column variablessummary(x)################################################################# Subsetting a dataframe by columns with dplyr using 'select'################################################################## Try selecting a single column of a data fileselect(x, 'column_name')## Alternatively select all columns except one form a dataframe.select(x, -"column_name_to_exclude")############################################################# Subsetting a dataframe by rows with dplyr using 'slice'############################################################## Extract a single row. In this case row 2.slice(x, 2)## Extract a group of sequential rows.slice, (x, 2:10)## Extract specific non sequential combinations of rows. In this case rows 2, 6 and 9.slice(x, c(2,6,9))## Just be aware - dplyr renames the new dataframe with continuous row numbers, NOT the original data frame nos.###################################### Filtering a data set with 'filter'######################################## For categorical and numerical variables you can extract only data you want from a dataframe## For a numerical variable you could extract only those rows that have a value above the 3rd quartilefilter(x, "column_name" > 80)## Or you may want to take both the above 3rd quartile and below 1st quartile rows.##  We use the Boolean '|' symbol here to pipe both requirements.filter(x, "column_name" >80 | "column_name" <20)## Other useful Booleans## == Equals - returns rows that exactly equal the figure entered.## != does not equal.## >= greater than or equal to.## <= Less than or equal to.## & allows you to filter across two or more columnsfilter (x, "column_name" > 80 & "column_name2" < 5)## To filter by a categorical variable, remember you need to use quote marksfilter (x, "column_name" == "Variable_name")###################################Transforming data using 'mutate'#################################### Doing a log transformation or converting to percentage etc might be needed for a variable# mutate allows you to create a new column in the dataframe with the transform of another columndata<-mutate(x, "log_column_name" = log("column_name"))###################################################Sorting entries from a dataframe using 'arrange'#####################################################  You may want to arrange the dataframe by one numerical / conintuous column in ascending order.## Time series data require a temporal orderarrange(x, "column_name")# you can also arrange by multiple variables - see help file for arrange()#################################Piping to link dplyr functions#################################### The pipe function in R is %>%## If you wanted to take your data frame, filter it by a value in one column, then select only those values form another column, you can do thisx %>%filter("column_name" >= 75) %>%select ("column_name2")######################################Summarising data within a dataframe####################################### The summarise function enables you to extract mean, sd etc for data within the frame## You can use the group_by function to split up the table by categorical variables.x %>%group_by("categorical_column_name") %>%summarise(mean"columnname" = mean("columnname""))##EXAMPLE#####################> compensation %>%#+ group_by(Grazing) %>%#+ summarise(meanFruit=mean(Fruit))###################################You can extend summarise to include other stats toox %>%group_by("categorical_column_name") %>%summarise(mean"columnname" = mean("columnname"), sd"columnname" = sd("columnname"),median"columnname" = median("columnname"))