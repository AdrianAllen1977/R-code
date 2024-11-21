###########################################################################
## Comparing two lists of numbers / identifiers and excluding duplicates ##
###########################################################################

## Read in data sources / lists of identifiers

cows <- read.csv("TB_Adv_Cows_list.csv", header=T)

dams <- read.csv("TB_Adv_Dams_list.csv", header=T)

## Count number of entries in lists

dim(cows)
dim(dams)

## Extract relevant column with the identifier in it

cows2 <- cows$CowID
dams2 <- dams$DamID

##  Check for unique entries in both lists

unique_cows<-unique(cows2)
length(unique_cows)

unique_dams<-unique(dams2)
length(unique_dams)  ## Multiple dams in list can have multiple progeny in cows list so appear more than once.

## Find duplicate identifiers in both lists

duplicates <- intersect(cows2, dams2)

## Count number of duplicate entries
dups<-as.data.frame(duplicates)
dim(dups)

### Remove duplicates found in both lists from the unique_dams list - you don't want to remove them from both lists

unique_dams2 <- setdiff(unique_dams, duplicates)
length(unique_dams2)

## Combine unique elements from both lists

unique_cows_and_dams <- union(unique_cows, unique_dams2)
length(unique_cows_and_dams)

## Check how many of the new list are unique
check<-unique(unique_cows_and_dams) 
length(check) ## if same as length of unique_cows_and_dams then all is good.


## Write combined list to file

write.table(unique_cows_and_dams)