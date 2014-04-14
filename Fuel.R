library(dplyr)



## Downloaded housing unit information from 2008-2012
## for Oregon.

## The goal is to examine the distribution of types of heating
## fuel favored by the owners of different types of housing units.


##### Read data #####
or_sm <- read.csv("D:/bigdata/ss12hor.csv",
                  stringsAsFactors = FALSE)
str(or_sm)


##### Convert data to tbl_df for viewing #####
or_df = tbl_df(or_sm)
dim(or_df)


##### Subset data frame into variables of interest #####
or_fuel = select(or_df, HFL,BLD)
save(or_fuel, file="or_fuel.Rda") ## save the subsetted data


##### 

