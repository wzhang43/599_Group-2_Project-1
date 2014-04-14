library(dplyr)
library(ggplot2)


## Downloaded housing unit information from 2008-2012
## for Oregon. Download link: http://www2.census.gov/acs2012_5yr/pums/csv_hor.zip

## The goal is to examine the distribution of types of heating
## fuel favored by the owners of different types of housing units.

## We will focus on two variables here: "House heating fuel"(HFL) and
## "units in structure"(BLD). Both variables are categorical, and the
## levels of each are coded as follows:

## BLD (units in structure):
##  bb. N/A
##  01. Mobile home or trailer
##  02. One-family house detached
##  03. One-family house attached
##  04. 2 apartments
##  05. 3-4 apartments
##  06. 5-9 apartments
##  07. 10-19 apartments
##  08. 20-49 apartments
##  09. 50 or more apartments
##  10. Boat, RV, van, etc.

## HFL (house heating fuel):
##  b. N/A
##  1. Utility gas
##  2. Bottled, tank, or LP gas
##  3. Electricity
##  4. Fuel oil, kerosene, etc.
##  5. Coal or coke
##  6. Wood
##  7. Solar energy
##  8. Other fuel
##  9. No fuel used





##### Read data #####
or_sm <- read.csv(unz("D:/bigdata/csv_hor.zip", "ss12hor.csv"),
                  stringsAsFactors = FALSE)
str(or_sm)


##### Convert data to tbl_df for viewing #####
or_df = tbl_df(or_sm)
dim(or_df)


##### Subset data frame into variables of interest #####
or_fuel = select(or_df, HFL,BLD)

## We'll use or_fuel as the working dataframe from this point on.


##### Extracting information #####

## Since we're interested in the distribution of types of fuels
## within each type of housing unit, first group the data by the variable
## BLD (units in structure):

house = group_by(or_fuel, BLD)


summarise(house, house_count=n())
















