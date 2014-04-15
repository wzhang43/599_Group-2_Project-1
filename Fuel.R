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


######################################################################################


##### Read data #####
or_sm <- read.csv(unz("D:/bigdata/csv_hor.zip", "ss12hor.csv"),
                  stringsAsFactors = FALSE)
str(or_sm)


##### Convert data to tbl_df for viewing #####
or_df = tbl_df(or_sm)
dim(or_df)

which(names(or_df) %in% c("HFL","BLD"))


##### Subset data frame into variables of interest #####
or_fuel = select(or_df, HFL,BLD)

HFL_code = c("1"="1.Utility gas",
             "2"="2.Bottled, tank or LP gas",
             "3"="3.Electricity",
             "4"="4.Fuel oil, kerosene, etc.",
             "5"="5.Coal or coke",
             "6"="6.Wood",
             "7"="7.Solar energy",
             "8"="8.Other fuel",
             "9"="9.No fuel used")

BLD_code = c("1"="Trailer",
             "2"="1-family detached",
             "3"="1-family attached",
             "4"="2 APT",
             "5"="3-4 APT",
             "6"="5-9 APT",
             "7"="10-19 APT",
             "8"="20-49 APT",
             "9"=">50 APT",
             "10"="Boat/RV/van")

or_fuel = mutate(or_fuel, fuel_type=HFL_code[as.character(HFL)],
                 house_type=BLD_code[as.character(BLD)])

## We'll use or_fuel as the working dataframe from this point on.


######################################################################################


##### Extracting information #####

## Since we're interested in the distribution of types of fuels
## within each type of housing unit, first group the data by the variable
## BLD (units in structure):
house = group_by(or_fuel,house_type)

## Also group by type of heating fuels:
fuel = group_by(or_fuel,fuel_type)

## Obtain distribution of fuel types for each housing unit type:
house_sum = house %.% 
  summarise(house_count=n(),
          type1=sum(HFL==1,na.rm=TRUE),
          type2=sum(HFL==2,na.rm=TRUE),
          type3=sum(HFL==3,na.rm=TRUE),
          type4=sum(HFL==4,na.rm=TRUE),
          type5=sum(HFL==5,na.rm=TRUE),
          type6=sum(HFL==6,na.rm=TRUE),
          type7=sum(HFL==7,na.rm=TRUE),
          type8=sum(HFL==8,na.rm=TRUE),
          type9=sum(HFL==9,na.rm=TRUE),
          prop1=type1/house_count,
          prop2=type2/house_count,
          prop3=type3/house_count,
          prop4=type4/house_count,
          prop5=type5/house_count,
          prop6=type6/house_count,
          prop7=type7/house_count,
          prop8=type8/house_count,
          prop9=type9/house_count     
          ) %.%
  filter(house_type!="NA")

## Retain only the proportions ##
house_prop = select(house_sum,prop1:prop9)

## Obtain distribution of fuel types in total:
fuel_sum = fuel %.%
  summarise(fuel_count=n()) %.%
  filter(fuel_type!="NA")




######################################################################################



## Bar chart for distribution of housing unit types ##
house_df <- data.frame(Units_in_structure=as.character(BLD_code), Count=house_sum$house_count)

require(ggplot2)
ggplot(house_df, aes(x=Units_in_structure, y=Count)) + geom_bar(position=position_dodge())+
  ggtitle(bquote(atop(.("Bar Chart for Housing Units"), atop(italic(.("Oregon 2008-2012")), "")))) # add title & subtitle

# Observation: 
# (1) Not surprisingly, one-family detached home takes up the largest proportion of all types of homes in Oregon.
#
# (2) The second most type of housing unit is trailer/mobile home.
#
# (3) Originally I thought there would be more RV/van residents


######################################################################################



## Bar chart for distribution of heating fuel types ##
require(ggplot2)
ggplot(fuel_sum, aes(x=fuel_type, y=fuel_count)) + geom_bar(position=position_dodge())+
  ggtitle(bquote(atop(.("Bar Chart for House Heating Fuel"), atop(italic(.("Oregon 2008-2012")), "")))) # add title & subtitle

# Observation:
# (1) Electricity is the most widely-used type of heating fuel, seconded by utility gas.
#
# (2) Very few housesholds used coal/coke or solar energy for heating.



######################################################################################



## Side-by-side bar chart for fuel types within housing unit types ##
# x-axis labels for housing unit types #
column1 <- c(rep("Trailer", 9), 
             rep("1-family detached", 9),
             rep("1-family attached", 9),
             rep("2 APT", 9),
             rep("2-4 APT", 9),
             rep("5-9 APT", 9),
             rep("10-19 APT", 9),
             rep("20-49 APT", 9),
             rep(">50 APT", 9),
             rep("Boat/RV/van", 9))

# Within-group labels for fuel types #
column2 <- gl(9,1,9,labels=as.character(HFL_code))

# Vector of entries #
# Convert data into a vector for graphing use #
house_prop_vec = rep(0,90)
for(i in 0:9){
  j=9*i+1
  k=9*i+9
  l=i+1
  house_prop_vec[j:k]=as.numeric(house_prop[l,])
}

column3 <- house_prop_vec

fuel_label = as.character(rep(c(1:9),10))

df <- data.frame(Units_in_structure=column1, House_heating_fuel=column2, Percentage=column3, Fuel_label=fuel_label)

# Create bar chart panels, grouped by housing unit type #
require(ggplot2)
ggplot()+
  facet_wrap(~Units_in_structure)+ # panels separated by housing unit types
  geom_bar(mapping=aes(x=Fuel_label, y=Percentage, fill=House_heating_fuel),data=df)+
  xlab("House heating fuel")+
  ggtitle(bquote(atop(.("Percentages of types of heating fuels for each type of housing unit"), atop(italic(.("Oregon 2008-2012")), "")))) # add title & subtitle

# Observations:
# (1) For all but one type of home (one-family detached), electricity is the most favored by residents. Utility gas is most popular among owners of one-family detached homes.
#
# (2) For all but two types of home, utility gas is the second favorite type of heating fuel for home owners. For boat/RV/van owners, their second favorite is bottled/tank/LP gas, and for trailer residents, it's wood.
 
