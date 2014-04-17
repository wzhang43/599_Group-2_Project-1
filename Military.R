### 4/13 - Big Data Analysis

## Downloaded Oregon 2012 1-year to start with smaller data set. Opened easily in excel

## Question from Original Discussion:
# Army Service(!)\\
#   as related to education level or insurance\\
#   Matt suggested to see how education level and/or income related for those who had never served vs those who had served, but were now retired.\\
#   (4/9 update) large state w/ army base (Texas) or two small states, one with Army Base one w/o

## Per data dictionary:
# MIL - Military Service - Col AR
#   NA - Less t han 17
#   5 - Never Served
#   3 - Previous service (not w/in 12 months)
#   2 - Previous service (w/in 12 months)

# PINCP - Total Person's income  - Col CZ
##  Monetary Value (?)


## School - BM-BO
# SCH - School Level Attending
# SCHG - Grade Level Attending
# SCHL - Educational Attainment **BO

#   16 .Regular high school diploma
#   17 .GED or alternative credential
#   18 .Some college, but less than 1 year
#   19 .1 or more years of college credit, no degree
#   20 .Associate's degree
#   21 .Bachelor's degree
#   22 .Master's degree
#   23 .Professional degree beyond a bachelor's degree
#   24 .Doctorate degree

## WAGP - Col 75

# They have codes for if you have insurance, y/n, but not levels of insurance. So I'm going to drop that part of the question.

## Running the cut command gave me some problems with the file I unzipped by double clicking.
#   going back and using the unzip protocol fixed it.

unzip("data/csv_por.zip", list = TRUE)

## Find numeric # for column
# got ##'s from here: http://moonatnoon.com/puzzles/reference/a1b2z26.html rather than counting on fingers.

## Ran Cut
system("cut -d, -f44,67,72,104 data/ss12por.csv > data/ss12por-cut.csv")
system.time(or.mil <- read.csv("data/ss12por-cut.csv", stringsAsFactors = FALSE))

head(or.mil)

## There are rows with NA in MIL and PINCP, because they didn't track those variables for
#   Individuals under 17 years of age. Can we remove at import?

# Not seeing an easy option, and in fact seeing a recommendation to do that in R:
#   http://bconnelly.net/working-with-csvs-on-the-command-line/#Extracting_Rows_Based_on_Some_Value

library(dplyr)

or.mil.2 <- filter(or.mil, MIL!="NA")
head(or.mil.2)

or.mil.df <- tbl_df(or.mil.2)
or.mil.df

MIL_status <- c("1"="Active Duty", "2"="Retired <1yr", "3"="Retired > 1yr", "4"="No, Reserves", "5"="Never Served")

MIL_status[2] ## @TOTO

or.mil.df <- mutate(or.mil.df, MIL_code=MIL_status[MIL])
head(or.mil.df)
or.mil.G <- group_by(or.mil.df, MIL)
all_data <- summarise(or.mil.G, n(), avg_inc = mean(PINCP), sd_inc=sd(PINCP), 
          Q1_inc=min(PINCP), med_inc=median(PINCP),max_inc=max(PINCP),
            avg_schl=mean(SCHL), sd_schl=sd(SCHL))

library(ggplot2)
## Boxplot of Income
ggplot(or.mil.G, aes(factor(MIL), PINCP))+geom_boxplot()
#   Lots of outliers

#plotting, cutting PINCP at 150,000
ggplot(or.mil.G, aes(factor(MIL), PINCP))+geom_boxplot() + ylim(0,100000)

## Density curve of Income (Density curve so sheer numbers wouldn't make them incomparible)
ggplot(data=or.mil.G, aes(PINCP,  colour=factor(MIL), group=MIL))+geom_density()+scale_colour_brewer(palette = "Dark2")
?geom_density
?scale_colour_brewer

## Right now, I think teh story here is that folks just out of service (2) are making less, maybe not working for a bit
## and folks who have never served (5) represent the full range of the normal population, so we'd expect them to be wider, 
#   And tehre are some draggingn teh average down.
#   But for the "out for a while (3)" category and #5, it looks like Military servcie might actually raise income
#   I'm not sure we can do a t-test here because teh sample sizes are SO different.


## using what I did below for the school to compare Income
ggplot(or.mil.G, aes(PINCP)) + 
  xlim(0,200000) +
  geom_histogram(aes(y=..count../sum(..count..))) + 
  facet_grid(MIL ~ ., scales="free")

# And this is for the wages category.
ggplot(or.mil.G, aes(WAGP)) + 
  xlim(0,200000) +
  geom_histogram(aes(y=..count../sum(..count..))) + 
  facet_grid(MIL ~ ., scales="free")

#and it's really telling that only cats 1 and 2 really show any distribution other than 0


sum(or.mil.G$PINCP > 25000) #13882
sum(or.mil.G$PINCP > 50000) #6385
sum(or.mil.G$PINCP > 100000) ## 1540/31436 = 5%
sum(or.mil.G$PINCP > 200000) ## 365 / 31436 = 1% of data
sum(or.mil.G$PINCP > 300000) ## 252
sum(or.mil.G$PINCP > 400000) ## 27
sum(or.mil.G$PINCP > 500000) ## 12
sum(or.mil.G$PINCP < 0) ## 35


sum(or.mil.G$WAGP > 25000) #9690
sum(or.mil.G$WAGP > 50000) ## 4556
sum(or.mil.G$WAGP > 100000) ## 1003
sum(or.mil.G$WAGP > 200000) ## 238 
sum(or.mil.G$WAGP > 300000) ## 238
sum(or.mil.G$WAGP > 400000) ## 0
sum(or.mil.G$WAGP > 500000) ## 0
sum(or.mil.G$WAGP < 0) ## 0

## School Only Summary
summarise(or.mil.G, n(), avg_schl=mean(SCHL), sd_schl=sd(SCHL), 
          min_sch=min(SCHL), med_sch=median(SCHL), max_sch=max(SCHL)
          )

## the median is teh same for all of them, and the average is approximately the same, between 18 & 19


## Boxplot of School
ggplot(or.mil.G, aes(factor(MIL), SCHL))+geom_boxplot()

mil.3 <-filter(or.mil.G, MIL==3)
mil.5 <-filter(or.mil.G, MIL==5)
## percent y axis from : http://stackoverflow.com/questions/11766856/normalizing-y-axis-in-histograms-in-r-ggplot-to-proportion
ggplot(mil.3, aes(factor(SCHL)))+geom_histogram(aes(y=..count../sum(..count..)))+ggtitle("Education Lvl: Prior Service, out >12 mo")

ggplot(mil.5, aes(factor(SCHL)))+geom_histogram(aes(y=..count../sum(..count..)))+ggtitle("Education Lvl: No Military Service")


## facet grid from http://docs.ggplot2.org/0.9.3.1/facet_grid.html
ggplot(or.mil.G, aes(factor(SCHL)))+geom_histogram(aes(y=..count../sum(..count..)))+facet_grid(MIL ~ ., scales="free")


## Okay< i think I have the tools to apply this to a larger data set, like Texas
## Military bases from : http://en.wikipedia.org/wiki/List_of_United_States_military_bases
## not certain if i want to try to compare texas to another state, it looks like other large states 
##  have equivalent #'s of bases. 


## ======== 4/14 ===========
# May want to look at WAGP instead of or in addition to PINCP, as it is ONLY wage/salary income, where PINCP is ALL income.

# t-Type = first(transport_type)

# finding column names: 
#   names(or_df) # gives you names
#   which( names (or_df)=="WAGP")
#  which (names(or_df) %in% c("WAGP", "COW"))



