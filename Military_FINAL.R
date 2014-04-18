### Final Code for Military Question

library(dplyr)
library(ggplot2)
## State Data from : 
# http://www.fonz.net/blog/archives/2008/04/06/csv-of-states-and-state-abbreviations/
state_data <- read.csv("states.csv", stringsAsFactors=FALSE)
state_abbrv <- tolower(state_data$Abbreviation)
MIL_status <- c("1"="Active_Duty", "2"="Retired_Recent", "3"="Retired", "4"="No_Reserves", "5"="Never_Served")


wage.data <- NULL

for(i in 1:51) {  ## ====================== FOR LOOP ===============================
                   dl_url <- paste("http://www2.census.gov/acs2012_3yr/pums/csv_p",state_abbrv[i],".zip", sep="")
                   dir_zip <- paste("data2/csv_p",state_abbrv[i],".zip", sep="")
                   dl_file <- paste("ss12p",state_abbrv[i],".csv", sep="")
                   dir_file <- paste("data2/", dl_file, sep="")
                   dir_cut <- paste("data2/ss12p",state_abbrv[i],"-cut.csv",sep="")
                   
                   ## download file.
                   download.file(dl_url, destfile = dir_zip)
                   
                   ## unzip file
                   system(paste("unzip -n", dir_zip, dl_file,"-d data2"))
                   
                   ## cut desired columns & import (47,70,75) # col #'s change with 3yr data.
                   system(paste("cut -d, -f47,70,75", dir_file,">", dir_cut))
                   temp.mil <- read.csv(dir_cut, stringsAsFactors=FALSE)
                   
                   
                   temp.mil.G <- temp.mil %.%
                     filter(MIL!="NA", MIL!=1, MIL!=4, WAGP>0) %.%    # Only interested in cats 2,3,5 and Salary > 0
                     group_by(MIL) %.%
                     summarise(n(), avg_inc = mean(WAGP), sd_inc=sd(WAGP), 
                               med_inc=median(WAGP), Q1_inc=quantile(WAGP, 0.25), Q3_inc=quantile(WAGP, 0.75),
                               avg_schl=mean(SCHL), sd_schl=sd(SCHL),
                               med_sch=median(SCHL), Q1_sch=quantile(SCHL, 0.25), Q3_sch=quantile(SCHL, 0.75),
                               State=state_abbrv[i]
                     ) %.%
                     mutate(MIL_Status=MIL_status[MIL])
                   
                   
                   wage.data <- rbind(wage.data, temp.mil.G)
                   
                   ## Remove files
                   
                   system("rm -f data2/*.csv")
                   system(paste("rm -f", dir_zip))
                   
} ## ================================= END FOR LOOP ======================================

## choked on CA, had to DL that one manually & add. 
# & GA (restarted loop at index) & IN (same), then did okay on the rest. It wasn't downloading the whole file for some reason & choked on the check.

## DL # 2 - made it all the way to HI before locking up.

write.csv(wage.data, "MilWageData.csv", row.names=FALSE)

## Only run if not downloading 
wage.data <- read.csv("MilWageData.csv", stringsAsFactors=FALSE)

wage.data.st <- group_by(wage.data, State, MIL)

## Wage Graph # 1 - All-in-One Error Bars
ggplot(wage.data.st, aes( reorder(factor(State),med_inc),y=med_inc, ymin=Q1_inc, ymax=Q3_inc, group=MIL, colour=factor(MIL_Status)))+geom_point(size=3, position=position_dodge(width=0.2))+
  geom_errorbar( position=position_dodge(width=0.2), width=0.2)+
  xlab("State")+ylab("Wage")+geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Median Wage (with IQR) by State & Service")

# code works. Not sure I like this graph for readability. Also, I can't seem to get the "reorder" to latch onto a single category. It's all /kind of/ going in a general "less" direction, but not explicitly.

## Wage Graph # 2
ggplot(wage.data.st, aes( reorder(factor(State),med_inc),y=med_inc, ymin=Q1_inc, ymax=Q3_inc))+geom_point(size=3, position=position_dodge(width=0.2))+
  geom_errorbar( position=position_dodge(width=0.2), width=0.2)+
  xlab("State")+ylab("Wage")+geom_line()+
  facet_grid(. ~ MIL_Status)
## This works too, I'm not sure if it's better/worse or simply different from the previous.

## Wage Graph # 3 - HIstogram
ggplot(wage.data.st, aes(med_inc)) + 
  xlim(0,75000) + xlab("Median Wage Per State") + ylab("Density")+
  geom_histogram(aes(y=..count../sum(..count..))) + 
  facet_grid(MIL_Status ~ .)

# I think this one is better than the previous in terms of the story I'm tryin to tell.


# School Plot 1 - HIstograms
ggplot(wage.data.st, aes(med_sch))+geom_histogram(aes(y=..count../sum(..count..)))+facet_grid(MIL_Status ~ ., scales="free")
# this doesn't do much for me. When I did the original on my test data, I was using the actual values, whereas here I've just got the median. 

## School Plot #2 - Dotplot @ median w/ IQR
ggplot(wage.data.st, aes( reorder(factor(State),med_sch),y=med_sch, ymin=Q1_sch, ymax=Q3_sch, group=MIL, colour=factor(MIL_Status)))+geom_point(size=4, position=position_dodge(width=0.4))+
  geom_errorbar( position=position_dodge(width=0.4), size=0.5)+
  xlab("State")+ylab("School")+geom_line()+
  scale_y_continuous(breaks=c(16,17,18,19,20,21,22, 23), labels=c("HS", "GED", "<1YR Coll", "Coll, ND", "AS", "BS", "MS", "Prof >BS"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Median School (with IQR) by State & Service")


# this is not a perfect graph, but I think it's better than the histogram for this option.
