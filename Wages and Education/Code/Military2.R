## == Military 2 ==
#   Automatic downnloading of files
library(dplyr)
library(ggplot2)
## State Data from : 
# http://www.fonz.net/blog/archives/2008/04/06/csv-of-states-and-state-abbreviations/
state_data <- read.csv("states.csv", stringsAsFactors=FALSE)
state_abbrv <- tolower(state_data$Abbreviation)
MIL_status <- c("1"="Active_Duty", "2"="Retired_Recent", "3"="Retired", "4"="No_Reserves", "5"="Never_Served")


wage.data <- NULL

for(i in 15:51) {  ## ====================== FOR LOOP ===============================
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



wage.data.st <- group_by(wage.data, State, MIL)

wage.data.5 <- filter(wage.data.st, MIL==5)
wage.data.5 <- wage.data.5[order(-wage.data.5[,"avg_inc"]),]

ggplot(wage.data.5, aes( avg_inc, factor(State), group=MIL, order= ))+geom_point()+geom_line()+xlab("Wage")+ylab("State")+scale_colour_brewer(palette = "Dark2")


ggplot(wage.data.5, aes( avg_inc, reorder(factor(State),avg_inc), group=MIL))+geom_point(size=3)+geom_line()+
  geom_errorbarh(aes(xmax=avg_inc+sd_inc, xmin=avg_inc-sd_inc))+
  xlab("Wage")+ylab("State")


# i like this plot, but let's re do it with median & IQR. Which means I'll need to update to include Q1 and Q3

wage.data.235 <- filter(wage.data, MIL %in% c(2,3,5))

ggplot(wage.data.st, aes( reorder(factor(State),med_inc),y=med_inc, ymin=Q1_inc, ymax=Q3_inc, group=MIL, colour=factor(MIL)))+geom_point(size=3, position=position_dodge(width=0.2))+
  geom_errorbar( position=position_dodge(width=0.2), width=0.2)+
  xlab("State")+ylab("Wage")+coord_flip()+geom_line()
#okay, working for one state.
## This helped immensely: http://stackoverflow.com/questions/20197118/dotplot-with-error-bars-two-series-light-jitter

## This is not really sorting for one state. It's /kind of/ in decreasing order, but not really.


## when adjusting to median, got medians of 0 for first state, lets look at hist:

t.mil.f <- filter(temp.mil, MIL!="NA")
t.mil.f.g <- group_by(t.mil.f, MIL)
ggplot(t.mil.f.g, aes(WAGP)) + 
  xlim(0,200000) +
  geom_histogram(aes(y=..count../sum(..count..))) + 
  facet_grid(MIL ~ ., scales="free")
# maybe alabama is a really bad example. Lets look at AZ



## Below this line is the code for testing the download in prep for making it a loop.

# This code has been modified from original code provided in class by Charlotte Wickham

tx_sm <- read.csv(unz("data2/csv_ptx.zip", "ss12ptx.csv"), nrows = 10, # notice just 10 lines! why?
                  stringsAsFactors = FALSE)
str(tx_sm)

which(colnames(tx_sm) == "WAGP")


## download file.
download.file("http://www2.census.gov/acs2012_1yr/pums/csv_ptx.zip", destfile = "data2/csv_ptx.zip")

## unzip file
#unz("data2/csv_ptx.zip", "data2/ss12ptx.csv")
system("unzip data2/csv_ptx.zip ss12ptx.csv -d data2")

## cut desired columns & upload (44, 104, 75)
system("cut -d, -f44,67,72 data2/ss12ptx.csv > data2/ss12ptx-cut.csv")
tx.mil <- read.csv("data2/ss12ptx-cut.csv", stringsAsFactors = FALSE)

## Add State and Status
## Cut out rows with NA
tx.mil.filt <- filter(tx.mil, MIL!="NA")


## Add to primary data file.

## Remove files

system("rm -f data2/*.csv")
system(paste("rm -f", dir_zip))








tx.mil.G <- tx.mil %.%
  filter(MIL!="NA") %.%
  group_by(MIL) %.%
  summarise(n(), avg_inc = mean(WAGP), sd_inc=sd(WAGP), 
            min_inc=min(WAGP), med_inc=median(WAGP),max_inc=max(WAGP),
            avg_schl=mean(SCHL), sd_schl=sd(SCHL)) %.%
  mutate(State="tx")
  
all_data <- rbind(all_data, tx.mil.G)
  
  
  names(all_data)
  
  all_data <- summarise(or.mil.G, n(), avg_inc = mean(PINCP), sd_inc=sd(PINCP), 
                        min_inc=min(PINCP), med_inc=median(PINCP),max_inc=max(PINCP),
                        avg_schl=mean(SCHL), sd_schl=sd(SCHL), State="or")


### playing with graph again 4/17

wage.data.5 <- filter(wage.data.st, MIL==5)

ggplot(wage.data.5, aes( reorder(factor(State),med_inc),y=med_inc, ymin=Q1_inc, ymax=Q3_inc))+geom_point(size=3, position=position_dodge(width=0.2))+
  geom_errorbar( position=position_dodge(width=0.2), width=0.2)+
  xlab("State")+ylab("Wage")+coord_flip()+geom_line()
## This looks okay and is sorted, but doesn't really compare them.

ggplot(wage.data.st, aes( reorder(factor(State),med_inc),y=med_inc, ymin=Q1_inc, ymax=Q3_inc))+geom_point(size=3, position=position_dodge(width=0.2))+
  geom_errorbar( position=position_dodge(width=0.2), width=0.2)+
  xlab("State")+ylab("Wage")+geom_line()+
  facet_grid(. ~ MIL_Status)
## This works too, I'm not sure if it's better/worse or simply different from the previous.