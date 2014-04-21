library(ggplot2)
library(dplyr)


########################## download & unzip all data ###############################

state_names <- tolower(state.abb)

# 1. download zip files
base_url <- "http://www2.census.gov/acs2012_5yr/pums/csv_p"

get_acs_pfile <- function(state_name){
  url <- paste(base_url, state_name, ".zip", sep = "")
  dest <- paste("Data/csv_p", state_name, ".zip", sep = "")
  download.file(url, destfile = dest)
}

status <- lapply(state_names,
  failwith("failed to download", get_acs_pfile))

any(status == "failed to download")


# 2. unzip all files
unzip_acs = function(state_name){
  zipname = paste("D:/Project_1/Data/csv_p", state_name, ".zip", sep="")
  unzip(zipname)
}

status <- lapply(state_names,
  failwith("failed to unzip", unzip_acs))

any(status == "failed to unzip")



########################### read, cut & summarize all data #############################

state_abbrv = tolower(state.abb)
nstate = length(state_abbrv)
Insurance.data = NULL

# Read and cut raw data into smaller files, for each state:
for (i in 1:nstate){
  data_raw = read.csv(paste("D:/Project_1/ss12p",state_abbrv[i],".csv",sep=""),
    stringsAsFactors = FALSE)
  
  data_df = tbl_df(data_raw)
  
  data_cut = data_df %.%
      select(HINS1:HINS7, MIL) %.%
      mutate(ins_ind=(HINS1==1 | HINS2==1 | HINS3==1 | HINS4==1 | HINS5==1 | HINS6==1 | HINS7==1),
             mil_ind=(MIL==1 | MIL==2 | MIL==3)) %.%
      filter(MIL!="NA")
  
  data_summary = data_cut %.%
    group_by(mil_ind) %.%
    summarise(ins_prop=mean(ins_ind)) %.%
    mutate(state_name = state_abbrv[i])
  
  Insurance.data = rbind(Insurance.data, data_summary)
}


save(Insurance.data, file="Insurance_summary.rda") # save it!




############################### graphics #####################################

load("Insurance_summary.rda")


# Minor change to label, and add a column for service status:
colnames(Insurance.data)[3] = "state"
by_state = Insurance.data %.%
  group_by(state) %.%
  mutate(serv_stat = ifelse(mil_ind, "Served", "Not Served"))



# Calculate odds ratio of getting health insurance for each state:
odds_ratio_bystate = Insurance.data %.% 
  group_by(state) %.% 
  summarise(odds_ratio=(ins_prop[mil_ind==1]/(1-ins_prop[mil_ind==1]))/
      (ins_prop[mil_ind==0]/(1-ins_prop[mil_ind==0])))
max(odds_ratio_bystate$odds_ratio)
min(odds_ratio_bystate$odds_ratio)

# Subset proportions by military service status:
served_bystate = Insurance.data %.% filter(mil_ind==1)
not_served_bystate = Insurance.data %.% filter(mil_ind==0)

max(served_bystate$ins_prop) # MA
min(served_bystate$ins_prop) # AR
max(not_served_bystate$ins_prop) # MA
min(not_served_bystate$ins_prop) # TX


# Chloropleth map for odds ratio of insurance coverage:
usa_df <- readRDS("usa-state-map_all.rds") # make sure you have this file from class repository
served_st <- inner_join(odds_ratio_bystate, usa_df, by = "state")

qplot(x, y, data = served_st,
  geom = "polygon", group = group, fill = odds_ratio) +
  coord_equal() +
  ggtitle(bquote(atop(.("Odds Ratio of Health Insurance Coverage, Served vs. Not Served"), atop(italic(.("United States 2008-2012")), "")))) # add title & subtitle

ggsave("insurance_oddsmap.png", width=7, height=4, units="in", dpi=400) # produces high-resolution plot for LATEX



# Side-by-side maps for both military service status:
library(reshape2)
props <- melt(select(by_state, serv_stat, state, ins_prop))
props_st <- inner_join(props, usa_df, by = "state")

qplot(x, y, data = props_st,
  geom = "polygon", group = group, fill = value) +
  coord_equal() +
  facet_grid(~serv_stat) +
    ggtitle(bquote(atop(.("Map of Health Insurance Coverage Rate"), atop(italic(.("United States 2008-2012")), "")))) # add title & subtitle

ggsave("insurance_sbsmap.png", width=7, height=4, units="in", dpi=400) # produces high-resolution plot for LATEX








