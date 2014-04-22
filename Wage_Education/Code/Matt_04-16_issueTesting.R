##### Matt, 4/16
# something is wonky with the WAGP and PINCP


or_sm <- read.csv("data/ss12por.csv", nrows = 10, stringsAsFactors = FALSE)
str(or_sm)

which(names(or_sm) %in% c("WAGP", "SEMP", "INTP", "SSP", "SSIP", "PAP", "RETP", "OIP", "PINCP"))
which(names(or_sm) %in% "MIL") #44
which(names(or_sm) %in% "PINCP") #104


system("cut -d, -f44,32,61,62,64,68,70,71,72,104 data/ss12por.csv > data/ss12por-inc.csv")
or.inc <- read.csv("data/ss12por-inc.csv", stringsAsFactors = FALSE)

head(or.inc)

or.inc.f <- filter(or.inc, MIL!="NA")
or.inc.f <- tbl_df(or.inc.f)
or.inc.f


## Compare the averages of wage groups, and make sure they add up to the right amount.
or.inc.f %.% group_by(MIL) %.%
  summarise(n(), avg_wage=mean(WAGP), avg_oip=mean(OIP), avg_self=mean(SEMP),
            avg_pap=mean(PAP), avg_SSP=mean(SSP), avg_SSI=mean(SSIP),
            avg_int=mean(INTP), avg_ret=mean(RETP),
            avg_2 = avg_wage+avg_oip+avg_self+avg_pap+avg_SSP+avg_SSI+avg_int+avg_ret,
            avg_inc=mean(PINCP)          
            )


#Look at the medians
or.inc.f %.% group_by(MIL) %.%
  summarise(n(), avg_wage=median(WAGP), avg_oip=median(OIP), avg_self=median(SEMP),
            avg_pap=median(PAP), avg_SSP=median(SSP), avg_SSI=median(SSIP),
            avg_int=median(INTP), avg_ret=median(RETP),
            avg_2 = avg_wage+avg_oip+avg_self+avg_pap+avg_SSP+avg_SSI+avg_int+avg_ret,
            avg_inc=median(PINCP)
            )

#that's a lot of zeros. See if I can find where they're coming from:
or.inc.f[1:100,]

# okay. so, what is going on, is that 0=no income from that category. So, they are REALLY heavily weighted for 0s. for the purposes of what I'm trying to answer, it would really be better to have an NA, almost.

# and I can't just replace the 0's with NA's, because there are some rows with no income. Urg.

# so, i think what needs to happen here is that i need to decide what my question of interest is, maybe change it up a bit if necessary. I think I'm good on my graphs and stuff, I just need to solidify my question in light of what I know about the data. It might be going back to the PINCP variable, it might mean just looking at the entries with a WAGP > 0

sum(or.inc.f$WAGP > 0)/31436 # 17817 / 31436 = 56%
sum(or.inc.f$PINCP> 0)/31436 #  = 90%

#yeah, that 44% of 0s is what's massively skewing that data.


# 4/17 realized project explicitly stated "3 or 5 year" data, so, will need to adjust that.


# Col #'s change with 3-year data. Of course they do.
or_sm <- read.csv("data2/ss12pmo.csv", nrows = 10, stringsAsFactors = FALSE)
str(or_sm)

which(names(or_sm) %in% "WAGP")  # 75
which(names(or_sm) %in% "MIL") #47
which(names(or_sm) %in% "SCHL") #70
