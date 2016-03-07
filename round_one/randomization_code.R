#Data Exploration
#Philadelphia Real Estate Tax Evasion - Code for Replicating Randomization Scheme
#Michael Chirico
#April 23, 2014
library(data.table)

setwd("Desktop/research/Sieg_LMI_Real_Estate_Delinquency/")
rm(list=ls(all=T))
set.seed(as.integer(-776009528)) #For future reproducibility, set the seed.
                                 #Note that I got this seed by running
                                 # (-1)^(runif(1)>.5)*sample(2^31,size=1)
                                 # (+/- 2^31 are all acceptable seed values)

#Get list of all weekdays from November through December 10th, excluding Nov 27& 28 (T-Day & Black Friday)

all_days <-seq(as.Date("2014-11-03"),as.Date("2014-12-12"),by="day")
week_days<-all_days[(!(weekdays(all_days) %in% c("Saturday","Sunday")))& #Exclude Saturdays, Sundays
                      !(all_days %in% c(as.Date("2014-11-27"),as.Date("2014-11-28")))] #Exclude Holidays

#28 non-holiday weekdays between these two days = 7 complete rotations of the 4 treatments.

treatment_assignment<-matrix(cbind(as.character(week_days),
                                   c(sample(4),sample(4),sample(4),
                                     sample(4),sample(4),sample(4),sample(4)),
                                   rep(NA,times=length(week_days))),
                             ncol=3,dimnames=list(1:length(week_days),c("Date","Treatment","Cycle")))

write.csv(treatment_assignment,file="random_assignments_2014.csv",row.names=F,quote=F,na="")