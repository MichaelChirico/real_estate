#Data Cleaning for Main Analysis
#Philadelphia Real Estate Tax Evasion
#Michael Chirico
#February 12, 2015
# PACKAGES AND CLEANUP ####
rm(list=ls(all=T))
setwd("~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/")
library(data.table)
library(boot)
library(foreach)
library(doMC)
registerDoMC(4)
library(texreg)
library(foreign)
library(AER)
library(survival)
library(reshape2)

# CONVENIENT FUNCTIONS ####
abbr_to_colClass<-function(inits,counts){
  x<-substring(inits,1:nchar(inits),1:nchar(inits))
  types<-ifelse(x=="c","character",
                ifelse(x=="f","factor",
                       ifelse(x=="i","integer",
                              "numeric")))
  rep(types,substring(counts,1:nchar(counts),1:nchar(counts)))
}

#READING IN THE DATA ####
##PAYMENTS DATA
###OCTOBER 1 - DECEMBER 4, 2014
payments_nov<-fread("/media/data_drive/real_estate/payment_data_oct_encrypted.csv")
payments_nov[,c("tax","encrypted_id","account_id"):=NULL]
payments_nov[,c("period","valid","posting","due_date"):=lapply(.SD,as.Date,format="%Y%m%d"),
             .SDcols=c("period","valid","posting","due_date")]

###DECEMBER 3 - JANUARY 6
payments_dec<-setnames(fread("/media/data_drive/real_estate/payment_data_dec.txt",sep="|",
                             colClasses=abbr_to_colClass("fcnfcn","143292")),
                       c("tax","period","valid","posting","due_date","principal",
                         "interest_and_penalty","other_paid","grp","tran",
                         "ref_number","legal_name","serv","sic","naic","tpt",
                         "opa_no","cn","bn","interest","penalty"))[,tax:=NULL]

payments_dec[,c("period","valid","posting","due_date"):=lapply(.SD,as.Date,format="%Y%m%d"),
             .SDcols=c("period","valid","posting","due_date")]

###Combine the data sets
###Note: There is some overlap between the November and December payments files.
###      In particular, it seems every payment from 12/1 in the November file
###      is in the December file (but not vice versa). As such we'll just clip the
###      end of the November file.
####This code evidences the duplicates
#####Narrow Nov & Dec files to the period of overlap
#### payments_nov_overlap<-payments_nov[valid>=as.Date("2014-12-01"),][,set:="NOV"]
#### payments_dec_overlap<-payments_dec[valid<=as.Date("2014-12-03"),][,set:="DEC"]
#####Combine and look for duplicates
#### payments_overlap<-rbind(payments_nov_overlap,payments_dec_overlap)
#####If there's a duplicate, the count when grouped by all variables will be >1
##### (exclude legal name, as there are minor discrepancies between the files)
#### payments_overlap[,count:=.N,by=setdiff(names(payments_overlap),c("set","legal_name"))]
####Notice that nrow(payments_overlap[count>2,set=="NOV",])==nrow(payments_nov_overlap) --
####  that is, ever payment in the tail of the November data was found again in December.
payments<-setkey(rbind(payments_nov,payments_dec),opa_no,posting)
rm(list=ls(pattern="payments_"))

###Code to count the duplicates / duplicate-look-alikes among all payments
# payments[,count:=.N,by=setdiff(names(payments),"legal_name")]
# nrow(unique(setkey(payments)[unique(payments[duplicated(payments)]),]))
# ####238 duplicated rows repeated to a total of 505 rows
# payments[count>1,sum(-balance_change)]-payments[count>1,sum(-balance_change/count)]
# #### $23101.06 extra recorded due to (possible) duplication
# dup_opas<-unique(payments[count>1,opa_no])
# #### Turns out NO OPAs associated with duplicated payments made final analysis file

payments[,balance_change:=-principal-interest_and_penalty-other_paid]

###Aggregate the files
####From transcation level to posting day level
####  (combine all transactions posted on the same day)
payments_by_day<-payments[,.(balance_change=sum(balance_change)),by=.(opa_no,posting)]
rm(payments)

##DELINQUENCY DATA
dor_data_oct<-fread("/media/data_drive/real_estate/dor_data_15_oct_encrypted.csv")
dates<-c("date_latest_pmt","max_period","min_period")
dor_data_oct[,(dates):=lapply(.SD,as.Date,format="%Y%m%d"),.SDcols=dates]; rm(dates)
#Found/approximated by hand the council districts for some properties where missing
dor_data_oct[,council_flag:=0]
setkey(dor_data_oct,council,zip)
setkey(dor_data_oct[.("","19130"),c("council","council_flag"):=list("5",1)],council,zip)
setkey(dor_data_oct[.("",c("19125","19147")),c("council","council_flag"):=list("1",1)],council,zip)
setkey(dor_data_oct[.("","19138"),c("council","council_flag"):=list("8",1)],council,zip)
dor_data_oct[,council:=factor(council,levels=1:10)]            

#Add flags for owner occupancy (as proxied by whether a
#  homestead exemption is taken) and residential zoning 
dor_data_oct[,owner_occ:=homestead>0]
dor_data_oct[,residential:=bldg_group %in% c("apartmentLarge","apartmentSmall","condo",
                                             "house","house ","miscResidential","")]

setkey(dor_data_oct,opa_no)
###SAMPLE RESTRICTIONS
### Original delinquent sample size: 134888 properties
####NO PAYMENT AGREEMENT
####  (Drop 31456(23%) properties; 103432 remain)
dor_data_oct<-dor_data_oct[payment_agreement=="N",]
####NO TAX ABATEMENT
#### (Drop 4706(5%) additional properties; 98726 remain)
dor_data_oct<-dor_data_oct[abate_exempt_code %in% c("","     ","\\    "),]
####HANDLED IN-HOUSE (CASE STATUS)
#### (Drop 61170(62%) additional properties; 37556 remain)
dor_data_oct<-dor_data_oct[case_status=="",]
####NOT UP FOR SHERIFF'S SALE
#### (Drop 4098(11%) additional properties; 33458 remain)
dor_data_oct<-dor_data_oct[sheriff_sale=="N",]
####NOT IN BANKRUPTCY
#### (Drop 948(3%) additional properties; 32510 remain)
dor_data_oct<-dor_data_oct[bankruptcy=="N",]
####NOT IN SEQUESTRATION
#### (Drop 1130(3%) additional properties; 31380 remain)
dor_data_oct<-dor_data_oct[sequestration=="N",]
####NOT FLAGGED TO RECEIVE NO MAIL
#### (Drop 1429(5%) additional properties; 29951 remain)
dor_data_oct<-dor_data_oct[returned_mail_flag=="NO",]
###Now cut out the fat
dor_data_oct[,c("account_id","street_code","house_no","returned_mail_flag",
                paste0("collag_",c("count","max","min","principal",
                                   "rcv_total","calc_total")),
                "building_code","bldg_desc","city","state","zip",
                "azavea_nbhd",paste0("payment_agreement",
                                     c("","_agency","_org","_status")),
                "sequestration","bankruptcy","sheriff_sale","case_status"):=NULL]

##Property characteristics
opa_data<-setnames(fread("/media/data_drive/real_estate/prop2014.txt",
                         select=c("PARCEL","MV","CAT CD","TOT AREA","EXT COND","NO RM")),
                   c("opa_no","market_value","category","land_area","exterior","rooms"))
setkey(opa_data,opa_no)
opa_data[,market_value:=as.numeric(market_value)]
opa_data[,land_area:=as.numeric(land_area)/100] #Divide by 100 to convert to sq. feet
##Interpolate land area for properties where it's 0    *land_interp
##approach: set area equal to average of nearby properties
opa_data[data.table(read.dbf("./gis_data/dor_data_aug_with_zoning_base_id.dbf")
                    )[,c("opa_no","longitude","latitude",
                         "zone_district","OBJECTID"):=
                        list(paste0(0,opa_no),NULL,NULL,OBJECTID,NULL) #rename var, begin to convert opa_no
                      ][,opa_no:=substr(opa_no,nchar(opa_no)-8,nchar(opa_no))], #convert opa_no to proper format
         zone_district:=zone_district] #merge into dor_data_oct
opa_data[zone_district==0,zone_district:=NA] #0 means not matched--set to NA
opa_data[land_area<100,land_area:=NA] #100 sq ft = 10x10--too small.
opa_data[,land_area_mean:=mean(land_area,na.rm=T),by=zone_district]
opa_data[,land_area_flag:=is.na(land_area)]
opa_data[is.na(land_area),land_area:=land_area_mean]
opa_data[,land_area_mean:=NULL]
opa_data<-opa_data[land_area<1e6,] #Some properties with extreme values here
opa_data[,rooms:=as.integer(rooms)/10]
opa_data[,zone_district:=NULL] #no longer need
opa_data[,c("category","exterior"):=lapply(.SD,as.factor),.SDcols=c("category","exterior")]
levels(opa_data$category)<-c("Residential","Hotels&Apts","Store w/ Dwelling",
                             "Commercial","Industrial","Vacant")
levels(opa_data$exterior)<-c("N/A","N/A","New/Rehab","Above Average","Average",
                             "Below Average","Vacant","Sealed/Compromised")

##CYCLE & TREATMENT GROUP DATA
cycle_info<-setnames(fread("/media/data_drive/real_estate/opa_cycles.csv",
                           select=c("OPA #","Billing Cycle")),c("opa_no","cycle"))

#Correct coding of OPA# to match other files
cycle_info[,opa_no:=paste(0,opa_no,sep="")]
cycle_info[,opa_no:=substr(opa_no,nchar(opa_no)-8,nchar(opa_no))]

###2014 TREATMENT GROUP INCLUDES ONLY CYCLES 33-47
###  Include cycles 31,32 as leave-out samples for robustness
cycle_info<-cycle_info[cycle %in% 31:47,]

###APPROXIMATE MAILING DAYS BY CYCLE
####APPROACH: ADD 5 DAYS, ROUND TO NEAREST MONDAY
####Specifically
####CYCLE-CAL. DAY-WKDY-MAILING DAY $CYCLE-CAL. DAY-WKDY-MAILING DAY $
#### 31    Oct. 31  F    Nov. 5     $ 40    Nov. 14  M    Nov. 19    $
#### 32    Nov. 3   M    Nov. 10    $ 41    Nov. 17  M    Nov. 20    $ 
#### 33    Nov. 4   M    Nov. 10    $ 42    Nov. 18  M    Nov. 21    $
#### 34    Nov. 5   M    Nov. 12    $ 43    Nov. 19  M    Nov. 24    $
#### 35    Nov. 6   M    Nov. 13    $ 44    Nov. 20  M    Nov. 24    $ 
#### 36    Nov. 7   M    Nov. 14    $ 45    Nov. 21  M    Nov. 25    $
#### 37    Nov. 11  M    Nov. 17    $ 46    Nov. 24  M    Dec. 1     $
#### 38    Nov. 12  M    Nov. 17    $ 47    Nov. 25  M    Dec. 1     $
#### 39    Nov. 13  M    Nov. 18    $
setkey(cycle_info,cycle)[data.table(cycle=31:47,
                                    mailing_day=
                                      as.Date(c(paste0("2014-11-",
                                                       c(5,10,10,12,13,14,17,17,
                                                         18,19,20,21,24,24,25)),
                                                rep("2014-12-01",2)))),
                         mailing_day:=mailing_day]

###INTENDED TREATMENTS WERE AS FOLLOWS
levels_int<-c()
levels_int[c(31,32)]="Leave-Out"
levels_int[c(35,37,44)]="Threat"
levels_int[c(33,40,41,47)]="Service"
levels_int[c(34,38,43,46)]="Civic"
levels_int[c(36,39,42,45)]="Control"
cycle_info[data.table(cycle=31:47,treatment_int=
                   as.factor(levels_int[!is.na(levels_int)])),
      treatment_int:=treatment_int]
rm(levels_int)

###ACTUAL TREATMENTS WERE AS FOLLOWS
levels_act<-c()
levels_act[c(31,32)]="Leave-Out"
levels_act[c(35,36,44)]="Threat"
levels_act[c(33,40,41,42,47)]="Service"
levels_act[c(34,37,38,43,46)]="Civic"
levels_act[c(39,45)]="Control"
cycle_info[data.table(cycle=31:47,treatment=
                        as.factor(levels_act[!is.na(levels_act)])),
           treatment:=treatment]
rm(levels_act)

###TREATMENT FIDELITY WAS HIGHLY COMPROMISED (>20%) FOR FLAGGED CYCLES
cycle_info[,fidelity_flag:=0]
cycle_info[.(c(34:38,42)),fidelity_flag:=1]
setkey(cycle_info,opa_no)



#MERGE THE DATA
##Add delinquency data to cycle info
dor_cycle<-cycle_info[fidelity_flag==0,][opa_data][dor_data_oct]
rm(cycle_info,dor_data_oct,opa_data)
dor_cycle[,market_value_flag:=(market_value==0)]
dor_cycle[market_value==0,market_value:=total_assessment]
dor_cycle[total_assessment==0,total_assessment:=market_value]
setkey(dor_cycle,opa_no)

#Eliminate mismatched observations & missing/obviously mismeasured market values
dor_cycle<-dor_cycle[!is.na(cycle)&market_value>0,]

#Merge time-dependent payments data to the other background data
#  Also add in blanks for each account on each unpaid day
payments_by_day<-
  payments_by_day[data.table(opa_no=rep(dor_cycle$opa_no,                             #These lines add blanks
                                        each=diff(range(payments_by_day$posting))+1), # for uncovered dates
                             posting=rep(seq(min(payments_by_day$posting),
                                             max(payments_by_day$posting),by="day"),
                                         times=nrow(dor_cycle)),
                             key=c("opa_no","posting"))
                  ][is.na(balance_change),balance_change:=0][dor_cycle]

#Compute the balance remaining on all accounts on all days
## In particular, current_balance is the balance due
##  AFTER the payment (credit) is debited (credited)
##  on posting
payments_by_day[posting>as.Date("2014-11-03"),
                    current_balance:=cumsum(balance_change)+
                      calc_total_due,by=opa_no]
setorder(payments_by_day,opa_no,-posting)
payments_by_day[posting<=as.Date("2014-11-03"),
                    current_balance:=cumsum(-balance_change)+
                      balance_change+calc_total_due,by=opa_no]
setorder(payments_by_day,opa_no,posting)

#Add counter for relative time since mailing
payments_by_day[,posting_rel:=posting-mailing_day]
setkey(payments_by_day,posting_rel)

#Define the total due by at mailing
##Use the balance from the day prior to mailing because the
##  day-of balance includes any day-of payments
payments_by_day[payments_by_day[.(-1)],total_due_at_mailing:=current_balance]
###Round to the nearest two digits (some numerical problems pushing values away from 0)
payments_by_day[,total_due_at_mailing:=round(max(total_due_at_mailing,na.rm=T),2),by=opa_no]
##Now delete those properties who had paid off their debt by mailing day
## AND those who owed less than .607 (=exp(-.5))
payments_by_day<-payments_by_day[total_due_at_mailing>.61,]
##Now round up those with balances less than $1 to $1 to keep all log values nonnegative
payments_by_day[total_due_at_mailing<1,total_due_at_mailing:=1]

#To get all cycles on even footing, align the analysis period
#  by cycle (and hence by treatment)
##All cycles extend at least max_length_act days past mailing day
max_length<-payments_by_day[,max(as.integer(posting_rel)),by=cycle][,min(V1)]
##All cycles extend at least -min_length_act days prior to mailing day
min_length<-payments_by_day[,min(as.integer(posting_rel)),by=cycle][,max(V1)]
payments_by_day<-payments_by_day[.(min_length:max_length)]

#Define cumulative payments as the accumulation of
# payments PAST mailing day
# (i.e., cumulative RELEVANT payments)
payments_by_day[.(0:max_length),cum_treated_pmts:=cumsum(-balance_change),by=opa_no]

##Also aggregate payments by week for a weekly analysis file
payments_by_day[.(0:max_length),
                    week_no:=cut(as.integer(posting_rel),
                                 breaks=seq(0,max_length+6,by=7),
                                 include.lowest=T,right=F,
                                 labels=paste0("week_",1:ceiling(max_length/7)))]
payments_by_week<-
  dor_cycle[payments_by_day[!is.na(week_no),
                                    .(total_due_at_mailing=
                                        unique(total_due_at_mailing),
                                      weekly_change=sum(balance_change),
                                      week_end_balance=current_balance[.N]),
                                    by=c("opa_no","week_no")]]; rm(dor_cycle)
payments_by_week[,cum_treated_pmts_wk:=cumsum(-weekly_change),by=opa_no]

#Define ever-paid indicator variable
# (among RELEVANT payments)
payments_by_day[.(0:max_length),ever_paid:=cum_treated_pmts>0]

#Define variable indicating date of first payment (NA if never)
setkey(payments_by_day,posting_rel,ever_paid)
payments_by_day[.(0:max_length,T),
                    date_of_first_payment:=min(posting),by=opa_no]
payments_by_day[.(0:max_length,T),
                    date_of_first_payment_rel:=min(posting_rel),by=opa_no]

#Define paid-in-full indicator variable
# (among RELEVANT payments)
payments_by_day[.(0:max_length),paid_full:=cum_treated_pmts>=.95*total_due_at_mailing]

#Define indicator to get end of period
payments_by_day[,end:=(posting_rel==max_length)]; rm(min_length,max_length)

#Finally, write the data sets to be loaded in the analysis file
write.csv(payments_by_day,file="analysis_file.csv",quote=T,row.names=F)
write.csv(payments_by_day[end==1&fidelity_flag==0,],file="analysis_file_end_only_act.csv",quote=T,row.names=F)

#Swing Payments by Week wide for analysis file
write.csv(setnames(dcast.data.table(payments_by_week[,!c("weekly_change","week_end_balance"),with=F],
                                    opa_no+...~week_no,value.var="cum_treated_pmts_wk"
                                    )[,c("ever_paid_week_6","paid_full_week_6"):=
                                        list((week_6>0),(week_6>=.95*total_due_at_mailing))],
                   paste0("week_",1:6),paste0("cum_treated_pmts_week_",1:6)),
          file="analysis_file_weekly_wide.csv",quote=T,row.names=F)