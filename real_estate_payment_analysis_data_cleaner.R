#Data Cleaning for Main Analysis
#Philadelphia Real Estate Tax Evasion
#Michael Chirico
#February 12, 2015
# PACKAGES, CLEANUP, CONVENIENT FUNCTIONS ####
rm(list=ls(all=T))
setwd("~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/")
data_wd<-"/media/data_drive/real_estate/"
code_wd<-"./analysis_code/"
#funchir is Michael Chirico's package of convenience functions
#  install with devtools::install_github("MichaelChirico/funchir")
library(funchir)
library(data.table)
library(foreign)
write.packages(code_wd%+%"logs/real_estate_payment_"%+%
                 "analysis_data_cleaner_session.txt")

#READING IN THE DATA ####
##PAYMENTS DATA
###OCTOBER 1 - DECEMBER 4, 2014
payments_nov<-fread(data_wd%+%"payment_data_oct_encrypted.csv",
                    drop=c("tax","encrypted_id","account_id"))
dates<-c("period","valid","posting","due_date")
payments_nov[,(dates):=lapply(.SD,as.Date,format="%Y%m%d"),.SDcols=dates]

###DECEMBER 3 - JANUARY 6
payments_dec<-
  setnames(fread(data_wd%+%"payment_data_dec.txt",sep="|",
                 colClasses=abbr_to_colClass("fcnfcn","143292")),
           c("tax","period","valid","posting","due_date","principal",
             "interest_and_penalty","other_paid","grp","tran",
             "ref_number","legal_name","serv","sic","naic","tpt",
             "opa_no","cn","bn","interest","penalty"))[,tax:=NULL]

payments_dec[,(dates):=lapply(.SD,as.Date,format="%Y%m%d"),
             .SDcols=dates]; rm(dates)

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
payments<-rbind(payments_nov,payments_dec)
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
payments_by_day<-
  payments[order(opa_no,posting),
           .(balance_change=sum(balance_change)),
           by=.(opa_no,posting)]
rm(payments)

##DELINQUENCY DATA
dor_data_oct<-fread(data_wd%+%"dor_data_15_oct_encrypted.csv")
dates<-c("date_latest_pmt","max_period","min_period")
dor_data_oct[,(dates):=lapply(.SD,as.Date,format="%Y%m%d"),.SDcols=dates]; rm(dates)
#Found/approximated by hand the council districts for some properties where missing
dor_data_oct[,council_flag:=FALSE]
cz<-c("council","zip")
setkeyv(dor_data_oct,cz)
ccf<-c("council","council_flag")
setkeyv(dor_data_oct[.("","19130"),(ccf):=.("5",T)],cz)
setkeyv(dor_data_oct[.("",c("19125","19147")),(ccf):=.("1",T)],cz)
setkeyv(dor_data_oct[.("","19138"),(ccf):=.("8",T)],cz)
dor_data_oct[,council:=factor(council,levels=1:10)]; rm(cz,ccf)           

#Add flags for owner occupancy (as proxied by whether a
#  homestead exemption is taken) and residential zoning 
dor_data_oct[,owner_occ:=homestead>0]
dor_data_oct[,residential:=bldg_group %in% c("apartmentLarge","apartmentSmall","condo",
                                             "house","house ","miscResidential","")]

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
                "collag_"%+%c("count","max","min","principal","rcv_total","calc_total"),
                "building_code","bldg_desc","city","state","zip",
                "azavea_nbhd","payment_agreement"%+%c("","_agency","_org","_status"),
                "sequestration","bankruptcy","sheriff_sale","case_status"):=NULL]

##Property characteristics
opa_data<-setnames(fread(data_wd%+%"prop2014.txt",
                         select=c("PARCEL","MV","CAT CD","TOT AREA","EXT COND","NO RM")),
                   c("opa_no","market_value","category","land_area","exterior","rooms"))
opa_data[,market_value:=as.numeric(market_value)]
opa_data[,land_area:=as.numeric(land_area)/100] #Divide by 100 to convert to sq. feet
##Interpolate land area for properties where it's 0    *land_interp
##approach: set area equal to average of nearby properties
opa_data[setDT(read.dbf("./gis_data/dor_data_aug_with_zoning_base_id.dbf")
               #opa_no to proper format
               )[,opa_no:=sprintf("%09d",opa_no)], 
         zone_district:=i.OBJECTID,on="opa_no"] #merge into dor_data_oct
opa_data[zone_district==0,zone_district:=NA] #0 means not matched--set to NA
opa_data[land_area<100,land_area:=NA] #100 sq ft = 10x10--too small.
opa_data[,land_area_mean:=mean(land_area,na.rm=T),by=zone_district]
opa_data[,land_area_flag:=is.na(land_area)]
opa_data[is.na(land_area),land_area:=land_area_mean]
opa_data[,land_area_mean:=NULL]
opa_data<-opa_data[land_area<1e6,] #Some properties with extreme values here
opa_data[,rooms:=as.integer(rooms)/10]
opa_data[,zone_district:=NULL] #no longer need
opa_data[,c("category","exterior"):=lapply(.SD,as.factor),
         .SDcols=c("category","exterior")]
levels(opa_data$category)<-c("Residential","Hotels&Apts","Store w/ Dwelling",
                             "Commercial","Industrial","Vacant")
levels(opa_data$exterior)<-c("N/A","N/A","New/Rehab","Above Average","Average",
                             "Below Average","Vacant","Sealed/Compromised")

##CYCLE & TREATMENT GROUP DATA
cycle_info<-
  setnames(fread(data_wd%+%"opa_cycles.csv",
                 select=c("OPA #","Billing Cycle")),c("opa_no","cycle")
           #Round 1 Treatments: 33-47
           )[cycle %in% 33:47,]

#Correct coding of OPA# to match other files
cycle_info[,opa_no:=sprintf("%09d",opa_no)]

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
cycle_info[data.table(cycle=33:47,
                      mailing_day=
                        as.Date(c(paste0("2014-11-",
                                         c(10,12,13,14,17,17,
                                           18,19,20,21,24,24,25)),
                                  rep("2014-12-01",2)))),
           mailing_day:=i.mailing_day,on="cycle"]

###ACTUAL TREATMENTS WERE AS FOLLOWS
levels_act<-character()
levels_act[c(35,36,44)]="Threat"
levels_act[c(33,40,41,42,47)]="Public Service"
levels_act[c(34,37,38,43,46)]="Civic Duty"
levels_act[c(39,45)]="Control"
cycle_info[data.table(cycle=33:47,treatment=levels_act[33:47]),
           treatment:=factor(i.treatment),on="cycle"]
rm(levels_act)

###TREATMENT FIDELITY WAS HIGHLY COMPROMISED (>20%) FOR FLAGGED CYCLES
cycle_info[,fidelity_flag:=FALSE]
cycle_info[cycle%in%c(34:38,42),fidelity_flag:=TRUE]

#MERGE THE DATA
##Add delinquency data to cycle info
dor_cycle<-cycle_info[(!fidelity_flag)][opa_data,on="opa_no"
                      ][dor_data_oct,on="opa_no"]
rm(cycle_info,dor_data_oct,opa_data)
dor_cycle[,market_value_flag:=(market_value==0)]
dor_cycle[market_value==0,market_value:=total_assessment]
dor_cycle[total_assessment==0,total_assessment:=market_value]

#Eliminate mismatched observations & missing/obviously mismeasured market values
dor_cycle<-dor_cycle[!is.na(cycle)&market_value>0,]

#Merge time-dependent payments data to the other background data
#  Also add in blanks for each account on each unpaid day
setkey(payments_by_day,opa_no,posting)
payments_by_day<-
  payments_by_day[CJ(unique(dor_cycle$opa_no),
                     seq(min(posting),max(posting),by="day")
                     )][dor_cycle,on="opa_no"
                        ][is.na(balance_change),
                          balance_change:=0]

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
payments_by_day[payments_by_day[.(-1L),.(opa_no,current_balance)],
                total_due_at_mailing:=i.current_balance,
                on="opa_no"]
##Now delete those properties who had paid off their debt by mailing day
## AND those who owed less than .607 (=exp(-.5)), rounded
payments_by_day<-payments_by_day[total_due_at_mailing>.61,]
##Now round up those with balances less than $1 to $1 to keep all log values nonnegative
##  (total of 18 people adjusted as of September 20, 2015)
payments_by_day[total_due_at_mailing<1,total_due_at_mailing:=1]

#To get all cycles on even footing, align the analysis period
#  by cycle (and hence by treatment)
##All cycles extend at least max_length_act days past mailing day
max_length<-payments_by_day[,max(as.integer(posting_rel)),by=cycle][,min(V1)]
payments_by_day<-payments_by_day[.(0:max_length)]

#Define cumulative payments 
payments_by_day[,cum_treated_pmts:=cumsum(-balance_change),by=opa_no]

#Define ever-paid indicator variable
# (among RELEVANT payments)
payments_by_day[,ever_paid:=cum_treated_pmts>0]

#Define variable indicating date of first payment (NA if never)
payments_by_day[(ever_paid),date_of_first_payment:=min(posting),by=opa_no]
payments_by_day[(ever_paid),date_of_first_payment_rel:=min(posting_rel),by=opa_no]

#Define paid-in-full indicator variable
# (among RELEVANT payments) as
#  the account being at least 95% repaid
payments_by_day[,paid_full:=cum_treated_pmts>=.95*total_due_at_mailing]

#Define indicator to get end of period
payments_by_day[,end:=(posting_rel==max_length)]; rm(max_length)

#Finally, write the data sets to be loaded in the analysis file
write.csv(payments_by_day,file="analysis_file.csv",quote=T,row.names=F)
write.csv(payments_by_day[(end&!fidelity_flag)],
          file="analysis_file_end_only_act.csv",quote=T,row.names=F)