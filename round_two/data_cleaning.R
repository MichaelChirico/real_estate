#Data Cleaning
#Philadelphia Real Estate Tax Evasion
#  Round 2
#Michael Chirico

##Packages
rm(list=ls(all=T))
gc()
#Michael Chirico's function of convenience packages;
#  install via devtools::install_github("MichaelChirico/funchir")
library(funchir)
library(data.table)
library(texreg)
library(sandwich)
library(xtable)
library(readxl)
library(xlsx)
library(sp)
library(doParallel)
library(RgoogleMaps)
library(maptools)
library(lmtest)
setwd(mn <- "~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/")
wds<-c(data = (dwd <- "/media/data_drive/") %+% "real_estate/",
       proj = mn %+% "round_two/", log = mn %+% "logs/round_two/",
       imga = mn %+% "round_two/images/analysis/",
       imgb = mn %+% "round_two/images/balance/",
       gis = dwd %+% "gis_data/PA/",
       sher = dwd %+% "real_estate/sheriffs_sales/",
       cens = dwd %+% "census/")
write.packages(wds["log"] %+% "cleaning_session.txt")

#"metavariables"
trt.nms <- c("Control", "Amenities", "Moral", 
             "Duty", "Peer", "Lien", "Sheriff")
trt.nms8 <- c("Holdout", trt.nms)

mos <- c("jul", "sep", "dec")

#number of bootstrap replications in all such exercises
BB <- 5000

#Convenient Functions 
get.col<-function(st){
  cols<-c(Big="blue",Small="red",Control="blue",Amenities="yellow",
          Moral="cyan",Duty="darkgreen",Lien="red",Sheriff="orchid",
          Peer="orange",Holdout="darkgray")
  cols[gsub("\\s.*","",as.character(st))]
}

txt.col<-function(st){
  cols<-c("black","white")
  cols[st%in%c("Big","Control","Duty")+1]
}

#since min(NA,na.rm=T) is Inf when I'd prefer NA
#  note that I'm setting my preferred default
min2<-function(..., na.rm = TRUE){
  m <- do.call("min",c(list(...),na.rm=na.rm))
  if (is.infinite(m)) NA else m
}

#Data Import ####
##Block I: Main Sample
### total_due is pre-study balance
### current_balance is as of July 22, 2015
### total_paid is the accrual between June 1, 2015
###   and July 22, 2015
mainI<-{setnames(setDT(read.xlsx3(
  wds["data"]%+%"Payments and Balance Penn Letter Experiment_150727.xlsx",
  colIndex=c(2,5,8,9,13:15),
  sheetName="DETAILS",header=T,startRow=9,stringsAsFactors=F,
  colClasses=abbr_to_colClass("cnDn","4111"))),
  c("opa_no","treat15","paid_full","ever_paid",
    "current_balance","earliest_pmt","total_paid"))}

##Block II: Holdout Sample
holdoutII<-{setnames(setDT(read.xlsx3(
  wds["data"]%+%"req20150709_PennLetterExperiment_"%+%
    "v2_Commissioners Control Details.xlsx",
  colIndex=c(2,8,9,12:14),
  sheetName="DETAILS",header=T,startRow=9,stringsAsFactors=F,
  colClasses=abbr_to_colClass("cnDn","3111"))),
  c("opa_no","paid_full","ever_paid",
    "current_balance","earliest_pmt","total_paid")
  )[,treat15:="Holdout"]}

###The following account changed OPA # between samples,
###  See correspondence with Darryl Watson dated:
###    Tue, Sep 15, 2015 at 4:09 PM
###  (should also be able to corroborate with Account ID)

update_opas<-data.table(old="057027304",
                        new="057026500")
holdoutII[update_opas,opa_no:=i.old,on=c("opa_no"="new")]

##Block III: Follow-up Sample (September 2015)
followupIII<-setDT(read_excel(
  wds["data"]%+%"req20150709_PennLetterExperiment (September 2015 update) v2.xlsx",
  ##** NOTE: I'm using my own branch of readxl here which
  ##   supports multiple NA values; installed via
  ##   devtools::install_github("MichaelChirico/readxl@multiple_na") **
  sheet="DETAILS",skip=7,na=c("NULL","-"),
  col_names=c("x","opa_no","x","x","latitude","longitude",
              rep("x",5),"paid_full","ever_paid","pmt_agr",
              rep("x",3),"current_balance","earliest_pmt",
              "total_paid","pmt_agr_type","pmt_agr_status",
              "pmt_agr_start","pmt_agr_amt","x"),
  col_types=abbr_to_colClass("tnttndntdt","4292111212"))
  )[,c("earliest_pmt","pmt_agr_start"):=
      #Deleting last row b/c read_excel read too many rows
      lapply(list(earliest_pmt,pmt_agr_start),as.Date)][-.N,!"x",with=F]

###The following accounts changed OPA #s between samples,
###  See correspondence with Darryl Watson dated:
###    Tue, Sep 15, 2015 at 4:09 PM
###  (should also be able to corroborate with Account ID)
update_opas<-data.table(old=c("151102600","884350465"),
                        new=c("151102610","881577275"))
followupIII[update_opas,opa_no:=i.old,on=c("opa_no"="new")]

##Block IV: Follow-up Sample (December 2015)
followupIV<-setDT(read_excel(
  wds["data"]%+%"req20150709_PennLetterExperiment (December 2015 update) v2.xlsx",
  sheet="DETAILS",skip=7,na=c("NULL","-"),
  col_names=c("account","opa_no",rep("x",9),
              "paid_full","ever_paid","pmt_agr",
              rep("x",3),"current_balance","earliest_pmt",
              "total_paid","pmt_agr_type","pmt_agr_status",
              "pmt_agr_start","pmt_agr_amt","x"),
  #Deleting last row b/c read_excel read too many rows
  col_types=abbr_to_colClass("tnttnnntnt","4292111212"))
  )[,c("earliest_pmt","pmt_agr_start"):=
      lapply(.(earliest_pmt,pmt_agr_start),
             as.Date,origin=as.Date("1899-12-30"))
    ][-.N,!"x",with=F]

###In addition to the two from the three-month sample,
###  two more properties have updated OPAs.
###  **TO DO: INSERT E-MAIL META DATA FROM DOR CONFIRMATION**
update_opas<-
  data.table(old=c("151102600","884350465","882932475","213155700"),
             new=c("151102610","881577275","882932476","881081460"))
followupIV[update_opas,opa_no:=i.old,on=c("opa_no"="new")]

##Block V: Main Sample Background Data
mainBGV<-fread(wds["proj"]%+%"round_2_full_data.csv",drop="treatment")

##Block VI: Holdout Sample Background Data
holdBGVI<-fread(wds["proj"]%+%"holdout_sample.csv")

##Block VII: Supplemental Geocoding Data
geosuppVII<-fread(wds["data"]%+%"round_2_supplement_lon_lat.csv")

##Block VIII: Spatial Data for Sheriff's Sale properties
geosherVIII<-
  fread(wds["data"]%+%"/sheriffs_sales/"%+%
          "delinquent_sold_used_round_2_w_lon_lat.csv",
        select=c("address","longitude","latitude"))

##Block IX: Assorted Geographic Indicators
##  (as well as associated supplemental stats)

###Census Tract-Level Data on Education
###  (via ACS 2013 5-year tables, as
###   encapsulated in table S1501, and
###   retrieved from American FactFinder)
### VC13: % (age 25+) with: Bachelor's degree
### VC14: % (age 25+) with: Grad./prof. degree
cens.kp<-"HC01_EST_VC"%+%13:14
cens.ed.data<-
  fread(wds["cens"]%+%
          "ACS_13_5YR_S1501_with_ann.csv",
        select=c("GEO.id2",cens.kp),na.strings="-",
        colClasses="character"
        )[,.(census_tract=GEO.id2,
             pct_coll_grad=
               as.numeric(HC01_EST_VC13)+
               as.numeric(HC01_EST_VC14))]

###Census Tract-Level Data on Median income
###  (via ACS 2013 5-year tables, as
###   encapsulated in table S1903, and
###   retrieved from American FactFinder)
cens.inc.data<-
  setnames(fread(wds["cens"]%+%
                   "ACS_13_5YR_S1903_with_ann.csv",
                 select=c("GEO.id2","HC02_EST_VC02"),
                 na.strings="-",
                 colClasses=c(GEO.id2="character",
                              HC02_EST_VC02="numeric")),
           c("census_tract","ct_median_hh_income"))

##Ward Division-Level Voter Registration Data
###  Via e-mail & phone exchange with Daniel A. Pearson,
###  Principal Assistant Office of City Commissioner Al Schmidt
###  Data accurate through November 10, 2015
kp.col<-c("Precinct","Dem","Rep","Ind","Total")
reg.data<-
  setDT(read_excel(
    wds["data"]%+%"Qualified Voter Lisiting 2015 Primary.xlsx",
    #Check: https://github.com/hadley/readxl/issues/147
    #  to see if this workaround of specifying nrows
    #  has updated
    sheet=1,na=c("","Totals:"))
    )[-.N][,Precinct:=sprintf("%04d",Precinct)
           ][,kp.col,with=FALSE
             ][,.(political_ward=Precinct,
                  pct_democrat=Dem/Total,
                  pct_republican=Rep/Total,
                  pct_independent=Ind/Total)]
  
geoindIX<-
  fread(wds["data"]%+%"round_2_spatial_regions.csv"
        )[cens.ed.data,on="census_tract",nomatch=0L
          ][cens.inc.data,on="census_tract",nomatch=0L
            ][reg.data,on="political_ward",nomatch=0L]

###Block X: Other Background Data
###  From OPA-issued Property Data CD
###  (certified for 2015, received May 2014)
opabgX<-
  setnames(fread(wds["data"]%+%"prop2015.txt",
                 select=c("PARCEL","MV","CAT CD","XMPT CD")),
           c("opa_no","assessed_mv","category","exempt_code"))
                
        
##Quilting time!
### Framework:
###  I  III IV V  VII VIII IX X
###  II III IV VI VII   -  IX X
bgvars<-names(mainBGV)%\%"opa_no"
geovars<-c("latitude","longitude")
updvars<-c("paid_full","ever_paid","current_balance",
           "earliest_pmt","total_paid")
updvars2<-c(updvars,"pmt_agr","pmt_agr_type",
            "pmt_agr_status","pmt_agr_start","pmt_agr_amt")
properties<-
  setnames(rbind(mainI,holdoutII),
           updvars,updvars%+%"_jul"
           )[setnames(followupIII,updvars2,updvars2%+%"_sep"),
             on="opa_no"
             ][setnames(followupIV,updvars2,updvars2%+%"_dec"),
               on="opa_no"
               ][rbind(mainBGV,holdBGVI,fill=T),
                 (bgvars):=mget("i."%+%bgvars),on="opa_no"
                 ][geosuppVII,(geovars):=mget("i."%+%geovars),
                   on="opa_no"]

nms<-names(geoindIX)%\%"opa_no"
properties[geoindIX,(nms):=mget("i."%+%nms),on="opa_no"]

properties[opabgX,`:=`(assessed_mv=as.numeric(i.assessed_mv),
                       residential=i.category=="1",
                       exempt_code=i.exempt_code),on="opa_no"]

addrs<-"example_address_"%+%1:3
for (addr in addrs){
  ll<-addr%+%c("_longitude","_latitude")
  properties[geosherVIII,(ll):=
               .(i.longitude,i.latitude),
             on=setNames("address",addr)]
  #Should update this if we ever add corresponding
  #  SS/Amenities for holdout properties for comparison
  #**Note: Distance is in kilometers (see ?spDists)**
  properties[treat15!="Holdout",addr%+%"_distance":=
               spDists(x=cbind(longitude,latitude),
                       y=Reduce(cbind,mget(ll)),
                       longlat=T,diagonal=T)]
}

properties[,sheriff_distance_min:=
             do.call("pmin",mget(addrs%+%"_distance"))]

properties[,sheriff_distance_mean:=
             rowMeans(Reduce(cbind,mget(addrs%+%"_distance")))]



##Data Clean-up
###Account ID with extra whitespace
properties[,account:=gsub("\\s","",account)]
###Re-set indicators as T/F instead of Y/N
inds<-c("paid_full_jul","ever_paid_jul",
        "paid_full_sep","ever_paid_sep",
        "paid_full_dec","ever_paid_dec",
        "pmt_agr_sep","pmt_agr_dec")
properties[,(inds):=lapply(.SD,function(x)x=="Y"),.SDcols=inds]
###Paid Full actually stored opposite because 
###  question in data is: "Does property have a balance?"
pf <- "paid_full_" %+% mos
properties[,(pf):=lapply(.SD,`!`),.SDcols=pf]

properties[,paid_part_jul:=ever_paid_jul&!paid_full_jul]
properties[,paid_part_sep:=ever_paid_sep&!paid_full_sep]
properties[,paid_part_dec:=ever_paid_dec&!paid_full_dec]

properties[,treat15:=gsub("\\sE.*","",gsub("_"," ",treat15))]

##Define some flags
### Is this a holdout property?
properties[,holdout:=treat15=="Holdout"]
### Was more than one treatment received at this mailing address?
properties[,flag_multiple_address:=uniqueN(treat15)>1,
        by=.(mail_address,mail_city,mail_state)]
### Did this owner receive more than one letter?
properties[,flag_multiple_property:=.N>1,by=owner1]
### Was this owner in both the holdout sample and the treatment panel?
properties[,flag_holdout_overlap:=any(treat15=="Holdout")&
             any(treat15!="Holdout"),by=owner1]
### Was this property treated in Round 1?
properties[,flag_round_one_overlap:=
          opa_no %in% fread(wds["data"]%+%"analysis_file_end_only_act.csv",
                            select=c("opa_no"))[,unique(opa_no)]]
### Does this property have any of the
###   tax exemptions excluded in Round 1?
properties[,flag_abate_exempt:=exempt_code!=""]

###Define alternative treatment sets:
####14 treatments (exclude holdout)
properties[(!holdout),treat14:=treat15]
####2 & 3 treatments (big vs. small (vs. holdout))
properties[(!holdout),treat3:=gsub("(.*) (.*)","\\2",treat15)]
properties[(holdout),treat3:=treat15]
properties[(!holdout),treat2:=treat3]
####7 & 8 treatments (main treatments)
properties[(!holdout),treat7:=gsub("\\s.*","",treat14)]
properties[(!holdout),treat8:=treat7]
properties[(holdout),treat8:=treat15]
####Reorder main treatments for plotting purposes
properties[,treat14:=factor(treat14,
                            paste(rep(trt.nms, each=2),
                                  c("Small","Big")))]
properties[,treat15:=factor(treat15,
                            c("Holdout",levels(treat14)))]
properties[,treat8:=factor(treat8, trt.nms8)]
properties[,treat7:=factor(treat7, trt.nms)]

properties[,treat3:=factor(treat3, c("Holdout","Small","Big"))]
properties[,treat2:=factor(treat2, c("Small","Big"))]

rm(list=ls()%\%c("owners","properties","trt.nms",
                 "trt.nms8","wds","get.col","mos","BB"))

###Get owner-level version of data, keeping only key analysis variables
owners<-{
  properties[order(treat14),
             .(treat2 = treat2[1L],treat3 = treat3[1L],
               treat7 = treat7[1L],treat8 = treat8[1L],
               treat14=treat14[1L],treat15=treat15[1L],
               rand_id=rand_id[1L],
               #Not sure what to do with overlaps...
               holdout=all(holdout),
               ever_paid_jul=any(ever_paid_jul),
               ever_paid_sep=any(ever_paid_sep),
               ever_paid_dec=any(ever_paid_dec),
               paid_full_jul=all(paid_full_jul),
               paid_full_sep=all(paid_full_sep),
               paid_full_dec=all(paid_full_dec),
               paid_part_jul=any(ever_paid_jul)&
                 !all(paid_full_jul),
               paid_part_sep=any(ever_paid_sep)&
                 !all(paid_full_sep),
               paid_part_dec=any(ever_paid_dec)&
                 !all(paid_full_dec),
               total_paid_jul=sum(total_paid_jul),
               total_paid_sep=sum(total_paid_sep),
               total_paid_dec=sum(total_paid_dec),
               earliest_pmt_jul=
                 if (all(is.na(earliest_pmt_jul))){
                   as.Date(NA)
                   }else{min(earliest_pmt_jul)},
               earliest_pmt_sep=
                 if (all(is.na(earliest_pmt_sep))){
                   as.Date(NA)
                   }else{min(earliest_pmt_sep)},
               earliest_pmt_dec=
                 if (all(is.na(earliest_pmt_dec))){
                   as.Date(NA)
                 }else{min(earliest_pmt_dec)},
               total_due=sum(total_due),
               current_balance_jul=
                 sum(current_balance_jul),
               current_balance_sep=
                 sum(current_balance_sep),
               current_balance_dec=
                 sum(current_balance_dec),
               assessed_mv=sum(assessed_mv),
               pmt_agr1_sep=any(pmt_agr_sep),
               pmt_agr1_dec=any(pmt_agr_dec),
               pmt_agrA_sep=all(pmt_agr_sep),
               pmt_agrA_dec=all(pmt_agr_dec),
               pmt_agr_start_sep=
                 if (all(is.na(pmt_agr_start_sep))){
                   as.Date(NA)
                 }else{min(pmt_agr_start_sep)},
               pmt_agr_start_dec=
                 if (all(is.na(pmt_agr_start_dec))){
                   as.Date(NA)
                 }else{min(pmt_agr_start_dec)},
               flag_holdout_overlap=
                 flag_holdout_overlap[1L],
               flag_round_one_overlap=
                 any(flag_round_one_overlap),
               #only residential if _no_ commercial
               residential=all(residential),
               phila_mailing=
                 all(grepl("PH",mail_city)&
                       !grepl("[XMR]",mail_city)),
               sheriff_distance_mean=
                 mean(sheriff_distance_mean,na.rm=T),
               .N),
             by=owner1]}

###  Subsample Flags
####  a) 1% and b) 5% of accounts by repayment numbers
owners[(!holdout),flag_top01:=total_due>=quantile(total_due,.99)]
owners[(!holdout),flag_top05:=total_due>=quantile(total_due,.95)]

####  Same, now including holdout sample population
owners[,flag_top01_h:=total_due>=quantile(total_due,.99)]
owners[,flag_top05_h:=total_due>=quantile(total_due,.95)]

owners[,unq_own:=N==1]

### Write output
fwrite(owners, wds["data"] %+% "round_two_analysis_owners.csv", quote = TRUE)
fwrite(properties, wds["data"] %+% "round_two_analysis_properties.csv", quote = TRUE)
