#Data Analysis
#Philadelphia Real Estate Tax Evasion
#  Round 2
#Michael Chirico
#July 29, 2015

#Note: See e-mail "now we're in a hurry" from cloef@sas.upenn.edu
#  for confirmation that June 12 is the day small envelope
#  versions were sent to the DoR Mail Room.
#   See e-mail "Status" from rmcfadden@lawlerdirect.com
#  or "Next steps?" (from same) on June 22
#  for confirmation that June 23 is the postage date for
#  the large envelopes sent by Lawler Direct.

#Setup: Random Seed, Packages,
#  Working Directory, Convenient Functions ####
## @knitr setup
##Random Seed
###Alternating digits (1,3,...) of
###  my CVS Extra Care card number
set.seed(4746966)

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
setwd(mn<-"~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/")
wds<-c(data=(dwd<-"/media/data_drive/")%+%"real_estate/",
       proj=mn %+% "round_two/", log = mn %+% "logs/round_two/",
       imga=mn %+% "round_two/images/analysis/",
       imgb=mn %+% "round_two/images/balance/",
       gis=dwd %+% "gis_data/PA/",
       sher=dwd %+% "real_estate/sheriffs_sales/",
       cens=dwd %+% "census/")
write.packages(wds["log"] %+% "analysis_session.txt")

#"metavariables"
trt.nms <- c("Control", "Amenities", "Moral", "Duty", "Peer", "Lien", "Sheriff")
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
                 "trt.nms8","wds","get.col","mos"))

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
fwrite(owners, wds["data"] %+% "round_two_analysis_owners.csv")
fwrite(properties, wds["data"] %+% "round_two_analysis_properties.csv")

#Descriptive Stats ####
## @knitr descriptives
#We count an address as in Philadelphia when:
#  Mailing city contains "PH" but NOT
# "X","M", or "R" -- this eliminates
#  Alpharetta (GA), Gulph Mills (PA),
#  Memphis (TN), Mount Ephraim (NJ), Phoenix (AZ),
#  and Phoenixville (PA). This in particular
#  captures most (but perhaps not all)
#  of the multitudinous typos for Philadelphia.
print.xtable(xtable(
  t(owners[N == 1,
           .(`Number Observations`=prettyNum(.N,big.mark=","),
             `Med. Amount Due (June)`=dol.form(median(total_due)),
             `Med. Assessed Property Value`=
               dol.form(median(assessed_mv,na.rm=T)),
             `% Residential`=
               to.pct(mean(residential,na.rm=T),dig=0)), 
           by = .(Variable = c("Main Sample", "Holdout")[holdout + 1L])]),
  caption=c("Descriptive Statistics -- Background (Owners of One Property)"),
  align=c("|r|r|r|"),label="table:descriptivesI"),caption.placement="top",
  include.colnames=F,comment=F, hline.after = c(0, 1, 5))

vrl <- list(jul = (ov <- c("ever_paid", "paid_full", "current_balance")) %+% "_jul",
            sep = c(ov, "pmt_agr1") %+% "_sep", dec = c(ov, "pmt_agr1") %+% "_dec")
vr <- setNames(unlist(vrl), unlist(vrl))
full_row.n <- c("% Ever Paid", "% Paid in Full",
                "Amount Due", "% in Pmt. Agreement")
sum_fn <- list(v = function(y) as.character(to.pct(mean(y), dig = 0)),
               a = function(y) as.character(to.pct(mean(y), dig = 0)),
               u = function(y) dol.form(mean(y)),
               m = function(y) as.character(to.pct(mean(y), dig = 1)))
print.xtable(xtable(setnames(melt(
  owners[(!holdout), lapply(vr, function(x) sum_fn[[substr(x, 2, 2)]](get(x)))],
  measure = vrl)[ , variable := full_row.n[variable]],
  c("Variable", "One Month", "Three Months", "Six Months")),
  caption = c("Descriptive Statistics -- Outcomes (Owners, Non-Holdout)"),
  align = c("|r|r|r|r|r|"), label = "table:descriptivesII"),
  caption.placement = "top", include.rownames = FALSE, comment = FALSE)

#Balance on Observables ####
## @knitr balance
##Graphic Tests
###Figure 1: Log Balance by Letter
pdf2(wds["imgb"]%+%"dist_log_due_by_trt_7_box.pdf")
par(mar=c(2.6, 5.1, 4.1, 2.1))
boxplot(total_due ~ treat7, data = owners, cex.main = 1.5,
        main = "Box Plots of Initial Debt\nOwner Level, By Treatment",
        log = "x", xaxt = "n", col = get.col(trt.nms),
        notch = TRUE, #notch CIs: ~= 1.58 * IQR / sqrt(n); see ?boxplot.stats
        boxwex = .5, horizontal = TRUE, las = 1L, xlab = "", 
        names = owners[(!holdout), .N, keyby = treat7
                       ][ , paste0(treat7, "\n(n = ", 
                                   prettyNum(N, big.mark = ","), ")")])
axis(side = 1L, at = p10 <- 10L^(1L:6L), cex.axis = .8,
     labels = "$" %+% prettyNum(p10, big.mark = ",", scientific = FALSE))
abline(v = md <- owners[treat7 == "Control", median(total_due)], lty = 2L)
text(md, .5, pos = 4L, cex = .8, 
     labels = "Control Median = $" %+% round(md))
dev.off2()

##Bar Plot: Number of Properties and Owners by Letter
properties[(!holdout),
           .(.N,uniqueN(owner1)),keyby=treat7
           ][,{pdf2(wds["imgb"]%+%"number_prop_own_by_trt_7_bar.pdf")
             layout(mat=matrix(1:2),heights=c(.8,.2))
             par(mar=c(1,4.1,4.1,2.1))
             x<-barplot(c(rbind(N,V2)),names.arg=rep(treat7,each=2),las=3,
                        main="Numbers of Properties and Owners\nBy Treatment",
                        col=get.col(rep(trt.nms,each=2)),
                        density=rep(c(-1,20),.N),cex.names=.75,
                        ylim=c(0,1.3*max(N)))
             par(mar=rep(0,4))
             plot(0,0,type="n",ann=F,axes=F,xlim=range(x))
             legend(max(x)/2,0,bty="n",xjust=.3,
                    legend=c("# Properties","# Owners"),
                    density=c(-1,20),horiz=T,text.width=5)
             dev.off2()}]

###Balance Table
####p-values to be binded in

lmfp<-function(formula){
  #extract F-statistic & DoF from LM call
  mdf <- summary(do.call("lm", list(formula=formula)))$fstatistic
  #copied (ported) from print.summary.lm
  unname(pf(mdf[1L], mdf[2L], mdf[3L], lower.tail = FALSE))
}

pvs <- c(sapply(c(
  `Amount Due (June)`="total_due",
  `Assessed Property Value`="assessed_mv",
  `\\% with Unique Owner`="unq_own",
  `\\% Overlap with Holdout`="flag_holdout_overlap",
  `\\# Properties per Owner`="N"),
  function(x) owners[(!holdout), lmfp(get(x) ~ treat7)]),
  `\\# Owners`=
    owners[(!holdout), chisq.test(table(treat7))$p.value])
pvs <- c("$p$-value", round(pvs, 2L))

print.xtable(xtable(cbind(gsub("$", "\\$", t(
  owners[(!holdout),
         .(`Amount Due (June)`=dol.form(mean(total_due)),
           `Assessed Property Value`=dol.form(mean(assessed_mv, na.rm=TRUE)),
           `\\% with Unique Owner`=to.pct(mean(unq_own), 1L),
           `\\% Overlap with Holdout`=to.pct(mean(flag_holdout_overlap), 2L),
           `\\# Properties per Owner`=round(mean(N), 2L),
           `\\# Owners`=prettyNum(.N, big.mark = ",")),
         keyby=.(Variable=treat7)]), fixed = TRUE), pvs),
  caption="Balance on Observables",
  label="tbl:balance", align = "|r|lllllll|l|"),
  include.colnames=FALSE, comment=FALSE, 
  sanitize.text.function=identity,
  floating.environment="sidewaystable", 
  floating=TRUE, hline.after=c(0, 1),
  add.to.row=
    list(pos = list(7),
         command = c("\\hline \n \\multicolumn{9}{l}" %+% 
                       "{\\scriptsize{$p$-values in rows 1-5 are " %+% 
                       "$F$-test $p$-values from regressing each " %+% 
                       "variable on treatment dummies. A $\\chi^2$ " %+% 
                       "test was used for the count of owners.}} \\\\ \n")))

#Point Estimate CIs ####
# @knitr analysis_bs
#For the standard confidence intervals,
#  we can condense the process by storing
#  the key changing parameters in this list;
#  also store a bunch of plotting parameters
#  since we'll use the result of this list for that
outvar <- parse(text = paste0(
  "setNames(list(", 
  paste(paste0("mean(100*ever_paid_", mos, ")"), 
        collapse = ","), "), outn)"))

delq <- quote(c(Treatment = list(treat8[-1L]),
                lapply(.SD, function(x) x[-1L] - x[1L])))

outn <- c("One Month", "Three Months", "Six Months")

dt <- owners[(unq_own)]

boot <- 
  rbindlist(lapply(integer(BB), function(...)
            #calculate point estimate in re-sample
            dt[sample(.N, rep = TRUE), eval(outvar),
               keyby = treat8][ , eval(delq), .SDcols = outn]))

star <- function(x){
  q1.5.10 <- quantile(x, c(.005, .025, .05, .95, .975, .995))
  if (!0 %between% q1.5.10[c(1, 6)]) return("***")
  if (!0 %between% q1.5.10[c(2, 5)]) return("**")
  if (!0 %between% q1.5.10[c(3, 4)]) return("*")
  return("")
}

print(xtable(rbind(
  dt[ , eval(outvar), keyby = treat8
      ][ , c(Treatment = list(treat8), lapply(.SD, function(x) 
        round(c(x[1L], x[-1L] - x[1L]), 1) %+% "")), .SDcols = outn
        ][boot[ , lapply(.SD, star), by = Treatment, .SDcols = outn],
          (outn) := lapply(outn, function(jj) 
            get(jj) %+% get("i." %+% jj)), on = "Treatment"],
  boot[ , lapply(.SD, function(x) round(sd(x), 1)), by = Treatment], 
  idcol = "type")[type == 2, (outn) := lapply(.SD, function(x)
    paste0("(", x, ")")), .SDcols = outn
    ][order(Treatment)][type == 2, Treatment := ""
                        ][ , !"type", with = FALSE],
  caption = "Participation Rates by Treatment over Time, " %+% 
    "Unique Owners vs. Holdout", align = "rrlll", label = "tbl:marg_ep"),
  include.rownames = FALSE, comment = FALSE, hline.after = c(-1, 1),
      caption.placement = "top", 
  add.to.row = 
    list(pos = list(15), 
         command = 
           "\n \\hline \n \\multicolumn{4}{l}{\\scriptsize{" %+% 
           "$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$; " %+% 
           "Holdout values are in levels; " %+% 
           "remaining figures are relative to Holdout}} \\\\ \n"))

## Cumulative Partial Participation
## @knitr analysis_ch_ep
### get range of dates for day-by-day averages
dt.rng <- owners[(!holdout & unq_own),{
  rng <- range(earliest_pmt_dec, na.rm = TRUE)
  seq(from = rng[1L], to = rng[2L], by = "day")}]
#For pretty printing, get once/week subset
dt.rng2 <- dt.rng[seq(1L, length(dt.rng), length.out = 7L)]

date.dt <- 
  #not all treatments saw activity on each day,
  #  so we'll have to "fill-in-the-blanks" with this
  CJ(treat8 = c("Holdout","Control"), date = dt.rng,
     unique = TRUE, sorted = FALSE)
owners[(unq_own), hold_cont := treat8 %in% c("Holdout","Control")]
cum_haz <- 
  owners[(hold_cont),
         #total entrants by day, treatment
         sum(ever_paid_dec) + 0., #convert to numeric
         keyby = .(treat8, earliest_pmt_dec)
         #running total of entrants; take care to eliminate NAs
         ][ , .(ep = cumsum(V1[idx <- !is.na(earliest_pmt_dec)]),
                date = earliest_pmt_dec[idx]), by=treat8
            #merge to get the denominator
            ][owners[(hold_cont), .N, treat8],
              ep := ep/i.N, on = "treat8"
              ][date.dt, on = c("treat8", "date"), roll = TRUE
                #express relative to holdout
                ][ , .(ep = ep[idx <- treat8 == "Control"] -
                         ep[!idx]), by = date]

cis <- dcast(rbindlist(lapply(integer(BB), function(...){
  dt <- owners[(hold_cont)][sample(.N, rep = TRUE)]
  dt[ , sum(ever_paid_dec) + 0., keyby = .(treat8, earliest_pmt_dec)
     ][ , .(ep = cumsum(V1[idx <- !is.na(earliest_pmt_dec)]),
          date = earliest_pmt_dec[idx]), by = treat8
       ][dt[ , .N, treat8], ep := ep/i.N, on = "treat8"
         ][date.dt, on = c("treat8", "date"), roll = TRUE
           ][ , .(ep = ep[idx <- treat8 == "Control"] - ep[!idx]),
             by = date]}), idcol = "bootID"
  #extract for 95% CI
  )[ , quantile(ep, c(.025, .975), na.rm = TRUE), by = date],
  date ~ c("low", "high")[rowid(date)], value.var = "V1")

## @knitr ignore
### plot
pdf2(wds["imga"] %+% "cum_haz_ever_paid_control_holdout_own.pdf")
cum_haz[
  cis, on = c("date")
  ][ , {
    matplot(date, do.call("cbind", mget(c("low", "ep", "high"))),
            type = "l", lty=c(2L, 1L, 2L), col = get.col("Control"),
            xaxt = "n", lwd = 3L, las = 2L,
            ylab = "Probability Ever Paid vs. Holdout")
    axis(side = 1L, at = dt.rng2, labels = format(dt.rng2, "%b %d"))
    abline(h = 0, col = "black", lwd = 2L)
    title("Cumulative Partial Participation\n" %+%
            "Control vs. Holdout (Unique Owners Only)")}]
dev.off2()

#Regression Tables ####
# @knitr analysis_reg
reg_vars <- c("ever_paid", "paid_full", "total_paid")
reg_all <- levels(interaction(reg_vars, mos, sep="_"))

reg_j <- 
  parse(text = 
          paste0(".(", paste(paste0(
            reg_all, "=list(", sapply(reg_all, function(x) 
    if (grepl("total", x)) 
      paste0("lm(", x," ~ treat7)")
    else paste0("glm(", x, " ~ treat7, ",
                "family = binomial)")), ")"), 
    collapse = ","), ")"))

coef_j = 
  parse(text = 
          paste0(".(", paste(paste0(
            reg_all, "=", sapply(reg_all, function(x) 
              if (grepl("total", x)) 
                paste0("lm.fit(model.matrix(", x,
                       " ~ treat7), ",x,")")
              else paste0("glm.fit(model.matrix(", x, 
                          " ~ treat7), ",x,
                          ', start = starts[["',x,'"]],',
                          "family=binomial())")), 
            "$coefficients"), 
            collapse = ","), ")"))

setkey(owners, rand_id)
setindex(owners, holdout)
DT <- owners[(!holdout)]
regs <- owners[(!holdout), eval(reg_j)]
starts <- 
  lapply(reg_all[!grepl("total", reg_all)], 
         function(rr) regs[[rr]][[1L]]$coefficients)
RBs <- owners[(!holdout), unique(rand_id)]

tmp <- tempfile()
progress <- txtProgressBar(1, BB, style = 3L)

cl <- makeCluster(8L, outfile = "")
invisible(clusterEvalQ(cl, library(data.table)))
clusterExport(cl, c("DT", "RBs", "coef_j", "starts", 
                    "progress", "tmp"),
              envir = environment())
boot_dist <- rbindlist(parLapply(cl, 1:BB, function(bb){
  cat("\n", file = tmp, append = TRUE)
  setTxtProgressBar(
    progress, as.integer(system(paste("wc -l <", tmp), intern = TRUE)))
  DT[.(sample(RBs, rep = TRUE)), eval(coef_j)]}))
stopCluster(cl); close(progress); unlink(tmp)

reg_info <- 
  rbind(regs, boot_dist[ , lapply(.SD, function(x)
    list(var(matrix(x, ncol = 7, byrow = TRUE))))]
    )[ , lapply(.SD, function(x)
      c(x, list(sqrt(diag(x[[2]])))))
      ][ , lapply(.SD, function(x) 
        c(x, list(coeftest(x[[1]], vcov = x[[2]])[ , 4])))]

#now for single-owner samples
##exclude total_paid for single owner analysis
DT <- owners[(!holdout & unq_own)]
regs <- owners[(!holdout & unq_own), eval(reg_j)]
starts <- 
  lapply(reg_all, function(rr) regs[[rr]][[1L]]$coefficients)
RBs <- owners[(!holdout & unq_own), unique(rand_id)]

tmp <- tempfile()
progress <- txtProgressBar(1, BB, style = 3L)

cl <- makeCluster(8L, outfile = "")
invisible(clusterEvalQ(cl, library(data.table)))
clusterExport(cl, c("DT", "RBs", "coef_j", "starts", "progress", "tmp"),
              envir = environment())
boot_dist <- rbindlist(parLapply(cl, 1:BB, function(bb){
  cat("\n", file = tmp, append = TRUE)
  setTxtProgressBar(
    progress, as.integer(system(paste("wc -l <", tmp), intern = TRUE)))
  DT[.(sample(RBs, rep = TRUE)), eval(coef_j)]}))
stopCluster(cl); close(progress); unlink(tmp)

reg_info_so <- 
  rbind(regs, boot_dist[ , lapply(.SD, function(x)
    list(var(matrix(x, ncol = 7, byrow = TRUE))))]
    )[ , lapply(.SD, function(x)
      c(x, list(sqrt(diag(x[[2]])))))
      ][ , lapply(.SD, function(x) 
        c(x, list(coeftest(x[[1]], vcov = x[[2]])[ , 4])))]


###Difference in Mean Tests
rename_coef<-function(obj){
  nms <- names(obj$coefficients)
  idx <- !grepl("treat7", nms)
  #Add XXX to coefficients we'll exclude
  nms[idx] <- nms[idx] %+% "XXX"
  nms <- gsub("treat7", "", nms)
  names(obj$coefficients) <- nms
  obj
}

proper <- c(ever_paid="Ever Paid",
            paid_full="Paid Full",
            total_paid="Total Paid")
abbr <- c(ever_paid="ep", paid_full="pf", total_paid="tp")
invisible(lapply(reg_vars, function(rr){
  cat("\n\n\n\n***********************\n", proper[rr])
  x <- capture.output(texreg(c(lapply(
    tp <- paste(rr, mos, sep="_"), function(mo)
      rename_coef(reg_info[[mo]][[1]])),
    lapply(tp, function(mo)
      rename_coef(reg_info_so[[mo]][[1]]))),
    custom.model.names=
      rep(c("One Month","Three Months","Six Months"), 2),
    caption=
      "Estimated Average Treatment Effects: " %+% proper[rr],
    override.se=
      c(lapply(tp, function(mo) reg_info[[mo]][[3]]),
        lapply(tp, function(mo) reg_info_so[[mo]][[3]])),
    override.pval=
      c(lapply(tp, function(mo) reg_info[[mo]][[4]]),
        lapply(tp, function(mo) reg_info_so[[mo]][[4]])),
    caption.above=TRUE, stars=c(.01,.05,.1),
    label="tbl:reg7_" %+% abbr[rr], 
    include.rsquared=FALSE, include.adjrs=FALSE, 
    include.rmse=FALSE, include.aic=FALSE, 
    include.bic=FALSE, include.deviance=FALSE,
    #Exclude Intercept
    omit.coef="XXX$", float.pos="htbp", 
    sideways = TRUE, use.packages = FALSE))
  
  ## add vertical lines to separate populations
  x[5] <- "\\begin{tabular}{|l| c c c| c c c| }"
  
  ## add multicolumn to distinguish populations
  x[6] <- x[6] %+% "\n & \\multicolumn{3}{c}{All Owners} & " %+% 
    "\\multicolumn{3}{|c|}{Single-Property Owners} \\\\"
  
  ## print again
  cat(x, sep = "\n")}))







#*************REPEATING FOR JUST EXCLUDING TOP 2 BLOCKS***************
reg_vars <- "ever_paid"
reg_all <- reg_vars %+% "_" %+% mos

reg_j <- 
  parse(text = 
          paste0(".(", paste(paste0(
            reg_all, "=list(", sapply(reg_all, function(x) 
    if (grepl("total", x)) 
      paste0("lm(", x," ~ treat7)")
    else paste0("glm(", x, " ~ treat7, ",
                "family = binomial)")), ")"), 
    collapse = ","), ")"))

coef_j = 
  parse(text = 
          paste0(".(", paste(paste0(
            reg_all, "=", sapply(reg_all, function(x) 
              if (grepl("total", x)) 
                paste0("lm.fit(model.matrix(", x,
                       " ~ treat7), ",x,")")
              else paste0("glm.fit(model.matrix(", x, 
                          " ~ treat7), ",x,
                          ', start = starts[["',x,'"]],',
                          "family=binomial())")), 
            "$coefficients"), 
            collapse = ","), ")"))

setkey(owners, rand_id)
setindex(owners, holdout)
DT <- owners[(!holdout & rand_id > 2)]
regs <- owners[(!holdout & rand_id > 2), eval(reg_j)]
starts <- 
  lapply(reg_all, function(rr) regs[[rr]][[1L]]$coefficients)
RBs <- owners[(!holdout & rand_id > 2), unique(rand_id)]

tmp <- tempfile()
progress <- txtProgressBar(1, BB, style = 3L)

cl <- makeCluster(8L, outfile = "")
invisible(clusterEvalQ(cl, library(data.table)))
clusterExport(cl, c("DT", "RBs", "coef_j", "starts", 
                    "progress", "tmp"),
              envir = environment())
boot_dist <- rbindlist(parLapply(cl, 1:BB, function(bb){
  cat("\n", file = tmp, append = TRUE)
  setTxtProgressBar(
    progress, as.integer(system(paste("wc -l <", tmp), intern = TRUE)))
  DT[.(sample(RBs, rep = TRUE)), eval(coef_j)]}))
stopCluster(cl); close(progress); unlink(tmp)

reg_info_x28 <- 
  rbind(regs, boot_dist[ , lapply(.SD, function(x)
    list(var(matrix(x, ncol = 7, byrow = TRUE))))]
    )[ , lapply(.SD, function(x)
      c(x, list(sqrt(diag(x[[2]])))))
      ][ , lapply(.SD, function(x) 
        c(x, list(coeftest(x[[1]], vcov = x[[2]])[ , 4])))]

#now for single-owner samples
##exclude total_paid for single owner analysis
DT <- owners[(!holdout & unq_own & rand_id > 2)]
regs <- owners[(!holdout & unq_own & rand_id > 2), eval(reg_j)]
starts <- 
  lapply(reg_all, function(rr) regs[[rr]][[1L]]$coefficients)
RBs <- owners[(!holdout & unq_own & rand_id > 2), unique(rand_id)]

tmp <- tempfile()
progress <- txtProgressBar(1, BB, style = 3L)

cl <- makeCluster(8L, outfile = "")
invisible(clusterEvalQ(cl, library(data.table)))
clusterExport(cl, c("DT", "RBs", "coef_j", "starts", "progress", "tmp"),
              envir = environment())
boot_dist <- rbindlist(parLapply(cl, 1:BB, function(bb){
  cat("\n", file = tmp, append = TRUE)
  setTxtProgressBar(
    progress, as.integer(system(paste("wc -l <", tmp), intern = TRUE)))
  DT[.(sample(RBs, rep = TRUE)), eval(coef_j)]}))
stopCluster(cl); close(progress); unlink(tmp)

reg_info_so_x28 <- 
  rbind(regs, boot_dist[ , lapply(.SD, function(x)
    list(var(matrix(x, ncol = 7, byrow = TRUE))))]
    )[ , lapply(.SD, function(x)
      c(x, list(sqrt(diag(x[[2]])))))
      ][ , lapply(.SD, function(x) 
        c(x, list(coeftest(x[[1]], vcov = x[[2]])[ , 4])))]


###Difference in Mean Tests
rename_coef<-function(obj){
  nms <- names(obj$coefficients)
  idx <- !grepl("treat7", nms)
  #Add XXX to coefficients we'll exclude
  nms[idx] <- nms[idx] %+% "XXX"
  nms <- gsub("treat7", "", nms)
  names(obj$coefficients) <- nms
  obj
}

x <- capture.output(texreg(c(lapply(
  tp <- reg_all, function(mo)
    rename_coef(reg_info_x28[[mo]][[1]])),
  lapply(tp, function(mo)
    rename_coef(reg_info_so_x28[[mo]][[1]]))),
  custom.model.names=
    rep(c("One Month","Three Months","Six Months"), 2),
  caption=
    "Estimated Average Treatment Effects " %+% 
    "(Excluding top 28 Owners): Ever Paid",
  override.se=
    c(lapply(tp, function(mo) reg_info_x28[[mo]][[3]]),
      lapply(tp, function(mo) reg_info_so_x28[[mo]][[3]])),
  override.pval=
    c(lapply(tp, function(mo) reg_info_x28[[mo]][[4]]),
      lapply(tp, function(mo) reg_info_so_x28[[mo]][[4]])),
  caption.above=TRUE, stars=c(.01,.05,.1),
  label="tbl:reg7_ep_x28", 
  include.rsquared=FALSE, include.adjrs=FALSE, 
  include.rmse=FALSE, include.aic=FALSE, 
  include.bic=FALSE, include.deviance=FALSE,
  #Exclude Intercept
  omit.coef="XXX$", float.pos="htbp", 
  sideways = TRUE, use.packages = FALSE))

## add vertical lines to separate populations
x[5] <- "\\begin{tabular}{|l| c c c| c c c| }"

## add multicolumn to distinguish populations
x[6] <- x[6] %+% "\n & \\multicolumn{3}{c}{All Owners} & " %+% 
  "\\multicolumn{3}{|c|}{Single-Property Owners} \\\\"

## print again
cat(x, sep = "\n")






#Now for main vs. holdout
reg_all <- c("paid_full_", "ever_paid_") %+% 
  rep(mos, each = 2)

reg_j <- 
  parse(text = 
          paste0(".(", paste(paste0(
            reg_all, "=list(glm(", reg_all,
            " ~ treat8, family = binomial))"), 
            collapse = ","), ")"))

coef_j = 
  parse(text = 
          paste0(".(", paste(paste0(
            reg_all, "=", sapply(reg_all, function(x) 
              paste0("glm.fit(model.matrix(", x, 
                     " ~ treat8), ",x,
                     ', start = starts[["',x,'"]],',
                     "family=binomial())")), 
            "$coefficients"), 
            collapse = ","), ")"))

DT <- owners[(unq_own)]
regs <- owners[(unq_own), eval(reg_j)]
starts <- 
  lapply(reg_all, function(rr) regs[[rr]][[1L]]$coefficients)

tmp <- tempfile()
progress <- txtProgressBar(1, BB, style = 3L)

cl <- makeCluster(8L, outfile = "")
invisible(clusterEvalQ(cl, library(data.table)))
clusterExport(cl, c("DT", "coef_j", "starts", "progress", "tmp"),
              envir = environment())
boot_dist <- rbindlist(parLapply(cl, 1:BB, function(bb){
  cat("\n", file = tmp, append = TRUE)
  setTxtProgressBar(
    progress, as.integer(system(paste("wc -l <", tmp), intern = TRUE)))
  DT[sample(.N, rep = TRUE), eval(coef_j)]}))
stopCluster(cl); close(progress); unlink(tmp)

reg_info_8 <- 
  rbind(regs, boot_dist[ , lapply(.SD, function(x)
    list(var(matrix(x, ncol = 8, byrow = TRUE))))]
    )[ , lapply(.SD, function(x)
      c(x, list(sqrt(diag(x[[2]])))))
      ][ , lapply(.SD, function(x) 
        c(x, list(coeftest(x[[1]], vcov = x[[2]])[ , 4])))]


###Difference in Mean Tests
rename_coef<-function(obj){
  nms <- names(obj$coefficients)
  idx <- !grepl("treat8", nms)
  #Add XXX to coefficients we'll exclude
  nms[idx] <- nms[idx] %+% "XXX"
  nms <- gsub("treat8", "", nms)
  names(obj$coefficients) <- nms
  obj
}

ep <- reg_all[grepl("ever", reg_all)]
print(texreg(
  lapply(ep, function(mo)
    rename_coef(reg_info_8[[mo]][[1]])),
  custom.model.names=
    c("One Month","Three Months","Six Months"),
  caption=
    "Estimated Average Treatment Effects: Ever Paid, vs. Holdout",
  override.se=lapply(ep, function(mo) reg_info_8[[mo]][[3]]),
  override.pval=lapply(ep, function(mo) reg_info_8[[mo]][[4]]),
  caption.above=TRUE, stars=c(.01,.05,.1),label="tbl:reg8_ep", 
  include.rsquared=FALSE, include.adjrs=FALSE, include.rmse=FALSE,
  #Exclude Intercept
  omit.coef="XXX$", float.pos="htbp"))

#Cost-Benefit: Estimating returns to speed-up ####
## Parameters of estimate
##  - Date when properties were distributed to co-council
##  - Proportion of converted debt distributed to co-council
params <- list(switch_date = D("2015-08-15"),
               lawyer_rate = .06)
delta <- 
  #count those having paid by hand-off
  owners[earliest_pmt_dec <= params$switch_date,
         .N, keyby = treat8
         ][owners[ , .N, keyby = treat8], 
           #what's the difference in percentage
           #  of owners ever paid in each 
           #  treatment/control vs. holdout?
           .(treat8[-1], N[-1]/i.N[-1] - N[1]/i.N[1]), 
           on = "treat8"][ , setNames(V2, V1)]

repaid_by_switch <- 
  owners[earliest_pmt_dec <= params$switch_date &
           !treat8 == "Holdout"]

setkey(repaid_by_switch, treat7)

dist_benefits <- 
  sapply(integer(BB), function(...)
    sum(sapply(trt.nms, function(tr)
      #going treatment-by-treatment, pull out 
      #  properties in quantity roughly equal to the 
      #  difference in treatment effect vs. holdout 
      #  by the switch date
      repaid_by_switch[.(tr)][
        sample(.N, round(delta[tr] * .N)), 
        #find what would have been the lawyers'
        #  take of the total amount paid by
        #  these properties
        params$lawyer_rate * 
          sum(total_paid_jul)])))

pdf2(wds["imga"] %+% "hist_benefit_acceleration.pdf")
x <- hist(dist_benefits, breaks = 20, freq = FALSE,  
          main = "Distribution of Simulated Benefits to Acceleration",
          col = "lightblue", xlab = "$", yaxt = "n", ylab = "")
abline(v = md <- median(dist_benefits), col = "red", lty = 2, lwd = 3)
mtext(side = 2L, text = "Density")
text(md, max(x$density), pos = 4,
     labels = "Median Benefit: " %+% dol.form(md))
dev.off2()

##Regression-based approach
setkey(owners, treat8)
setindex(owners, treat8, assessed_mv)
regs <- 
  owners[assessed_mv > 0, 
         .(list(reg=lm(total_paid_jul ~ log(assessed_mv) + 
                         log(total_due) + residential + 
                         phila_mailing + log(N)))), keyby = treat8]

params$lawyer_rate * sum(sapply(trt.nms, function(tr)
  sum((x <- owners[.(tr)][assessed_mv > 0])[ , total_paid_jul] - 
        predict(regs[.("Holdout"), V1][[1]], x))))

params$lawyer_rate * sum(sapply(trt.nms, function(tr)
  sum(predict(regs[.(tr), V1][[1]], x <- owners[.(tr)][assessed_mv > 0]) - 
        predict(regs[.("Holdout"), V1][[1]], x))))

##Back-of-the-envelope approach

