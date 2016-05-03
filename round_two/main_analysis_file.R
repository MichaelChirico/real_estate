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
###  **INSERT E-MAIL META DATA FROM DOR CONFIRMATION**
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
  mdf<-summary(do.call("lm",list(formula=formula)))$fstatistic
  #copied (ported) from print.summary.lm
  unname(pf(mdf[1L], mdf[2L], mdf[3L], lower.tail = FALSE))
}

pvs<-data.table(treat7="$p$-value",
                rbind(sapply(c(
                  `Amount Due (June)`="total_due",
                  `Assessed Property Value`="assessed_mv",
                  `% with Unique Owner`="unq_own",
                  `% Overlap with Holdout`="flag_holdout_overlap",
                  `# Properties per Owner`="N"),
                  function(x)owners[(!holdout),lmfp(get(x)~treat7)])),
                `# Owners`=
                  owners[(!holdout),chisq.test(table(treat7))$p.value])

print.xtable(xtable(dcast(melt(rbind(
  owners[(!holdout),
         .(`Amount Due (June)`=mean(total_due),
           `Assessed Property Value`=mean(assessed_mv,na.rm=T),
           `% with Unique Owner`=to.pct(mean(unq_own)),
           `% Overlap with Holdout`=to.pct(mean(flag_holdout_overlap)),
           `# Properties per Owner`=mean(N),
           `# Owners`=.N),keyby=treat7],pvs),
  id.vars="treat7",variable.name="Variable"),Variable~treat7),
  caption="Balance on Observables",label="table:balance",
  digits=cbind(matrix(c(2,0,1,2,2,0),nrow=6,ncol=9),rep(2,6))),
  include.rownames=F,sanitize.colnames.function=identity)

#Analysis ####
## @knitr analysis_bs
## Bootstrap simulations ####
###Number of repetitions for all bootstrap
###  exercises
BB<-5000

###Point Estimate CIs ####
###For the standard confidence intervals,
###  we can condense the process by storing
###  the key changing parameters in this list;
###  also store a bunch of plotting parameters
###  since we'll use the result of this list for that
setkey(owners, rand_id)

outvar <- parse(text = paste0(
  ".(", paste(paste0("mean(ever_paid_", mos, ")"),
              collapse = ","), ")"))

outvar2 <- parse(text = paste0(
  ".(", paste(paste0("mean(",
                     c("ever_paid_", "total_paid_"),
                     rep(mos, each = 2), ")"),
              collapse = ","), ")"))

outn <- "ep" %+% c("j","s","d")

outn2 <- c("ep", "tp") %+% rep(c("j","s","d"), each = 2)

bootlist<-{
  #By owner, big vs. small
  list(o2=list(dt=owners[(!holdout)],tr="treat2",
               rI = owners[(!holdout), unique(rand_id)],
               ri = quote(.(sample(rI, rep = TRUE))),
               exprs=outvar,nms=outn,fn="2_own",
               tl="Big/Small",lv=owners[,levels(treat2)],
               nx=.75,sp=3,yl=c(2,10),dn=quote(NULL)),
       #By owner, main 7 treatments
       o7=list(dt=owners[(!holdout)],tr="treat7",
               rI = owners[(!holdout), unique(rand_id)],
               ri = quote(.(sample(rI, rep = TRUE))),
               exprs=outvar2, nms=outn2,
               fn="7_own",tl="Treatment",lv=trt.nms,nx=.75,
               sp=NULL,yl=NULL,dn=quote(NULL)))}

boot.cis<-{
  lapply(bootlist,function(z){
    cat(z$tl)
    with(z,list("dt"=setnames(
      #First, point estimates from raw data
      dt[ , eval(exprs), keyby = tr
          ][dcast(rbindlist(lapply(integer(BB), function(...)
            #calculate point estimate in re-sample
            dt[eval(ri), eval(exprs), keyby = tr])
          )[ , lapply(.SD, quantile, c(.025, .975)), by = tr],
          as.formula(paste0(tr, "~",  'c("low", "high")[rowid(', tr, ")]")),
          value.var = "V" %+% 1:length(nms)), on = tr],
      -1L, c(nms, rep(nms, each = 2)) %+% 
        c(rep("", length(nms)), rep(c("_high","_low"), length(nms)))),
      "fn"=fn,"tr"=tr,"tl"=tl,"rf"=lv[1L],
      "nx"=nx,"sp"=sp,"yl"=yl,"dn"=dn))})}

##Bar Plots ####
## @knitr ignore
###Ever Paid, three cross-sections, vs. Control
pdf2(wds["imga"] %+% "bar_plot_ever_paid_julsepdec_7_own.pdf")
with(boot.cis$o7,
     dt[order(treat7),
        {par(mfrow = c(1,3), oma = c(5.6, 5.1, 4.1, 2.1))
          Mo <- c(j = "One Month", s = "Three Months", d = "Six Months")
          yup <- nx.mlt(1.05*to.pct(max(epj_high,eps_high,epd_high)),5)
          for (mo in c("j", "s", "d")){
            par(mar=c(0,0,0,0))
            vals<-lapply(mget("ep"%+%mo%+%c("","_low","_high")),to.pct)
            ind<-which(treat7=="Control")
            x<-barplot(vals[[1]],names.arg=treat7,xlim=yl,
                       ylim=c(0, yup),las=2,yaxt="n",
                       col=get.col(treat7),main="",
                       space=sp, cex.names = 1.3)
            title(Mo[mo], line = -1)
            arrows(x,vals[[2]],x,vals[[3]],code=3,
                   angle=90,length=.07,lwd=2)
            abline(h=c(vals[[2]][ind],vals[[3]][ind]),lty=2)
            tile.axes(match(mo, names(Mo)), 1L, 3L, 
                      params = list(y = list(las = 2L, cex.axis = 1.3)),
                      use.x = FALSE)
            box()}
          title("Percent Ever Paid (by Owner)", outer = TRUE, cex = 1.3)
          mtext("Percent",side=2,outer=T,line=2.5)}])
dev.off2()

###Average Paid, three cross-sections, vs. Control
pdf2(wds["imga"] %+% "bar_plot_aver_paid_julsepdec_7_own.pdf")
with(boot.cis$o7,
     dt[order(treat7),
        {par(mfrow = c(1,3), oma = c(5.6, 5.1, 4.1, 2.1))
          Mo <- c(j = "One Month", s = "Three Months", d = "Six Months")
          yup <- nx.mlt(1.05*max(tpj_high,tps_high,tpd_high),10)
          for (mo in c("j", "s", "d")){
            par(mar=c(0,0,0,0))
            vals<-mget("tp" %+% mo %+% c("","_low","_high"))
            ind<-which(treat7=="Control")
            x<-barplot(vals[[1]],names.arg=treat7,xlim=yl,
                       ylim=c(0, yup),las=2,yaxt="n",
                       col=get.col(treat7),main="",
                       space=sp, cex.names = 1.3)
            title(Mo[mo], line = -1)
            arrows(x,vals[[2]],x,vals[[3]],code=3,
                   angle=90,length=.07,lwd=2)
            abline(h=c(vals[[2]][ind],vals[[3]][ind]),lty=2)
            tile.axes(match(mo, names(Mo)), 1L, 3L, 
                      params = list(y = list(las = 2L, cex.axis = 1.3)),
                      use.x = FALSE)
            box()}
          title("Average Paid (by Owner)", outer = TRUE)
          mtext("$",side=2,outer=T,line=2.5)}])
dev.off2()

###Ever Paid, three cross-sections vs. Small
pdf2(wds["imga"] %+% "bar_plot_ever_paid_julsepdec_2_own.pdf")
with(boot.cis$o2,
     dt[order(treat2),
        {par(mfrow = c(1,3), oma = c(5.6, 5.1, 4.1, 2.1))
          Mo <- c(j = "One Month", s = "Three Months", d = "Six Months")
          xup <- nx.mlt(1.05*to.pct(max(epj_high,eps_high,epd_high)),5)
          for (mo in c("j", "s", "d")){
            par(mar=c(0,0,0,0))
            vals<-lapply(mget("ep"%+%mo%+%c("","_low","_high")),to.pct)
            ind<-which(treat2=="Small")
            x<-barplot(vals[[1]],names.arg=treat2,xlim=c(0, 5),
                       ylim=c(0, xup),las=2,yaxt="n",
                       col=get.col(treat2),main="", space = .5,
                       cex.names = 1.3, width = 1.5)
            title(Mo[mo], line = -1)
            arrows(x,vals[[2]],x,vals[[3]],code=3,
                   angle=90,length=.07,lwd=2)
            abline(h=c(vals[[2]][ind],vals[[3]][ind]),lty=2)
            tile.axes(match(mo, names(Mo)), 1L, 3L, 
                      params = list(y = list(las = 2L, cex.axis = 1.3)),
                      use.x = FALSE)
            box()}
          title("Percent Ever Paid (by Owner)", outer = TRUE)
          mtext("Percent",side=2,outer=T,line=2.5)}])
dev.off2()

## Cumulative Partial Participation
## @knitr analysis_ch_ep
### get range of dates for day-by-day averages
dt.rng<-owners[(!holdout&unq_own),{
  rng<-range(earliest_pmt_dec,na.rm=T)
  seq(from=rng[1],to=rng[2],by="day")}]
#For pretty printing, get once/week subset
dt.rng2<-dt.rng[seq(1,length(dt.rng),length.out=7)]

date.dt <- 
  #not all treatments saw activity on each day,
  #  so we'll have to "fill-in-the-blanks" with this
  CJ(treat8 = c("Holdout","Control"), date = dt.rng,
     unique=TRUE, sorted = FALSE)
owners[(unq_own), hold_cont := treat8%in%c("Holdout","Control")]
cum_haz<-owners[(hold_cont),
                #total entrants by day, treatment
                sum(ever_paid_dec)+0., #convert to numeric
                keyby=.(treat8,earliest_pmt_dec)
                #running total of entrants; take care to eliminate NAs
                ][,.(ep=cumsum(V1[idx<-!is.na(earliest_pmt_dec)]),
                     date=earliest_pmt_dec[idx]),by=treat8
                  #merge to get the denominator
                  ][owners[(hold_cont),.N,treat8],
                    ep:=ep/i.N,on="treat8"
                    ][date.dt, on = c("treat8", "date"), roll = TRUE
                      #express relative to holdout
                      ][,.(ep=ep[idx<-treat8=="Control"]-ep[!idx]),by=date]

BB <- 5000 #number of bootstrap simulations
cis <- dcast(rbindlist(lapply(integer(BB), function(...){
  dt <- owners[(hold_cont)][sample(.N,rep=T)]
  dt[,sum(ever_paid_dec)+0.,keyby=.(treat8,earliest_pmt_dec)
     ][,.(ep=cumsum(V1[idx<-!is.na(earliest_pmt_dec)]),
          date=earliest_pmt_dec[idx]),by=treat8
       ][dt[,.N,treat8],ep:=ep/i.N,on="treat8"
         ][date.dt, on = c("treat8", "date"),roll = TRUE
           ][,.(ep=ep[idx<-treat8=="Control"]-ep[!idx]),
             by=date]}),idcol="bootID"
  #extract for 95% CI
  )[,quantile(ep, c(.025, .975), na.rm=T), by = date],
  date~c("low","high")[rowid(date)],value.var="V1")

## @knitr ignore
### plot
pdf2(wds["imga"] %+% "cum_haz_ever_paid_control_holdout_own.pdf")
cum_haz[
  cis,on=c("date")
  ][,{
    matplot(date, do.call("cbind",mget(c("low","ep","high"))),
            type="l",lty=c(2,1,2),col=get.col("Control"),
            xaxt="n",lwd=3, las = 2,
            ylab="Probability Ever Paid vs. Holdout")
    axis(side=1,at=dt.rng2,labels=format(dt.rng2,"%b %d"))
    abline(h = 0, col = "black", lwd = 2)
    abline(v = ds <- unclass(D("2015-06-12", "2015-06-22")),
           col = "black", lwd = 2, lty = 2)
    text(ds, .07, pos = 4, labels = c("1", "2"), col = "red")
    text(unclass(D("2015-10-21")), .07, col = "red", adj = c(0,0),
         labels = "1: Small Envelopes\n2: Big Envelopes", pos = 4)
    title("Cumulative Partial Participation"%+%
            "\nControl vs. Holdout (Unique Owners Only)")}]
dev.off2()

##Regression Tables ####
## @knitr analysis_reg
reg_vars <- c("ever_paid","paid_full","total_paid")
reg_all <- levels(interaction(reg_vars, mos,sep="_"))

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
           
samp_i <- 
  expression("Main Sample" = (!holdout),
             "Non-Commercial" = (residential&!holdout),
             "Unique Owner" = (unq_own&!holdout))

BB <- 5000
setkey(owners, rand_id)
setindex(owners, holdout)
system.time(
reg_info <- 
  rbindlist(lapply(samp_i, function(i_ev){
    message("\n\n", i_ev, ":")
    DT <- owners[eval(i_ev)]
    
    regs <- DT[ , eval(reg_j)]
    
    starts <- 
      sapply(reg_all[!grepl("total", reg_all)], 
             function(rr) 
               regs[[rr]][[1]]$coefficients,
             simplify = FALSE)
    
    RBs <- DT[ , unique(rand_id)]
    
    cl <- makeCluster(8L, outfile = "")
    clusterEvalQ(cl, library(data.table))
    clusterExport(cl, c("DT", "RBs", "coef_j","starts"),
                  envir = environment())
    boot_dist <- rbindlist(parLapply(cl, 1:BB, function(bb){
      if (bb %% 625 == 0) cat("Replication", bb, "\n")
      DT[.(sample(RBs, rep = TRUE)), eval(coef_j)]}))
    stopCluster(cl)
    
    rbind(regs, boot_dist[ , lapply(.SD, function(x)
      list(var(matrix(x, ncol = 7, byrow = TRUE))))]
      )[ , lapply(.SD, function(x)
        c(x, list(sqrt(diag(x[[2]])))))
        ][ , lapply(.SD, function(x) 
          c(x, list(coeftest(x[[1]], vcov = x[[2]])[ , 4])))]
  }), idcol = "sample")
)

###Difference in Mean Tests
rename_coef<-function(obj){
  nms<-names(obj$coefficients)
  idx<-!grepl("treat7",nms)
  #Add XXX to coefficients we'll exclude
  nms[idx]<-nms[idx]%+%"XXX"
  nms<-gsub("treat7","",nms)
  names(obj$coefficients)<-nms
  obj
}

proper <- c(ever_paid="Ever Paid",
            paid_full="Paid Full",
            total_paid="Total Paid")
invisible(sapply(
  transpose(expand.grid(reg_vars, names(samp_i))),
  function(meta)
    print(texreg(
      lapply(tp <- paste(rr <- meta[1],mos,sep="_"),function(mo)
        rename_coef(reg_info[sample==meta[2]][[mo]][[1]])),
      custom.model.names=
        c("One Month","Three Months","Six Months"),
      caption="Estimated Average Treatment Effects: " %+% 
        proper[rr] %+% ", " %+% meta[2],
      override.se=lapply(tp,function(mo)
        reg_info[sample==meta[2]][[mo]][[3]]),
      override.pval=lapply(tp,function(mo)
        reg_info[sample==meta[2]][[mo]][[4]]),
      caption.above=TRUE,label="dif_mean",stars=c(.01,.05,.1),
      include.rsquared=F,include.adjrs=F,include.rmse=F,
      #Exclude Intercept
      omit.coef="XXX$",float.pos="htbp"))))

##Financial Analysis ####
sapply(
  list(list(ii=expression((!holdout)),ky="treat7",nm=""),
       list(ii=expression((!holdout&rand_id>2)),ky="treat7",
            nm=" (Excluding 28 Most Indebted Owners)"),
       list(ii=expression((!holdout&rand_id>2&unq_own)),ky="treat7",
            nm=" (Excluding 28 Most Indebted Owners and Multiple Owners"),
       list(ii=expression((rand_id>2|is.na(rand_id))&unq_own),ky="treat8",
            nm=" (vs. Holdout, Excluding 28 Most Indebted Owners and" %+%
              "Multiple Owners"),
       list(ii=expression(((rand_id>2|is.na(rand_id))&unq_own&
                             treat8 %in% c("Holdout","Control","Lien"))),
            nm=" (vs. Holdout, Excluding 28 Most Indebted Owners and" %+%
              "Multiple Owners)",ky="treat8")),
  function(lst)
    lyx.xtable(xtable(
      x <-
        owners[eval(lst$ii),
               .(.N,tot_due=sum(total_due),
                 tpj=sum(total_paid_jul),
                 tps=sum(total_paid_sep),
                 tpd=sum(total_paid_dec)),keyby=c(lst$ky)
               ][,.("Treatment"=get(lst$ky),
                    "Total Owed"=dol.form(tot_due, dig=1, suff="m"),
                    "Jul."=dol.form(tpj, 1, "m"),
                    "Sep."=dol.form(tps, 1, "m"),
                    "Dec."=dol.form(tpd, 1, "m"),
                    "Jul."=to.pct(tpj/tot_due),
                    "Sep."=to.pct(tps/tot_due),
                    "Dec."=to.pct(tpd/tot_due),
                    "Jul."=
                      c(NA, dol.form(tpj[-1]/(Nnc <- N[-1])-tpj[1]/N[1])),
                    "Sep."=c(NA, dol.form(tps[-1]/Nnc-tps[1]/N[1])),
                    "Dec."=c(NA, dol.form(tpd[-1]/Nnc-tpd[1]/N[1])),
                    "Jul."=
                      c(NA, dol.form(tpj[-1]-tpj[1]/N[1]*Nnc, suff="k")),
                    "Sep."=
                      c(NA, dol.form(tps[-1]-tps[1]/N[1]*Nnc, suff="k")),
                    "Dec."=
                      c(NA, dol.form(tpd[-1]-tpd[1]/N[1]*Nnc, suff="k")))],
      caption="Summary of Effectiveness of Treatment" %+% lst$nm,
      label="table:fiscal", digits = 0,align=c("c|lr|lll|rrr|lll|lll|")),
      include.rownames=F, hline.after=c(0, nrow(x)), 
      NA.string="---", comment = FALSE,
      add.to.row = 
        list(pos=list(-1),
             command=
               paste("\\hline & & \\multicolumn{3}{c|}{Dollars Received} &", 
                     "\\multicolumn{3}{c|}{Pct. Debt Rec'd} &",
                     "\\multicolumn{3}{c|}{\\$ Net per Owner} &",
                     "\\multicolumn{3}{c|}{Total Surplus} \\\\"))))

###Excluding top two randomization blocks:
lyx.xtable(xtable(
  owners[(!holdout&rand_id>2),
         .(.N,tot_due=sum(total_due),
           tpj=sum(total_paid_jul),
           tps=sum(total_paid_sep),
           tpd=sum(total_paid_dec)),keyby=treat7
    ][,.("Treatment"=treat7,
         "Total Owed"=dol.form(tot_due, dig=1, suff="m"),
         "Jul."=dol.form(tpj, 1, "m"),
         "Sep."=dol.form(tps, 1, "m"),
         "Dec."=dol.form(tpd, 1, "m"),
         "Jul."=to.pct(tpj/tot_due),
         "Sep."=to.pct(tps/tot_due),
         "Dec."=to.pct(tpd/tot_due),
         "Jul."=c(NA, dol.form(tpj[-1]/(Nnc <- N[-1])-tpj[1]/N[1])),
         "Sep."=c(NA, dol.form(tps[-1]/Nnc-tps[1]/N[1])),
         "Dec."=c(NA, dol.form(tpd[-1]/Nnc-tpd[1]/N[1])),
         "Jul."=c(NA, dol.form(tpj[-1]-tpj[1]/N[1]*Nnc, suff="k")),
         "Sep."=c(NA, dol.form(tps[-1]-tps[1]/N[1]*Nnc, suff="k")),
         "Dec."=c(NA, dol.form(tpd[-1]-tpd[1]/N[1]*Nnc, suff="k")))],
  caption="Summary of Effectiveness of Treatment " %+% 
    "(Excluding 28 Most Indebted Owners)",
  label="table:fiscal", digits = 0,align=c("c|lr|lll|rrr|lll|lll|")),
  include.rownames=F, hline.after=c(0, 7), NA.string="---",
  add.to.row = 
    list(pos=list(-1),
         command=paste("\\hline & & \\multicolumn{3}{c|}{Dollars Received} &", 
                       "\\multicolumn{3}{c|}{Pct. Debt Rec'd} &",
                       "\\multicolumn{3}{c|}{\\$ Net per Owner} &",
                       "\\multicolumn{3}{c|}{Total Surplus} \\\\")))

###Excluding top two randomization blocks:
lyx.xtable(xtable(
  owners[((rand_id>2|is.na(rand_id))&unq_own&
            treat8 %in% c("Holdout","Control","Lien")),
         .(.N,tot_due=sum(total_due),
           tpj=sum(total_paid_jul),
           tps=sum(total_paid_sep),
           tpd=sum(total_paid_dec)),keyby=treat8
    ][,.("Treatment"=treat8,
         "Total Owed"=dol.form(tot_due, dig=1, suff="m"),
         "Jul."=dol.form(tpj, 1, "m"),
         "Sep."=dol.form(tps, 1, "m"),
         "Dec."=dol.form(tpd, 1, "m"),
         "Jul."=to.pct(tpj/tot_due),
         "Sep."=to.pct(tps/tot_due),
         "Dec."=to.pct(tpd/tot_due),
         "Jul."=c(NA, dol.form(tpj[-1]/(Nnc <- N[-1])-tpj[1]/N[1])),
         "Sep."=c(NA, dol.form(tps[-1]/Nnc-tps[1]/N[1])),
         "Dec."=c(NA, dol.form(tpd[-1]/Nnc-tpd[1]/N[1])),
         "Jul."=c(NA, dol.form(tpj[-1]-tpj[1]/N[1]*Nnc, suff="k")),
         "Sep."=c(NA, dol.form(tps[-1]-tps[1]/N[1]*Nnc, suff="k")),
         "Dec."=c(NA, dol.form(tpd[-1]-tpd[1]/N[1]*Nnc, suff="k")))],
  caption="Summary of Effectiveness of Treatment vs. Holdout" %+% 
    "(Excluding 28 Most Indebted Owners and Multiple Owners)",
  label="table:fiscal", digits = 0,align=c("c|lr|lll|rrr|lll|lll|")),
  include.rownames=F, hline.after=c(0, 3), NA.string="---",
  add.to.row = 
    list(pos=list(-1),
         command=paste("\\hline & & \\multicolumn{3}{c|}{Dollars Received} &", 
                       "\\multicolumn{3}{c|}{Pct. Debt Rec'd} &",
                       "\\multicolumn{3}{c|}{\\$ Net per Owner} &",
                       "\\multicolumn{3}{c|}{Total Surplus} \\\\")))

sapply(list(list(dt=owners[(!holdout)],fl="7_own",tl="",
                 tr="treat7",rf="Control"),
            list(dt=owners[(flag_top01&!holdout)],
                 fl="7_own_x01",tl="\nTop 1% of Debtors Removed",
                 tr="treat7",rf="Control"),
            list(dt=owners[(flag_top01&!holdout)],
                 fl="7_own_x05",tl="\nTop 5% of Debtors Removed",
                 tr="treat7",rf="Control"),
            list(dt=owners,fl="8_own",
                 tl="\nvs. Holdout",tr="treat8",
                 rf="Holdout"),
            list(dt=owners[treat8%in%c("Holdout","Control")],
                 fl="8_own_control",tl="\nControl vs. Holdout",
                 tr="treat8",rf="Holdout"),
            list(dt=owners[(!holdout)],fl="2_own",
                 tl="\nBig vs. Small",tr="treat2",rf="Small"),
            list(dt=owners[treat7%in%c("Control","Lien")],
                 fl="7_own_lien",tr="treat7",
                 tl="\nLien vs. Control",rf="Control"),
            list(dt=owners[treat7%in%c("Control","Lien","Peer")],
                 fl="7_own_lien.peer",tr="treat7",
                 tl="\nLien & Peer vs. Control",rf="Control")),
       function(x){
         with(x,{setkeyv(dt,tr)
           lessref<-function(x)x-x[,rf]
           dist<-lessref(sapply(
             dt[,paste0(unique(get(tr)))],
             function(y){tpx<-dt[.(y),total_paid_jul]
             replicate(BB,mean(sample(tpx,rep=T)))})) %*%
             dt[,.N,by=tr]$N
           pdf2(wds["imga"]%+%"histogram_surplus_jul_"%+%fl%+%".pdf")
           hist(dist,xaxt="n",xlab="$ Received above "%+%rf,
                main="Distribution of (Bootstrapped) Total Surplus"%+%
                  "\nAcross All Treatments (July)"%+%tl,col="cyan",breaks=50)
           axis(side=1,
                at=seq(round(par("usr")[1],-6),
                       round(par("usr")[2],-6),by=500000))
           qs<-quantile(dist,c(.025,.975))
           abline(v=qs,lwd=3)
           abline(v=0,lwd=3,col="red")
           text(x=qs,y=par("usr")[4]*.8,
                labels=c("2.5 %-ile","97.5 %-ile"))
           text(x=0,y=par("usr")[4]*.6,
                labels=round(100*ecdf(dist)(0),1)%+%"%-ile")
           dev.off2()})})

##Time Series Analysis ####
sapply(list(list(dt=owners[(!holdout)],vr="ever_paid",
                 mn="jul",Mn=" (July)",Vr="Ever Paid",
                 tl="Partial Participation"),
            list(dt=owners[(!holdout)],vr="ever_paid",
                 mn="sep",Mn=" (Sep.)",Vr="Ever Paid",
                 tl="Partial Participation"),
            list(dt=owners[(!holdout)],vr="ever_paid",
                 mn="dec",Mn=" (Dec.)",Vr="Ever Paid",
                 tl="Partial Participation"),
            list(dt=owners[(!holdout)],vr="paid_full",
                 mn="jul",Mn=" (July)",Vr="Paid Full",
                 tl="Full Participation"),
            list(dt=owners[(!holdout)],vr="paid_full",
                 mn="sep",Mn=" (Sep.)",Vr="Paid Full",
                 tl="Full Participation"),
            list(dt=owners[(!holdout)],vr="paid_full",
                 mn="dec",Mn=" (Dec.)",Vr="Paid Full",
                 tl="Full Participation")),
       function(x){
         with(x,{
           #Get range of data
           dt.rng<-dt[,{rng<-range(get("earliest_pmt_"%+%mn),na.rm=T)
           seq(from=rng[1],to=rng[2],by="day")}]
           #For pretty printing, get once/week subset
           dt.rng2<-dt.rng[seq(1,length(dt.rng),by=7)]
           pdf2(paste0(wds["imga"],"cum_haz_",vr,"_",mn,"_7_own.pdf"))
           matplot(dt.rng,t(sapply(
             dt.rng,function(tt){
               dt[,{var<-get(vr%+%"_"%+%mn)
               dts<-get("earliest_pmt_"%+%mn)
               #Total ever paid (at least once) by tt
               #  divided by total in group (.N)
               to.pct(sum(var[dts<=tt],na.rm=T)/.N)},
               keyby=treat7][,{x<-V1[treat7!="Control"]
               x-V1[treat7=="Control"]}]})),
             type="l",lty=1,lwd=3,col=get.col(trt.nms[-1]),
             main=paste0("Cumulative ",tl," by Treatment",
                         Mn,"\nRelative to Control"),
             xaxt="n",xlab="",ylab="Percent "%+%Vr%+%" vs. Control",las=1)
           axis(side=1,at=dt.rng2,labels=format(dt.rng2,"%B %d"),
                las=1,cex.axis=.75)
           legend("topleft",legend=trt.nms[-1],
                  col=get.col(trt.nms[-1]),
                  lwd=3,y.intersp=.5,bty="n")
           dev.off2()})})

dt.rng<-
  owners[(!holdout),{rng<-range(pmt_agr_start,na.rm=T)
  seq(from=rng[1],to=rng[2],by="day")}]
dt.rng2<-dt.rng[seq(1,length(dt.rng),by=7)]
pdf2(wds["imga"]%+%"cum_haz_pmt_agr_7_own.pdf")
matplot(dt.rng,t(sapply(
  dt.rng,function(tt){
    owners[(!holdout),
               to.pct(sum(pmt_agr_start<=tt,na.rm=T)/.N),
               keyby=treat7]$V1})),
  type="l",lty=1,lwd=3,col=get.col(trt.nms),
  main="Cumulative Entrance to Payment Agreements\nby Treatment",
  xaxt="n",xlab="",ylab="Percent in P.A.",las=1)
axis(side=1,at=dt.rng2,labels=format(dt.rng2,"%B %d"),
     las=1,cex.axis=.75)
legend("topleft",legend=trt.nms,col=get.col(trt.nms),
       lwd=3,y.intersp=.5,bty="n")
dev.off2()

pdf2(wds["imga"]%+%"bar_plot_ever_paid_followup_7_own.pdf")
owners[(!holdout),.(0,mean(ever_paid_jul),mean(ever_paid_sep)),
       keyby=treat7
       ][,{xmx<-nx.mlt(max(V3),.1)
       plot(0,xaxt="n",yaxt="n",bty="n",pch="",ylab="",
            xlab="",xlim=c(0,xmx),ylim=c(0,8),
            main="3-Month Follow-Up Ever Paid Rates")
       yl<-1:.N; yu<-yl+.8
       rect(V1,yl,V2,yu,col=get.col(treat7))
       rect(V2,yl,V3,yu,col=get.col(treat7),density=50)
       axis(side=1,at=seq(0,xmx,by=.1),pos=.8)
       axis(side=2,at=.5*(yl+yu),tick=F,las=1,
            labels=treat7,pos=0)}]
dev.off2()

pdf2(wds["imga"]%+%"bar_plot_paid_full_followup_7_own.pdf")
owners[(!holdout),.(0,mean(paid_full_jul),mean(paid_full_sep)),
       keyby=treat7
            ][,{xmx<-nx.mlt(max(V3),.1)
            plot(0,xaxt="n",yaxt="n",bty="n",pch="",ylab="",
                 xlab="",xlim=c(0,xmx),ylim=c(0,8),
                 main="3-Month Follow-Up Paid Full Rates")
            yl<-1:.N; yu<-yl+.8
            rect(V1,yl,V2,yu,col=get.col(treat7))
            rect(V2,yl,V3,yu,col=get.col(treat7),density=85)
            axis(side=1,at=seq(0,xmx,by=.1),pos=.8)
            axis(side=2,at=.5*(yl+yu),tick=F,las=1,
                 labels=treat7,pos=0)}]
dev.off2()
