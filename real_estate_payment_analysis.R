#Data Analysis
#Philadelphia Real Estate Tax Evasion
#Michael Chirico
#April 3, 2015

#Setup: Packages, Random Seed, Working Directory ####
rm(list=ls(all=T))
gc()
##First and last 4 digits of my Cosi rewards card
set.seed(60008645)
setwd("~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/")
data_wd<-"/media/data_drive/real_estate/"
code_wd<-"./analysis_code/"
log_fl<-"./papers_presentations/round_one_analysis_output.txt"
BB<-3000 #number of bootstrap/resample repetitions throughout
#funchir is Michael Chirico's package of convenience functions
#  install with devtools::install_github("MichaelChirico/funchir")
library(funchir)
library(data.table)
library(texreg)
library(xtable)
write.packages(code_wd%+%"logs/real_estate_payment_"%+%
                 "analysis_session.txt")

trt.nms <- c("Control","Threat","Public Service","Civic Duty")
trt.col <- c(Control="black",Threat="red",
             "Public Service"="blue", "Civic Duty"="green")

rename_fcn<-function(nms){
  indx<-!grepl("Intercept|treatment|"%+%
                 "balance_quartile_pretty",nms)
  nms[indx]<-nms[indx]%+%"XXX"
  nms<-gsub("(Intercept)","Intercept",nms,fixed=TRUE)
  nms<-gsub("treatment","",nms)
  nms<-gsub("balance_quartile_pretty","",nms)
  nms<-gsub(":","*",nms,fixed=TRUE)
  nms<-gsub("&","\\&",nms,fixed=TRUE)
  nms<-gsub("(.*)\\*(.*)","\\2*\\1",nms)
  nms
}

#Set up Analysis Data Sets ####
analysis_data_main<-
  fread("analysis_file_end_only_act.csv",
        colClasses=c(date_of_first_payment="character"))
#CLUSTERS DEFINED AT THE OWNER LEVEL
#OWNER DEFINED AS ANYONE WITH IDENTICAL NAME & MAILING ADDRESS
clust<-c("legal_name","mail_address")
analysis_data_main[,cluster_id:=.GRP,by=clust]

###tidying up data classifications
factors<-c("treatment","category","exterior","council")
analysis_data_main[,(factors):=lapply(.SD,as.factor),.SDcols=factors]
analysis_data_main[,treatment:=factor(treatment,levels=trt.nms)]
#since read as strings, order is out of whack
analysis_data_main[,council:=factor(council,levels=paste0(1:10))]
cat_ord <- c("Residential","Hotels&Apts","Store w/ Dwelling",
             "Commercial","Industrial","Vacant")
analysis_data_main[,category:=factor(category,levels=cat_ord)]
analysis_data_main[,posting:=as.Date(posting)]
rm(factors,cat_ord)

##create some variables which can be defined on the main data set
## and passed through without change to the subsamples
analysis_data_main[,treatment_count:=uniqueN(opa_no),by=treatment]
analysis_data_main[,treatment_days :=uniqueN(cycle), by=treatment]

analysis_data_main[,years_cut:=cut(years_count,include.lowest=T,
                                   breaks=c(0,1,2,5,40),
                                   labels=c(1:2,"3-5","6+")%+%" Years")]
analysis_data_main[,rooms_cut:=cut(rooms,breaks=c(0,5,6,20),
                                   include.lowest=T,labels=c("0-5","6","7+"))]

##DEFINE DATA SETS FOR THE SUBSAMPLES
###MAIN SAMPLE
#### (reset factor levels once leave-out sample is eliminated)
analysis_data_main[,smpl:="Full"]
###NON-COMMERCIAL PROPERTIES
analysis_data_ncom<-
  copy(analysis_data_main[category!="Commercial"][,category:=factor(category)])
analysis_data_ncom[,smpl:="Non-Commercial"]
###SINGLE-OWNER PROPERTIES
analysis_data_sown<-
  copy(analysis_data_main[,if (uniqueN(opa_no)==1L) .SD,by=clust])
analysis_data_sown[,smpl:="Unique Owner"]

###Tags & data descriptions for file naming for each data set
comment(analysis_data_main)<-c("main","Full Sample","analysis_data_main")
comment(analysis_data_ncom)<-c("non_comm","Non-Commercial Properties","analysis_data_ncom")
comment(analysis_data_sown)<-c("single_owner","Unique-Owner Properties","analysis_data_sown")

all_samples<-list("main"=analysis_data_main,
                  "non_comm"=analysis_data_ncom,
                  "single_owner"=analysis_data_sown)

for (dt in all_samples){
  ##Define the sample-specific variables:
  ## total_due_at_mailing_grp: total due by properties
  ##                           in each group at mailing
  ## sample quartiles for balance due, market value, and land area
  
  dummy<-copy(dt)
  
  dummy[,total_due_at_mailing_grp:=
          sum(current_balance[posting_rel==0L]),by=treatment]
  
  qs <- c(.25,.5,.75)
  dummy[,balance_quartile:={
    x<-"$"%+%round(quantile(total_due_at_mailing,qs),-2)
    lbs<-c("< "%+%x[1],paste0("[",x[1],", ",x[2],")"),
           paste0("[",x[2],", ",x[3],")"),"> "%+%x[3])
    create_quantiles(total_due_at_mailing,4,labels=lbs)}]
  
  dummy[,balance_quartile_pretty:=
          create_quantiles(total_due_at_mailing,4,
                           labels="Balance "%+%
                             c("LOW","MOD","HIGH","VHIGH"))]
  
  dummy[,mv_quartile:={
    x<-"$"%+%round(quantile(market_value/1000,qs))%+%"k"
    lbs<-c("< "%+%x[1],paste0("[",x[1],", ",x[2],")"),
           paste0("[",x[2],", ",x[3],")"),"> "%+%x[3])
    create_quantiles(market_value,4,labels=lbs)}]
  
  dummy[,area_quartile:={
    x<-round(quantile(land_area,qs),-2)
    sf <- " sq. ft"
    lbs<-c("< "%+%x[1]%+%sf,paste0("[",x[1],", ",x[2],")",sf),
           paste0("[",x[2],", ",x[3],")",sf),"> "%+%x[3]%+%sf)
    create_quantiles(land_area,4,labels=lbs)}]
  
  assign(comment(dt)[3],dummy)
}; rm(dt,dummy)

###Unfortunately, since list made a copy,
###  all_samples was not updated, either.
###  So have to make a new copy.
all_samples<-list("main"=analysis_data_main,
                  "non_comm"=analysis_data_ncom,
                  "single_owner"=analysis_data_sown)

#Results ####
##TABLE 1
##  Produced by Bob

##TABLE 2 ####
##MAIN DESCRIPTIVES TABLE
##  Using full and restricted samples for comparison
full_delinquent<-fread(data_wd%+%"dor_data_15_oct_encrypted.csv")
resid_grp <- c("apartmentLarge","apartmentSmall","condo",
               "house","house ","miscResidential","")
full_delinquent[,residential:=bldg_group %in% resid_grp]
#Now applying our sample restrictions
rest_delinquent<-
  full_delinquent[payment_agreement=="N"
                  &abate_exempt_code %in% c("","     ","\\    ")
                  &case_status==""&sheriff_sale=="N"
                  &bankruptcy=="N"&sequestration=="N"
                  &returned_mail_flag=="NO"]
#To compare properties assigned to outside law firms
#  to those in our sample (all other restrictions being equal)
rest_law_firm<-
  full_delinquent[payment_agreement=="N"
                  &abate_exempt_code %in% c("","     ","\\    ")
                  &case_status!=""&sheriff_sale=="N"
                  &bankruptcy=="N"&sequestration=="N"
                  &returned_mail_flag=="NO"]

#We count an address as in Philadelphia when:
#  1) Mailing address is blank -- this means the
#     mailing address is the property address
#  2) Mailing city contains "PH" but NOT
#     "X","M", or "R" -- this eliminates
#     Alpharetta (GA), Gulph Mills (PA),
#     Memphis (TN), Phelan (CA), Phoenix (AZ),
#     Phoenixville (PA), Randolph (MA, NJ),
#     and Zephyrhills (FL). This in particular
#     captures most (but perhaps not all)
#     of the multitudinous typos for Philadelphia.
phila_addr<-quote(mail_address %in% c("","                    ")|
                    (grepl("PH",mail_city)&!grepl("[XMR]",mail_city)))
capture.output(print.xtable(xtable(sapply(list(
  "All Delinquent"=full_delinquent,
  "Law Firm"=rest_law_firm,
  "Restricted"=rest_delinquent,
  "Analysis"=analysis_data_main[(end)]),
  function(y)y[,.(`Amount Due`=dol.form(mean(calc_total_due)),
                  `Assessed Property Value`=
                    dol.form(mean(total_assessment,na.rm=T)),
                  `Tax Due`=
                    dol.form(mean(net_tax_value_after_homestead,
                                  na.rm=T)),
                  `Years of Debt`=round(mean(years_count)),
                  `% Residential`=to.pct(mean(residential),dig=0),
                  `% with Philadelphia Mailing Address`=
                    to.pct(mean(eval(phila_addr)),dig=0),
                  `% Owner-Occupied`=
                    to.pct(mean(homestead>0,na.rm=T),dig=0),
                  `Number Observations`=
                    prettyNum(.N,big.mark=","))],USE.NAMES=T),
  caption=c("Descriptive Statistics"),align=c("|r|r|r|r|r|"),
  label="table:descriptives"),caption.placement="top"),file=log_fl)
  
rm(phila_addr,resid_grp)

##TABLE 3 ####
##BALANCE ON OBSERVABLES
###Set of variables which we'll test
###  to probe balance on observables
test_vars<-c("balance_quartile","mv_quartile",
             "area_quartile","rooms_cut",
             "years_cut","category")

###Expected distribution of properties across
###  cycles given unbalanced number of treatment days
p_exp<-unique(analysis_data_main,by="cycle"
              )[,table2(treatment,prop=T,dig=Inf)]

###Get p-values for tests via permutation tests
####Subset here to minimize memory usage
uniq_treat<-unique(analysis_data_main[
  !analysis_data_main[,uniqueN(treatment),
                      by=cluster_id][V1>1],
  .(cluster_id,treatment),on="cluster_id"])

####Permutation distribution
chisq_dist<-
  replicate(BB,analysis_data_main[
    #Shuffle treatment, merge back by cluster ID
    uniq_treat[,trt2:=sample(treatment)],
    #now perform all the tests
    c(sapply(test_vars,function(x){
      unname(chisq.test(get(x),i.trt2)$statistic)}),
      #properties by day treated separately
      "properties"=
        unname(chisq.test(table(i.trt2),p=p_exp)$statistic)),
    on="cluster_id"])

####Baseline values
baseline<-
  analysis_data_main[,c(sapply(test_vars,function(x){
    unname(chisq.test(get(x),treatment)$statistic)}),
    "properties"=unname(chisq.test(table(treatment),p=p_exp)$statistic))]

####P value is pct of null dist'n above baseline
chisq_ps<-rowMeans(chisq_dist > baseline)

{capture.output(print.xtable(xtable(rbindlist(c(lapply(
  #Create one data.table for each test,
  #  then stack them all and print as LaTeX
  test_vars,function(x){
    #Instead of doing table, count and reshape wide
    dcast(
      analysis_data_main[,.N,keyby=c(x,"treatment")
                         ][,.(treatment,N/sum(N)),
                           by=.("Variable"=get(x))
                           ],Variable~treatment,value.var="V2"
      #only want the p-value to show up in the first row
    )[1L,"$p$-value":=chisq_ps[x]]}),
  #add a data.table for the distribution of properties
  list(data.table(
    "Variable"="Distribution of Properties",
    rbind(analysis_data_main[,table2(treatment,prop=T,dig=Inf)]),
    "$p$-value"=chisq_ps["properties"])),
  #add one more table to show the expected distribution
  list(data.table("Variable"="Expected Distribution",
                  rbind(p_exp),"$p$-value"=NA)))),
  caption="Tests of Sample Balance on Observables",
  align=c("|c|l|c|c|c|c|c|"),label="table:balance",
  digits=2),include.rownames=F,hline.after=c(-1,25:27),
  #don't remove the $ from p-value column so it prints as math
  sanitize.colnames.function=identity,caption.placement="top",
  #use sanitize2 for now to deal properly with left brackets;
  #  check for updates to xtable to see if this is fixed natively
  sanitize.text.function=sanitize2,
  add.to.row=list(pos=list(0L,4L,8L,12L,15L,19L),
                  command=c("\\hline \nTaxes Due Quartiles & & & & & \\\\ \n",
                            "\\hline \nMarket Value Quartiles & & & & & \\\\ \n",
                            "\\hline \nLand Area Quartiles & & & & & \\\\ \n",
                            "\\hline \n\\# Rooms & & & & & \\\\ \n",
                            "\\hline \nYears of Debt & & & & & \\\\ \n",
                            "\\hline \nCategory & & & & & \\\\ \n"))),
  file=log_fl,append=TRUE)}
rm(full_delinquent,rest_delinquent,rest_law_firm)

##TABLE 4 ####
##SUMMARY OF EFFECTIVENESS OF EACH SAMPLE
capture.output(print.xtable(xtable(rbindlist(lapply(
  all_samples,function(x){
    setkey(x,treatment
           )[,.(.N,smpl=smpl[1],
                tot_due=sum(total_due_at_mailing),
                ev_pd=mean(ever_paid),pd_fl=mean(paid_full),
                tot_pmt=sum(cum_treated_pmts)),
             by=treatment
             ][,.("Sample"=c(smpl[1],paste0(
               "(N=",prettyNum(sum(N),big.mark=","),")"),"",""),
                  "Treatment (Properties)"=paste0(
                    treatment," (n=",prettyNum(N,big.mark=","),")"),
                  "Total Debt Owed"=dol.form(tot_due),
                  "Percent Ever Paid"=to.pct(ev_pd,dig=0),
                  "Percent Paid in Full"=to.pct(pd_fl,dig=0),
                  "Dollars Received"=dol.form(tot_pmt),
                  "Percent Debt Received"=to.pct(tot_pmt/tot_due,dig=1),
                  "Dollars above Control Per Property"=
                    dol.form(tot_pmt/N-tot_pmt[1]/N[1]),
                  "Total Surplus over All Properties"=
                    dol.form(tot_pmt-tot_pmt[1]/N[1]*N))]})),
  caption=c("Estimated Average Treatment Effects: Revenues"),label="table:summary",
  align=paste0("|c|p{2.2cm}|p{1.4cm}|p{1.8cm}|p{1.2cm}|",
               "p{1.2cm}|p{1.4cm}|p{1.4cm}|p{2.2cm}|p{1.8cm}|"),
  digits=c(rep(0,7),1,0,0)),include.rownames=F,hline.after=c(-1,0,4,8,12),
  floating.environment="sidewaystable",caption.placement="top"),
  file=log_fl,append=TRUE)

# Regressions ####
##TABLE 5 ####
##DIFFERENCE IN MEAN TESTS
capture.output(texreg(lapply(all_samples,
              function(y)y[,{x<-lm(cum_treated_pmts~treatment)
              names(x$coefficients)<-
                rename_fcn(names(x$coefficients))
              coeftest(x, vcov=vcovHC(x))}]),
       custom.model.names=
         c("Main Sample","Non-Commercial Sample",
           "Unique Owner Sample"),
       caption="Estimated Average Treatment Effects: Revenues",
       caption.above=T,label="dif_mean",stars=c(.01,.05,.1)),
       file=log_fl,append=TRUE)

##TABLE 6 ####
##LOGIT - EVER PAID (I)
epq<-quote(ever_paid)
pfq<-quote(paid_full)
trq<-quote(treatment)
ctq<-quote(I(land_area/1e3)+I(years_count<=5)+
             council+category+owner_occ+
             I(exterior=="Sealed/Compromised")+
             I(market_value/1e5)+
             balance_quartile_pretty*treatment)

reg_specs<-
  list(epI=list("reg_lhs"=epq,"reg_rhs"=trq),
       epII=list("reg_lhs"=epq,"reg_rhs"=ctq),
       pfI=list("reg_lhs"=pfq,"reg_rhs"=trq),
       pfII=list("reg_lhs"=pfq,"reg_rhs"=ctq))
lapply(all_samples,setkeyv,"cluster_id")

reg_out<-
  lapply(reg_specs,function(reg){
    print(as.character(reg$reg_lhs))
    with(reg,lapply(all_samples,function(dt){
      regf<-dt[,glm(eval(reg_lhs)~eval(reg_rhs),family=binomial)]
      names(regf$coefficients)<-
        rename_fcn(names(regf$coefficients))
      cl <- makeCluster(detectCores())
      clusterEvalQ(cl,library("data.table"))
      clusterExport(cl,"dt",envir=environment())
      boot_dist<-parSapply(cl,1:BB,function(i,...){
        dt[.(sample(unique(cluster_id),rep=T)),
           glm(eval(reg_lhs)~eval(reg_rhs),
               family=binomial)$coefficients]})
      stopCluster(cl)
      bootses<-apply(boot_dist,1,sd)
      bootps<-coeftest(regf,vcov=unname(var(t(boot_dist))))[,"Pr(>|z|)"]
      list(mod=regf,ses=bootses,pvals=bootps)}))})

capture.output(texreg(lapply(
  reg_out[["epI"]],function(x)x$mod),stars=c(.01,.05,.1),
  custom.model.names=c("Full Sample","Non-Commercial","Sole Owner"),
  caption="Logistic Regressions for Ever Paid: Compliance",
  caption.above=TRUE,label="table:ep_log_I",float.pos="htbp",
  include.aic=FALSE,include.bic=FALSE,include.deviance=FALSE,
  override.se=lapply(reg_out[["epI"]],function(x)x$ses),
  override.pval=lapply(reg_out[["epI"]],function(x)x$pvals)),
  file=log_fl,append=TRUE)
  
##TABLE 7 ####
##LOGIT - EVER PAID (II)
capture.output(texreg(lapply(
  reg_out[["epII"]],function(x)x$mod),stars=c(.01,.05,.1),
  custom.model.names=c("Full Sample","Non-Commercial","Sole Owner"),
  caption="Logistic Regressions for Ever Paid "%+%
    "with Interactions: Compliance",omit.coef="XXX$|Intercept",
  caption.above=TRUE,label="table:ep_log_II",float.pos="htbp",
  include.aic=FALSE,include.bic=FALSE,include.deviance=FALSE,
  reorder.coef=c(1:4,7:9,5,10:12,6,13:15),
  override.se=lapply(reg_out[["epII"]],function(x)x$ses),
  override.pval=lapply(reg_out[["epII"]],function(x)x$pvals)),
  file=log_fl,append=TRUE)

##TABLE 8 ####
##MARGINAL PREDICTIONS - EVER PAID
###Predicted Probability of Repayment at Control Means by Quartile
###  via p_hat = exp(beta*x_bar)/(1+exp(beta*x_bar))
###  log odds would be exp(beta*x_bar)
####first, get the sample means for the variables we don't vary in the table
####  (use sample *median* for land area and market value since they're
####   so skewed)
sample_means_control<-
  c(1,analysis_data_main[,median(land_area/1e3)],
    unlist(analysis_data_main[,lapply(list(
      years_count<=5,council==2,council==3,council==4,
      council==5,council==6,council==7,council==8,council==9,
      council==10,category=="Hotels&Apts",category=="Store w/ Dwelling",
      category=="Commercial",category=="Industrial",category=="Vacant",
      owner_occ,exterior=="Sealed/Compromised"),mean)]),
    analysis_data_main[,median(market_value/1e5)])

tcoefn<-analysis_data_main[,levels(treatment)[-1]]
bcoefn<-analysis_data_main[,levels(balance_quartile_pretty)[-1]]
xcoefn<-tcoefn%+%"*"%+%rep(bcoefn,each=3)
contn<-setdiff(names(reg_out$epII$main$mod$coefficients),
               c(tcoefn,bcoefn,xcoefn))

x_beta_control<-
  sum(reg_out$epII$main$mod$coefficients[contn]*sample_means_control)

tcoef<-reg_out$epII$main$mod$coefficients[tcoefn]
bcoef<-reg_out$epII$main$mod$coefficients[bcoefn]
xcoef<-reg_out$epII$main$mod$coefficients[xcoefn]

#the idea: add the following matrices to get the latent x*betas for each cell
#   0 0 0 0      0 1 2 3     0 0 0 0  : T-Threat/S-Service/C-Civic/1-Q1/2-Q2/3-Q3
#   T T T T      0 1 2 3     0 a d g  : a-Threat*Q1/b-Service*Q1/c-Civic*Q1
#   S S S S   +  0 1 2 3  +  0 b e h  : d-Threat*Q2/e-Service*Q2/f-Civic*Q2
#   C C C C      0 1 2 3     0 c f i  : g-Threat*Q3/h-Service*Q3/i-Civic*Q3
#  (but more parsimonious to specify in list form, casting ex-post as matrix)
capture.output(print.xtable(xtable(matrix(
  sapply(rep(c(0,tcoef),times=4)+
           rep(c(0,bcoef),each=4)+
           c(embed.mat(matrix(xcoef,ncol=3),4,4,2,2))+
           x_beta_control,function(x){to.pct(1/(1+exp(-x)))}),
  ncol=4,dimnames=list(trt.nms,c("LOW","MOD","HIGH","VHIGH"))),
  caption="Marginal Predictions - Ever Paid",digits=2,
  label="table:modelI_marg",align="|l|c|c|c|c|"),
  table.placement="htbp",caption.placement="top"),
  file=log_fl,append=TRUE)

##TABLE 9 ####
##LOGIT - PAID FULL (I)
capture.output(texreg(lapply(
  reg_out[["pfI"]],function(x)x$mod),stars=c(.01,.05,.1),
  custom.model.names=c("Full Sample","Non-Commercial","Sole Owner"),
  caption="Logistic Regressions for Paid in Full: Compliance",
  caption.above=TRUE,label="table:pf_log_I",float.pos="htbp",
  include.aic=FALSE,include.bic=FALSE,include.deviance=FALSE,
  override.se=lapply(reg_out[["pfI"]],function(x)x$ses),
  override.pval=lapply(reg_out[["pfI"]],function(x)x$pvals)),
  file=log_fl,append=TRUE)

##TABLE 10 ####
##LOGIT - PAID FULL (II)
capture.output(texreg(lapply(
  reg_out[["pfII"]],function(x)x$mod),stars=c(.01,.05,.1),
  custom.model.names=c("Full Sample","Non-Commercial","Sole Owner"),
  caption="Logistic Regressions for Paid in Full "%+%
    "with Interactions: Compliance",omit.coef="XXX$|Intercept",
  caption.above=TRUE,label="table:pf_log_II",float.pos="htbp",
  include.aic=FALSE,include.bic=FALSE,include.deviance=FALSE,
  reorder.coef=c(1:4,7:9,5,10:12,6,13:15),
  override.se=lapply(reg_out[["pfII"]],function(x)x$ses),
  override.pval=lapply(reg_out[["pfII"]],function(x)x$pvals)),
  file=log_fl,append=TRUE)

##TABLE 11 ####
##MARGINAL PREDICTIONS - PAID FULL
x_beta_control<-
  sum(reg_out$pfII$main$mod$coefficients[contn]*sample_means_control)

tcoef<-reg_out$pfII$main$mod$coefficients[tcoefn]
bcoef<-reg_out$pfII$main$mod$coefficients[bcoefn]
xcoef<-reg_out$pfII$main$mod$coefficients[xcoefn]

capture.output(print.xtable(xtable(matrix(sapply(
    rep(c(0,tcoef),times=4)+
      rep(c(0,bcoef),each=4)+
      c(embed.mat(matrix(xcoef,ncol=3),4,4,2,2))+
      x_beta_control,function(x){to.pct(1/(1+exp(-x)))}),
    ncol=4,dimnames=list(trt.nms,c("LOW","MOD","HIGH","VHIGH"))),
    caption="Marginal Predictions - Paid in Full",digits=2,
    label="table:modelII_marg",align="|l|c|c|c|c|"),
    table.placement="htbp",caption.placement="top"),
    file=log_fl,append=TRUE)
