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
#funchir is Michael Chirico's package of convenience functions
#  install with devtools::install_github("MichaelChirico/funchir")
library(funchir)
library(data.table)
library(texreg)
library(sandwich)
library(xtable)
library(Zelig)
library(foreign)
library(lmtest)
library(glmmML)
write.packages(code_wd%+%"logs/real_estate_payment_"%+%
                 "analysis_session.txt")

trt.nms <- c("Control","Threat","Service","Civic")
trt.col <- c(Control="black",Threat="red",
             Service="blue", Civic="green")

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

all_samples<-list(analysis_data_main,
                  analysis_data_ncom,
                  analysis_data_sown)
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
xtable(sapply(list(
  "All Properties"=full_delinquent,
  "Restricted Properties"=rest_delinquent,
  "Analysis Sample"=analysis_data_main[(end)]),
  function(y)y[,.(`Amount Due`=mean(calc_total_due),
                  `Assessed Property Value`=
                    mean(total_assessment,na.rm=T),
                  `Tax Due`=
                    mean(net_tax_value_after_homestead,na.rm=T),
                  `Years of Debt`=mean(years_count),
                  `% Residential`=to.pct(mean(residential)),
                  `% with Philadelphia Mailing Address`=
                    to.pct(mean(eval(phila_addr))),
                  `% Owner-Occupied`=
                    to.pct(mean(homestead>0,na.rm=T)),
                  `Number Observations`=.N)],USE.NAMES=T),
  caption=c("Descriptive Statistics"),
  label="table:descriptives",digits=0); rm(phila_addr,resid_grp)

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

{print.xtable(xtable(rbindlist(c(lapply(
  #Create one data.table for each test,
  #  then stack them all and print as LaTeX
  test_vars,function(x){
    #The p-value for the current test -- find the CDF
    #  of the bootstrapped test statistics, then find
    #  the proportion of observations exceeding the
    #  in-sample statistic (i.e., 1-P[test > in-sample])
    pv<-analysis_data_sown[,chisq.test(get(x),treatment)$p.value]
    #Instead of doing table, count and reshape wide
    dcast(
      analysis_data_main[,.N,keyby=c(x,"treatment")
                         ][,.(treatment,N/sum(N)),
                           by=.("Variable"=get(x))
                           ],Variable~treatment,value.var="V2"
      #only want the p-value to show up in the first row
    )[1L,"$p$-value":=pv]}),
  #add a data.table for the distribution of properties
  list(data.table(
    "Variable"="Distribution of Properties",
    rbind(analysis_data_main[,table2(treatment,prop=T,dig=Inf)]),
    "$p$-value"=
      analysis_data_sown[,chisq.test(table(treatment),
                                     p=p_exp)$p.value])),
  #add one more table to show the expected distribution
  list(data.table("Variable"="Expected Distribution",
                  rbind(p_exp),"$p$-value"=NA)))),
  caption="Tests of Sample Balance on Observables",
  align=c("|c|l|c|c|c|c|c|"),label="table:balance",
  digits=2),include.rownames=F,hline.after=c(-1,25:27),
  #don't remove the $ from p-value column so it prints as math
  sanitize.colnames.function=identity,
  #use sanitize2 for now to deal properly with left brackets;
  #  check for updates to xtable to see if this is fixed natively
  sanitize.text.function=sanitize2,
  add.to.row=list(pos=list(0L,4L,8L,12L,15L,19L),
                  command=c("\\hline \nTaxes Due Quartiles & & & & & \\\\ \n",
                            "\\hline \nMarket Value Quartiles & & & & & \\\\ \n",
                            "\\hline \nLand Area Quartiles & & & & & \\\\ \n",
                            "\\hline \n\\# Rooms & & & & & \\\\ \n",
                            "\\hline \nYears of Debt & & & & & \\\\ \n",
                            "\\hline \nCategory & & & & & \\\\ \n")))}

##TABLE 4 ####
##SUMMARY OF EFFECTIVENESS OF EACH SAMPLE
print.xtable(xtable(rbindlist(lapply(
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
  caption=c("Summary of Effectiveness of Treatment"),label="table:summary",
  align=paste0("|c|p{2.2cm}|p{1.4cm}|p{1.8cm}|p{1.2cm}|",
               "p{1.2cm}|p{1.4cm}|p{1.4cm}|p{2.2cm}|p{1.8cm}|"),
  digits=c(rep(0,7),1,0,0)),include.rownames=F,hline.after=c(-1,0,4,8,12),
  floating.environment="sidewaystable")

# Regressions ####
##TABLE 5 ####
##DIFFERENCE IN MEAN TESTS
texreg(lapply(all_samples,
              function(y)y[,{x<-lm(cum_treated_pmts~treatment)
              names(x$coefficients)<-
                gsub("[()]|treatment","",names(x$coefficients))
              coeftest(x, vcov=vcovHC(x))}]),
       custom.model.names=
         c("Main Sample","Non-Commercial Sample",
           "Unique Owner Sample"),
       caption="Estimated Average Treatment Effects: Revenues",
       caption.above=T,label="dif_mean",stars=c(.01,.05,.1))

##TABLE 6 ####
##LOGIT - EVER PAID (I)
###Perform blocked bootstrap to
###  get parameter estimates
BB<-10000
qs<-c(.005,.025,.05,.95,.975,.995)
coef_bs<-lapply(all_samples,function(x){
  setkey(x,cluster_id)
  coef_dist<-replicate(
    BB,x[.(sample(unique(cluster_id),replace=T)),
         glm(ever_paid~treatment,
             family=binomial)$coefficients])
  apply(coef_dist,1,
        function(x)c("SE"=sd(x),quantile(x,qs)))
})

##REGRESSION TABLES
###Running regressions

###Model I: LPM of Ever-Paid
texreg2(regs[paste0(rep("reg1",3),"_",
                    unlist(lapply(list(analysis_data_main,analysis_data_ncom,
                                       analysis_data_sown),
                                  function(x){comment(x)[1]})))],
        custom.model.names=c("Full Sample","Residential","Residential, Unique Owner"),
        custom.coef.names=c("Intercept","Threat","Service","Civic"),
        caption="Model I: Linear Probability Models -- Ever Paid",label="table:modelIa",
        float.pos="htbp",include.aic=F,include.bic=F,include.loglik=F,
        include.rsquared=F,include.adjrs=F,include.fstatistic=F,stars=c(.01,.05,.1))


###Model I: Logistic of Ever-Paid with Controls, Quartile Interactions
texreg2(regs[paste(rep("reg1c",3),
                   unlist(lapply(list(analysis_data_main,analysis_data_ncom,
                                      analysis_data_sown),
                                 function(x){comment(x)[1]})),sep="_")],
        custom.model.names=c("Full Sample","Residential","Residential, Unique Owner"),
        custom.coef.names=c("Intercept","Threat","Service","Civic",
                            paste(c("Medium","High","Very High"),"Debt"),
                            paste0("control",
                                   c("Land Area","Owes <= 5 Years",
                                     paste("Dist.",2:10),"Hotels-Apts",
                                     "Store w. Dwelling","Commercial",
                                     "Industrial","Vacant","Sealed-Compromised",
                                     "Homestead","Prop. Val. (\\$100k)")),
                            paste0(c("Threat","Service","Civic"),"*",
                                   rep(c("Moderate","High","Very High"),each=3))),
        omit.coef="Intercept|control",reorder.coef=c(4,5,6,1,7,10,13,2,8,11,14,3,9,12,15),
        caption="Model I: Logistic Regressions -- Ever Paid",include.deviance=F,
        label="table:modelIb",float.pos="htbp",include.bic=F,include.aic=F,stars=c(.01,.05,.1))

####***BAND-AID TO WRAP LONG NOTE UNTIL TEXREG UPDATED:***
custom.note<-
  gsub("%stars","$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$",
       paste("%stars. Other controls omitted for brevity: land area,",
             "length of debt indicator, City Council District dummies,",
             "dummies for categories (hotels/apartments, stores with",
             "dwellings, commerical, industrial, and vacant status vs.",
             "residential status), sealed/compromised indicator,",
             "owner-occupancy indicator, and property value."))
cat(paste(paste0("\\multicolumn{",4,"}{l}{\\scriptsize{",
                 strwrap(custom.note,width=60),"}}"),
          collapse=" \\\\ \n"),perl=T); rm(custom.note)

###Model I: Predicted Probability of Repayment at Control Means by Quartile
#### via p_hat = exp(beta*x_bar)/(1+exp(beta*x_bar))
#### log odds would be exp(beta*x_bar)
#####first, get the sample means for the variables we don't vary in the table
#####  (use sample *median* for land area and market value since they're
#####   so skewed)
sample_means_control12<-
  c(1,analysis_data_main[,median(land_area/1e3)],
    unlist(analysis_data_main[end==1,lapply(list(
      years_count<=5,council==2,council==3,council==4,
      council==5,council==6,council==7,council==8,council==9,
      council==10,category=="Hotels&Apts",category=="Store w/ Dwelling",
      category=="Commercial",category=="Industrial",category=="Vacant",
      exterior=="Sealed/Compromised",owner_occ),mean)]),
    analysis_data_main[end==1,median(market_value/1e5)])
x_beta_control1<-sum(regs[["reg1c_main"]]$result$coefficients[c(1,8:26)]*sample_means_control12)

main_coef<-regs[["reg1c_main"]]$result$coefficients[c(2:7,27:35)]
#the idea: add the following matrices to get the latent x*betas for each cell
#   0 0 0 0      0 1 2 3     0 0 0 0  : T-Threat/S-Service/C-Civic/1-Q1/2-Q2/3-Q3
#   T T T T      0 1 2 3     0 a d g  : a-Threat*Q1/b-Service*Q1/c-Civic*Q1
#   S S S S   +  0 1 2 3  +  0 b e h  : d-Threat*Q2/e-Service*Q2/f-Civic*Q2
#   C C C C      0 1 2 3     0 c f i  : g-Threat*Q3/h-Service*Q3/i-Civic*Q3
#  (but more parsimonious to specify in list form, casting ex-post as matrix)
print.xtable2(
  xtable(matrix(sapply(rep(c(0,main_coef[1:3]),times=4)+
                         rep(c(0,main_coef[4:6]),each=4)+
                         c(rep(0,4),rbind(rep(0,3),matrix(main_coef[7:15],ncol=3)))
                       +x_beta_control1,
                       function(x){round(100*exp(x)/(1+exp(x)),1)}),ncol=4,
                dimnames=list(c("Control","Threat","Service","Civic"),
                              paste(c("Low","Medium","High","Very High"),"Debt"))),
         caption="Model I: Probability Ever Paid by Treatment, Debt Level at Sample Center",
         label="table:modelI_marg",align="lcccc"),table.placement="htbp")

###Model II: Logistic of Paid in Full
####Logistic Coefficients Table: Plain model
texreg2(regs[paste0(rep("reg2",3),"_",
                    unlist(lapply(list(analysis_data_main,analysis_data_ncom,
                                       analysis_data_sown),
                                  function(x){comment(x)[1]})))],
        custom.model.names=c("Full Sample","Residential","Residential, Unique Owner"),
        custom.coef.names=c("Intercept","Threat","Service","Civic"),
        caption="Model II: Linear Probability Models -- Paid in Full",label="table:modelIIa",
        float.pos="htbp",include.aic=F,include.bic=F,include.loglik=F,
        include.rsquared=F,include.adjrs=F,include.fstatistic=F,stars=c(.01,.05,.1))

####Logistic Coefficients Table: Quartile Interactions
texreg2(regs[paste(rep("reg2c",3),
                  unlist(lapply(list(analysis_data_main,analysis_data_ncom,
                                     analysis_data_sown),
                                function(x){comment(x)[1]})),sep="_")],
       custom.model.names=c("Full Sample","Non-Commercial","Sole Owner"),
       custom.coef.names=c("Intercept","Service","Civic","Threat",
                           paste("Balance",c("Q2","Q3","Q4")),
                           paste0("control",
                                  c("Land Area","Owes <= 5 Years",
                                    paste("Dist.",2:10),"Hotels-Apts",
                                    "Store w. Dwelling","Commercial",
                                    "Industrial","Vacant","Sealed-Compromised",
                                    "Homestead","Prop. Val. (\\$100k)")),
                           paste0(c("Service","Civic","Threat"),"*",
                                  rep(paste("Balance",c("Q2","Q3","Q4")),each=3))),
       omit.coef="Intercept|control",reorder.coef=c(4,5,6,1,7,10,13,2,8,11,14,3,9,12,15),
       custom.note=c("%stars. Control coefficients omitted for brevity; see Appendix."),
       caption="Model II: Logistic Regressions -- Paid in Full",include.deviance=F,
       label=c("table:modelIIb"),float.pos="htbp",include.bic=F,include.aic=F,stars=c(.01,.05,.1))

####***BAND-AID TO WRAP LONG NOTE UNTIL TEXREG UPDATED:***
custom.note<-
  gsub("%stars","$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$",
       paste("%stars. Other controls omitted for brevity: land area,",
             "length of debt indicator, City Council District dummies,",
             "dummies for categories (hotels/apartments, stores with",
             "dwellings, commerical, industrial, and vacant status vs.",
             "residential status), sealed/compromised indicator,",
             "owner-occupancy indicator, and property value."))
cat(paste(paste0("\\multicolumn{",4,"}{l}{\\scriptsize{",
                 strwrap(custom.note,width=60),"}}"),
          collapse=" \\\\ \n"),perl=T); rm(custom.note)

####Marginal Predictions at Control Means
x_beta_control2<-sum(regs[["reg2c_main"]]$result$coefficients[c(1,8:26)]*sample_means_control12)

tab_mat<-matrix(sapply(rep(c(0,main_coef[1:3]),times=4)+
                         rep(c(0,main_coef[4:6]),each=4)+
                         c(rep(0,4),rbind(rep(0,3),matrix(main_coef[7:15],ncol=3)))
                       +x_beta_control2,
                       function(x){round(100*exp(x)/(1+exp(x)),1)}),ncol=4,
                dimnames=list(c("Control","Service","Civic","Threat"),
                              paste(c("Low","Medium","High","Very High"),"Debt")))

main_coef<-regs[["reg2c_main"]]$result$coefficients[c(2:7,27:35)]
print.xtable2(
  xtable(matrix(sapply(rep(c(0,main_coef[1:3]),times=4)+
                         rep(c(0,main_coef[4:6]),each=4)+
                         c(rep(0,4),rbind(rep(0,3),matrix(main_coef[7:15],ncol=3)))
                       +x_beta_control2,
                       function(x){round(100*exp(x)/(1+exp(x)),1)}),ncol=4,
                dimnames=list(c("Control","Service","Civic","Threat"),
                              paste(c("Low","Medium","High","Very High"),"Debt"))),
         caption="Model II: Probability Paid in Full by Treatment, Debt Level at Sample Center",
         label="table:modelII_marg",align="lcccc"),table.placement="htbp")
rm(main_coef,sample_means_control12,x_beta_control2,x_beta_control1)