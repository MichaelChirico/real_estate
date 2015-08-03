#Data Analysis
#Philadelphia Real Estate Tax Evasion
#Michael Chirico
#April 3, 2015

#Setup: Packages, Working Directory, Etc.####
rm(list=ls(all=T))
gc()
setwd("~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/")
library(data.table)
library(texreg)
library(sandwich)
library(xtable)
library(Zelig)
library(readstata13)

#Convenient Functions ####
table2<-function(...,dig=NULL,prop=F,ord=F,pct=F){
  dots<-list(...)
  args<-names(dots) %in% names(formals(prop.table))
  tab<-if (prop) do.call(
    'prop.table',c(list(
      do.call('table',if (length(args)) dots[!args] else dots)),
      dots[args])) else do.call('table',list(...))
  if (ord) tab<-tab[order(tab)]
  if (pct) tab<-100*tab
  if (is.null(dig)) tab else round(tab,digits=dig)
}

print.xtable2<-function(...){
  #For pretty copy-pasting into LyX
  cat(capture.output(do.call('print.xtable',list(...))),sep="\n\n")
}

texreg2<-function(...){
  #For pretty copy-pasting into Lyx
  cat(capture.output(do.call('texreg',list(...))),sep="\n\n")
}

pdf2<-function(...){
  graphics.off()
  dev.new()
  do.call('pdf',list(...))
  dev.set(which=dev.list()["RStudioGD"])
}

dev.off2<-function(){
  dev.copy(which=dev.list()["pdf"])
  invisible(dev.off(which=dev.list()["pdf"]))
}

create_quantiles<-function(x,num,right=F,include.lowest=T){
  cut(x,breaks=quantile(x,probs=seq(0,1,by=1/num)),labels=1:num,right=right,include.lowest=include.lowest)
}

dol_form<-function(x,dig=0){paste0("$",prettyNum(round(x,digits=dig),big.mark=","))}

to.pct<-function(x,dig=0)round(100*x,digits=dig)

get_treats<-function(x){if(comment(x)[1]=="act_leave_out")
  c("Leave-Out","Control","Threat","Service","Civic")
  else c("Control","Threat","Service","Civic")}

get_treats_col<-function(x){if(comment(x)[1]=="act_leave_out")
  c("orange","black","red","blue","green")
  else c("black","red","blue","green")}

#Set up Analysis Data Sets ####
analysis_data_main<-fread("analysis_file.csv",colClasses=c(date_of_first_payment="character"))
#CLUSTERS DEFINED AT THE OWNER LEVEL
#OWNER DEFINED AS ANYONE WITH IDENTICAL NAME & MAILING ADDRESS
setkey(setkey(analysis_data_main,legal_name,mail_address
              )[unique(analysis_data_main,by=c("legal_name","mail_address")
                       )[,I:=.I],cluster_id:=i.I],opa_no,posting_rel)

###tidying up data classifications
factors<-c("treatment","category","exterior","council")
analysis_data_main[,(factors):=lapply(.SD,as.factor),.SDcols=factors]
analysis_data_main[,treatment:=
                     factor(treatment,levels(treatment)[c(3,2,5,4,1)])]
#since read as strings, order is out of whack
analysis_data_main[,council:=factor(council,levels=levels(council)[c(1,3:10,2)])]
analysis_data_main[,category:=factor(category,levels=levels(category)[c(4,2,5,1,3,6)])]
analysis_data_main[,posting:=as.Date(posting)]
rm(factors)

##create some variables which can be defined on the main data set
## and passed through without change to the subsamples
analysis_data_main[,treatment_count:=uniqueN(opa_no),by=treatment]
analysis_data_main[,treatment_days :=uniqueN(cycle), by=treatment]

analysis_data_main[,years_cut:=cut(years_count,breaks=c(0,1,2,5,40),
                                   include.lowest=T,labels=c(1:2,"3-5",">5"))]
analysis_data_main[,rooms_cut:=cut(rooms,breaks=c(0,5,6,20),
                                   include.lowest=T,labels=c("0-5","6",">6"))]

##DEFINE DATA SETS FOR THE SUBSAMPLES
###LEAVE-OUT SAMPLE (2 DAYS PRIOR TO TREATMENT)
analysis_data_lout<-analysis_data_main
analysis_data_lout[,smpl:="Leave-Out"]
###MAIN SAMPLE
#### (reset factor levels once leave-out sample is eliminated)
analysis_data_main<-copy(analysis_data_main)[cycle>=33,][,treatment:=factor(treatment)]
analysis_data_main[,smpl:="Full"]
###RESIDENTIAL PROPERTIES
#### (also eliminating 3 properties with category=Commercial or Vacant
####  due to inconsistency in definition vis-a-vis bldg_group)
#### (reset factor levels once nonresidential properties are eliminated)
analysis_data_resd<-analysis_data_main[residential==T&
                                         !category %in% c("Commercial","Vacant"),
                                       ][,category:=factor(category)]
analysis_data_resd[,smpl:="Residential"]
###SINGLE RESIDENTIAL OWNER PROPERTIES--NO MATCH ON LEGAL_NAME/MAILING_ADDRESS
analysis_data_sres<-
  analysis_data_main[residential==T&!category %in% c("Commercial","Vacant"),
                     if (uniqueN(opa_no)==1) .SD,
                     by=.(legal_name,mail_address)][,category:=factor(category)]
analysis_data_sres[,smpl:="Residential, Unique Owner"]

###Tags & data descriptions for file naming for each data set
comment(analysis_data_main)<-c("main","Full Sample","analysis_data_main")
comment(analysis_data_lout)<-c("leave_out","Including Leave-Out Sample","analysis_data_lout")
comment(analysis_data_resd)<-c("residential","Residential Properties","analysis_data_resd")
comment(analysis_data_sres)<-
  c("residential_single_owner","Unique-Owner Residential Properties","analysis_data_sres")

for (dt in list(analysis_data_main,analysis_data_lout,analysis_data_resd,analysis_data_sres)){
  ##Define the sample-specific variables:
  ## total_due_at_mailing_grp: total due by properties
  ##                           in each group at mailing
  ## sample quartiles for balance due, market value, and land area
  
  dummy<-copy(dt)
  
  dummy[,total_due_at_mailing_grp:=sum(current_balance[posting_rel==0]),by=treatment]
  
  dummy[,balance_quartile:=create_quantiles(total_due_at_mailing,4)]
  dummy[,mv_quartile:=create_quantiles(market_value,4)]
  dummy[,area_quartile:=create_quantiles(land_area,4)]
  
  assign(comment(dt)[3],dummy)
}; rm(dt,dummy)

##CITY-LEVEL TAX COMPLIANCE DATA FOR COMPARIONS TABLE
city_data<-setkey(setDT(read.dta13("/media/data_drive/real_estate/Tax Levies and Collections.dta")
                        )[,City:=gsub("\\s+$","",City)],City
                  )[,city2:=ifelse(City=="DC","Washington, DC",City)]

###bootstrap confidence interval
####Seed basis: current time & date
set.seed(6222042)
R<-200
means<-matrix(nrow=city_data[,uniqueN(Year)],ncol=R,dimnames=list(city_data[,unique(Year)],1:R))
for (rr in 1:R){
  #resample at the city level to capture within-city autocorrelation
  means[,rr]<-city_data[.(sample(unique(City),uniqueN(City),replace=T))
                        ][PctCollected>0,mean(100*PctCollected),by=Year]$V1
}
setkey(city_data,Year)[setnames(
  data.table(cbind(city_data[,unique(Year)],
                   t(sapply(1:nrow(means),function(x)
                     #looking for interval ~+/- 1 SD
                     quantile(means[x,],c(1-pnorm(1),pnorm(1))))))),
  c("Year","mean_lq","mean_uq")),`:=`(mean_uq=i.mean_uq,
                                      mean_lq=i.mean_lq)]

###now plot
city_data[.(2005:2013)][PctCollected>0,.(mean(100*PctCollected),mean_lq[1],mean_uq[1]),
                        by=Year][,matplot(Year,cbind(V1,V2,V3),type="l",lty=c(1,2,2),
                                                 col=c("black","red","red"),lwd=c(3,2,2),
                                                 main="Average Percent Collected by Year",
                                                 ylab="Percent Collected",ylim=c(90,100))]

###something else done by Inman's .do file
# city_data[,lapply(.SD,function(x)c(mean(100*x,na.rm=T),sd(100*x,na.rm=T),length(x))),
#           by=Year,.SDcols=c("SubsequentPctCollected",
#                             "SubsequentAnnualAvgPctCollect",
#                             "DelinquentAnnualAveragePctCol")]

print.xtable2(xtable(rbind(city_data[PctCollected>0,.(city2="Large City Average",
                                 pct_com=paste0(to.pct(city_data[PctCollected>0,
                                                                 PctCollected[.N],
                                                                 by=City][,mean(V1)],dig=1),
                                                "; ",to.pct(mean(PctCollected),dig=1)),
                                 del_5y =to.pct(mean(DelinquentAnnualAveragePctCol[Year>=2010],
                                                     na.rm=T),dig=1))],
      city_data[PctCollected>0,.(pct_com=paste0(to.pct(PctCollected[.N],dig=1),
                                                "; ",to.pct(mean(PctCollected),dig=1)),
                                 del_5y =to.pct(mean(DelinquentAnnualAveragePctCol[Year>=2010],
                                                     na.rm=T),dig=1)),
                by=city2])),include.rownames=F)

#Tables ####
##MAIN DESCRIPTIVES TABLE
##  Using full and restricted samples for comparison
full_delinquent<-fread("/media/data_drive/real_estate/dor_data_15_oct_encrypted.csv")
full_delinquent[,owner_occ:=homestead>0]
full_delinquent[,residential:=bldg_group %in% c("apartmentLarge","apartmentSmall","condo",
                                                "house","house ","miscResidential","")]
rest_delinquent<-
  full_delinquent[payment_agreement=="N"
                  &abate_exempt_code %in% c("","     ","\\    ")
                  &case_status==""&sheriff_sale=="N"
                  &bankruptcy=="N"&sequestration=="N"
                  &returned_mail_flag=="NO"]

print.xtable(xtable(matrix(t(rbindlist(lapply(list(
  full_delinquent,rest_delinquent,analysis_data_main[end==1,]),
  function(y)y[,lapply(list(calc_total_due,total_assessment,
                            net_tax_value_after_homestead,
                            years_count,100*residential,
                            100*(mail_address %in% c("","                    ")|
                                   (grepl("PH",mail_city)&!grepl("[XMR]",mail_city))),
                            100*owner_occ,.N),function(x)round(mean(x,na.rm=T)))]))),
  dimnames=list(c("Amount Due","Assessed Property Value","Value of Tax","Length of Debt",
                  "% Residential","% w/ Phila. Mailing Address",
                  "% Owner-Occupied","Number Observations"),
                c("Full Sample","Restricted Sample","Analysis Sample")),ncol=3),
  caption=c("Summary Averages for Philadelphia Tax Non-Compliants"),
  label="table:descriptives",digits=0))


##MAIN SAMPLE SUMMARY TABLE
setkey(analysis_data_main,treatment)
print.xtable2(xtable(matrix(rbind(cbind(
  analysis_data_main[,table2(years_cut,prop=T)],
  analysis_data_main[,table2(years_cut,treatment,
                             margin=2,prop=T)]),
  cbind(analysis_data_main[,table2(category,prop=T)],
        analysis_data_main[,table2(category,treatment,
                                   margin=2,prop=T)]),
  c(analysis_data_main[,mean(exterior=="Sealed/Compromised")],
    analysis_data_main[,mean(exterior=="Sealed/Compromised"),
                       by=treatment]$V1),
  c(analysis_data_main[,mean(homestead>0)],
    analysis_data_main[,mean(homestead>0),
                       by=treatment]$V1)),ncol=5,
  dimnames=list(c(paste("Years:",c("1","2",
                                   "[3,5]","(5,40]")),
                  paste("Cat:",c("Commercial","Hotels/Apts",
                                 "Industrial","Residential",
                                 "Store+Resid","Vacant")),
                  "% Sealed/Compromised","% Owner-Occupied"),
                c("Full","Control","Threat","Service","Civic"))),
  digits=2))

##EFFECTIVENESS SUMMARY TABLE
###Summary of Effectiveness by Treatment & Sample
print.xtable2(xtable(rbindlist(lapply(list(
  analysis_data_main,analysis_data_resd,analysis_data_sres),
  function(x){setkey(x,treatment
  )[end==1,.(.N,smpl=smpl[1],
             tot_due=sum(total_due_at_mailing),
             ev_pd=mean(ever_paid),pd_fl=mean(paid_full),
             tot_pmt=sum(cum_treated_pmts)),
    by=treatment
    ][,.("Sample"=c(smpl[1],paste0("(N=",prettyNum(sum(N),big.mark=","),")"),"",""),
         "Treatment (Obs.)"=paste0(treatment," (n=",prettyNum(N,big.mark=","),")"),
         "Total Debt Owed"=dol_form(tot_due),
         "Percent Ever Paid"=to.pct(ev_pd),
         "Percent Paid in Full"=to.pct(pd_fl),
         "Dollars Received"=dol_form(tot_pmt),
         "Percent Debt Received"=to.pct(tot_pmt/tot_due,dig=1),
         "Dollars above Control Per Property"=
           dol_form(tot_pmt/N-tot_pmt[1]/N[1]),
         "Total Surplus over All Properties"=
           dol_form(tot_pmt-tot_pmt[1]/N[1]*N))]})),
  caption=c("Summary of Effectiveness of Treatment"),label="table:summary",
  align=paste0("|c|p{2.2cm}|p{1.4cm}|p{1.8cm}|p{1.2cm}|",
               "p{1.2cm}|p{1.4cm}|p{1.4cm}|p{2.2cm}|p{1.8cm}|"),
  digits=c(rep(0,7),1,0,0)),include.rownames=F,hline.after=c(-1,0,4,8,12),
  floating.environment="sidewaystable")

###Summary of Effectiveness by Treatment & Sample, vs. Leave-Out Sample
print.xtable2(xtable(
  setkey(analysis_data_lout,treatment)[
    end==1,.(.N,smpl=smpl[1],
             tot_due=sum(total_due_at_mailing),
             ev_pd=mean(ever_paid),pd_fl=mean(paid_full),
             tot_pmt=sum(cum_treated_pmts)),
    by=treatment
    ][,.("Sample"=c(smpl[1],paste0("(N=",prettyNum(sum(N),big.mark=","),")"),"","",""),
         "Treatment (Obs.)"=paste0(treatment," (n=",prettyNum(N,big.mark=","),")"),
         "Total Debt Owed"=dol_form(tot_due),
         "Percent Ever Paid"=to.pct(ev_pd),
         "Percent Paid in Full"=to.pct(pd_fl),
         "Dollars Received"=dol_form(tot_pmt),
         "Percent Debt Received"=to.pct(tot_pmt/tot_due),
         "Dollars above Control Per Property"=
           dol_form(tot_pmt/N-tot_pmt[1]/N[1]),
         "Total Surplus over All Properties"=
           dol_form(tot_pmt-tot_pmt[1]/N[1]*N))],
caption=c("Summary of Effectiveness of Treatment vs. Leave-Out Sample"),
label="table:summary_leave_out",
align=paste0("|c|p{2.2cm}|p{1.4cm}|p{1.8cm}|p{1.2cm}|",
             "p{1.2cm}|p{1.4cm}|p{1.4cm}|p{2.2cm}|p{1.8cm}|"),
digits=c(rep(0,7),1,0,0)),include.rownames=F,hline.after=c(-1,0,5),
floating.environment="sidewaystable")

# ##REGRESSION TABLES
# ###Running regressions
# regs<-setNames(vector("list",20),
#                paste0("reg",rep(1:3,c(8,8,4)),rep(c("","c","","c",""),each=4),"_",
#                       rep(c("main","residential","residential_single_owner","leave_out"),5)))
# for (data in list(analysis_data_main,analysis_data_lout,analysis_data_resd,analysis_data_sres)){
#   data_end<-data[end==1,]
#   comm<-comment(data)[1]
#   rf<-if (comm=="leave_out") "Leave-Out" else "Control"
#   
#   controls<-(~I(land_area/1e3)+I(years_count<=5)+council+category+
#                I(exterior=="Sealed/Compromised")+owner_occ+I(market_value/1e5))
#   
#   #1(Ever Paid) Differs by Treatment?
#   ##First, plain LPM
#   regs[[paste0("reg1_",comm)]]<-
#     zelig(ever_paid~relevel(treatment,ref=rf),
#           data=data_end,model="ls",cite=F)
#   ##Now, controlled logistic
#   regs[[paste0("reg1c_",comm)]]<-
#     zelig(update(controls,ever_paid~relevel(treatment,ref=rf)*balance_quartile+.),
#           data=data_end,model="logit",cite=F)
#   
#   #1(Bill Paid in Full) Differs by Treatment?
#   ##First, plain LPM
#   regs[[paste0("reg2_",comm)]]<-
#     zelig(paid_full~relevel(treatment,ref=rf),
#           data=data_end,model="ls",cite=F)
#   ##Now, controlled logistic
#   regs[[paste0("reg2c_",comm)]]<-
#     zelig(update(controls,paid_full~relevel(treatment,ref=rf)*balance_quartile+.),
#           data=data_end,model="logit",cite=F)
#   
#   #Tobit of payment LEVELS
#   #  to get expected return per letter
#   regs[[paste0("reg3_",comm)]]<-
#     zelig(cum_treated_pmts~relevel(treatment,ref=rf)*balance_quartile+
#             I(land_area/1e3)+I(years_count<=5)+council+category+
#             I(exterior=="Sealed/Compromised")+owner_occ+I(market_value/1e5),
#           data=data_end,model="tobit",cite=F)
# }; rm(data,data_end,controls,comm,rf)
# 
# ###Model I: LPM of Ever-Paid
# texreg2(regs[paste0(rep("reg1",3),"_",
#                     unlist(lapply(list(analysis_data_main,analysis_data_resd,
#                                        analysis_data_sres),
#                                   function(x){comment(x)[1]})))],
#         custom.model.names=c("Full Sample","Residential","Residential, Unique Owner"),
#         custom.coef.names=c("Intercept","Threat","Service","Civic"),
#         caption="Model I: Linear Probability Models -- Ever Paid",label="table:modelIa",
#         float.pos="htbp",include.aic=F,include.bic=F,include.loglik=F,
#         include.rsquared=F,include.adjrs=F,include.fstatistic=F,stars=c(.01,.05,.1))
# 
# 
# ###Model I: Logistic of Ever-Paid with Controls, Quartile Interactions
# texreg2(regs[paste(rep("reg1c",3),
#                    unlist(lapply(list(analysis_data_main,analysis_data_resd,
#                                       analysis_data_sres),
#                                  function(x){comment(x)[1]})),sep="_")],
#         custom.model.names=c("Full Sample","Residential","Residential, Unique Owner"),
#         custom.coef.names=c("Intercept","Threat","Service","Civic",
#                             paste(c("Medium","High","Very High"),"Debt"),
#                             paste0("control",
#                                    c("Land Area","Owes <= 5 Years",
#                                      paste("Dist.",2:10),"Hotels-Apts",
#                                      "Store w. Dwelling","Commercial",
#                                      "Industrial","Vacant","Sealed-Compromised",
#                                      "Homestead","Prop. Val. (\\$100k)")),
#                             paste0(c("Threat","Service","Civic"),"*",
#                                    rep(c("Moderate","High","Very High"),each=3))),
#         omit.coef="Intercept|control",reorder.coef=c(4,5,6,1,7,10,13,2,8,11,14,3,9,12,15),
#         caption="Model I: Logistic Regressions -- Ever Paid",include.deviance=F,
#         label="table:modelIb",float.pos="htbp",include.bic=F,include.aic=F,stars=c(.01,.05,.1))
# 
# ####***BAND-AID TO WRAP LONG NOTE UNTIL TEXREG UPDATED:***
# custom.note<-
#   gsub("%stars","$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$",
#        paste("%stars. Other controls omitted for brevity: land area,",
#              "length of debt indicator, City Council District dummies,",
#              "dummies for categories (hotels/apartments, stores with",
#              "dwellings, commerical, industrial, and vacant status vs.",
#              "residential status), sealed/compromised indicator,",
#              "owner-occupancy indicator, and property value."))
# cat(paste(paste0("\\multicolumn{",4,"}{l}{\\scriptsize{",
#                  strwrap(custom.note,width=60),"}}"),
#           collapse=" \\\\ \n"),perl=T); rm(custom.note)
# 
# ###Model I: Predicted Probability of Repayment at Control Means by Quartile
# #### via p_hat = exp(beta*x_bar)/(1+exp(beta*x_bar))
# #### log odds would be exp(beta*x_bar)
# #####first, get the sample means for the variables we don't vary in the table
# #####  (use sample *median* for land area and market value since they're
# #####   so skewed)
# sample_means_control12<-
#   c(1,analysis_data_main[,median(land_area/1e3)],
#     unlist(analysis_data_main[end==1,lapply(list(
#       years_count<=5,council==2,council==3,council==4,
#       council==5,council==6,council==7,council==8,council==9,
#       council==10,category=="Hotels&Apts",category=="Store w/ Dwelling",
#       category=="Commercial",category=="Industrial",category=="Vacant",
#       exterior=="Sealed/Compromised",owner_occ),mean)]),
#     analysis_data_main[end==1,median(market_value/1e5)])
# x_beta_control1<-sum(regs[["reg1c_main"]]$result$coefficients[c(1,8:26)]*sample_means_control12)
# 
# main_coef<-regs[["reg1c_main"]]$result$coefficients[c(2:7,27:35)]
# #the idea: add the following matrices to get the latent x*betas for each cell
# #   0 0 0 0      0 1 2 3     0 0 0 0  : T-Threat/S-Service/C-Civic/1-Q1/2-Q2/3-Q3
# #   T T T T      0 1 2 3     0 a d g  : a-Threat*Q1/b-Service*Q1/c-Civic*Q1
# #   S S S S   +  0 1 2 3  +  0 b e h  : d-Threat*Q2/e-Service*Q2/f-Civic*Q2
# #   C C C C      0 1 2 3     0 c f i  : g-Threat*Q3/h-Service*Q3/i-Civic*Q3
# #  (but more parsimonious to specify in list form, casting ex-post as matrix)
# print.xtable2(
#   xtable(matrix(sapply(rep(c(0,main_coef[1:3]),times=4)+
#                          rep(c(0,main_coef[4:6]),each=4)+
#                          c(rep(0,4),rbind(rep(0,3),matrix(main_coef[7:15],ncol=3)))
#                        +x_beta_control1,
#                        function(x){round(100*exp(x)/(1+exp(x)),1)}),ncol=4,
#                 dimnames=list(c("Control","Threat","Service","Civic"),
#                               paste(c("Low","Medium","High","Very High"),"Debt"))),
#          caption="Model I: Probability Ever Paid by Treatment, Debt Level at Sample Center",
#          label="table:modelI_marg",align="lcccc"),table.placement="htbp")
# 
# ###Model II: Logistic of Paid in Full
# ####Logistic Coefficients Table: Plain model
# texreg2(regs[paste0(rep("reg2",3),"_",
#                     unlist(lapply(list(analysis_data_main,analysis_data_resd,
#                                        analysis_data_sres),
#                                   function(x){comment(x)[1]})))],
#         custom.model.names=c("Full Sample","Residential","Residential, Unique Owner"),
#         custom.coef.names=c("Intercept","Threat","Service","Civic"),
#         caption="Model II: Linear Probability Models -- Paid in Full",label="table:modelIIa",
#         float.pos="htbp",include.aic=F,include.bic=F,include.loglik=F,
#         include.rsquared=F,include.adjrs=F,include.fstatistic=F,stars=c(.01,.05,.1))
# 
# ####Logistic Coefficients Table: Quartile Interactions
# texreg2(regs[paste(rep("reg2c",3),
#                   unlist(lapply(list(analysis_data_main,analysis_data_resd,
#                                      analysis_data_sres),
#                                 function(x){comment(x)[1]})),sep="_")],
#        custom.model.names=c("Full Sample","Non-Commercial","Sole Owner"),
#        custom.coef.names=c("Intercept","Service","Civic","Threat",
#                            paste("Balance",c("Q2","Q3","Q4")),
#                            paste0("control",
#                                   c("Land Area","Owes <= 5 Years",
#                                     paste("Dist.",2:10),"Hotels-Apts",
#                                     "Store w. Dwelling","Commercial",
#                                     "Industrial","Vacant","Sealed-Compromised",
#                                     "Homestead","Prop. Val. (\\$100k)")),
#                            paste0(c("Service","Civic","Threat"),"*",
#                                   rep(paste("Balance",c("Q2","Q3","Q4")),each=3))),
#        omit.coef="Intercept|control",reorder.coef=c(4,5,6,1,7,10,13,2,8,11,14,3,9,12,15),
#        custom.note=c("%stars. Control coefficients omitted for brevity; see Appendix."),
#        caption="Model II: Logistic Regressions -- Paid in Full",include.deviance=F,
#        label=c("table:modelIIb"),float.pos="htbp",include.bic=F,include.aic=F,stars=c(.01,.05,.1))
# 
# ####***BAND-AID TO WRAP LONG NOTE UNTIL TEXREG UPDATED:***
# custom.note<-
#   gsub("%stars","$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$",
#        paste("%stars. Other controls omitted for brevity: land area,",
#              "length of debt indicator, City Council District dummies,",
#              "dummies for categories (hotels/apartments, stores with",
#              "dwellings, commerical, industrial, and vacant status vs.",
#              "residential status), sealed/compromised indicator,",
#              "owner-occupancy indicator, and property value."))
# cat(paste(paste0("\\multicolumn{",4,"}{l}{\\scriptsize{",
#                  strwrap(custom.note,width=60),"}}"),
#           collapse=" \\\\ \n"),perl=T); rm(custom.note)
# 
# ####Marginal Predictions at Control Means
# x_beta_control2<-sum(regs[["reg2c_main"]]$result$coefficients[c(1,8:26)]*sample_means_control12)
# 
# tab_mat<-matrix(sapply(rep(c(0,main_coef[1:3]),times=4)+
#                          rep(c(0,main_coef[4:6]),each=4)+
#                          c(rep(0,4),rbind(rep(0,3),matrix(main_coef[7:15],ncol=3)))
#                        +x_beta_control2,
#                        function(x){round(100*exp(x)/(1+exp(x)),1)}),ncol=4,
#                 dimnames=list(c("Control","Service","Civic","Threat"),
#                               paste(c("Low","Medium","High","Very High"),"Debt")))
# 
# main_coef<-regs[["reg2c_main"]]$result$coefficients[c(2:7,27:35)]
# print.xtable2(
#   xtable(matrix(sapply(rep(c(0,main_coef[1:3]),times=4)+
#                          rep(c(0,main_coef[4:6]),each=4)+
#                          c(rep(0,4),rbind(rep(0,3),matrix(main_coef[7:15],ncol=3)))
#                        +x_beta_control2,
#                        function(x){round(100*exp(x)/(1+exp(x)),1)}),ncol=4,
#                 dimnames=list(c("Control","Service","Civic","Threat"),
#                               paste(c("Low","Medium","High","Very High"),"Debt"))),
#          caption="Model II: Probability Paid in Full by Treatment, Debt Level at Sample Center",
#          label="table:modelII_marg",align="lcccc"),table.placement="htbp")
# rm(main_coef,sample_means_control12,x_beta_control2,x_beta_control1)

#Plots ####
##Time Series Plots
trt<-get_treats(analysis_data_main)
trt_col<-get_treats_col(analysis_data_main)
draw_time_series<-function(plist){
  pdf2(paste0("./papers_presentations/images/analysis/main",
              "/time_series_",plist$file,"_main.pdf"))
  dcast.data.table(
    ts_data[,c("posting_rel","treatment",plist$series),with=F],
    posting_rel~treatment,value.var=plist$series
    )[,matplot(posting_rel,.SD[,!"posting_rel",with=F],
               type="l",lty=1,xlab="Days Since Mailing",
               ylab=plist$y,lwd=2,col=trt_col,
               main=paste0(plist$title,"\nFull Sample"))]
  legend(plist$pos,trt,lty=1,lwd=2,
         col=trt_col,bg="aliceblue")
  dev.off2()
}

ts_data<-analysis_data_main[posting_rel>=0,
                            .(evpd=mean(100*ever_paid),
                              pdfl=mean(100*paid_full),
                              avpm=mean(cum_treated_pmts),
                              cmpm=sum(100*current_balance/total_due_at_mailing_grp)),
                            by=c("posting_rel","treatment")]

plot_params<-list(ever_paid=list(file="ever_paid",y="%",series="evpd",pos="topleft",
                                 title="Percent of Group Ever Having Paid"),
                  paid_full=list(file="paid_full",y="%",series="pdfl",pos="topleft",
                                 title="Percent of Group Having Paid in Full"),
                  avg_paymt=list(file="average_payments",y="$",series="avpm",pos="topleft",
                                 title="Cumulative Average Payments Since Mail Date"),
                  debt_paid=list(file="debt_paydown",y="%",series="cmpm",pos="bottomleft",
                                 title="Percentage of Mailing Day Debt Owed"))

lapply(plot_params,draw_time_series); rm(ts_data,plot_params)

##By-Quartile graphs
quartile_cutoffs<-
  round(analysis_data_main[,quantile(total_due_at_mailing,probs=c(.25,.5,.75))],-2)

setkey(analysis_data_main,balance_quartile,treatment)
###Percent Ever Paid by treatment
pct_ever_paid_q<-
  dcast.data.table(analysis_data_main[posting_rel>=0,mean(100*ever_paid),
                                      by=list(balance_quartile,treatment,posting_rel)],
                   posting_rel~balance_quartile+treatment,value.var="V1")

uplim<-max(pct_ever_paid_q[,!"posting_rel",with=F])

pdf2(paste0("./papers_presentations/images/analysis/main",
            "/time_series_pct_ever_paid_by_quartile_main.pdf"))
layout(mat=matrix(c(1,3,5,2,4,5),nrow=3),heights=c(.4,.4,.2))
par(oma=c(0,0,3,0))
par(mar=c(0,4.1,4.1,2.1))
pct_ever_paid_q[,c("posting_rel",paste0("1_",trt)),with=F
                ][,matplot(posting_rel,.SD[,!"posting_rel",with=F],
                         type="l",lty=1,col=trt_col,xlab="",
                         lwd=2,xaxt="n",main="",ylab="%",
                         ylim=c(0,max(.SD[,!"posting_rel",with=F])))]
text(7,.75*par("usr")[4],
     paste("1st Quartile: \n",
           paste0("Below $",quartile_cutoffs[["25%"]])))

par(mar=c(0,4.1,4.1,2.1))
pct_ever_paid_q[,c("posting_rel",paste0("2_",trt)),with=F
                ][,matplot(posting_rel,.SD[,!"posting_rel",with=F],
                           type="l",lty=1,col=trt_col,xlab="",
                           lwd=2,xaxt="n",main="",ylab="%",
                           ylim=c(0,max(.SD[,!"posting_rel",with=F])))]
text(7,.75*par("usr")[4],
     paste("2nd Quartile: \n",
           paste0("$",quartile_cutoffs[["25%"]],
                  "-$",quartile_cutoffs[["50%"]])))

par(mar=c(5.1,4.1,0,2.1))
pct_ever_paid_q[,c("posting_rel",paste0("3_",trt)),with=F
                ][,matplot(posting_rel,.SD[,!"posting_rel",with=F],
                           type="l",lty=1,col=trt_col,lwd=2,
                           main="",ylab="%",xlab="Days Since Mailing",
                           ylim=c(0,max(.SD[,!"posting_rel",with=F])))]
text(7,.75*par("usr")[4],
     paste("3rd Quartile: \n",
           paste0("$",quartile_cutoffs[["50%"]],
                  "-$",quartile_cutoffs[["75%"]])))

par(mar=c(5.1,4.1,0,2.1))
pct_ever_paid_q[,c("posting_rel",paste0("4_",trt)),with=F
                ][,matplot(posting_rel,.SD[,!"posting_rel",with=F],
                           type="l",lty=1,col=trt_col,lwd=2,
                           main="",ylab="%",xlab="Days Since Mailing",
                           ylim=c(0,max(.SD[,!"posting_rel",with=F])))]
text(7,.75*par("usr")[4],
     paste("4th Quartile: \n",
           paste0("Above $",quartile_cutoffs[["75%"]])))

par(mar=c(0,0,0,0))
plot(1,type="n",axes=F,xlab="",ylab="")
legend("top",legend=trt,lty=1,lwd=2,
       col=trt_col,horiz=T,inset=0)
mtext(paste0("Percent of Group Ever Having Paid\n",
             "By Balance Quartile, Full Sample"),
      side=3,line=-2,outer=T)
dev.off2()
rm(uplim,pct_ever_paid_q)

###Percent Paid in Full by treatment
pct_paid_full_q<-
  dcast.data.table(analysis_data_main[posting_rel>=0,mean(100*paid_full),
                                      by=list(balance_quartile,treatment,posting_rel)],
                   posting_rel~balance_quartile+treatment,value.var="V1")

uplim<-max(pct_paid_full_q[,!"posting_rel",with=F])

pdf2(paste0("./papers_presentations/images/analysis/main",
            "/time_series_pct_paid_full_by_quartile_main.pdf"))
layout(mat=matrix(c(1,3,5,2,4,5),nrow=3),heights=c(.4,.4,.2))
par(oma=c(0,0,3,0))
par(mar=c(0,4.1,4.1,2.1))
pct_paid_full_q[,c("posting_rel",paste0("1_",trt)),with=F
                ][,matplot(posting_rel,.SD[,!"posting_rel",with=F],
                           type="l",lty=1,col=trt_col,xlab="",
                           lwd=2,xaxt="n",main="",ylab="%",
                           ylim=c(0,max(.SD[,!"posting_rel",with=F])))]
text(7,.75*par("usr")[4],
     paste("1st Quartile: \n",
           paste0("Below $",quartile_cutoffs[["25%"]])))

par(mar=c(0,4.1,4.1,2.1))
pct_paid_full_q[,c("posting_rel",paste0("2_",trt)),with=F
                ][,matplot(posting_rel,.SD[,!"posting_rel",with=F],
                           type="l",lty=1,col=trt_col,xlab="",
                           lwd=2,xaxt="n",main="",ylab="%",
                           ylim=c(0,max(.SD[,!"posting_rel",with=F])))]
text(7,.75*par("usr")[4],
     paste("2nd Quartile: \n",
           paste0("$",quartile_cutoffs[["25%"]],
                  "-$",quartile_cutoffs[["50%"]])))

par(mar=c(5.1,4.1,0,2.1))
pct_paid_full_q[,c("posting_rel",paste0("3_",trt)),with=F
                ][,matplot(posting_rel,.SD[,!"posting_rel",with=F],
                           type="l",lty=1,col=trt_col,lwd=2,
                           main="",ylab="%",xlab="Days Since Mailing",
                           ylim=c(0,max(.SD[,!"posting_rel",with=F])))]
text(7,.75*par("usr")[4],
     paste("3rd Quartile: \n",
           paste0("$",quartile_cutoffs[["50%"]],
                  "-$",quartile_cutoffs[["75%"]])))

par(mar=c(5.1,4.1,0,2.1))
pct_paid_full_q[,c("posting_rel",paste0("4_",trt)),with=F
                ][,matplot(posting_rel,.SD[,!"posting_rel",with=F],
                           type="l",lty=1,col=trt_col,lwd=2,
                           main="",ylab="%",xlab="Days Since Mailing",
                           ylim=c(0,max(.SD[,!"posting_rel",with=F])))]
text(7,.75*par("usr")[4],
     paste("4th Quartile: \n",
           paste0("Above $",quartile_cutoffs[["75%"]])))

par(mar=c(0,0,0,0))
plot(1,type="n",axes=F,xlab="",ylab="")
legend("top",legend=trt,lty=1,lwd=2,
       col=trt_col,horiz=T,inset=0)
mtext(paste0("Percent of Group Having Paid in Full\n",
             "By Balance Quartile, Full Sample"),
      side=3,line=-2,outer=T)
dev.off2()
rm(uplim,pct_paid_full_q)
