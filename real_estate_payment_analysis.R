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
library(foreign)
library(sandwich)
library(xtable)
library(Zelig)

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

print.xtable2<-function(...){
  #For pretty copy-pasting into LyX
  cat(capture.output(do.call('print.xtable',list(...))),sep="\n\n")
}

create_quantiles<-function(x,num,right=F,include.lowest=T){
  cut(x,breaks=quantile(x,probs=seq(0,1,by=1/num)),labels=1:num,right=right,include.lowest=include.lowest)
}

dol_form<-function(x,dig=0){paste0("$",prettyNum(round(x,digits=dig),big.mark=","))}

to.pct<-function(x,dig=0){round(100*x,digits=dig)}

get_treats<-function(x){if(comment(x)[1]=="act_leave_out")
  c("Threat","Service","Civic","Control","Leave-Out")
  else c("Threat","Service","Civic","Control")}

get_treats_col<-function(x){if(comment(x)[1]=="act_leave_out")
  c("red","blue","green","black","orange")
  else c("red","blue","green","black")}

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
max_length<-min(analysis_data_main[,max(as.integer(posting_rel)),by=cycle]$V1)
min_length<-max(analysis_data_main[,min(as.integer(posting_rel)),by=cycle]$V1)

analysis_data_main[,treatment_count:=uniqueN(opa_no),by=treatment]
analysis_data_main[,treatment_days :=uniqueN(cycle), by=treatment]

analysis_data_main[,years_cut:=cut(years_count,breaks=c(0,1,2,5,40),
                                   include.lowest=T,labels=c(1:2,"3-5",">5"))]
analysis_data_main[,rooms_cut:=cut(rooms,breaks=c(0,5,6,20),
                                   include.lowest=T,labels=c("0-5","6",">6"))]

##DEFINE DATA SETS FOR THE SUBSAMPLES
###LEAVE-OUT SAMPLE (2 DAYS PRIOR TO TREATMENT)
analysis_data_lout<-analysis_data_main
analysis_data_lout[,smpl:="IV"]
###MAIN SAMPLE
#### (reset factor levels once leave-out sample is eliminated)
analysis_data_main<-copy(analysis_data_main)[cycle>=33,][,treatment:=factor(treatment)]
analysis_data_main[,smpl:="I"]
###RESIDENTIAL PROPERTIES
#### (reset factor levels once nonresidential properties are eliminated)
analysis_data_resd<-analysis_data_main[residential==T,][,category:=factor(category)]
analysis_data_resd[,smpl:="II"]
###SINGLE RESIDENTIAL OWNER PROPERTIES--NO MATCH ON LEGAL_NAME/MAILING_ADDRESS
#### (reset factor levels once nonresidential properties are eliminated)
analysis_data_sres<-
  analysis_data_main[residential==T,if (uniqueN(opa_no)==1) .SD,
                     by=.(legal_name,mail_address)][,category:=factor(category)]
analysis_data_sres[,smpl:="III"]

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
}; rm(dt)

##FIRST GLANCE: SOME DISTRIBUTIONS & TABLES
###MAIN DESCRIPTIVES TABLE
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

#Main Regressions, Tables, and Associated Predictions ####
regs<-list(reg1_main="",reg1_residential="",reg1_residential_single_owner="",reg1_leave_out="",
           reg1c_main="",reg1c_residential="",reg1c_residential_single_owner="",reg1c_leave_out="",
           reg2_main="",reg2_residential="",reg2_residential_single_owner="",reg2_leave_out="",
           reg2c_main="",reg2c_residential="",reg2c_residential_single_owner="",reg2c_leave_out="")
for (data in list(analysis_data_main,analysis_data_lout,analysis_data_resd,analysis_data_sres)){
  data_end<-data[end==1,]
  
  controls<-(~I(land_area/1e3)+I(years_count<=5)+council+category+
               I(exterior=="Sealed/Compromised")+I(homestead>0))
  
  #1(Ever Paid) Differs by Treatment?
  regs[[paste0("reg1_",comment(data)[1])]]<-
    zelig(ever_paid~
            relevel(treatment,ref=if (comment(data)[1]=="leave_out") "Leave-Out" else "Control"),
          data=data_end,model="ls",robust=T,cluster="cluster_id",cite=F)
  regs[[paste0("reg1c_",comment(data)[1])]]<-
    zelig(update(controls,ever_paid~
                   relevel(treatment,ref=if (comment(data)[1]=="leave_out")
                     "Leave-Out" else "Control")*balance_quartile
                 +.+I(market_value/1e5)),
          data=data_end,model="logit",robust=T,cluster="cluster_id",cite=F)
  
  #1(Bill Paid in Full) Differs by Treatment?
  regs[[paste0("reg2_",comment(data)[1])]]<-
    zelig(paid_full~
            relevel(treatment,ref=if (comment(data)[1]=="leave_out") "Leave-Out" else "Control"),
          data=data_end,model="ls",robust=T,cluster="cluster_id",cite=F)
  regs[[paste0("reg2c_",comment(data)[1])]]<-
    zelig(update(controls,I(cum_treated_pmts>=total_due_at_mailing)~
                   relevel(treatment,ref=if (comment(data)[1]=="leave_out") 
                     "Leave-Out" else "Control")*balance_quartile
                 +.+I(market_value/1e5)),
          data=data_end,model="logit",robust=T,cluster="cluster_id",cite=F)
}; rm(data,data_end,controls)

##Regression tables

###Table: Summary of Effectiveness by Treatment & Sample
ord<-c(4,2,3,1)
print.xtable(xtable(rbindlist(lapply(list(
  analysis_data_main,analysis_data_resd,analysis_data_sres),
  function(x){x[end==1,.(days=treatment_days[1],.N,smpl=smpl[1],
                         tot_due=sum(total_due_at_mailing),
                         ev_pd=mean(ever_paid),pd_fl=mean(paid_full),
                         tot_pmt=sum(cum_treated_pmts)),
                by=treatment
                ][order(treatment)
                  ][ord,.("Sample"=smpl,"Group"=treatment,
                          "No. Treated (Treated Days)"=
                            paste0(prettyNum(N,big.mark=",")," (",days,")"),
                          "Total Debt Owed"=dol_form(tot_due),
                          "Percent Ever Paid"=to.pct(ev_pd),
                          "Percent Paid in Full"=to.pct(pd_fl),
                          "Dollars Received"=dol_form(tot_pmt),
                          "Percent Debt Received"=to.pct(tot_pmt/tot_due),
                          "Dollars above Control Per Day"=
                            dol_form(tot_pmt/days-tot_pmt[4]/days[4]),
                          "Total Generated over All Days"=
                            dol_form(tot_pmt-tot_pmt[4]/days[4]*days))]})),
  caption=c("Summary of Effectiveness of Treatment"),label="table:summary",
  align=paste0("|c|p{1.3cm}|p{1.3cm}|p{1.4cm}|p{2cm}|p{1.4cm}|",
               "p{1.4cm}|p{1.4cm}|p{1.4cm}|p{1.4cm}|p{1.6cm}|"),
  digits=c(rep(0,8),1,0,0)),include.rownames=F,hline.after=c(-1,0,4,8,12),
  floating.environment="sidewaystable"); rm(ord)

###Table: Summary of Effectiveness by Treatment & Sample, VS LEAVE-OUT SAMPLE
ord<-c(5,3,4,1,2)
print.xtable(xtable(
  analysis_data_lout[end==1,.(days=treatment_days[1],.N,smpl=smpl[1],
                              tot_due=sum(total_due_at_mailing),
                              ev_pd=mean(ever_paid),pd_fl=mean(paid_full),
                              tot_pmt=sum(cum_treated_pmts)),
                     by=treatment
                     ][order(treatment)
                       ][ord,.("Sample"=smpl,"Group"=treatment,
                               "No. Treated (Treated Days)"=
                                 paste0(prettyNum(N,big.mark=",")," (",days,")"),
                               "Total Debt Owed"=dol_form(tot_due),
                               "Percent Ever Paid"=to.pct(ev_pd),
                               "Percent Paid in Full"=to.pct(pd_fl),
                               "Dollars Received"=dol_form(tot_pmt),
                               "Percent Debt Received"=to.pct(tot_pmt/tot_due),
                               "Dollars above Control Per Day"=
                                 dol_form(tot_pmt/days-tot_pmt[5]/days[5]),
                               "Total Generated over All Days"=
                                 dol_form(tot_pmt-tot_pmt[5]/days[5]*days))],
  caption=c("Summary of Effectiveness of Treatment vs. Leave-Out Sample"),label="table:summary_leave_out",
  align=paste0("|c|p{1.3cm}|p{1.3cm}|p{1.4cm}|p{2cm}|p{1.4cm}|",
               "p{1.4cm}|p{1.4cm}|p{1.4cm}|p{1.4cm}|p{1.6cm}|"),
  digits=c(rep(0,8),1,0,0)),include.rownames=F,hline.after=c(-1,0,5),
  floating.environment="sidewaystable"); rm(ord)

###Model I: Logistic of Ever-Paid
####Logistic Coefficients Table: Plain model
texreg(regs[paste(rep("reg1",3),
                  unlist(lapply(list(analysis_data_main,analysis_data_resd,
                                     analysis_data_sres),
                                function(x){comment(x)[1]})),sep="_")],
       custom.model.names=c("Full Sample","Non-Commercial","Sole Owner"),
       custom.coef.names=c("Intercept","Service","Civic","Threat"),
       caption="Model I: Logistic Regressions -- Ever Paid",label="table:modelIa",
       float.pos="htbp",include.deviance=F,include.aic=F,include.bic=F)


####Logistic Coefficients Table: Quartile Interactions
texreg(regs[paste(rep("reg1c",3),
                  unlist(lapply(list(analysis_data_main,analysis_data_resd,
                                     analysis_data_sres),
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
       caption="Model I: Logistic Regressions -- Ever Paid",include.deviance=F,
       label="table:modelIb",float.pos="htbp",include.bic=F,include.aic=F)

####Marginal Predictions at Control Means
#### via p_hat = exp(beta*x_bar)/(1+exp(beta*x_bar))
sample_means_control12c<-
  c(1,analysis_data_main[,median(land_area/1e3)],
    unlist(analysis_data_main[end==1,lapply(list(years_count<=5,council==2,
                                                 council==3,council==4,council==5,
                                                 council==6,council==7,council==8,council==9,
                                                 council==10,category=="Hotels&Apts",
                                                 category=="Store w/ Dwelling",
                                                 category=="Commercial",category=="Industrial",
                                                 category=="Vacant",exterior=="Sealed/Compromised",
                                                 homestead>0),mean)]),
    analysis_data_main[end==1,median(market_value/1e5)])
log_odds_control1c<-(regs[["reg1c_act"]]$result$coefficients[c(1,8:26)] %*% sample_means_control12c)[1]

coef<-regs[["reg1c_act"]]$result$coefficients
print.xtable(
  xtable(matrix(sapply(matrix(rep(c(0,coef[2:4]),times=4),ncol=4)+
                         t(matrix(rep(c(0,coef[5:7]),times=4),ncol=4))+log_odds_control1c+
                         cbind(rep(0,4),rbind(rep(0,3),matrix(coef[27:35],ncol=3))),
                       function(x){round(100*exp(x)/(1+exp(x)),1)}),ncol=4,
                dimnames=list(c("Control","Service","Civic","Threat"),paste0("Q",1:4))),
         caption="Model I: Marginal Predictions of Logistic Regressions -- Ever Paid",
         label="table:modelI_marg",align="lcccc"),table.placement="htbp")

###Model II: Logistic of Paid in Full
####Logistic Coefficients Table: Plain model
texreg(regs[paste(rep("reg2",3),
                  unlist(lapply(list(analysis_data_main,analysis_data_resd,
                                     analysis_data_sres),
                                function(x){comment(x)[1]})),sep="_")],
       custom.model.names=c("Full Sample","Non-Commercial","Sole Owner"),
       custom.coef.names=c("Intercept","Service","Civic","Threat"),
       caption="Model II: Logistic Regressions -- Paid in Full",label="table:modelIIa",
       float.pos="htbp",include.deviance=F,include.aic=F,include.bic=F)

####Logistic Coefficients Table: Quartile Interactions
texreg(regs[paste(rep("reg2c",3),
                  unlist(lapply(list(analysis_data_main,analysis_data_resd,
                                     analysis_data_sres),
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
       label=c("table:modelIIb"),float.pos="htbp",include.bic=F,include.aic=F)

####Marginal Predictions at Control Means
#### via p_hat = exp(beta*x_bar)/(1+exp(beta*x_bar))
log_odds_control2c<-(regs[["reg2c_act"]]$result$coefficients[c(1,8:26)] %*% sample_means_control12c)[1]

coef<-regs[["reg2c_act"]]$result$coefficients
print.xtable(
  xtable(matrix(sapply(matrix(rep(c(0,coef[2:4]),times=4),ncol=4)+
                         t(matrix(rep(c(0,coef[5:7]),times=4),ncol=4))+log_odds_control2c+
                         cbind(rep(0,4),rbind(rep(0,3),matrix(coef[27:35],ncol=3))),
                       function(x){round(100*exp(x)/(1+exp(x)),1)}),ncol=4,
                dimnames=list(c("Control","Service","Civic","Threat"),paste0("Q",1:4))),
         caption="Model II: Marginal Predictions of Logistic Regressions -- Paid in Full",
         label="table:modelII_marg",align="lcccc"),table.placement="htbp")
rm(regs,coef,list=ls(pattern="_control"))
