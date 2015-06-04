Data Analysis
#Philadelphia Real Estate Tax Evasion
#Michael Chirico
#April 3, 2015

#Setup: Packages, Working Directory, Etc.####
rm(list=ls(all=T))
setwd("~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/")
library(data.table)
library(texreg)
library(foreign)
library(sandwich)
library(xtable)
library(Zelig)

#Convenient Functions ####
prop.table2<-function(...){
  dots<-list(...)
  args<-names(dots) %in% names(formals(prop.table))
  do.call('prop.table',
          c(list(do.call('table',if (length(args)) dots[!args] else dots)),
            dots[args]))
}

create_quantiles<-function(x,num,right=F,include.lowest=T){
  cut(x,breaks=quantile(x,probs=seq(0,1,by=1/num)),labels=1:num,right=right,include.lowest=include.lowest)
}

dol_form<-function(x,dig=0){paste0("$",prettyNum(round(x,digits=dig),big.mark=","))}

to.pct<-function(x,dig=0){round(100*x,digits=dig)}

get_treats<-function(x){if(comment(x)[1]=="act_leave_out")
                          c("Threat","Moral","Peer","Control","Leave-Out")
                        else c("Threat","Moral","Peer","Control")}

get_treats_col<-function(x){if(comment(x)[1]=="act_leave_out")
                              c("red","blue","green","black","orange")
                            else c("red","blue","green","black")}

#Set up Analysis Data Sets ####
analysis_data<-fread("analysis_file.csv",colClasses=c(date_of_first_payment="character"))
#CLUSTERS DEFINED AT THE OWNER LEVEL
#OWNER DEFINED AS ANYONE WITH IDENTICAL NAME & MAILING ADDRESS
setkey(setkey(analysis_data,legal_name,mail_address
              )[unique(analysis_data,by=c("legal_name","mail_address")
              )[,I:=.I],cluster_id:=I],opa_no,posting_rel)

###tidying up data classifications
factors<-c("treatment","category","exterior","council")
analysis_data[,(factors):=lapply(.SD,as.factor),.SDcols=factors]
#since read as strings, order is out of whack
analysis_data[,council:=factor(council,levels=levels(council)[c(1,3:10,2)])]
analysis_data[,category:=factor(category,levels=levels(category)[c(4,2,5,1,3,6)])]
analysis_data[,posting:=as.Date(posting)]
rm(factors)

##create some variables which can be defined on the main data set
## and passed through without change to the subsamples
max_length<-analysis_data[,max(as.integer(posting_rel)),by=cycle][,min(V1)]
min_length<-analysis_data[,min(as.integer(posting_rel)),by=cycle][,max(V1)]

analysis_data[,treatment_count:=sum(end==1),by=treatment]
analysis_data[,treatment_days:=uniqueN(cycle),by=treatment]

analysis_data[,years_cut:=cut(years_count,breaks=c(0,1,2,5,40),
                              include.lowest=T,labels=c(1:2,"3-5",">5"))]
analysis_data[,rooms_cut:=cut(rooms,breaks=c(0,5,6,20),
                              include.lowest=T,labels=c("0-5","6",">6"))]

##DEFINE DATA SETS FOR THE SUBSAMPLES
###LEAVE-OUT SAMPLE (2 DAYS PRIOR TO TREATMENT)
analysis_data_lout<-analysis_data
analysis_data_lout[,smpl:="IV"]
###MAIN SAMPLE
#### (reset factor levels once leave-out sample is eliminated)
analysis_data<-copy(analysis_data)[cycle>=33,][,treatment:=factor(treatment)]
analysis_data[,smpl:="I"]
###NONCOMMERCIAL PROPERTIES
#### (reset factor levels once commercial properties are eliminated)
analysis_data_ncomm<-analysis_data[commercial==0,][,category:=factor(category)]
analysis_data_ncomm[,smpl:="II"]
###SINGLE OWNER PROPERTIES--NO MATCH ON LEGAL_NAME/MAILING_ADDRESS
analysis_data_sown<-analysis_data[,count:=.N,by=.(legal_name,mail_address)
                                  ][count-min(count)==0,][,count:=NULL]
analysis_data[,count:=NULL]
analysis_data_sown[,smpl:="III"]

###Tags & data descriptions for file naming for each data set
comment(analysis_data)<-c("act","Full Sample","analysis_data")
comment(analysis_data_lout)<-c("act_leave_out","Including Leave-Out Sample","analysis_data_lout")
comment(analysis_data_ncomm)<-c("act_non_commercial","Non-Commercial Properties","analysis_data_ncomm")
comment(analysis_data_sown)<-c("act_single_owner","Single-Owner Properties","analysis_data_sown")

for (dt in list(analysis_data,analysis_data_lout,analysis_data_ncomm,analysis_data_sown)){
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

#Tests of Randomness ####

##TESTS OF BALANCE ON OBSERVABLES
p_exp<-analysis_data[!duplicated(cycle),prop.table2(treatment)]
test_counts  <-chisq.test(analysis_data[end==1,table(treatment)],p=p_exp)
test_years   <-chisq.test(analysis_data[end==1,table(years_cut,treatment)])
test_council <-chisq.test(analysis_data[end==1,table(as.integer(council),treatment)])
test_category<-chisq.test(analysis_data[end==1,table(category,treatment)])
test_balance <-chisq.test(analysis_data[end==1,table(total_due_at_mailing,treatment)])
test_mv_q    <-chisq.test(analysis_data[end==1,table(mv_quartile,treatment)])
test_area_q  <-chisq.test(analysis_data[end==1,table(area_quartile,treatment)])
test_rooms   <-chisq.test(analysis_data[end==1,table(rooms_cut,treatment)])

###Table 1: 4 tests
print.xtable(
  xtable(
    matrix(
      rbind(rep("",5),cbind(round(
        analysis_data[end==1,prop.table2(balance_quartile,
                                   treatment,margin=1)],2),
                  c(round(test_balance$p.value,2),rep("",3))),
        rep("",5),cbind(round(
          analysis_data[end==1,prop.table2(mv_quartile,
                                     treatment,margin=1)],2),
          c(round(test_mv_q$p.value,2),rep("",3))),
        rep("",5),cbind(round(
          analysis_data[end==1,prop.table2(area_quartile,
                                           treatment,margin=1)],2),
          c(round(test_area_q$p.value,2),rep("",3))),
        c(round(analysis_data[end==1,prop.table2(treatment)],2),
          round(test_counts$p.value,2)),
        c(round(p_exp,2),"")),ncol=5,
      dimnames=list(c("Balance Due Quartiles",
                      paste0("<\\$",round(quantile(analysis_data$total_due_at_mailing,.25),-2)),
                      paste0("\\lbrack\\$",round(quantile(analysis_data$total_due_at_mailing,.25),-2),",\\$",
                             round(quantile(analysis_data$total_due_at_mailing,.5),-2),")"),
                      paste0("\\lbrack\\$",round(quantile(analysis_data$total_due_at_mailing,.5),-2),",\\$",
                             round(quantile(analysis_data$total_due_at_mailing,.75),-2),")"),
                      paste0("\\$",round(quantile(analysis_data$total_due_at_mailing,.75),-2),"+"),
                      "Market Value Quartiles",
                      paste0("<\\$",round(quantile(analysis_data$market_value,.25),-3)/1000,"k"),
                      paste0("\\lbrack\\$",round(quantile(analysis_data$market_value,.25),-3)/1000,"k,\\$",
                             round(quantile(analysis_data$market_value,.5),-3)/1000,"k)"),
                      paste0("\\lbrack\\$",round(quantile(analysis_data$market_value,.5),-3)/1000,"k,\\$",
                             round(quantile(analysis_data$market_value,.75),-3)/1000,"k)"),
                      paste0("\\$",round(quantile(analysis_data$market_value,.75),-3)/1000,"k+"),
                      "Land Area Quartiles",
                      paste0("<",round(quantile(analysis_data$land_area,.25),-2)," sq. ft."),
                      paste0("\\lbrack",round(quantile(analysis_data$land_area,.25),-2),",",
                             round(quantile(analysis_data$land_area,.5),-2),") sq. ft."),
                      paste0("\\lbrack",round(quantile(analysis_data$land_area,.5),-2),",",
                             round(quantile(analysis_data$land_area,.75),-2),") sq. ft."),
                      paste0(">",round(quantile(analysis_data$land_area,.75),-2)," sq. ft."),
                      "Distribution of Properties","Expected Distribution"),
                    c("Threat","Moral","Peer","Control","$p$-value"))),
    caption="Tests of Sample Balance on Observables I",label="table:balanceI"),
  hline.after=c(5,10,15:17),sanitize.text.function=identity,table.placement="htbp")

print.xtable(
  xtable(
    matrix(
      rbind(rep("",5),cbind(round(analysis_data[end==1,
                                      prop.table2(rooms_cut,treatment,
                                                  margin=1)],2),
                  c(round(test_rooms$p.value,2),rep("",2))),
            rep("",5),cbind(round(analysis_data[end==1,
                                      prop.table2(years_cut,treatment,
                                                  margin=1)],2),
                  c(round(test_rooms$p.value,2),rep("",3))),
            rep("",5),cbind(round(analysis_data[end==1,
                                      prop.table2(category,treatment,
                                                  margin=1)],2),
                  c(round(test_category$p.value,2),rep("",5))),
            c(round(p_exp,2),"")),ncol=5,
      dimnames=list(c("\\# Rooms","0-5","6","7+",
                      "Years of Debt","1 Year","2 Years",
                      "3-5 Years","6+ Years",
                      "Category","Residential",
                      "Hotels\\&Apts","Store w. Dwell.",
                      "Commercial","Industrial","Vacant Land",
                      "Expected Distribution"),
                    c("Threat","Moral","Peer","Control","$p$-value"))),
    caption="Tests of Sample Balance on Observables II",label="table:balanceII"),
  hline.after=c(4,9,16,17),sanitize.text.function=identity,table.placement="htbp")
rm(p_exp,list=ls(pattern="test"))


### Fidelity check--did we assign the right day as "start of treatment"?
graphics.off()
dev.new()
pdf(paste0("./papers_presentations/images/analysis/act/",
           "distribution_delinquency_mail_day_act.pdf"))
dev.set(which=dev.list()["RStudioGD"])
x<-analysis_data[-5<=posting_rel&posting_rel<=5,list(avg_pmt=mean(-balance_change)),by=c("cycle","posting_rel")]
plot(x[cycle==33,.(posting_rel,avg_pmt)],main="Average Payment Near \n Assigned Treatment Boundary \n By Cycle",
     xlab="Days Since/From Mailing",ylab="Average Payment",ylim=range(x$avg_pmt),type="l",col="black",lwd=3)
lines(x[cycle==39,.(posting_rel,avg_pmt)],type="l",col="red",lwd=3)
lines(x[cycle==40,.(posting_rel,avg_pmt)],type="l",col="blue",lwd=3)
lines(x[cycle==41,.(posting_rel,avg_pmt)],type="l",col="green",lwd=3)
lines(x[cycle==43,.(posting_rel,avg_pmt)],type="l",col="orange",lwd=3)
lines(x[cycle==44,.(posting_rel,avg_pmt)],type="l",col="cyan",lwd=3)
lines(x[cycle==45,.(posting_rel,avg_pmt)],type="l",col="brown",lwd=3)
lines(x[cycle==46,.(posting_rel,avg_pmt)],type="l",col="purple",lwd=3)
lines(x[cycle==47,.(posting_rel,avg_pmt)],type="l",col="grey",lwd=3)
legend("topleft",legend=c(33,39,40,41,43,44,45,46,47),
       col=c("black","red","blue","green",lwd=3,
             "orange","cyan","brown","purple","grey"),lty=1,
       lwd=c(rep(1,8),2))
dev.copy(which=dev.list()["pdf"])
dev.off(which=dev.list()["pdf"])
rm(x)

### Checking Observables Balance
graphics.off()
dev.new()
pdf(paste0("./papers_presentations/images/analysis/act/",
           "distribution_delinquency_mail_day_act.pdf"))
dev.set(which=dev.list()["RStudioGD"])
par(xpd=T,mar=c(5.1,4.1,4.1,4.1))
barplot(cbind(prop.table(table(analysis_data$treatment)),
              prop.table(table(analysis_data[,.(treatment,council)]),2)),
        col=c("black","blue","green","red"),names.arg=c("All",1:10),
        xlab="Council District",
        main="Treatment Group Distribution \n By Council District")
legend(13.5,.8,c("Threat","Peer","Moral","Control"),
       cex=.75,fill=c("red","green","blue","black"))
dev.copy(which=dev.list()["pdf"])
dev.off(which=dev.list()["pdf"])

#Descriptive Statistics/Plots ####

##Log balance distribution, by treatment
graphics.off()
dev.new()
pdf('./papers_presentations/images/balance/total_balance_by_treatment.pdf')
dev.set(which=dev.list()["RStudioGD"])
par(mfrow=c(2,2),oma=c(0,0,3,0))
hist(analysis_data[end==1&treatment=="Threat",
                   log10(total_due_at_mailing)],
     col="red",main="Threat",xlab="Log_10 $",freq=F,breaks=seq(-2,6,by=.25))
hist(analysis_data[end==1&treatment=="Moral",
                   log10(total_due_at_mailing)],
     col="blue",main="Moral",xlab="Log_10 $",freq=F,breaks=seq(-2,6,by=.25))
hist(analysis_data[end==1&treatment=="Peer",
                   log10(total_due_at_mailing)],
     col="green",main="Peer",xlab="Log_10 $",freq=F,breaks=seq(-2,6,by=.25))
hist(analysis_data[end==1&treatment=="Control",
                   log10(total_due_at_mailing)],
     col="black",main="Control",xlab="Log_10 $",freq=F,breaks=seq(-2,6,by=.25))
title("Distribution of Mail-day Balance by Treatment",outer=T)
dev.copy(which=dev.list()["pdf"])
dev.off(which=dev.list()["pdf"])

##FIRST GLANCE: SOME DISTRIBUTIONS & TABLES
###MAIN DESCRIPTIVES TABLE
xtable(matrix(rbind(cbind(analysis_data[,prop.table2(years_cut)],
                          analysis_data[,prop.table2(years_cut,treatment,margin=2)
                                        ][,c(4,2,3,1)]),
                    cbind(analysis_data[,prop.table2(category)],
                          analysis_data[,prop.table2(category,treatment,margin=2)
                                        ][,c(4,2,3,1)]),
                    c(analysis_data[,mean(exterior=="Sealed/Compromised")],
                      analysis_data[,mean(exterior=="Sealed/Compromised"),
                                    by=treatment]$V1[c(4,2,3,1)]),
                    c(analysis_data[,mean(homestead>0)],
                      analysis_data[,mean(homestead>0),
                                    by=treatment]$V1[c(4,2,3,1)])),ncol=5,
              dimnames=list(c(paste("Years:",c("[0,5]","(5,10]",
                                               "(10,20]","(20,40]")),
                              paste("Cat:",c("Commercial","Hotels/Apts",
                                             "Industrial","Residential",
                                             "Store+Resid","Vacant")),
                              "% Sealed/Compromised","% Homestead"),
                            c("Full","Threat","Moral","Peer","Control"))),
       digits=2)
###MARGINAL BALANCE DISTRIBUTION
graphics.off()
dev.new()
pdf(paste0("./papers_presentations/images/analysis/act/",
           "distribution_delinquency_mail_day_act.pdf"))
dev.set(which=dev.list()["RStudioGD"])
hist(log10(analysis_data[posting_rel==0,calc_total_due]),
     main="Distribution of (Log) Delinquent Balance \n As of Mailing Day",
     xaxt="n",xlab="Log_10 $",col="blue",freq=F,breaks=seq(-2,6.5,by=.5),ylim=c(0,.6))
axis(1,at=-2:6)
text(0,.8*par('usr')[4],
     paste0("n=",nrow(analysis_data)/(max_length-min_length+1)))
text(0,.7*par('usr')[4],
     paste0("mean=",
            round(mean(analysis_data[posting_rel==0,
                                     calc_total_due]),2)))
text(0,.6*par('usr')[4],
     paste0("median=",
            median(analysis_data[posting_rel==0,
                                 calc_total_due])))
dev.copy(which=dev.list()["pdf"])
dev.off(which=dev.list()["pdf"])

###JOINT BALANCE-PAYMENT DISTRIBUTION
graphics.off()
dev.new()
pdf(paste0("./papers_presentations/images/analysis/act/",
           "distribution_delinquency_mail_day_act.pdf"))
dev.set(which=dev.list()["RStudioGD"])
plot(analysis_data[cum_treated_pmts>0&end==1,
                   list(log10(total_due_at_mailing),
                        log10(cum_treated_pmts))],
     xlab="Log_10 Owed",ylab="Log_10 Paid",
     col=ifelse(analysis_data[cum_treated_pmts>0&end==1,
                              cum_treated_pmts>total_due_at_mailing-20],
                "red","blue"),
     ylim=range(analysis_data[cum_treated_pmts>0&end==1,
                              log10(cum_treated_pmts)]),
     main=paste0("Joint Distribution of (Log) $ Owed at Mailing vs. ",
                 "(Log) $ Paid in first ",as.integer(max_length)," days"))
text(0,par('usr')[4]-1,
     paste0("n above 45-degree line: ",
            nrow(analysis_data[cum_treated_pmts>0
                               &end==1
                               &cum_treated_pmts>
                                 total_due_at_mailing-20,])),
     col="red")
dev.copy(which=dev.list()["pdf"])
dev.off(which=dev.list()["pdf"])

### JOINT BALANCE-% PAID DISTRIBUTION
###  (Eliminating those who paid more than they owed)
graphics.off()
dev.new()
pdf(paste0("./papers_presentations/images/analysis/act/",
           "distribution_delinquency_mail_day_act.pdf"))
dev.set(which=dev.list()["RStudioGD"])
plot(analysis_data[(total_due_at_mailing>cum_treated_pmts-10)
                   &end==1
                   &cum_treated_pmts>0&total_due_at_mailing>10,
                   list(log10(total_due_at_mailing),
                        cum_treated_pmts/total_due_at_mailing)],
     main="Joint Distribution of (Log) Owed vs. % Paid \n Among Non-Overpayers Who Owed >$10",
     xlab="Log_10 $ Owed",ylab="Percentage of Debt Repaid",col="blue",ylim=c(0,1.2))
text(3,1.1,paste0("n fully repaid: ",
                  nrow(analysis_data[cum_treated_pmts/total_due_at_mailing>.95
                                     &end==1,])))
dev.copy(which=dev.list()["pdf"])
dev.off(which=dev.list()["pdf"])

### MARGINAL $ PAID/MARKET VALUE DISTRIBUTION
graphics.off()
dev.new()
pdf(paste0("./papers_presentations/images/analysis/act/",
           "distribution_delinquency_mail_day_act.pdf"))
dev.set(which=dev.list()["RStudioGD"])
x<-analysis_data[cum_treated_pmts>0&market_value>0
                 &end==1,
                 cum_treated_pmts/market_value]
hist(x,main="Distribution of \n Amount Paid/Assesed Value",
     xlab="Amount Paid/Market Value",col="blue",xaxt="n",
     xlim=c(0,1))
axis(1,at=seq(0,1,by=.2))
text(mean(par('usr')[1:2]),.8*par('usr')[4],
     paste0("50th %ile: ",round(quantile(x,.5),3)))
text(mean(par('usr')[1:2]),.7*par('usr')[4],
     paste0("90th %ile: ",round(quantile(x,.9),3)))
text(mean(par('usr')[1:2]),.6*par('usr')[4],
     paste0("95th %ile: ",round(quantile(x,.95),3)))
text(mean(par('usr')[1:2]),.5*par('usr')[4],
     paste0("99th %ile: ",round(quantile(x,.99),3)))
text(mean(par('usr')[1:2]),.4*par('usr')[4],
     paste0("99.9th %ile: ",round(quantile(x,.999),3)))
dev.copy(which=dev.list()["pdf"])
dev.off(which=dev.list()["pdf"])
rm(x)

#Time Series Plots ####
for (data in list(analysis_data,analysis_data_lout,analysis_data_ncomm,analysis_data_sown)){
  ##Cumulative Payments by Treatment (normalized by group size)
  graphics.off()
  dev.new()
  pdf(paste0("./papers_presentations/images/analysis/",comment(data)[1],
             "/time_series_average_payments_",comment(data)[1],".pdf"))
  dev.set(which=dev.list()["RStudioGD"])
  matplot(0:max_length,
          setcolorder(dcast.data.table(
            data[posting_rel>=0,mean(cum_treated_pmts),
                 by=c("posting_rel","treatment")],
            posting_rel~treatment,value.var="V1")[,!"posting_rel",with=F],
            get_treats(data)),
          type="l",lty=1,
          col=get_treats_col(data),
          xlab="Days Since Mailing",ylab="$",lwd=2,
          main=paste("Cumulative Average Payments Since Mail Date \n",
                     "By",comment(data)[2]))
  legend("topleft",get_treats(data),lty=1,lwd=2,
         col=get_treats_col(data),bg="aliceblue")
  dev.copy(which=dev.list()["pdf"])
  dev.off(which=dev.list()["pdf"])
  
  ##DRAW-DOWN OF TOTAL BALANCE (GIVEN NOV 3 DATE OF OCTOBER DELINQUENCY DATA)
  graphics.off()
  dev.new()
  pdf(paste0("./papers_presentations/images/analysis/",comment(data)[1],
             "/time_series_debt_paydown_",comment(data)[1],".pdf"))
  dev.set(which=dev.list()["RStudioGD"])
  matplot(0:max_length,
          setcolorder(dcast.data.table(
            data[posting_rel>=0,
                 sum(100*current_balance/total_due_at_mailing_grp),
                 by=c("posting_rel","treatment")],
            posting_rel~treatment,value.var="V1")[,!"posting_rel",with=F],
            get_treats(data)),
          type="l",lty=1,
          col=get_treats_col(data),
          xlab="Days Since Mailing",ylab="%",lwd=2,
          main=paste("Percentage of Mailing Day Debt Owed \n",
                     "By",comment(data)[2]))
  legend("bottomleft",get_treats(data),lty=1,lwd=2,
         col=get_treats_col(data),bg="aliceblue")
  dev.copy(which=dev.list()["pdf"])
  dev.off(which=dev.list()["pdf"])
  
  ##Percent Ever Paid by treatment
  graphics.off()
  dev.new()
  pdf(paste0("./papers_presentations/images/analysis/",comment(data)[1],
             "/time_series_ever_paid_",comment(data)[1],".pdf"))
  dev.set(which=dev.list()["RStudioGD"])
  matplot(0:max_length,
          setcolorder(dcast.data.table(
            data[posting_rel>=0,mean(100*ever_paid),
                 by=c("posting_rel","treatment")],
            posting_rel~treatment,value.var="V1")[,!"posting_rel",with=F],
            get_treats(data)),
          type="l",lty=1,
          col=get_treats_col(data),
          xlab="Days Since Mailing",ylab="%",lwd=2,
          main=paste("Percent of Group Ever Having Paid",
                     "\n By",comment(data)[2]))
  legend("topleft",get_treats(data),lty=1,lwd=2,
         col=get_treats_col(data),bg="aliceblue")
  dev.copy(which=dev.list()["pdf"])
  dev.off(which=dev.list()["pdf"])
  
  ##Percent Paid in Full by treatment
  graphics.off()
  dev.new()
  pdf(paste0("./papers_presentations/images/analysis/",comment(data)[1],
             "/time_series_paid_full_",comment(data)[1],".pdf"))
  dev.set(which=dev.list()["RStudioGD"])
  matplot(0:max_length,
          setcolorder(dcast.data.table(
            data[posting_rel>=0,mean(100*paid_full),
                 by=c("posting_rel","treatment")],
            posting_rel~treatment,value.var="V1")[,!"posting_rel",with=F],
            get_treats(data)),
          type="l",lty=1,
          col=get_treats_col(data),
          xlab="Days Since Mailing",ylab="%",lwd=2,
          main=paste("Percent of Group Having Paid in Full",
                     "\n By",comment(data)[2]))
  legend("topleft",get_treats(data),lty=1,lwd=2,
         col=get_treats_col(data),bg="aliceblue")
  dev.copy(which=dev.list()["pdf"])
  dev.off(which=dev.list()["pdf"])
  
  ##Percent Paid in Full Conditional on Payment by treatment
  graphics.off()
  dev.new()
  pdf(paste0("./papers_presentations/images/analysis/",comment(data)[1],
             "/time_series_paid_full_given_paid_",comment(data)[1],".pdf"))
  dev.set(which=dev.list()["RStudioGD"])
  matplot(0:max_length,
          setcolorder(dcast.data.table(
            data[posting_rel>=0&ever_paid==1,mean(100*paid_full),
                 by=c("posting_rel","treatment")],
            posting_rel~treatment,value.var="V1")[,!"posting_rel",with=F],
            get_treats(data)),
          type="l",lty=1,
          col=get_treats_col(data),
          xlab="Days Since Mailing",ylab="%",lwd=2,
          main=paste("Percent of Group Having Paid in Full Given Some Payment",
                     "\n By",comment(data)[2]))
  legend("topleft",get_treats(data),lty=1,lwd=2,
         col=get_treats_col(data),bg="aliceblue")
  dev.copy(which=dev.list()["pdf"])
  dev.off(which=dev.list()["pdf"])
}; rm(data)

#Main Regressions, Tables, and Associated Predictions ####
regs<-list(reg1_act="",reg1_act_non_commercial="",reg1_act_single_owner="",reg1_act_leave_out="",
           reg1c_act="",reg1c_act_non_commercial="",reg1c_act_single_owner="",reg1c_act_leave_out="",
           reg2_act="",reg2_act_non_commercial="",reg2_act_single_owner="",reg2_act_leave_out="",
           reg2c_act="",reg2c_act_non_commercial="",reg2c_act_single_owner="",reg2c_act_leave_out="")
for (data in list(analysis_data,analysis_data_lout,analysis_data_ncomm,analysis_data_sown)){
  data_end<-data[end==1,]

  controls<-(~I(land_area/1e3)+I(years_count<=5)+council+category+
               I(exterior=="Sealed/Compromised")+I(homestead>0))
  
  #1(Ever Paid) Differs by Treatment?
  regs[[paste0("reg1_",comment(data)[1])]]<-
    zelig(ever_paid~
            relevel(treatment,ref=if (comment(data)[1]=="act_leave_out") "Leave-Out" else "Control"),
          data=data_end,model="logit",robust=T,cluster="cluster_id",cite=F)
  regs[[paste0("reg1c_",comment(data)[1])]]<-
    zelig(update(controls,ever_paid~
                   relevel(treatment,ref=if (comment(data)[1]=="act_leave_out") "Leave-Out" else "Control")*balance_quartile
                 +.+I(market_value/1e5)),
          data=data_end,model="logit",robust=T,cluster="cluster_id",cite=F)
  
  #1(Bill Paid in Full) Differs by Treatment?
  regs[[paste0("reg2_",comment(data)[1])]]<-
    zelig(paid_full~
            relevel(treatment,ref=if (comment(data)[1]=="act_leave_out") "Leave-Out" else "Control"),
          data=data_end,model="logit",robust=T,cluster="cluster_id",cite=F)
  regs[[paste0("reg2c_",comment(data)[1])]]<-
    zelig(update(controls,I(cum_treated_pmts>=total_due_at_mailing)~
                   relevel(treatment,ref=if (comment(data)[1]=="act_leave_out") "Leave-Out" else "Control")*balance_quartile
                 +.+I(market_value/1e5)),
          data=data_end,model="logit",robust=T,cluster="cluster_id",cite=F)
}; rm(data,data_end,controls)

##Regression tables

###Table: Summary of Effectiveness by Treatment & Sample
ord<-c(4,2,3,1)
print.xtable(xtable(rbindlist(lapply(list(
  analysis_data,analysis_data_ncomm,analysis_data_sown),
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
  align=paste0("|c|p{1.3cm}|p{1.3cm}|p{1.3cm}|p{2cm}|p{1.4cm}|",
               "p{1.4cm}|p{1.4cm}|p{1.4cm}|p{1.4cm}|p{1.6cm}|")),
  include.rownames=F,hline.after=c(-1,0,4,8,12),floating.environment="sidewaystable"); rm(ord)

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
  align=paste0("|c|p{1.3cm}|p{1.3cm}|p{1.3cm}|p{2cm}|p{1.4cm}|",
               "p{1.4cm}|p{1.4cm}|p{1.4cm}|p{1.4cm}|p{1.6cm}|")),
  include.rownames=F,hline.after=c(-1,0,5),floating.environment="sidewaystable"); rm(ord)

###Model I: Logistic of Ever-Paid
####Logistic Coefficients Table: Plain model
texreg(regs[paste(rep("reg1",3),
                  unlist(lapply(list(analysis_data,analysis_data_ncomm,
                                     analysis_data_sown),
                                function(x){comment(x)[1]})),sep="_")],
       custom.model.names=c("Full Sample","Non-Commercial","Sole Owner"),
       custom.coef.names=c("Intercept","Moral","Peer","Threat"),
       caption="Model I: Logistic Regressions -- Ever Paid",label="table:modelIa",
       float.pos="htbp",include.deviance=F,include.aic=F,include.bic=F)
       

####Logistic Coefficients Table: Quartile Interactions
texreg(regs[paste(rep("reg1c",3),
                  unlist(lapply(list(analysis_data,analysis_data_ncomm,
                                     analysis_data_sown),
                                function(x){comment(x)[1]})),sep="_")],
       custom.model.names=c("Full Sample","Non-Commercial","Sole Owner"),
       custom.coef.names=c("Intercept","Moral","Peer","Threat",
                           paste("Balance",c("Q2","Q3","Q4")),
                           paste0("control",
                                  c("Land Area","Owes <= 5 Years",
                                    paste("Dist.",2:10),"Hotels-Apts",
                                    "Store w. Dwelling","Commercial",
                                    "Industrial","Vacant","Sealed-Compromised",
                                    "Homestead","Prop. Val. (\\$100k)")),
                           paste0(c("Moral","Peer","Threat"),"*",
                                  rep(paste("Balance",c("Q2","Q3","Q4")),each=3))),
       omit.coef="Intercept|control",reorder.coef=c(4,5,6,1,7,10,13,2,8,11,14,3,9,12,15),
       custom.note=c("%stars. Control coefficients omitted for brevity; see Appendix."),
       caption="Model I: Logistic Regressions -- Ever Paid",include.deviance=F,
       label="table:modelIb",float.pos="htbp",include.bic=F,include.aic=F)

####Marginal Predictions at Control Means
#### via p_hat = exp(beta*x_bar)/(1+exp(beta*x_bar))
sample_means_control12c<-
  c(1,analysis_data[,median(land_area/1e3)],
    unlist(analysis_data[end==1,lapply(list(years_count<=5,council==2,
                                            council==3,council==4,council==5,
                                            council==6,council==7,council==8,council==9,
                                            council==10,category=="Hotels&Apts",
                                            category=="Store w/ Dwelling",
                                            category=="Commercial",category=="Industrial",
                                            category=="Vacant",exterior=="Sealed/Compromised",
                                            homestead>0),mean)]),
    analysis_data[end==1,median(market_value/1e5)])
log_odds_control1c<-(regs[["reg1c_act"]]$result$coefficients[c(1,8:26)] %*% sample_means_control12c)[1]

coef<-regs[["reg1c_act"]]$result$coefficients
print.xtable(
  xtable(matrix(sapply(matrix(rep(c(0,coef[2:4]),times=4),ncol=4)+
                         t(matrix(rep(c(0,coef[5:7]),times=4),ncol=4))+log_odds_control1c+
                         cbind(rep(0,4),rbind(rep(0,3),matrix(coef[27:35],ncol=3))),
                       function(x){round(100*exp(x)/(1+exp(x)),1)}),ncol=4,
                dimnames=list(c("Control","Moral","Peer","Threat"),paste0("Q",1:4))),
         caption="Model I: Marginal Predictions of Logistic Regressions -- Ever Paid",
         label="table:modelI_marg",align="lcccc"),table.placement="htbp")

###Model II: Logistic of Paid in Full
####Logistic Coefficients Table: Plain model
texreg(regs[paste(rep("reg2",3),
                  unlist(lapply(list(analysis_data,analysis_data_ncomm,
                                     analysis_data_sown),
                                function(x){comment(x)[1]})),sep="_")],
       custom.model.names=c("Full Sample","Non-Commercial","Sole Owner"),
       custom.coef.names=c("Intercept","Moral","Peer","Threat"),
       caption="Model II: Logistic Regressions -- Paid in Full",label="table:modelIIa",
       float.pos="htbp",include.deviance=F,include.aic=F,include.bic=F)

####Logistic Coefficients Table: Quartile Interactions
texreg(regs[paste(rep("reg2c",3),
                  unlist(lapply(list(analysis_data,analysis_data_ncomm,
                                     analysis_data_sown),
                                function(x){comment(x)[1]})),sep="_")],
       custom.model.names=c("Full Sample","Non-Commercial","Sole Owner"),
       custom.coef.names=c("Intercept","Moral","Peer","Threat",
                           paste("Balance",c("Q2","Q3","Q4")),
                           paste0("control",
                                  c("Land Area","Owes <= 5 Years",
                                    paste("Dist.",2:10),"Hotels-Apts",
                                    "Store w. Dwelling","Commercial",
                                    "Industrial","Vacant","Sealed-Compromised",
                                    "Homestead","Prop. Val. (\\$100k)")),
                           paste0(c("Moral","Peer","Threat"),"*",
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
                dimnames=list(c("Control","Moral","Peer","Threat"),paste0("Q",1:4))),
         caption="Model II: Marginal Predictions of Logistic Regressions -- Paid in Full",
         label="table:modelII_marg",align="lcccc"),table.placement="htbp")
rm(regs,coef,list=ls(pattern="_control"))

#Time Series Analysis by Quartile ####

for (data in list(analysis_data,analysis_data_lout,analysis_data_ncomm,analysis_data_sown)){
  quartile_cutoffs<-round(data[,quantile(total_due_at_mailing,probs=c(.25,.5,.75))],-2)
  
  setkey(data,balance_quartile,treatment)
  avg_pmts_by_q<-
    dcast.data.table(data[posting_rel>=0,mean(cum_treated_pmts),
                          by=c("balance_quartile","treatment","posting_rel")],
                     posting_rel~balance_quartile+treatment,value.var="V1")
  uplim<-max(avg_pmts_by_q[,!"posting_rel",with=F])
  ###Cumulative Payments by Treatment (normalized by group size)
  graphics.off()
  dev.new()
  pdf(paste0("./papers_presentations/images/analysis/",comment(data)[1],
             "/time_series_average_payments_by_quartile_",comment(data)[1],".pdf"))
  dev.set(which=dev.list()["RStudioGD"])
  layout(mat=matrix(c(1,3,5,2,4,5),nrow=3),heights=c(.4,.4,.2))
  par(oma=c(0,0,3,0))
  par(mar=c(0,4.1,4.1,0))
  matplot(0:max_length,
          avg_pmts_by_q[,paste0("1_",get_treats(data)),with=F],
          type="l",lty=1,col=get_treats_col(data),ylim=c(0,uplim),
          xlab="",ylab="$",lwd=2,main="",xaxt="n")
  text(max_length/2,.9*uplim,
       paste0("1st Quartile: \n",
              paste0("Below $",quartile_cutoffs[["25%"]])))
  
  par(mar=c(0,0,4.1,2.1))
  matplot(0:max_length,
          avg_pmts_by_q[,paste0("2_",get_treats(data)),with=F],
          type="l",lty=1,col=get_treats_col(data),ylim=c(0,uplim),
          xlab="",ylab="",lwd=2,main="",yaxt="n",xaxt="n")
  text(max_length/2,.9*uplim,
       paste0("2nd Quartile: \n",
              paste0("$",quartile_cutoffs[["25%"]],
                     "-$",quartile_cutoffs[["50%"]])))
  
  par(mar=c(5.1,4.1,0,0))
  matplot(0:max_length,
          avg_pmts_by_q[,paste0("3_",get_treats(data)),with=F],
          type="l",lty=1,col=get_treats_col(data),ylim=c(0,uplim),
          xlab="Days Since Mailing",ylab="$",lwd=2,main="")
  text(max_length/2,.9*uplim,
       paste0("3rd Quartile: \n",
              paste0("$",quartile_cutoffs[["50%"]],
                     "-$",quartile_cutoffs[["75%"]])))
  
  par(mar=c(5.1,0,0,2.1))
  matplot(0:max_length,
          avg_pmts_by_q[,paste0("4_",get_treats(data)),with=F],
          type="l",lty=1,col=get_treats_col(data),ylim=c(0,uplim),
          xlab="Days Since Mailing",ylab="",lwd=2,main="",yaxt="n")
  text(max_length/2,.9*uplim,
       paste0("4th Quartile: \n",
              paste0("Above $",quartile_cutoffs[["75%"]])))
  
  par(mar=c(0,0,0,0))
  plot(1,type="n",axes=F,xlab="",ylab="")
  legend("top",legend=get_treats(data),lty=1,lwd=2,
         col=get_treats_col(data),horiz=T,inset=0)
  mtext(paste("Normalized Cumulative Payments \n",
              "By Balance Quartile,",comment(data)[2]),
        side=3,line=-2,outer=T)
  dev.copy(which=dev.list()["pdf"])
  dev.off(which=dev.list()["pdf"])
  rm(uplim,avg_pmts_by_q)
  
  ##DRAW-DOWN OF TOTAL BALANCE
  dummy<-copy(data)
  dummy[,total_due_at_mailing_grp_q:=sum(current_balance[posting_rel==0]),by=.(treatment,balance_quartile)]
  assign("data",dummy)
  
  debt_drawdown_q<-
    dcast.data.table(data[posting_rel>=0,
                          sum(100*current_balance/total_due_at_mailing_grp_q),
                          by=c("balance_quartile","treatment","posting_rel")],
                     posting_rel~balance_quartile+treatment,value.var="V1")
  graphics.off()
  dev.new()
  pdf(paste0("./papers_presentations/images/analysis/",comment(data)[1],
             "/time_series_debt_paydown_by_quartile_",comment(data)[1],".pdf"))
  dev.set(which=dev.list()["RStudioGD"])
  layout(mat=matrix(c(1,3,5,2,4,5),nrow=3),heights=c(.4,.4,.2))
  par(oma=c(0,0,3,0))
  par(mar=c(0,4.1,4.1,2.1))
  matplot(0:max_length,
          debt_drawdown_q[,paste0("1_",get_treats(data)),with=F],
          type="l",lty=1,col=get_treats_col(data),
          ylim=c(min(debt_drawdown_q[,paste0("1_",get_treats(data)),with=F]),100),
          xlab="",ylab="%",lwd=2,main="",xaxt="n")
  text(5,sum(c(3,1)/4*par("usr")[3:4]),
       paste0("1st Quartile: \n",
              paste0("Below $",quartile_cutoffs[["25%"]])))
  
  par(mar=c(0,4.1,4.1,2.1))
  matplot(0:max_length,
          debt_drawdown_q[,paste0("2_",get_treats(data)),with=F],
          type="l",lty=1,col=get_treats_col(data),
          ylim=c(min(debt_drawdown_q[,paste0("2_",get_treats(data)),with=F]),100),
          xlab="",ylab="%",lwd=2,main="",xaxt="n")
  text(5,sum(c(3,1)/4*par("usr")[3:4]),
       paste0("2nd Quartile: \n",
              paste0("$",quartile_cutoffs[["25%"]],
                     "-$",quartile_cutoffs[["50%"]])))
  
  par(mar=c(5.1,4.1,0,2.1))
  matplot(0:max_length,
          debt_drawdown_q[,paste0("3_",get_treats(data)),with=F],
          type="l",lty=1,col=get_treats_col(data),
          ylim=c(min(debt_drawdown_q[,paste0("3_",get_treats(data)),with=F]),100),
          xlab="Days Since Mailing",ylab="%",lwd=2,main="")
  text(5,sum(c(3,1)/4*par("usr")[3:4]),
       paste0("3rd Quartile: \n",
              paste0("$",quartile_cutoffs[["50%"]],
                     "-$",quartile_cutoffs[["75%"]])))
  
  par(mar=c(5.1,4.1,0,2.1))
  matplot(0:max_length,
          debt_drawdown_q[,paste0("4_",get_treats(data)),with=F],
          type="l",lty=1,col=get_treats_col(data),
          ylim=c(min(debt_drawdown_q[,paste0("4_",get_treats(data)),with=F]),100),
          xlab="Days Since Mailing",ylab="%",lwd=2,main="")
  text(5,sum(c(3,1)/4*par("usr")[3:4]),
       paste0("4th Quartile: \n",
              paste0("Above $",quartile_cutoffs[["75%"]])))
  
  par(mar=c(0,0,0,0))
  plot(1,type="n",axes=F,xlab="",ylab="")
  legend("top",legend=get_treats(data),lty=1,lwd=2,
         col=get_treats_col(data),horiz=T,inset=0)
  mtext(paste("Percentage of Mailing Day Debt Owed \n",
              "By Balance Quartile,",comment(data)[2]),
        side=3,line=-2,outer=T)
  dev.copy(which=dev.list()["pdf"])
  dev.off(which=dev.list()["pdf"])
  rm(debt_drawdown_q)
  
  ##Percent Ever Paid by treatment
  pct_ever_paid_q<-
    dcast.data.table(data[posting_rel>=0,mean(100*ever_paid),
                          by=list(balance_quartile,treatment,posting_rel)],
                     posting_rel~balance_quartile+treatment,value.var="V1")
  
  uplim<-max(pct_ever_paid_q[,!"posting_rel",with=F])
  
  graphics.off()
  dev.new()
  pdf(paste0("./papers_presentations/images/analysis/",comment(data)[1],
             "/time_series_pct_ever_paid_by_quartile_",comment(data)[1],".pdf"))
  dev.set(which=dev.list()["RStudioGD"])
  layout(mat=matrix(c(1,3,5,2,4,5),nrow=3),heights=c(.4,.4,.2))
  par(oma=c(0,0,3,0))
  par(mar=c(0,4.1,4.1,2.1))
  matplot(0:max_length,
          pct_ever_paid_q[,paste0("1_",get_treats(data)),with=F],
          type="l",lty=1,col=get_treats_col(data),
          ylim=c(0,max(pct_ever_paid_q[,paste0("1_",get_treats(data)),with=F])),
          xlab="",ylab="%",lwd=2,main="",xaxt="n")
  text(7,.75*par("usr")[4],
       paste("1st Quartile: \n",
             paste0("Below $",quartile_cutoffs[["25%"]])))
  
  par(mar=c(0,4.1,4.1,2.1))
  matplot(0:max_length,
          pct_ever_paid_q[,paste0("2_",get_treats(data)),with=F],
          type="l",lty=1,col=get_treats_col(data),
          ylim=c(0,max(pct_ever_paid_q[,paste0("2_",get_treats(data)),with=F])),
          xlab="",ylab="%",lwd=2,main="",xaxt="n")
  text(7,.75*par("usr")[4],
       paste("2nd Quartile: \n",
             paste0("$",quartile_cutoffs[["25%"]],
                    "-$",quartile_cutoffs[["50%"]])))
  
  par(mar=c(5.1,4.1,0,2.1))
  matplot(0:max_length,
          pct_ever_paid_q[,paste0("3_",get_treats(data)),with=F],
          type="l",lty=1,col=get_treats_col(data),
          ylim=c(0,max(pct_ever_paid_q[,paste0("3_",get_treats(data)),with=F])),
          xlab="Days Since Mailing",ylab="%",lwd=2,main="")
  text(7,.75*par("usr")[4],
       paste("3rd Quartile: \n",
             paste0("$",quartile_cutoffs[["50%"]],
                    "-$",quartile_cutoffs[["75%"]])))
  
  par(mar=c(5.1,4.1,0,2.1))
  matplot(0:max_length,
          pct_ever_paid_q[,paste0("4_",get_treats(data)),with=F],
          type="l",lty=1,col=get_treats_col(data),
          ylim=c(0,max(pct_ever_paid_q[,paste0("4_",get_treats(data)),with=F])),
          xlab="Days Since Mailing",ylab="%",lwd=2,main="")
  text(7,.75*par("usr")[4],
       paste("4th Quartile: \n",
             paste0("Above $",quartile_cutoffs[["75%"]])))
  
  par(mar=c(0,0,0,0))
  plot(1,type="n",axes=F,xlab="",ylab="")
  legend("top",legend=get_treats(data),lty=1,lwd=2,
         col=get_treats_col(data),horiz=T,inset=0)
  mtext(paste("Percent of Group Ever Having Paid \n",
              "By Balance Quartile,",comment(data)[2]),
        side=3,line=-2,outer=T)
  dev.copy(which=dev.list()["pdf"])
  dev.off(which=dev.list()["pdf"])
  rm(uplim,pct_ever_paid_q)
  
  ##Percent Paid in Full by treatment
  pct_paid_full_q<-
    dcast.data.table(data[posting_rel>=0,mean(100*paid_full),
                          by=list(balance_quartile,treatment,posting_rel)],
                     posting_rel~balance_quartile+treatment,value.var="V1")
  
  graphics.off()
  dev.new()
  pdf(paste0("./papers_presentations/images/analysis/",comment(data)[1],
             "/time_series_pct_paid_full_by_quartile_",comment(data)[1],".pdf"))
  dev.set(which=dev.list()["RStudioGD"])
  layout(mat=matrix(c(1,3,5,2,4,5),nrow=3),heights=c(.4,.4,.2))
  par(oma=c(0,0,3,0))
  par(mar=c(0,4.1,4.1,2.1))
  matplot(0:max_length,
          pct_paid_full_q[,paste0("1_",get_treats(data)),with=F],
          type="l",lty=1,col=get_treats_col(data),
          ylim=c(0,max(pct_paid_full_q[,paste0("1_",get_treats(data)),with=F])),
          xlab="",ylab="%",lwd=2,main="",xaxt="n")
  text(7,.75*par("usr")[4],
       paste("1st Quartile: \n",
             paste0("Below $",quartile_cutoffs[["25%"]])))
  
  par(mar=c(0,4.1,4.1,2.1))
  matplot(0:max_length,
          pct_paid_full_q[,paste0("2_",get_treats(data)),with=F],
          type="l",lty=1,col=get_treats_col(data),
          ylim=c(0,max(pct_paid_full_q[,paste0("2_",get_treats(data)),with=F])),
          xlab="",ylab="%",lwd=2,main="",xaxt="n")
  text(7,.75*par("usr")[4],
       paste("2nd Quartile: \n",
             paste0("$",quartile_cutoffs[["25%"]],
                    "-$",quartile_cutoffs[["50%"]])))
  
  par(mar=c(5.1,4.1,0,2.1))
  matplot(0:max_length,
          pct_paid_full_q[,paste0("3_",get_treats(data)),with=F],
          type="l",lty=1,col=get_treats_col(data),
          ylim=c(0,max(pct_paid_full_q[,paste0("3_",get_treats(data)),with=F])),
          xlab="Days Since Mailing",ylab="%",lwd=2,main="")
  text(7,.75*par("usr")[4],
       paste("3rd Quartile: \n",
             paste0("$",quartile_cutoffs[["50%"]],
                    "-$",quartile_cutoffs[["75%"]])))
  
  par(mar=c(5.1,4.1,0,2.1))
  matplot(0:max_length,
          pct_paid_full_q[,paste0("4_",get_treats(data)),with=F],
          type="l",lty=1,col=get_treats_col(data),
          ylim=c(0,max(pct_paid_full_q[,paste0("4_",get_treats(data)),with=F])),
          xlab="Days Since Mailing",ylab="%",lwd=2,main="")
  text(7,.75*par("usr")[4],
       paste("4th Quartile: \n",
             paste0("Above $",quartile_cutoffs[["75%"]])))
  
  par(mar=c(0,0,0,0))
  plot(1,type="n",axes=F,xlab="",ylab="")
  legend("top",legend=get_treats(data),lty=1,lwd=2,
         col=get_treats_col(data),horiz=T,inset=0)
  mtext(paste("Percent of Group Having Paid in Full \n",
              "By Balance Quartile,",comment(data)[2]),
        side=3,line=-2,outer=T)
  dev.copy(which=dev.list()["pdf"])
  dev.off(which=dev.list()["pdf"])
  rm(pct_paid_full_q)
}; rm(dummy,quartile_cutoffs)