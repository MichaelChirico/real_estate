#Data Analysis
#Philadelphia Real Estate Tax Evasion
#Michael Chirico
#April 3, 2015

#Setup: Packages, Working Directory, Etc.####
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
library(erer)
library(survival)
library(reshape2)
library(sandwich)
library(xtable)
library(rms)
library(Zelig)
setEPS()

#Convenient Functions ####
prop.table2<-function(...,exclude=if(useNA=="no")c(NA,NaN),useNA=c("no","ifany", "always"),
                      dnn=list.names(...),deparse.level=1,margin=NULL){
  list.names <- function(...) {
    l <- as.list(substitute(list(...)))[-1L]
    nm <- names(l)
    fixup <- if (is.null(nm)) 
      seq_along(l)
    else nm == ""
    dep <- vapply(l[fixup], function(x) switch(deparse.level + 
                                                 1, "", if (is.symbol(x)) as.character(x) else "", 
                                               deparse(x, nlines = 1)[1L]), "")
    if (is.null(nm)) 
      dep
    else {
      nm[fixup] <- dep
      nm
    }
  }
  if (!missing(exclude) && is.null(exclude)) 
    useNA <- "always"
  useNA <- match.arg(useNA)
  prop.table(table(...,exclude=exclude,useNA=useNA,dnn=dnn,deparse.level=1),margin=margin)
}
create_quantiles<-function(x,num,right=F,include.lowest=T){
  cut(x,breaks=quantile(x,probs=seq(0,1,by=1/num)),labels=1:num,right=right,include.lowest=include.lowest)
}

extract.zelig<-function (model, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE, 
                         include.deviance = TRUE, include.nobs = TRUE, include.rsquared = TRUE, 
                         include.adjrs = TRUE, include.fstatistic = TRUE, ...) {
  s <- summary(model, ...)
  if ("relogit" %in% class(model) || "logit" %in% class(model) || 
        "ls" %in% class(model) || "probit" %in% class(model) || 
        "ologit" %in% class(model)) {
    coefficient.names <- rownames(s$coef)
    coefficients <- s$coef[, 1]
    standard.errors <- s$coef[, 2]
    if ("ologit" %in% class(model)) {
      tval <- s$coef[, 3]
      significance <- 2 * pt(-abs(tval), s$df.residual)
    }
    else {
      significance <- s$coef[, 4]
    }
    gof <- numeric()
    gof.names <- character()
    gof.decimal <- logical()
    if (include.aic == TRUE) {
      aic <- AIC(model)
      gof <- c(gof, aic)
      gof.names <- c(gof.names, "AIC")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.bic == TRUE) {
      bic <- BIC(model)
      gof <- c(gof, bic)
      gof.names <- c(gof.names, "BIC")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.loglik == TRUE) {
      lik <- logLik(model)[1]
      gof <- c(gof, lik)
      gof.names <- c(gof.names, "Log Likelihood")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.deviance == TRUE) {
      dev <- s$deviance
      if (!is.null(dev)) {
        gof <- c(gof, dev)
        gof.names <- c(gof.names, "Deviance")
        gof.decimal <- c(gof.decimal, TRUE)
      }
    }
    if (include.nobs == TRUE) {
      n <- nrow(model$data)
      gof <- c(gof, n)
      gof.names <- c(gof.names, "Num. obs.")
      gof.decimal <- c(gof.decimal, FALSE)
    }
    if (include.rsquared == TRUE) {
      rs <- s$r.squared
      if (!is.null(rs)) {
        gof <- c(gof, rs)
        gof.names <- c(gof.names, "R$^2$")
        gof.decimal <- c(gof.decimal, TRUE)
      }
    }
    if (include.adjrs == TRUE) {
      adj <- s$adj.r.squared
      if (!is.null(adj)) {
        gof <- c(gof, adj)
        gof.names <- c(gof.names, "Adj. R$^2$")
        gof.decimal <- c(gof.decimal, TRUE)
      }
    }
    if (include.fstatistic == TRUE) {
      fstat <- s$fstatistic[[1]]
      if (!is.null(fstat)) {
        gof <- c(gof, fstat)
        gof.names <- c(gof.names, "F statistic")
        gof.decimal <- c(gof.decimal, TRUE)
      }
    }
    tr <- createTexreg(coef.names = coefficient.names, coef = coefficients, 
                       se = standard.errors, pvalues = significance, gof.names = gof.names, 
                       gof = gof, gof.decimal = gof.decimal)
    return(tr)
  }
  else if ("mlogit" %in% class(model)) {
    coefficient.names <- rownames(s@coef3)
    coefficients <- s@coef3[, 1]
    standard.errors <- s@coef3[, 2]
    zval <- s@coef3[, 3]
    significance <- 2 * pnorm(abs(zval), lower.tail = FALSE)
    gof <- numeric()
    gof.names <- character()
    gof.decimal <- logical()
    if (include.loglik == TRUE) {
      lik <- logLik(model)[1]
      gof <- c(gof, lik)
      gof.names <- c(gof.names, "Log Likelihood")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.deviance == TRUE) {
      dev <- deviance(s)
      if (!is.null(dev)) {
        gof <- c(gof, dev)
        gof.names <- c(gof.names, "Deviance")
        gof.decimal <- c(gof.decimal, TRUE)
      }
    }
    if (include.nobs == TRUE) {
      n <- nrow(model$data)
      gof <- c(gof, n)
      gof.names <- c(gof.names, "Num. obs.")
      gof.decimal <- c(gof.decimal, FALSE)
    }
    tr <- createTexreg(coef.names = coefficient.names, coef = coefficients, 
                       se = standard.errors, pvalues = significance, gof.names = gof.names, 
                       gof = gof, gof.decimal = gof.decimal)
    return(tr)
  }
  else if ("tobit" %in% class(model)) {
    coefficient.names <- rownames(s$table)
    coefficients <- s$table[,1]
    standard.errors <- s$table[,2]
    significance <- s$table[,5]
    gof <- numeric()
    gof.names <- character()
    gof.decimal <- logical()
    if (include.aic == TRUE) {
      aic <- AIC(model)
      gof <- c(gof, aic)
      gof.names <- c(gof.names, "AIC")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.loglik == TRUE) {
      lik <- logLik(model)[1]
      gof <- c(gof, lik)
      gof.names <- c(gof.names, "Log Likelihood")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.nobs == TRUE) {
      n <- nrow(model$data)
      gof <- c(gof, n)
      gof.names <- c(gof.names, "Num. obs.")
      gof.decimal <- c(gof.decimal, FALSE)
    }
    tr <- createTexreg(coef.names = coefficient.names, coef = coefficients, 
                       se = standard.errors, pvalues = significance, gof.names = gof.names, 
                       gof = gof, gof.decimal = gof.decimal)
    return(tr)
  }
  else {
    stop(paste("Only the following Zelig models are currently supported:", 
               "logit, ls, mlogit, ologit, probit, relogit, tobit."))
  }
}
setMethod("extract", signature = className("zelig", "Zelig"),definition = extract.zelig)

#Set up Analysis Data Sets ####
analysis_data<-fread("analysis_file.csv",colClasses=c(date_of_first_payment="character"))[fidelity_flag==0,]
#CLUSTERS DEFINED AT THE OWNER LEVEL
#OWNER DEFINED AS ANYONE WITH IDENTICAL NAME & MAILING ADDRESS
setkey(setkey(analysis_data,legal_name,mail_address
              )[unique(analysis_data,by=c("legal_name","mail_address")
              )[,I:=.I],cluster_id:=I],opa_no,posting_rel)

###tidying up data classifications
factors<-c("cycle","treatment_int","treatment_act","category","exterior",
           "abate_exempt_code","bldg_group","bldg_category","council",
           "case_status2")
analysis_data[,(factors):=lapply(.SD,as.factor),.SDcols=factors]
#since read as strings, order is out of whack
analysis_data[,council:=factor(council,levels=levels(council)[c(1,3:10,2)])]
analysis_data[,category:=factor(category,levels=levels(category)[c(4,2,5,1,3,6)])]
dates<-c("posting","mailing_day","date_latest_pmt","max_period","min_period")
analysis_data[,(dates):=lapply(.SD,as.Date),.SDcols=dates]
setnames(analysis_data,"treatment_act","treatment")
rm(dates,factors)

##create some variables which can be defined on the main data set
## and passed through without change to the subsamples
max_length<-min(analysis_data[,max(as.integer(posting_rel)),by=cycle]$V1)
min_length<-max(analysis_data[,min(as.integer(posting_rel)),by=cycle]$V1)

analysis_data[,years_cut:=cut(years_count,breaks=c(0,1,2,5,40),
                              include.lowest=T,labels=c(1:2,"3-5",">5"))]
analysis_data[,rooms_cut:=cut(rooms,breaks=c(0,5,6,20),
                              include.lowest=T,labels=c("0-5","6",">6"))]

##DEFINE DATA SETS FOR THE SUBSAMPLES
###NONCOMMERCIAL PROPERTIES
analysis_data_ncomm<-analysis_data[commercial==0,]
###SINGLE OWNER PORcodPERTIES--NO MATCH ON LEGAL_NAME/MAILING_ADDRESS
analysis_data_sown<-analysis_data[,count:=.N,by=.(legal_name,mail_address)
                                  ][count-min(count)==0,][,count:=NULL]
analysis_data[,count:=NULL]

###Tags & data descriptions for file naming for each data set
comment(analysis_data)<-c("act","Empirical Treatment","analysis_data")
comment(analysis_data_ncomm)<-c("act_non_commercial","Empirical Treatment, Non-Commercial Properties","analysis_data_ncomm")
comment(analysis_data_sown)<-c("act_single_owner","Empirical Treatment, Single-Owner Properties","analysis_data_sown")

for (dt in list(analysis_data,analysis_data_ncomm,analysis_data_sown)){
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
}

#Tests of Randomness ####

##TESTS OF BALANCE ON OBSERVABLES
p_exp<-prop.table2(analysis_data[!duplicated(cycle),treatment])
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
                                    by=treatment_int]$V1[c(4,2,3,1)]),
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
for (data in list(analysis_data,analysis_data_ncomm,analysis_data_sown)){
  ##Cumulative Payments by Treatment (normalized by group size)
  graphics.off()
  dev.new()
  pdf(paste0("./papers_presentations/images/analysis/",comment(data)[1],
             "/time_series_average_payments_",comment(data)[1],".pdf"))
  dev.set(which=dev.list()["RStudioGD"])
  matplot(0:max_length,
          dcast.data.table(data[posting_rel>=0,mean(cum_treated_pmts),
                                by=c("posting_rel","treatment")],
                           posting_rel~treatment,value.var="V1")[,!"posting_rel",with=F],
          type="l",lty=1,col=c("black","blue","green","red"),
          xlab="Days Since Mailing",ylab="$",lwd=2,
          main=paste("Cumulative Average Payments Since Mail Date \n",
                     "By",comment(data)[2]))
  legend("topleft",c("Threat","Moral","Peer","Control"),lty=1,lwd=2,
         col=c("red","blue","green","black"),bg="aliceblue")
  dev.copy(which=dev.list()["pdf"])
  dev.off(which=dev.list()["pdf"])
  
  ##DRAW-DOWN OF TOTAL BALANCE (GIVEN NOV 3 DATE OF OCTOBER DELINQUENCY DATA)
  graphics.off()
  dev.new()
  pdf(paste0("./papers_presentations/images/analysis/",comment(data)[1],
             "/time_series_debt_paydown_",comment(data)[1],".pdf"))
  dev.set(which=dev.list()["RStudioGD"])
  matplot(0:max_length,
          dcast.data.table(data[posting_rel>=0,
                                sum(100*current_balance/total_due_at_mailing_grp),
                                by=c("posting_rel","treatment")],
                           posting_rel~treatment,value.var="V1")[,!"posting_rel",with=F],
          type="l",lty=1,col=c("black","blue","green","red"),
          xlab="Days Since Mailing",ylab="%",lwd=2,
          main=paste("Percentage of Mailing Day Debt Owed \n",
                     "By",comment(data)[2]))
  legend("bottomleft",c("Threat","Moral","Peer","Control"),lty=1,
         col=c("red","blue","green","black"),bg="aliceblue")
  dev.copy(which=dev.list()["pdf"])
  dev.off(which=dev.list()["pdf"])
  
  ##Percent Ever Paid by treatment
  graphics.off()
  dev.new()
  pdf(paste0("./papers_presentations/images/analysis/",comment(data)[1],
             "/time_series_debt_paydown_",comment(data)[1],".pdf"))
  dev.set(which=dev.list()["RStudioGD"])
  matplot(0:max_length,
          dcast.data.table(data[posting_rel>=0,mean(100*ever_paid),
                                by=c("posting_rel","treatment")],
                           posting_rel~treatment,value.var="V1")[,!"posting_rel",with=F],
          type="l",lty=1,col=c("black","blue","green","red"),
          xlab="Days Since Mailing",ylab="%",lwd=2,
          main=paste("Percent of Group Ever Having Paid",
                     "\n By",comment(data)[2]))
  legend("topleft",legend=c("Threat","Moral","Peer","Control"),
         lwd=2,col=c("red","blue","green","black"),bg="aliceblue")
  dev.copy(which=dev.list()["pdf"])
  dev.off(which=dev.list()["pdf"])
  
  ##Percent Paid in Full by treatment
  graphics.off()
  dev.new()
  pdf(paste0("./papers_presentations/images/analysis/",comment(data)[1],
             "/time_series_debt_paydown_",comment(data)[1],".pdf"))
  dev.set(which=dev.list()["RStudioGD"])
  matplot(0:max_length,
          dcast.data.table(data[posting_rel>=0,mean(100*paid_full),
                                by=c("posting_rel","treatment")],
                           posting_rel~treatment,value.var="V1")[,!"posting_rel",with=F],
          type="l",lty=1,col=c("black","blue","green","red"),
          xlab="Days Since Mailing",ylab="%",lwd=2,
          main=paste("Percent of Group Having Paid in Full",
                     "\n By",comment(data)[2]))
  legend("topleft",legend=c("Threat","Moral","Peer","Control"),
         lwd=2,col=c("red","blue","green","black"),bg="aliceblue")
  dev.copy(which=dev.list()["pdf"])
  dev.off(which=dev.list()["pdf"])
  
  ##Percent Paid in Full Conditional on Payment by treatment
  graphics.off()
  dev.new()
  pdf(paste0("./papers_presentations/images/analysis/",comment(data)[1],
             "/time_series_debt_paydown_",comment(data)[1],".pdf"))
  dev.set(which=dev.list()["RStudioGD"])
  matplot(0:max_length,
          dcast.data.table(data[posting_rel>=0&ever_paid==1,mean(100*paid_full),
                                by=c("posting_rel","treatment")],
                           posting_rel~treatment,value.var="V1")[,!"posting_rel",with=F],
          type="l",lty=1,col=c("black","blue","green","red"),
          xlab="Days Since Mailing",ylab="%",lwd=2,
          main=paste("Percent of Group Having Paid in Full Given Some Payment",
                     "\n By",comment(data)[2]))
  legend("bottomright",legend=c("Threat","Moral","Peer","Control"),
         lwd=2,col=c("red","blue","green","black"),bg="aliceblue")
  dev.copy(which=dev.list()["pdf"])
  dev.off(which=dev.list()["pdf"])
}

#Main Regressions, Tables, and Associated Predictions ####
regs<-list(reg1_act="",reg1_act_non_commercial="",reg1_act_single_owner="",
           reg1c_act="",reg1c_act_non_commercial="",reg1c_act_single_owner="",
           reg2_act="",reg2_act_non_commercial="",reg2_act_single_owner="",
           reg2c_act="",reg2c_act_non_commercial="",reg2c_act_single_owner="",
           reg3_act="",reg3_act_non_commercial="",reg3_act_single_owner="",
           reg3c_act="",reg3c_act_non_commercial="",reg3c_act_single_owner="",
           reg4_act="",reg4_act_non_commercial="",reg4_act_single_owner="",
           reg4c_act="",reg4c_act_non_commercial="",reg4c_act_single_owner="")
for (data in list(analysis_data,analysis_data_ncomm,analysis_data_sown)){
  data_end<-data[end==1,]

  #Can't include category=="Commercial" when no such properties exist
  if (comment(data)[1]=="act_non_commercial"){
    controls<-(~I(land_area/1e3)+I(years_count<=5)+council+
                 I(category=="Hotels&Apts")+I(category=="Store w/ Dwelling")+
                 I(category=="Industrial")+I(category=="Vacant")+
                 I(exterior=="Sealed/Compromised")+I(homestead>0))
  }else{
    controls<-(~I(land_area/1e3)+I(years_count<=5)+council+
                 I(category=="Hotels&Apts")+I(category=="Store w/ Dwelling")+
                 I(category=="Commercial")+I(category=="Industrial")+I(category=="Vacant")+
                 I(exterior=="Sealed/Compromised")+I(homestead>0))
  }
  
  #1(Ever Paid) Differs by Treatment?
  regs[[paste0("reg1_",comment(data)[1])]]<-
    zelig(ever_paid~relevel(treatment,ref="Control")*balance_quartile,
          data=data_end,model="logit",robust=T,cluster="cluster_id",cite=F)
  regs[[paste0("reg1c_",comment(data)[1])]]<-
    zelig(update(controls,ever_paid~
                   relevel(treatment,ref="Control")*balance_quartile
                 +.+I(market_value/1e5)),
          data=data_end,model="logit",robust=T,cluster="cluster_id",cite=F)
  
  #1(Bill Paid in Full) Differs by Treatment?
  regs[[paste0("reg2_",comment(data)[1])]]<-
    zelig(paid_full~relevel(treatment,ref="Control")*balance_quartile,
          data=data_end,model="logit",robust=T,cluster="cluster_id",cite=F)
  regs[[paste0("reg2c_",comment(data)[1])]]<-
    zelig(update(controls,I(cum_treated_pmts>=total_due_at_mailing)~
                   relevel(treatment,ref="Control")*balance_quartile
                 +.+I(market_value/1e5)),
          data=data_end,model="logit",robust=T,cluster="cluster_id",cite=F)
  
  #Eventual Amount Paid / Assessed Value Differs by Treatment?
  regs[[paste0("reg3_",comment(data)[1])]]<-
    zelig(cum_treated_pmts~
            relevel(treatment,ref="Control")*mv_quartile,
          data=data_end,model="tobit",below=0,above=Inf,
          robust=T,cluster="cluster_id",cite=F)
  regs[[paste0("reg3c_",comment(data)[1])]]<-
    zelig(update(controls,cum_treated_pmts~
                   relevel(treatment,ref="Control")*mv_quartile
                 +.+I(total_due_at_mailing/1e3)),
          data=data_end,model="tobit",robust=T,cluster="cluster_id",cite=F)
  
  #Eventual Amount Paid / Amount Owed Differs by Treatment? By Treatment by Amount Owed?
  regs[[paste0("reg4_",comment(data)[1])]]<-
    zelig(cum_treated_pmts~
            relevel(treatment,ref="Control")*balance_quartile,
          data=data_end,model="tobit",robust=T,cluster="cluster_id",cite=F)
  regs[[paste0("reg4c_",comment(data)[1])]]<-
    zelig(update(controls,cum_treated_pmts~
                   relevel(treatment,ref="Control")*balance_quartile
                 +.+I(market_value/1e5)),
          data=data_end,model="tobit",robust=T,cluster="cluster_id",cite=F)
}

##Regression tables  
###Model I: Logistic of Ever-Paid
####Logistic Coefficients Table
texreg(regs[paste(rep(c("reg1","reg1c"),3),
                  rep(unlist(lapply(list(analysis_data,analysis_data_ncomm,
                                         analysis_data_sown),
                                    function(x){comment(x)[1]})),each=2),sep="_")],
       custom.model.names=paste0(rep(c("Full Sample","Non-Comm.","Sole Owner"),each=2),
                                 c(""," (II)")),
       custom.coef.names=c("Intercept","Moral","Peer","Threat",
                           paste("Balance",c("Q2","Q3","Q4")),
                           paste0(c("Moral","Peer","Threat"),"*",
                                  rep(paste("Bal.",c("Q2","Q3","Q4")),each=3)),
                           paste0("control",
                                  c("Land Area","Owes <= 5 Years",
                                    paste("Dist.",2:10),"Hotels-Apts",
                                    "Store w. Dwelling","Commercial",
                                    "Industrial","Vacant","Sealed-Compromised",
                                    "Homestead")),"Prop. Val. (\\$100k)"),
       groups=list("Treatment Group"=2:4),omit.coef="control",
       caption="Model I: Logistic Regressions -- Ever Paid",include.deviance=F,
       label=c("table:modelI"),float.pos="htbp",include.bic=F,include.aic=F,
       custom.note=c("%stars. Control coefficients omitted for brevity; see Appendix"))

####Marginal Predictions at Control Means
#### via p_hat = exp(beta*x_bar)/(1+exp(beta*x_bar))
sample_means_control12c<-
  c(1,unlist(analysis_data[end==1,lapply(list(total_due_at_mailing,land_area,years_count<=5,
                                   council==2,council==3,council==4,council==5,
                                   council==6,council==7,council==8,council==9,
                                   council==10,category=="Hotels&Apts",
                                   category=="Store w/ Dwelling",
                                   category=="Commercial",category=="Industrial",
                                   category=="Vacant",exterior=="Sealed/Compromised",
                                   homestead>0,market_value),mean)]))
log_odds_control1c<-regs[["reg1c_act"]]$result$coefficients[c(1,5:24)] %*% sample_means_control12c
print.xtable(
  xtable(matrix(cbind(lapply(cbind(rep(1,4),rbind(rep(0,3),diag(3)[,c(2,3,1)]),
                                   rep(analysis_data[end==1,mean(total_due_at_mailing)],4),
                                   rbind(rep(0,3),
                                         analysis_data[end==1,mean(total_due_at_mailing)]*
                                           diag(3)[,c(2,3,1)])) %*% 
                               regs[["reg1_act"]]$result$coefficients,
                             function(x){round(100*exp(x)/(1+exp(x)),1)}),
                      lapply(cbind(rep(1,4),rbind(rep(0,3),diag(3)[,c(2,3,1)]),
                                   rep(analysis_data[end==1,mean(total_due_at_mailing)],4),
                                   rbind(rep(0,3),
                                         analysis_data[end==1,mean(total_due_at_mailing)]*
                                           diag(3)[,c(2,3,1)])) %*% 
                               regs[["reg1c_act"]]$result$coefficients[c(1:5,25:27)]+log_odds_control1c[1],
                             function(x){round(100*exp(x)/(1+exp(x)),1)})),
                ncol=2,dimnames=list(c("Control","Threat","Moral","Peer"),
                                     c("No Controls","With Controls"))),
         caption="Model I: Marginal Predictions of Logistic Regressions -- Ever Paid",
         label="table:modelI_marg",align="lcc"),table.placement="htbp")

###Model II: Logistic of Paid in Full
####Logistic Coefficients Table
texreg(regs[paste(rep(c("reg2","reg2c"),3),
                  rep(unlist(lapply(list(analysis_data,analysis_data_ncomm,
                                         analysis_data_sown),
                                    function(x){comment(x)[1]})),each=2),sep="_")],
       custom.model.names=paste0(rep(c("Full Sample","Non-Comm.","Sole Owner"),each=2),
                                 c(""," (II)")),
       custom.coef.names=c("Intercept","Moral","Peer","Threat",
                           paste("Balance",c("Q2","Q3","Q4")),
                           paste0(c("Moral","Peer","Threat"),"*",
                                  rep(paste("Bal.",c("Q2","Q3","Q4")),each=3)),
                           paste0("control",
                                  c("Land Area","Owes <= 5 Years",
                                    paste("Dist.",2:10),"Hotels-Apts",
                                    "Store w. Dwelling","Commercial",
                                    "Industrial","Vacant","Sealed-Compromised",
                                    "Homestead")),"Prop. Val. (\\$100k)"),
       groups=list("Treatment Group"=2:4),omit.coef="control",
       caption="Model II: Logistic Regressions -- Paid in Full",include.deviance=F,
       label=c("table:modelII"),float.pos="htbp",include.bic=F,include.aic=F,
       custom.note=c("%stars. Control coefficients omitted for brevity; see Appendix"))

####Marginal Predictions at Control Means
#### via p_hat = exp(beta*x_bar)/(1+exp(beta*x_bar))
log_odds_control2c<-regs[["reg2c_act"]]$result$coefficients[c(1,5:24)] %*% sample_means_control12c
print.xtable(
  xtable(matrix(cbind(lapply(cbind(rep(1,4),rbind(rep(0,3),diag(3)[,c(2,3,1)]),
                                   rep(analysis_data[end==1,mean(total_due_at_mailing)],4),
                                   rbind(rep(0,3),
                                         analysis_data[end==1,mean(total_due_at_mailing)]*
                                           diag(3)[,c(2,3,1)])) %*% 
                               regs[["reg2_act"]]$result$coefficients,
                             function(x){round(100*exp(x)/(1+exp(x)),1)}),
                      lapply(cbind(rep(1,4),rbind(rep(0,3),diag(3)[,c(2,3,1)]),
                                   rep(analysis_data[end==1,mean(total_due_at_mailing)],4),
                                   rbind(rep(0,3),
                                         analysis_data[end==1,mean(total_due_at_mailing)]*
                                           diag(3)[,c(2,3,1)])) %*% 
                               regs[["reg2c_act"]]$result$coefficients[c(1:5,25:27)]+log_odds_control2c[1],
                             function(x){round(100*exp(x)/(1+exp(x)),1)})),
                ncol=2,dimnames=list(c("Control","Threat","Moral","Peer"),
                                     c("No Controls","With Controls"))),
         caption="Model II: Marginal Predictions of Logistic Regressions -- Paid in Full",
         label="table:modelII_marg",align="lcc"),table.placement="htbp")

###Model III: Tobit of Paid vs. MV
texreg(regs[paste(rep(c("reg3","reg3c"),3),
                  rep(unlist(lapply(list(analysis_data,analysis_data_ncomm,
                                         analysis_data_sown),
                                    function(x){comment(x)[1]})),each=2),sep="_")],
       custom.model.names=paste0(rep(c("Full Sample","Non-Comm.","Sole Owner"),each=2),
                                 c(""," (II)")),
       custom.coef.names=c("Intercept","Moral","Peer","Threat",
                           paste("Market Value",c("Q2","Q3","Q4")),
                           paste0(c("Moral","Peer","Threat"),"*",
                                  rep(paste("MV",c("Q2","Q3","Q4")),each=3)),"Log Scale",
                           paste0("control",
                                  c("Land Area","Owes <= 5 Years",
                                    paste("Dist.",2:10),"Hotels-Apts",
                                    "Store w. Dwelling","Commercial",
                                    "Industrial","Vacant","Sealed-Compromised",
                                    "Homestead")),"Bal. Due (\\$1k)"),
       groups=list("Treatment Group"=2:4),omit.coef="control|Scale",
       caption="Model III: Tobit of Repayment vs. Market Value",
       label=c("table:modelIII"),float.pos="htbp",
       include.wald=F,include.aic=F,include.bic=F,include.deviance=F,
       custom.note=c("%stars. Control coefficients omitted for brevity; see Appendix"))

###Model IV: Tobit of Paid vs. Total Owed
texreg(regs[paste(rep(c("reg4","reg4c"),3),
                  rep(unlist(lapply(list(analysis_data,analysis_data_ncomm,
                                         analysis_data_sown),
                                    function(x){comment(x)[1]})),each=2),sep="_")],
       custom.model.names=paste0(rep(c("Full Sample","Non-Comm.","Sole Owner"),each=2),
                                 c(""," (II)")),
       custom.coef.names=c("Intercept","Moral","Peer","Threat",
                           paste("Balance ",c("Q2","Q3","Q4")),
                           paste0(c("Moral","Peer","Threat"),"*",
                                  rep(paste("Balance ",c("Q2","Q3","Q4")),each=3)),"Log Scale",
                           paste0("control",
                                  c("Land Area","Owes <= 5 Years",
                                    paste("Dist.",2:10),"Hotels-Apts",
                                    "Store w. Dwelling","Commercial",
                                    "Industrial","Vacant","Sealed-Compromised",
                                    "Homestead")),"Prop. Val. (\\$100k)"),
       groups=list("Treatment Group"=2:4),omit.coef="control|Scale",
       caption="Model IV: Tobit of Repayment vs. Total Debt",
       label=c("table:modelIV"),float.pos="htbp",
       include.wald=F,include.aic=F,include.bic=F,include.deviance=F,
       custom.note=c("%stars. Control coefficients omitted for brevity; see Appendix"))

# ###Predicting Total $ Amount of Effect
# ###  Since Moral had highest apparent effect, extrapolate to
# ###   rest of mailing cycles to estimate how much would have
# ###   been paid back if all properties (bar those that would
# ###   have been excluded from our sample) were in Moral.
# full_sample<-
#   setkey(
#     setnames(
#       fread("/media/data_drive/real_estate/prop2014.txt",
#             select=c("PARCEL","MV","CAT CD","TOT AREA","EXT COND")),
#       c("opa_no","market_value","category","land_area","exterior")),
#     opa_no)[,market_value:=as.numeric(market_value)
#             ][,land_area:=as.numeric(land_area)/100
#               ][data.table(read.dbf("./gis_data/dor_data_aug_with_zoning_base_id.dbf")
#                            )[,c("opa_no","longitude","latitude",
#                                 "zone_district","OBJECTID"):=
#                                list(paste0(0,opa_no),NULL,NULL,OBJECTID,NULL)
#                              ][,opa_no:=substr(opa_no,nchar(opa_no)-8,nchar(opa_no))],
#                 zone_district:=zone_district
#                 ][zone_district==0,zone_district:=NA
#                   ][land_area<100,land_area:=NA
#                     ][,land_area_mean:=mean(land_area,na.rm=T),
#                       by=zone_district
#                       ][is.na(land_area),land_area:=land_area_mean
#                         ][,land_area_mean:=NULL
#                           ][land_area<1e6,
#                             ][,c("category","exterior"):=lapply(.SD,as.factor),
#                               .SDcols=c("category","exterior")
#                               ][setkey(fread("/media/data_drive/real_estate/dor_data_15_oct_encrypted.csv"),
#                                        opa_no)[payment_agreement=="N"&case_status==""
#                                                &abate_exempt_code %in% c("","     ","\\    ")
#                                                &sheriff_sale=="N"&bankruptcy=="N"
#                                                &sequestration=="N"
#                                                &returned_mail_flag=="NO",
#                                                ][,.(opa_no,years_count,council,
#                                                     homestead,calc_total_due,zip)
#                                                  ][council=="",
#                                                    council:=ifelse(zip=="19130","5",
#                                                                    ifelse(zip %in% c("19125","19147"),"1",
#                                                                           ifelse(zip=="19138","8","")))
#                                                    ][,council:=factor(council,levels=1:10)]
#                                 ][!is.na(market_value),][,total_due_at_mailing:=calc_total_due]
# levels(full_sample$category)<-c("Residential","Hotels&Apts","Store w/ Dwelling",
#                                 "Commercial","Industrial","Vacant")
# levels(full_sample$exterior)<-c("N/A","N/A","New/Rehab","Above Average","Average",
#                                 "Below Average","Vacant","Sealed/Compromised")
# 
# full_sample[,treatment:=as.factor("Moral")]
# levels(full_sample$treatment)<-c("Threat","Moral","Peer","Control")
# predictions_3<-predict(reg3c,newdata=full_sample)

#Further analysis: ####

##Time Series Analysis by Quartile ####

quartile_cutoffs<-round(analysis_data[,quantile(total_due_at_mailing,probs=c(.25,.5,.75))],-2)

setkey(analysis_data,balance_quartile,treatment)
avg_pmts_by_q<-
  dcast.data.table(analysis_data[posting_rel>=0,mean(cum_treated_pmts),
                                 by=c("balance_quartile","treatment","posting_rel")],
                   posting_rel~balance_quartile+treatment,value.var="V1")
uplim<-max(avg_pmts_by_q[,!"posting_rel",with=F])
###Cumulative Payments by Treatment (normalized by group size)
graphics.off()
dev.new()
pdf(paste0("./papers_presentations/images/analysis/",
                  substr(file_suff,2,nchar(file_suff)),
                  "/time_series_average_payments_by_quartile",file_suff,".pdf"))
dev.set(which=dev.list()["RStudioGD"])
layout(mat=matrix(c(1,3,5,2,4,5),nrow=3),heights=c(.4,.4,.2))
par(oma=c(0,0,3,0))
par(mar=c(0,4.1,4.1,0))
matplot(0:max_length,
        avg_pmts_by_q[,paste0("1_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),ylim=c(0,uplim),
        xlab="",ylab="$",lwd=2,main="",xaxt="n")
text(max_length/2,.9*uplim,
     paste0("1st Quartile: \n",
            paste0("Below $",quartile_cutoffs[["25%"]])))

par(mar=c(0,0,4.1,2.1))
matplot(0:max_length,
        avg_pmts_by_q[,paste0("2_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),ylim=c(0,uplim),
        xlab="",ylab="",lwd=2,main="",yaxt="n",xaxt="n")
text(max_length/2,.9*uplim,
     paste0("2nd Quartile: \n",
            paste0("$",quartile_cutoffs[["25%"]],
                   "-$",quartile_cutoffs[["50%"]])))

par(mar=c(5.1,4.1,0,0))
matplot(0:max_length,
        avg_pmts_by_q[,paste0("3_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),ylim=c(0,uplim),
        xlab="Days Since Mailing",ylab="$",lwd=2,main="")
text(max_length/2,.9*uplim,
     paste0("3rd Quartile: \n",
            paste0("$",quartile_cutoffs[["50%"]],
                   "-$",quartile_cutoffs[["75%"]])))

par(mar=c(5.1,0,0,2.1))
matplot(0:max_length,
        avg_pmts_by_q[,paste0("4_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),ylim=c(0,uplim),
        xlab="Days Since Mailing",ylab="",lwd=2,main="",yaxt="n")
text(max_length/2,.9*uplim,
     paste0("4th Quartile: \n",
            paste0("Above $",quartile_cutoffs[["75%"]])))

par(mar=c(0,0,0,0))
plot(1,type="n",axes=F,xlab="",ylab="")
legend("top",legend=c("Threat","Moral","Peer","Control"),lty=1,lwd=2,
       col=c("red","blue","green","black"),horiz=T,inset=0)
mtext(paste("Normalized Cumulative Payments \n",
            "By",data_desc,"& Balance Quartile"),
      side=3,line=-2,outer=T)
dev.copy(which=dev.list()["pdf"])
dev.off(which=dev.list()["pdf"])
rm(uplim,avg_pmts_by_q)

##DRAW-DOWN OF TOTAL BALANCE
setkey(setkey(analysis_data,treatment,balance_quartile
)[analysis_data[posting_rel==0,sum(current_balance),
                by=list(treatment,balance_quartile)],
  total_due_at_mailing_grp_q:=V1],opa_no,posting_rel)

debt_drawdown_q<-
  dcast.data.table(analysis_data[posting_rel>=0,
                                 sum(100*current_balance/total_due_at_mailing_grp_q),
                                 by=c("balance_quartile","treatment","posting_rel")],
                   posting_rel~balance_quartile+treatment,value.var="V1")
graphics.off()
dev.new()
pdf(paste0("./papers_presentations/images/analysis/",
                  substr(file_suff,2,nchar(file_suff)),
                  "/time_series_debt_paydown_by_quartile",file_suff,".pdf"))
dev.set(which=dev.list()["RStudioGD"])
layout(mat=matrix(c(1,3,5,2,4,5),nrow=3),heights=c(.4,.4,.2))
par(oma=c(0,0,3,0))
par(mar=c(0,4.1,4.1,2.1))
matplot(0:max_length,
        debt_drawdown_q[,paste0("1_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(min(debt_drawdown_q[,paste0("1_",c("Control","Moral","Peer","Threat")),
                                   with=F]),100),
        xlab="",ylab="%",lwd=2,main="",xaxt="n")
text(5,sum(c(3,1)/4*par("usr")[3:4]),
     paste0("1st Quartile: \n",
            paste0("Below $",quartile_cutoffs[["25%"]])))

par(mar=c(0,4.1,4.1,2.1))
matplot(0:max_length,
        debt_drawdown_q[,paste0("2_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(min(debt_drawdown_q[,paste0("2_",c("Control","Moral","Peer","Threat")),
                                   with=F]),100),
        xlab="",ylab="%",lwd=2,main="",xaxt="n")
text(5,sum(c(3,1)/4*par("usr")[3:4]),
     paste0("2nd Quartile: \n",
            paste0("$",quartile_cutoffs[["25%"]],
                   "-$",quartile_cutoffs[["50%"]])))

par(mar=c(5.1,4.1,0,2.1))
matplot(0:max_length,
        debt_drawdown_q[,paste0("3_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(min(debt_drawdown_q[,paste0("3_",c("Control","Moral","Peer","Threat")),
                                   with=F]),100),
        xlab="Days Since Mailing",ylab="%",lwd=2,main="")
text(5,sum(c(3,1)/4*par("usr")[3:4]),
     paste0("3rd Quartile: \n",
            paste0("$",quartile_cutoffs[["50%"]],
                   "-$",quartile_cutoffs[["75%"]])))

par(mar=c(5.1,4.1,0,2.1))
matplot(0:max_length,
        debt_drawdown_q[,paste0("4_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(min(debt_drawdown_q[,paste0("4_",c("Control","Moral","Peer","Threat")),
                                   with=F]),100),
        xlab="Days Since Mailing",ylab="%",lwd=2,main="")
text(5,sum(c(3,1)/4*par("usr")[3:4]),
     paste0("4th Quartile: \n",
            paste0("Above $",quartile_cutoffs[["75%"]])))

par(mar=c(0,0,0,0))
plot(1,type="n",axes=F,xlab="",ylab="")
legend("top",legend=c("Threat","Moral","Peer","Control"),lty=1,lwd=2,
       col=c("red","blue","green","black"),horiz=T,inset=0)
mtext(paste("Percentage of Mailing Day Debt Owed \n",
            "By",data_desc,"& Balance Quartile"),
      side=3,line=-2,outer=T)
dev.copy(which=dev.list()["pdf"])
dev.off(which=dev.list()["pdf"])
rm(debt_drawdown_q)

##Percent Ever Paid by treatment
pct_ever_paid_q<-
  dcast.data.table(analysis_data[posting_rel>=0,mean(100*ever_paid),
                                 by=list(balance_quartile,treatment,posting_rel)],
                   posting_rel~balance_quartile+treatment,value.var="V1")

uplim<-max(pct_ever_paid_q[,!"posting_rel",with=F])

graphics.off()
dev.new()
pdf(paste0("./papers_presentations/images/analysis/",
                  substr(file_suff,2,nchar(file_suff)),
                  "/time_series_pct_ever_paid_by_quartile",file_suff,".pdf"))
dev.set(which=dev.list()["RStudioGD"])
layout(mat=matrix(c(1,3,5,2,4,5),nrow=3),heights=c(.4,.4,.2))
par(oma=c(0,0,3,0))
par(mar=c(0,4.1,4.1,2.1))
matplot(0:max_length,
        pct_ever_paid_q[,paste0("1_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(0,max(pct_ever_paid_q[,paste0("1_",c("Control","Moral","Peer","Threat")),
                                     with=F])),
        xlab="",ylab="%",lwd=2,main="",xaxt="n")
text(7,.75*par("usr")[4],
     paste("1st Quartile: \n",
           paste0("Below $",quartile_cutoffs[["25%"]])))

par(mar=c(0,4.1,4.1,2.1))
matplot(0:max_length,
        pct_ever_paid_q[,paste0("2_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(0,max(pct_ever_paid_q[,paste0("2_",c("Control","Moral","Peer","Threat")),
                                     with=F])),
        xlab="",ylab="%",lwd=2,main="",xaxt="n")
text(7,.75*par("usr")[4],
     paste("2nd Quartile: \n",
           paste0("$",quartile_cutoffs[["25%"]],
                  "-$",quartile_cutoffs[["50%"]])))

par(mar=c(5.1,4.1,0,2.1))
matplot(0:max_length,
        pct_ever_paid_q[,paste0("3_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(0,max(pct_ever_paid_q[,paste0("3_",c("Control","Moral","Peer","Threat")),
                                     with=F])),
        xlab="Days Since Mailing",ylab="%",lwd=2,main="")
text(7,.75*par("usr")[4],
     paste("3rd Quartile: \n",
           paste0("$",quartile_cutoffs[["50%"]],
                  "-$",quartile_cutoffs[["75%"]])))

par(mar=c(5.1,4.1,0,2.1))
matplot(0:max_length,
        pct_ever_paid_q[,paste0("4_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(0,max(pct_ever_paid_q[,paste0("4_",c("Control","Moral","Peer","Threat")),
                                     with=F])),
        xlab="Days Since Mailing",ylab="%",lwd=2,main="")
text(7,.75*par("usr")[4],
     paste("4th Quartile: \n",
           paste0("Above $",quartile_cutoffs[["75%"]])))

par(mar=c(0,0,0,0))
plot(1,type="n",axes=F,xlab="",ylab="")
legend("top",legend=c("Threat","Moral","Peer","Control"),lty=1,lwd=2,
       col=c("red","blue","green","black"),horiz=T,inset=0)
mtext(paste("Percent of Group Ever Having Paid \n",
            "By",data_desc,"& Balance Quartile"),
      side=3,line=-2,outer=T)
dev.copy(which=dev.list()["pdf"])
dev.off(which=dev.list()["pdf"])
rm(uplim,pct_ever_paid_q)

##Percent Paid in Full by treatment
pct_paid_full_q<-
  dcast.data.table(analysis_data[posting_rel>=0,mean(100*paid_full),
                                 by=list(balance_quartile,treatment,posting_rel)],
                   posting_rel~balance_quartile+treatment,value.var="V1")

graphics.off()
dev.new()
pdf(paste0("./papers_presentations/images/analysis/",
                  substr(file_suff,2,nchar(file_suff)),
                  "/time_series_pct_paid_full_by_quartile",file_suff,".pdf"))
dev.set(which=dev.list()["RStudioGD"])
layout(mat=matrix(c(1,3,5,2,4,5),nrow=3),heights=c(.4,.4,.2))
par(oma=c(0,0,3,0))
par(mar=c(0,4.1,4.1,2.1))
matplot(0:max_length,
        pct_paid_full_q[,paste0("1_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(0,max(pct_paid_full_q[,paste0("1_",c("Control","Moral","Peer","Threat")),
                                     with=F])),
        xlab="",ylab="%",lwd=2,main="",xaxt="n")
text(7,.75*par("usr")[4],
     paste("1st Quartile: \n",
           paste0("Below $",quartile_cutoffs[["25%"]])))

par(mar=c(0,4.1,4.1,2.1))
matplot(0:max_length,
        pct_paid_full_q[,paste0("2_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(0,max(pct_paid_full_q[,paste0("2_",c("Control","Moral","Peer","Threat")),
                                     with=F])),
        xlab="",ylab="%",lwd=2,main="",xaxt="n")
text(7,.75*par("usr")[4],
     paste("2nd Quartile: \n",
           paste0("$",quartile_cutoffs[["25%"]],
                  "-$",quartile_cutoffs[["50%"]])))

par(mar=c(5.1,4.1,0,2.1))
matplot(0:max_length,
        pct_paid_full_q[,paste0("3_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(0,max(pct_paid_full_q[,paste0("3_",c("Control","Moral","Peer","Threat")),
                                     with=F])),
        xlab="Days Since Mailing",ylab="%",lwd=2,main="")
text(7,.75*par("usr")[4],
     paste("3rd Quartile: \n",
           paste0("$",quartile_cutoffs[["50%"]],
                  "-$",quartile_cutoffs[["75%"]])))

par(mar=c(5.1,4.1,0,2.1))
matplot(0:max_length,
        pct_paid_full_q[,paste0("4_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(0,max(pct_paid_full_q[,paste0("4_",c("Control","Moral","Peer","Threat")),
                                     with=F])),
        xlab="Days Since Mailing",ylab="%",lwd=2,main="")
text(7,.75*par("usr")[4],
     paste("4th Quartile: \n",
           paste0("Above $",quartile_cutoffs[["75%"]])))

par(mar=c(0,0,0,0))
plot(1,type="n",axes=F,xlab="",ylab="")
legend("top",legend=c("Threat","Moral","Peer","Control"),lty=1,lwd=2,
       col=c("red","blue","green","black"),horiz=T,inset=0)
mtext(paste("Percent of Group Having Paid in Full \n",
            "By",data_desc,"& Balance Quartile"),
      side=3,line=-2,outer=T)
dev.copy(which=dev.list()["pdf"])
dev.off(which=dev.list()["pdf"])
rm(pct_paid_full_q)

##Analysis by Length of Debt ####
setkey(analysis_data,years_cut,treatment)
avg_pmts_by_y<-
  dcast.data.table(analysis_data[posting_rel>=0,mean(cum_treated_pmts),
                                 by=c("years_cut","treatment","posting_rel")],
                   posting_rel~years_cut+treatment,value.var="V1")
uplim<-max(avg_pmts_by_y[,!"posting_rel",with=F])
###Cumulative Payments by Treatment (normalized by group size)
graphics.off()
dev.new()
pdf(paste0("./papers_presentations/images/analysis/",
                  substr(file_suff,2,nchar(file_suff)),
                  "/time_series_average_payments_by_years_debt",file_suff,".pdf"))
dev.set(which=dev.list()["RStudioGD"])
layout(mat=matrix(c(1,3,5,2,4,5),nrow=3),heights=c(.4,.4,.2))
par(oma=c(0,0,3,0))
par(mar=c(0,4.1,4.1,0))
matplot(0:max_length,
        avg_pmts_by_y[,paste0("1_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),ylim=c(0,uplim),
        xlab="",ylab="$",lwd=2,main="",xaxt="n")
text(max_length/2,.9*uplim,"0-5 Years")

par(mar=c(0,0,4.1,2.1))
matplot(0:max_length,
        avg_pmts_by_y[,paste0("2_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),ylim=c(0,uplim),
        xlab="",ylab="",lwd=2,main="",yaxt="n",xaxt="n")
text(max_length/2,.9*uplim,"6-10 Years")

par(mar=c(5.1,4.1,0,0))
matplot(0:max_length,
        avg_pmts_by_y[,paste0("3-5_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),ylim=c(0,uplim),
        xlab="Days Since Mailing",ylab="$",lwd=2,main="")
text(max_length/2,.9*uplim,"11-20 Years")

par(mar=c(5.1,0,0,2.1))
matplot(0:max_length,
        avg_pmts_by_y[,paste0(">5_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),ylim=c(0,uplim),
        xlab="Days Since Mailing",ylab="",lwd=2,main="",yaxt="n")
text(max_length/2,.9*uplim,"21+ Years")

par(mar=c(0,0,0,0))
plot(1,type="n",axes=F,xlab="",ylab="")
legend("top",legend=c("Threat","Moral","Peer","Control"),lty=1,lwd=2,
       col=c("red","blue","green","black"),horiz=T,inset=0)
mtext(paste("Normalized Cumulative Payments \n",
            "By",data_desc,"& Years Owed"),
      side=3,line=-2,outer=T)
dev.copy(which=dev.list()["pdf"])
dev.off(which=dev.list()["pdf"])
rm(uplim,avg_pmts_by_y)

##DRAW-DOWN OF TOTAL BALANCE
setkey(setkey(analysis_data,treatment,years_cut
)[analysis_data[posting_rel==0,sum(current_balance),
                by=list(treatment,years_cut)],
  total_due_at_mailing_grp_y:=V1],opa_no,posting_rel)

debt_drawdown_y<-
  dcast.data.table(analysis_data[posting_rel>=0,
                                 sum(100*current_balance/total_due_at_mailing_grp_y),
                                 by=c("years_cut","treatment","posting_rel")],
                   posting_rel~years_cut+treatment,value.var="V1")
graphics.off()
dev.new()
pdf(paste0("./papers_presentations/images/analysis/",
                  substr(file_suff,2,nchar(file_suff)),
                  "/time_series_debt_paydown_by_years_debt",file_suff,".pdf"))
dev.set(which=dev.list()["RStudioGD"])
layout(mat=matrix(c(1,3,5,2,4,5),nrow=3),heights=c(.4,.4,.2))
par(oma=c(0,0,3,0))
par(mar=c(0,4.1,4.1,0))
matplot(0:max_length,
        debt_drawdown_y[,paste0("1_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(min(debt_drawdown_y[,paste0("1_",c("Control","Moral","Peer","Threat")),
                                   with=F]),1),
        xlab="",ylab="%",lwd=2,main="",xaxt="n")
text(.8*max_length,.999,"0-5 Years")

par(mar=c(0,0,4.1,2.1))
matplot(0:max_length,
        debt_drawdown_y[,paste0("2_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(min(debt_drawdown_y[,paste0("2_",c("Control","Moral","Peer","Threat")),
                                   with=F]),1),
        xlab="",ylab="",lwd=2,main="",yaxt="n",xaxt="n")
text(.8*max_length,.999,"6-10 Years")

par(mar=c(5.1,4.1,0,0))
matplot(0:max_length,
        debt_drawdown_y[,paste0("3-5_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(min(debt_drawdown_y[,paste0("3-5_",c("Control","Moral","Peer","Threat")),
                                   with=F]),1),
        xlab="Days Since Mailing",ylab="%",lwd=2,main="")
text(.8*max_length,.999,"11-20 Years")

par(mar=c(5.1,0,0,2.1))
matplot(0:max_length,
        debt_drawdown_y[,paste0(">5_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(min(debt_drawdown_y[,paste0(">5_",c("Control","Moral","Peer","Threat")),
                                   with=F]),1),
        xlab="Days Since Mailing",ylab="",lwd=2,main="",yaxt="n")
text(.8*max_length,.999,"21+ Years")

par(mar=c(0,0,0,0))
plot(1,type="n",axes=F,xlab="",ylab="")
legend("top",legend=c("Threat","Moral","Peer","Control"),lty=1,lwd=2,
       col=c("red","blue","green","black"),horiz=T,inset=0)
mtext(paste("Percentage of Mailing Day Debt Owed \n",
            "By",data_desc,"& Years Owed"),
      side=3,line=-2,outer=T)
dev.copy(which=dev.list()["pdf"])
dev.off(which=dev.list()["pdf"])
rm(debt_drawdown_y)

##Percent Ever Paid by treatment
pct_ever_paid_y<-
  dcast.data.table(analysis_data[posting_rel>=0,mean(100*ever_paid),
                                 by=list(years_cut,treatment,posting_rel)],
                   posting_rel~years_cut+treatment,value.var="V1")

uplim<-max(pct_ever_paid_y[,!"posting_rel",with=F])

graphics.off()
dev.new()
pdf(paste0("./papers_presentations/images/analysis/",
                  substr(file_suff,2,nchar(file_suff)),
                  "/time_series_pct_ever_paid_by_years_debt",file_suff,".pdf"))
dev.set(which=dev.list()["RStudioGD"])
layout(mat=matrix(c(1,3,5,2,4,5),nrow=3),heights=c(.4,.4,.2))
par(oma=c(0,0,3,0))
par(mar=c(0,4.1,4.1,0))
matplot(0:max_length,
        pct_ever_paid_y[,paste0("1_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(0,max(pct_ever_paid_y[,paste0("1_",c("Control","Moral","Peer","Threat")),
                                     with=F])),
        xlab="",ylab="%",lwd=2,main="",xaxt="n")
text(7,.9*max(pct_ever_paid_y[,paste0("1_",c("Control","Moral","Peer","Threat")),
                              with=F]),"0-5 Years")

par(mar=c(0,0,4.1,2.1))
matplot(0:max_length,
        pct_ever_paid_y[,paste0("2_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(0,max(pct_ever_paid_y[,paste0("2_",c("Control","Moral","Peer","Threat")),
                                     with=F])),
        xlab="",ylab="",lwd=2,main="",yaxt="n",xaxt="n")
text(7,.9*max(pct_ever_paid_y[,paste0("2_",c("Control","Moral","Peer","Threat")),
                              with=F]),"6-10 Years")

par(mar=c(5.1,4.1,0,0))
matplot(0:max_length,
        pct_ever_paid_y[,paste0("3-5_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(0,max(pct_ever_paid_y[,paste0("3-5_",c("Control","Moral","Peer","Threat")),
                                     with=F])),
        xlab="Days Since Mailing",ylab="%",lwd=2,main="")
text(7,.9*max(pct_ever_paid_y[,paste0("3-5_",c("Control","Moral","Peer","Threat")),
                              with=F]),"11-20 Years")

par(mar=c(5.1,0,0,2.1))
matplot(0:max_length,
        pct_ever_paid_y[,paste0(">5_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(0,max(pct_ever_paid_y[,paste0(">5_",c("Control","Moral","Peer","Threat")),
                                     with=F])),
        xlab="Days Since Mailing",ylab="",lwd=2,main="",yaxt="n")
text(7,.9*max(pct_ever_paid_y[,paste0(">5_",c("Control","Moral","Peer","Threat")),
                              with=F]),"21+ Years")

par(mar=c(0,0,0,0))
plot(1,type="n",axes=F,xlab="",ylab="")
legend("top",legend=c("Threat","Moral","Peer","Control"),lty=1,lwd=2,
       col=c("red","blue","green","black"),horiz=T,inset=0)
mtext(paste("Percent of Group Ever Having Paid \n",
            "By",data_desc,"& Years Owed"),
      side=3,line=-2,outer=T)
dev.copy(which=dev.list()["pdf"])
dev.off(which=dev.list()["pdf"])
rm(uplim,pct_ever_paid_y)

##Percent Paid in Full by treatment
pct_paid_full_y<-
  dcast.data.table(analysis_data[posting_rel>=0,mean(100*paid_full),
                                 by=list(years_cut,treatment,posting_rel)],
                   posting_rel~years_cut+treatment,value.var="V1")

graphics.off()
dev.new()
pdf(paste0("./papers_presentations/images/analysis/",
                  substr(file_suff,2,nchar(file_suff)),
                  "/time_series_pct_paid_full_by_years_debt",file_suff,".pdf"))
dev.set(which=dev.list()["RStudioGD"])
layout(mat=matrix(c(1,3,5,2,4,5),nrow=3),heights=c(.4,.4,.2))
par(oma=c(0,0,3,0))
par(mar=c(0,4.1,4.1,0))
matplot(0:max_length,
        pct_paid_full_y[,paste0("1_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(0,max(pct_paid_full_y[,paste0("1_",c("Control","Moral","Peer","Threat")),
                                     with=F])),
        xlab="",ylab="%",lwd=2,main="",xaxt="n")
text(7,.9*max(pct_paid_full_y[,paste0("1_",c("Control","Moral","Peer","Threat")),
                              with=F]),"0-5 Years")

par(mar=c(0,0,4.1,2.1))
matplot(0:max_length,
        pct_paid_full_y[,paste0("2_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(0,max(pct_paid_full_y[,paste0("2_",c("Control","Moral","Peer","Threat")),
                                     with=F])),
        xlab="",ylab="",lwd=2,main="",yaxt="n",xaxt="n")
text(7,.9*max(pct_paid_full_y[,paste0("2_",c("Control","Moral","Peer","Threat")),
                              with=F]),"6-10 Years")

par(mar=c(5.1,4.1,0,0))
matplot(0:max_length,
        pct_paid_full_y[,paste0("3-5_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(0,max(pct_paid_full_y[,paste0("3-5_",c("Control","Moral","Peer","Threat")),
                                     with=F])),
        xlab="Days Since Mailing",ylab="%",lwd=2,main="")
text(7,.9*max(pct_paid_full_y[,paste0("3-5_",c("Control","Moral","Peer","Threat")),
                              with=F]),"11-20 Years")

par(mar=c(5.1,0,0,2.1))
matplot(0:max_length,
        pct_paid_full_y[,paste0(">5_",c("Control","Moral","Peer","Threat")),with=F],
        type="l",lty=1,col=c("black","blue","green","red"),
        ylim=c(0,max(pct_paid_full_y[,paste0(">5_",c("Control","Moral","Peer","Threat")),
                                     with=F])),
        xlab="Days Since Mailing",ylab="",lwd=2,main="",yaxt="n")
text(7,.9*max(pct_paid_full_y[,paste0(">5_",c("Control","Moral","Peer","Threat")),
                              with=F]),"21+ Years")

par(mar=c(0,0,0,0))
plot(1,type="n",axes=F,xlab="",ylab="")
legend("top",legend=c("Threat","Moral","Peer","Control"),lty=1,lwd=2,
       col=c("red","blue","green","black"),horiz=T,inset=0)
mtext(paste("Percent of Group Having Paid in Full \n",
            "By",data_desc,"& Years Owed"),
      side=3,line=-2,outer=T)
dev.copy(which=dev.list()["pdf"])
dev.off(which=dev.list()["pdf"])
rm(pct_paid_full_y)