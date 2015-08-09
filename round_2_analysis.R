#Data Analysis
#Philadelphia Real Estate Tax Evasion
#  Round 2
#Michael Chirico
#July 29, 2015

#Setup: Packages, Working Directory, Convenient Functions ####
rm(list=ls(all=T))
gc()
setwd("~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/")
library(data.table)
library(texreg)
library(sandwich)
library(xtable)
library(xlsx)

#Convenient Functions 
table2<-function(...,dig=if (prop) 2 else NULL,prop=F,ord=F,pct=F){
  if (ord=="dec"){ dec<-T; ord<-T} else dec<-F
  dots<-list(...)
  args<-names(dots) %in% names(formals(prop.table))
  tab<-if (prop) do.call(
    'prop.table',c(list(
      do.call('table',if (length(args)) dots[!args] else dots)),
      dots[args])) else do.call('table',list(...))
  if (ord) tab<-tab[order(tab,decreasing=dec)]
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
  cut(x,breaks=quantile(x,probs=seq(0,1,by=1/num)),
      labels=1:num,right=right,include.lowest=include.lowest)
}

read.xlsx3<-function(...){
  x<-do.call('read.xlsx2',list(...))
  invisible(gc())
  x
}

abbr_to_colClass<-function(inits,counts){
  x<-strsplit(inits,split="")[[1]]
  types<-character(length(x))
  types[x=="c"]<-"character"
  types[x=="f"]<-"factor"
  types[x=="i"]<-"integer"
  types[x=="n"]<-"numeric"
  types[x=="d"]<-"Date"
  rep(types,strsplit(counts,split="")[[1]])
}

to.pct<-function(x,dig=0)round(100*x,digits=dig)

get.col<-function(n){if (n==2){c("blue","red")}
  else if(n==7){c("yellow","blue","darkgreen","red",
                  "cyan","orange","orchid")}
  else if(n==14){rep(c("yellow","blue","darkgreen","red",
                       "cyan","orange","orchid"),each=2)}
  else cat("Ya done goofed.")}

#Data Import ####
##Main outcomes data
### total_due is pre-study balance
### current_balance is as of July 22, 2015
### total_paid is the accrual between June 1, 2015
###   and July 22, 2015
data_r2<-setkey(setnames(setDT(read.xlsx3(paste0(
  "/media/data_drive/real_estate/",
  "Payments and Balance Penn Letter Experiment_150727.xlsx"),
  colIndex=c(2,5,7:15),
  sheetName="DETAILS",header=T,startRow=9,stringsAsFactors=F,
  colClasses=abbr_to_colClass("cidndn","512111"))),
  c("opa_no","treatment","pre_15_balance",
    "paid_full","ever_paid","years_count",
    "period_min","period_max","current_balance",
    "earliest_pmt","total_paid")),opa_no,treatment)
###Re-set indicators as T/F instead of Y/N
inds<-c("pre_15_balance","paid_full","ever_paid")
data_r2[,(inds):=lapply(.SD,function(x)x=="Y"),.SDcols=inds]
data_r2[,paid_full:=!paid_full]

###Define two alternative treatments:
###  Collapse Big/Small or Focus on Big/Small
data_r2[grepl("Big",treatment),big_small:="Big"]
data_r2[is.na(big_small),big_small:="Small"]

data_r2[,main_treat:=gsub("_.*","",treatment)]

##Original experiment data
###Merge what we need into main data, then delete
data_r2<-data_r2[setkey(fread("./round_two/round_2_full_data.csv"),
                        opa_no,treatment)]

###Define some flags
#### Was more than one treatment received at this mailing address?
data_r2[,flag_multiple_address:=uniqueN(treatment)>1,
        by=.(mail_address,mail_city,mail_state)]
#### Did this owner receive more than one letter?
data_r2[,flag_multiple_property:=.N>1,by=owner1]
#### Is more than one year of taxes owed 
####   (should all be 1; some noise in sample selection)
data_r2[,flag_years_count:=years_count>1]
#### Was this owner in both the holdout sample and the treatment panel?
data_r2[,flag_holdout_overlap:=
          owner1 %in% unique(unlist(fread("holdout_sample.csv",select="owner1")))]
#### Was this property treated in Round 1?
data_r2[,flag_round_one:=
          opa_no %in% fread("analysis_file.csv",select=c("opa_no","cycle")
                            )[cycle>=33,unique(opa_no)]]
###Get owner-level version of data, keeping only key analysis variables
data_r2_own<-setkey(
  data_r2[,.(main_treat=main_treat[1],
             treatment=treatment[1],
             big_small=big_small[1],
             ever_paid=any(ever_paid),
             paid_full=all(paid_full),
             total_paid=sum(total_paid),
             prop_count=.N),
          by=owner1],treatment)



##Background data merge

#Fidelity Checks ####
##Returned Mail Rates by Envelope Size
big_returns<-26+17+75+12+16+4+4+192
small_returns<-766+9
returns<-c(big_returns,small_returns)
xtable(matrix(rbind(returns,100*returns/data_r2[,.N,by=big_small]$N),
              ncol=2,dimnames=list(c("Count","Percentage"),
                                   paste(c("Large","Small"),
                                         "Envelopes"))),
       caption="Returned Mail by Envelope Type",
       label="table_return_env",digits=matrix(c(0,0,0,1,0,1),ncol=3))

##Letter Content Fidelity Checks
print.xtable(xtable(matrix(cbind(
  c(rep("Small",8),rep("Big",7)),
  c(273036500,882697200,123233800,152054820,314169700,421552000,
    381197900,"023067410",482183800,331052400,871550440,881105200,
    888290588,621279300,282208400),
  sprintf("%7.2f",c(253.45,29180.85,718.50,10726.30,64.41,191.84,
                    480.30,3488.15,1047.42,20.62,1834.39,56.14,
                    1849.80,1375.09,322.07)),
  c("Amenities","Lien","Peer",rep("Lien",6),"Moral","Duty",
    "Peer","Moral","Lien","Moral"),rep("Yes",15)),ncol=5,
  dimnames=list(1:15,c("Envelope Size","OPA No.",
                       "Balance","Treatment","Correct Content?"))),
  caption="OPA Numbers Checked for Content Fidelity",
  label="table_content_fidelity"),include.rownames=F)
                              
#Analysis ####
##Bar Plots
### Basic overview tables
####By all 7 Treatments (collapse big/small)
by_own_7<-data_r2_own[,.(ep=mean(ever_paid),
                         pf=mean(paid_full),
                         tp=mean(total_paid)),
                      keyby=main_treat]

BB<-1e4
setkey(data_r2_own,main_treat)
by_own_7<-
  by_own_7[setkey(setnames(data.table(t(sapply(
    data_r2_own[,unique(main_treat)],
    #replicate to bootstrap confidence intervals
    function(x)apply(replicate(
      BB,unlist(data_r2_own[.(x)][
        sample(.N,.N,T),
        .(ep=mean(ever_paid),
          pf=mean(paid_full),
          tp=mean(total_paid))])),
      1,quantile,c(.025,.975)),
    USE.NAMES=T)),keep.rownames=T),
    c("main_treat","ep.ci.lo","ep.ci.hi",
      "pf.ci.lo","pf.ci.hi",
      "tp.ci.lo","tp.ci.hi")),main_treat)]

#####Bar Plot of Result
######Ever Paid
pdf2("./round_two/images/analysis/bar_plot_ever_paid_7.pdf")
by_own_7[,{par(mar=c(5.1,5.1,4.1,1.6));
           x<-barplot(to.pct(ep),names.arg=main_treat,col=get.col(.N),
                      xlim=c(0,1.05*max(to.pct(ep.ci.hi))),horiz=T,
                      las=1,main="Percent Ever Paid by Treatment",
                      xlab="Percent",cex.names=.75); 
           arrows(to.pct(ep.ci.lo),x,to.pct(ep.ci.hi),x,
                  code=3,angle=90,length=.07,lwd=2);
           abline(v=to.pct(ep.ci.hi[main_treat=="Control"]),lty=2);
           abline(v=to.pct(ep.ci.lo[main_treat=="Control"]),lty=2)}]
dev.off2()
######Paid Full
pdf2("./round_two/images/analysis/bar_plot_paid_full_7.pdf")
by_own_7[,{par(mar=c(5.1,5.1,4.1,1.6));
  x<-barplot(to.pct(pf),names.arg=main_treat,col=get.col(.N),
             xlim=c(0,1.05*max(to.pct(pf.ci.hi))),horiz=T,
             las=1,main="Percent Paid Full by Treatment",
             xlab="Percent",cex.names=.75); 
  arrows(to.pct(pf.ci.lo),x,to.pct(pf.ci.hi),x,
         code=3,angle=90,length=.07,lwd=2);
  abline(v=to.pct(pf.ci.hi[main_treat=="Control"]),lty=2);
  abline(v=to.pct(pf.ci.lo[main_treat=="Control"]),lty=2)}]
dev.off2()
######Average Payment
pdf2("./round_two/images/analysis/bar_plot_aver_paid_7.pdf")
by_own_7[,{par(mar=c(5.1,5.1,4.1,1.6));
  x<-barplot(round(tp),names.arg=main_treat,col=get.col(.N),
             xlim=c(0,1.05*max(round(tp.ci.hi))),horiz=T,
             las=1,main="Average Paid by Treatment",
             xlab="$",cex.names=.75); 
  arrows(round(tp.ci.lo),x,round(tp.ci.hi),x,
         code=3,angle=90,length=.07,lwd=2);
  abline(v=round(tp.ci.hi[main_treat=="Control"]),lty=2);
  abline(v=round(tp.ci.lo[main_treat=="Control"]),lty=2)}]
dev.off2()



####By Big/Small
by_own_bs<-
  data_r2_own[,.(ep=mean(ever_paid),
                 pf=mean(paid_full),
                 tp=mean(total_paid)),
              keyby=big_small]

#####Bootstrapped confidence intervals
BB<-1e4
setkey(data_r2_own,big_small)
by_own_bs<-
  by_own_bs[setkey(setnames(data.table(t(sapply(
    data_r2_own[,unique(big_small)],
    function(x)apply(replicate(
      BB,unlist(data_r2_own[.(x)][
        sample(.N,.N,T),
        .(ep=mean(ever_paid),
          pf=mean(paid_full),
          tp=mean(total_paid))])),
      1,quantile,c(.025,.975)),
    USE.NAMES=T)),keep.rownames=T),
    c("big_small","ep.ci.lo","ep.ci.hi",
      "pf.ci.lo","pf.ci.hi",
      "tp.ci.lo","tp.ci.hi")),big_small)]

#####Bar Plots of Results
pdf2("./round_two/images/analysis/bar_plot_ever_paid_2.pdf")
by_own_bs[,{par(mar=c(5.1,5.1,4.1,1.6));
            x<-barplot(to.pct(ep),names.arg=big_small,col=get.col(.N),
                       xlim=c(0,1.05*max(to.pct(ep.ci.hi))),horiz=T,
                       las=1,main="Percent Ever Paid by Big/Small",
                       xlab="Percent"); 
            arrows(to.pct(ep.ci.lo),x,to.pct(ep.ci.hi),x,
                   code=3,angle=90,lwd=2);
            abline(v=to.pct(ep.ci.hi[big_small=="Small"]),lty=2);
            abline(v=to.pct(ep.ci.lo[big_small=="Small"]),lty=2)}]
dev.off2()
######Paid Full
pdf2("./round_two/images/analysis/bar_plot_paid_full_2.pdf")
by_own_bs[,{par(mar=c(5.1,5.1,4.1,1.6));
  x<-barplot(to.pct(pf),names.arg=big_small,col=get.col(.N),
             xlim=c(0,1.05*max(to.pct(pf.ci.hi))),horiz=T,
             las=1,main="Percent Paid Full by Big/Small",
             xlab="Percent"); 
  arrows(to.pct(pf.ci.lo),x,to.pct(pf.ci.hi),x,
         code=3,angle=90,lwd=2);
  abline(v=to.pct(pf.ci.hi[big_small=="Small"]),lty=2);
  abline(v=to.pct(pf.ci.lo[big_small=="Small"]),lty=2)}]
dev.off2()
######Average Paid
pdf2("./round_two/images/analysis/bar_plot_aver_paid_2.pdf")
by_own_bs[,{par(mar=c(5.1,5.1,4.1,1.6));
  x<-barplot(round(tp),names.arg=big_small,col=get.col(.N),
             xlim=c(0,1.05*max(round(tp.ci.hi))),horiz=T,
             las=1,main="Average Paid by Big/Small",
             xlab="$"); 
  arrows(round(tp.ci.lo),x,round(tp.ci.hi),x,
         code=3,angle=90,lwd=2);
  abline(v=round(tp.ci.hi[big_small=="Small"]),lty=2);
  abline(v=round(tp.ci.lo[big_small=="Small"]),lty=2)}]
dev.off2()

####By all 14 Treatments
by_own_all<-
  data_r2_own[,.(ep=mean(ever_paid),
                 pf=mean(paid_full),
                 tp=mean(total_paid)),
              keyby=treatment]

#####Bootstrapped confidence intervals
setkey(data_r2_own,treatment)
by_own_all<-
  by_own_all[setkey(setnames(data.table(t(sapply(
    data_r2_own[,unique(treatment)],
    function(x)apply(replicate(
      BB,unlist(data_r2_own[.(x)][
        sample(.N,.N,T),
        .(ep=mean(ever_paid),
          pf=mean(paid_full),
          tp=mean(total_paid))])),
      1,quantile,c(.025,.975)),
    USE.NAMES=T)),keep.rownames=T),
    c("treatment","ep.ci.lo","ep.ci.hi",
      "pf.ci.lo","pf.ci.hi",
      "tp.ci.lo","tp.ci.hi")),treatment)]

#####Bar Plot of Result
######Ever Paid
pdf2("./round_two/images/analysis/bar_plot_ever_paid_14.pdf")
by_own_all[,{par(mar=c(5.1,5.1,4.1,1.6));
             x<-barplot(to.pct(ep),names.arg=gsub("_E.*","",treatment),
                        col=get.col(.N),horiz=T,las=1,
                        xlim=c(0,1.05*max(to.pct(ep.ci.hi))),
                        main="Percent Ever Paid by Treatment / Big/Small",
                        xlab="Percent",cex.names=.5,
                        density=rep(c(-1,30),.N)); 
             arrows(to.pct(ep.ci.lo),x,to.pct(ep.ci.hi),x,
                    code=3,angle=90,lwd=2,length=.05);
             abline(v=to.pct(ep.ci.hi[treatment=="Control_Small_Envelope"]),lty=2);
             abline(v=to.pct(ep.ci.lo[treatment=="Control_Small_Envelope"]),lty=2)}]
dev.off2()
######Paid Full
pdf2("./round_two/images/analysis/bar_plot_paid_full_14.pdf")
by_own_all[,{par(mar=c(5.1,5.1,4.1,1.6));
  x<-barplot(to.pct(pf),names.arg=gsub("_E.*","",treatment),
             col=get.col(.N),horiz=T,las=1,
             xlim=c(0,1.05*max(to.pct(pf.ci.hi))),
             main="Percent Paid Full by Treatment / Big/Small",
             xlab="Percent",cex.names=.5,
             density=rep(c(-1,30),.N)); 
  arrows(to.pct(pf.ci.lo),x,to.pct(pf.ci.hi),x,
         code=3,angle=90,lwd=2,length=.05);
  abline(v=to.pct(pf.ci.hi[treatment=="Control_Small_Envelope"]),lty=2);
  abline(v=to.pct(pf.ci.lo[treatment=="Control_Small_Envelope"]),lty=2)}]
dev.off2()
######Average Paid
pdf2("./round_two/images/analysis/bar_plot_aver_paid_14.pdf")
by_own_all[,{par(mar=c(5.1,5.1,4.1,1.6));
  x<-barplot(round(tp),names.arg=gsub("_E.*","",treatment),
             col=get.col(.N),horiz=T,las=1,
             xlim=c(0,1.05*max(round(tp.ci.hi))),
             main="Average Paid by Treatment / Big/Small",
             xlab="$",cex.names=.5,
             density=rep(c(-1,30),.N)); 
  arrows(round(tp.ci.lo),x,round(tp.ci.hi),x,
         code=3,angle=90,lwd=2,length=.05);
  abline(v=round(tp.ci.hi[treatment=="Control_Small_Envelope"]),lty=2);
  abline(v=round(tp.ci.lo[treatment=="Control_Small_Envelope"]),lty=2)}]
dev.off2()