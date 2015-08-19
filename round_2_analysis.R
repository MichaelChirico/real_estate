#Data Analysis
#Philadelphia Real Estate Tax Evasion
#  Round 2
#Michael Chirico
#July 29, 2015

#Setup: Packages, Working Directory, Convenient Functions ####
rm(list=ls(all=T))
gc()
setwd("~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/")
data_wd<-"/media/data_drive/real_estate/"
img_wd<-"./papers_presentations/round_two/images/analysis/"
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

tile_axes<-function(n,M,N,...){
  if (n>(M-1)*N)do.call("axis",c(side=1,list(...)))
  if (n%%N==1)do.call("axis",c(side=2,list(...)))
}

to.pct<-function(x,dig=Inf)round(100*x,digits=dig)

#Get the nearest multiple of n weakly larger than x
nx.mlt<-function(x,n)n*ceiling(x/n)

#Shorthand for string concatenation
"%+%"<-function(s1,s2)paste0(s1,s2)

get.col<-function(n){
  cols<-c(Big="blue",Small="red",
          Holdout="darkgray",Control="blue",
          Amenities="yellow",Moral="cyan",
          Duty="darkgreen",Lien="red",
          Sheriff="orchid",Peer="orange")
  if (n==2){cols[1:2]}
  else if(n==7){cols[4:10]}
  else if(n==8){cols[3:10]}
  else if(n==14){rep(cols[4:10],each=2)}
  else cat("Ya done goofed.")
}

get.col.nm<-function(st){
  cols<-c(Big="blue",Small="red",
          Control="blue",Amenities="yellow",
          Moral="cyan",Duty="darkgreen",
          Lien="red",Sheriff="orchid",
          Peer="orange",Holdout="darkgray")
  cols[st]
}

#Data Import ####
##Main outcomes data
### total_due is pre-study balance
### current_balance is as of July 22, 2015
### total_paid is the accrual between June 1, 2015
###   and July 22, 2015
data_r2<-setkey(setnames(setDT(read.xlsx3(
  data_wd%+%"Payments and Balance Penn Letter Experiment_150727.xlsx",
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

data_r2[,main_treat:=factor(gsub("_.*","",treatment))]
####Reorder main treatments for plotting purposes
trt.nms<-c("Control","Amenities","Moral",
             "Duty","Lien","Sheriff","Peer")
data_r2[,main_treat:=
          factor(main_treat,trt.nms)]

##Original experiment data
###Merge what we need into main data
data_r2<-data_r2[fread("./round_two/round_2_full_data.csv"),on="opa_no"]

##Holdout data
data_holdout<-
  setkey(rbind(
    data_r2,setnames(setDT(read.xlsx3(
      data_wd%+%"req20150709_PennLetterExperiment_"%+%
        "v2_Commissioners Control Details.xlsx",
      colIndex=c(2,7:14),
      sheetName="DETAILS",header=T,startRow=9,stringsAsFactors=F,
      colClasses=abbr_to_colClass("cdndn","42111"))),
      c("opa_no","pre_15_balance","paid_full",
        "ever_paid","period_min","period_max",
        "current_balance","earliest_pmt",
        "total_paid")
      )[,treatment:="Holdout"
        ][,(inds):=lapply(.SD,function(x)x=="Y"),
          .SDcols=inds
          ][,paid_full:=!paid_full
            ][fread("holdout_sample.csv"),
              owner1:=owner1,on="opa_no"],fill=T),opa_no)

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
          owner1 %in% unique(unlist(fread(
            "holdout_sample.csv",select="owner1")))]
data_holdout[treatment=="Holdout",
             flag_main_sample_overlap:=
               owner1 %in% data_holdout[treatment!="Holdout",
                                        unique(owner1)]]
#### Was this property treated in Round 1?
data_r2[,flag_round_one:=
          opa_no %in% fread("analysis_file.csv",select=c("opa_no","cycle")
                            )[cycle>=33,unique(opa_no)]]
#### Does this property have any of the
####   tax exemptions excluded in Round 1?
data_r2[fread(data_wd%+%"prop2015.txt",select=c("PARCEL","XMPT CD")),
        flag_abate_exempt:=`i.XMPT CD`!="",on=c(opa_no="PARCEL")]

###Get owner-level version of data, keeping only key analysis variables
data_r2_own<-setkey(
  data_r2[,.(main_treat=main_treat[1],
             treatment=treatment[1],
             big_small=big_small[1],
             ever_paid=any(ever_paid),
             paid_full=all(paid_full),
             total_paid=sum(total_paid),
             total_due=sum(total_due),
             prop_count=.N),
          by=owner1],treatment)

###Get owner-level version of data, dropping the top
###  a) 5% and b) 10% of accounts by repayment numbers
data_r2_own_x05<-
  data_r2_own[total_paid<quantile(total_paid,.95)]
data_r2_own_x10<-
  data_r2_own[total_paid<quantile(total_paid,.90)]

###Get owner-level version of data, keeping only key analysis variables
data_holdout_own<-
  data_holdout[,.(treatment=treatment[1],
                  ever_paid=any(ever_paid),
                  paid_full=all(paid_full),
                  total_paid=sum(total_paid),
                  total_due=sum(total_due),
                  prop_count=.N),
               by=owner1]

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
## Bootstrap simulations ####
###Number of repetitions for all bootstrap
###  exercises
BB<-1e4 

###Confidence intervals for 7 treatments
###  on ever_paid, paid_full, total_paid
###  by owner (all measures pre-aggregated)
setkey(data_r2_own,main_treat)
by_own_7<-
  #First, point estimates from raw data
  data_r2_own[,.(ep=mean(ever_paid),
                         pf=mean(paid_full),
                         tp=mean(total_paid)),
                      by=main_treat
              ][data.table(t(sapply(
                #to guarantee equal representation of each
                #  treatment, block at the treatment level--
                #  resample N_t observations for each treatment t
                paste0(data_r2_own[,unique(main_treat)]),
                function(x)apply(replicate(
                  BB,unlist(data_r2_own[.(x)][
                    sample(.N,.N,T),
                    #calculate point estimate in re-sample
                    .(ep=mean(ever_paid),
                      pf=mean(paid_full),
                      tp=mean(total_paid))])),
                  #CIs are given by the 2.5 &
                  #  97.5 %ile of each measure
                  1,quantile,c(.025,.975)),
                USE.NAMES=T)),keep.rownames=T),
                `:=`(ep.ci.lo=i.V1,ep.ci.hi=i.V2,
                     pf.ci.lo=i.V3,pf.ci.hi=i.V4,
                     tp.ci.lo=i.V5,tp.ci.hi=i.V6),
                on=c(main_treat="rn")]

###Confidence intervals for 7 treatments
###  on ever_paid, paid_full, total_paid
###  by property, resampling at the owner level
setkey(data_r2,main_treat)
owners<-sapply(paste0(data_r2[,unique(main_treat)]),
               function(x)data_r2[.(x),unique(owner1)],
               USE.NAMES=T)
setkey(data_r2,owner1)
by_prop_7<-
  data_r2[,.(ep=mean(ever_paid),pf=mean(paid_full),
             tp=mean(total_paid)),by=main_treat
          ][data.table(t(sapply(
            trt.nms,function(x)apply(
              replicate(BB,unlist(
                data_r2[.(sample(owners[[x]],rep=T)),
                        .(mean(ever_paid),mean(paid_full),
                          mean(total_paid))])),1,quantile,c(.025,.975)),
            USE.NAMES=T)),keep.rownames=T),
            `:=`(ep.ci.lo=i.V1,ep.ci.hi=i.V2,
                 pf.ci.lo=i.V3,pf.ci.hi=i.V4,
                 tp.ci.lo=i.V5,tp.ci.hi=i.V6),
            on=c(main_treat="rn")]

###Confidence intervals for 7 treatments
###  on ever_paid, paid_full, total_paid
###  by owner (all measures pre-aggregated),
###  top 5% then 10% of payers excluded
setkey(data_r2_own_x05,main_treat)
by_own_7_x05<-
  data_r2_own_x05[,.(ep=mean(ever_paid),
                     pf=mean(paid_full),
                     tp=mean(total_paid)),
                  by=main_treat
                  ][data.table(t(sapply(
                    trt.nms,function(x)apply(replicate(
                      BB,unlist(data_r2_own_x05[.(x)][
                        sample(.N,.N,T),
                        .(ep=mean(ever_paid),
                          pf=mean(paid_full),
                          tp=mean(total_paid))])),
                      1,quantile,c(.025,.975)),
                    USE.NAMES=T)),keep.rownames=T),
                    `:=`(ep.ci.lo=i.V1,ep.ci.hi=i.V2,
                         pf.ci.lo=i.V3,pf.ci.hi=i.V4,
                         tp.ci.lo=i.V5,tp.ci.hi=i.V6),
                    on=c(main_treat="rn")]

setkey(data_r2_own_x10,main_treat)
by_own_7_x10<-
  data_r2_own_x10[,.(ep=mean(ever_paid),
                     pf=mean(paid_full),
                     tp=mean(total_paid)),
                  by=main_treat
                  ][data.table(t(sapply(
                    trt.nms,function(x)apply(replicate(
                      BB,unlist(data_r2_own_x10[.(x)][
                        sample(.N,.N,T),
                        .(ep=mean(ever_paid),
                          pf=mean(paid_full),
                          tp=mean(total_paid))])),
                      1,quantile,c(.025,.975)),
                    USE.NAMES=T)),keep.rownames=T),
                    `:=`(ep.ci.lo=i.V1,ep.ci.hi=i.V2,
                         pf.ci.lo=i.V3,pf.ci.hi=i.V4,
                         tp.ci.lo=i.V5,tp.ci.hi=i.V6),
                    on=c(main_treat="rn")]

###Confidence intervals for 2 treatments
###  on ever_paid, paid_full, total_paid
###  by owner (all measures pre-aggregated)
setkey(data_r2_own,big_small)
by_own_bs<-
  data_r2_own[,.(ep=mean(ever_paid),
                 pf=mean(paid_full),
                 tp=mean(total_paid)),
              by=big_small
              ][data.table(t(sapply(
                data_r2_own[,unique(big_small)],
                function(x)apply(replicate(
                  BB,unlist(data_r2_own[.(x)][
                    sample(.N,.N,T),
                    .(ep=mean(ever_paid),
                      pf=mean(paid_full),
                      tp=mean(total_paid))])),
                  1,quantile,c(.025,.975)),
                USE.NAMES=T)),keep.rownames=T),
                `:=`(ep.ci.lo=V1,ep.ci.hi=V2,
                     pf.ci.lo=V3,pf.ci.hi=V4,
                     tp.ci.lo=V5,tp.ci.hi=V6),
                on=c(big_small="rn")]

###Confidence intervals for 14 treatments
###  on ever_paid, paid_full, total_paid
###  by owner (all measures pre-aggregated)
setkey(data_r2_own,treatment)
by_own_all<-
  data_r2_own[,.(ep=mean(ever_paid),
                 pf=mean(paid_full),
                 tp=mean(total_paid)),
              by=treatment
              ][data.table(t(sapply(
                data_r2_own[,unique(treatment)],
                function(x)apply(replicate(
                  BB,unlist(data_r2_own[.(x)][
                    sample(.N,.N,T),
                    .(ep=mean(ever_paid),
                      pf=mean(paid_full),
                      tp=mean(total_paid))])),
                  1,quantile,c(.025,.975)),
                USE.NAMES=T)),keep.rownames=T),
                `:=`(ep.ci.lo=V1,ep.ci.hi=V2,
                     pf.ci.lo=V3,pf.ci.hi=V4,
                     tp.ci.lo=V5,tp.ci.hi=V6),
                on=c(treatment="rn")
                ]

###Confidence intervals for Holdout sample
###  on ever_paid, paid_full, total_paid
###  by owner (all measures pre-aggregated)
by_own_8<-rbind(
  by_own_7,data.table(
    main_treat="Holdout",
    rbind(setNames(c(unlist(
      data_holdout[!(flag_main_sample_overlap),
                   .(ep=mean(ever_paid),
                     pf=mean(paid_full),
                     tp=mean(total_paid))]),
      c(apply(replicate(
        BB,unlist(data_holdout[!(flag_main_sample_overlap)][
          sample(.N,.N,T),
          .(ep=mean(ever_paid),
            pf=mean(paid_full),
            tp=mean(total_paid))])),
        1,quantile,c(.025,.975)))),
      c("ep","pf","tp","ep.ci.lo","ep.ci.hi",
        "pf.ci.lo","pf.ci.hi","tp.ci.hi","tp.ci.lo"))))
  )[,main_treat:=
      factor(main_treat,c("Holdout",trt.nms))]

###Confidence Intervals for 7 treatments
###  On predictions of P[ever_paid] by log(total_due)
###  by owner (pre-aggregated)
total_grid<-data_r2_own[,seq(min(log(total_due)),
                             max(log(total_due)),
                             length.out=1e3)]
setkey(data_r2_own,main_treat)
ever_paid.lgt.ci<-
  dcast(data.table(grd=rep(total_grid,each=2),
                   id=rep(c("lower","upper"),
                          length(total_grid)),
            sapply(trt.nms,
                   function(x)apply(replicate(
                     BB,data_r2_own[.(x)][
                       sample(.N,rep=T),
                       predict(glm(ever_paid~log(total_due),
                                   family=binomial(link="logit")),
                               data.table(total_due=total_grid),
                               type="response")]),
                     1,quantile,c(.025,.975)),USE.NAMES=T)),
      grd~id,value.var=trt.nms)

paid_full.lgt.ci<-
  dcast(data.table(grd=rep(total_grid,each=2),
                   id=rep(c("lower","upper"),
                          length(total_grid)),
                   sapply(trt.nms,
                          function(x)apply(replicate(
                            BB,data_r2_own[.(x)][
                              sample(.N,rep=T),
                              predict(glm(paid_full~log(total_due),
                                          family=binomial(link="logit")),
                                      data.table(total_due=total_grid),
                                      type="response")]),
                            1,quantile,c(.025,.975)),USE.NAMES=T)),
        grd~id,value.var=trt.nms)

##Bar Plots ####
###Ever Paid Bar Plots
plot.params<-{list(list(dt=by_own_bs,fn="2_own",tr="big_small",
                        tl="Big/Small",rf="Small",nx=.75,
                        sp=3,yl=c(2,10),dn=NULL),
                   list(dt=by_own_7,fn="7_own",tr="main_treat",
                        tl="Treatment",rf="Control",nx=.75,
                        sp=NULL,yl=NULL,dn=NULL),
                   list(dt=by_own_7_x05,fn="7_own_x05",tr="main_treat",
                        tl="Treatment\nExcluding Top 5% of Payers",
                        rf="Control",nx=.75,sp=NULL,yl=NULL,dn=NULL),
                   list(dt=by_own_7_x10,fn="7_own_x10",tr="main_treat",
                        tl="Treatment\nExcluding Top 5% of Payers",
                        rf="Control",nx=.75,sp=NULL,yl=NULL,dn=NULL),
                   list(dt=by_prop_7,fn="7_prop",tr="main_treat",
                        tl="Treatment\nProperty Level, SEs "%+%
                          "Clustered by Owner",rf="Control",
                        nx=.75,sp=NULL,yl=NULL,dn=NULL),
                   list(dt=by_own_8,fn="8_own",tr="main_treat",
                        tl="Treatment\nIncluding Holdout Sample",
                        rf="Holdout",nx=.75,sp=NULL,yl=NULL,dn=NULL),
                   list(dt=by_own_all,fn="14_own",tr="treatment",
                        tl="Treatment / Big/Small",
                        rf="Control_Small",nx=.5,
                        sp=NULL,yl=NULL,dn=rep(c(-1,30),.N)))}

type.params<-list(list(mfn="bar_plot_ever_paid_",xn="ep",
                       mtl="Percent Ever Paid",xlb="Percent"),
                  list(mfn="bar_plot_paid_full_",xn="pf",
                       mtl="Percent Paid Full",xlb="Percent"),
                  list(mfn="bar_plot_aver_paid_",xn="tp",
                       mtl="Average Paid",xlb="$"))

sapply(type.params,
       function(y){
         with(y,sapply(plot.params,function(lst){
           with(lst,
                dt[order(get(tr)),{pdf2(img_wd%+%mfn%+%fn%+%".pdf")
                  par(mar=c(5.1,5.1,4.1,1.6))
                  vals<-lapply(mget(xn%+%c("",".ci.lo",".ci.hi")),to.pct)
                  ind<-which(get(tr)==rf)
                  x<-barplot(vals[[1]],names.arg=get(tr),ylim=yl,
                             xlim=c(0,nx.mlt(1.05*max(vals[[3]]),5)),
                             horiz=T,las=1,col=get.col(.N),
                             main=mtl%+%" by "%+%tl,space=sp,
                             xlab=xlb,cex.names=nx,density=dn)
                  arrows(vals[[2]],x,vals[[3]],x,code=3,
                         angle=90,length=.07,lwd=2)
                  abline(v=c(vals[[2]][ind],vals[[3]][ind]),lty=2)
                  dev.off2()}])}))})

##Logit Fit Plots ####
### Ever Paid
logit_ever_paid_7<-glm(ever_paid~log(total_due)*main_treat,
                       data=data_r2_own,
                       family=binomial(link="logit"))

ever_paid_log_preds<-
  dcast(data.table(expand.grid(total_due=total_grid,
                               main_treat=trt.nms)
  )[,l_pred:=predict(logit_ever_paid_7,.SD,type="response")],
  total_due~main_treat,value.var="l_pred"
  )[ever_paid.lgt.ci,on=c(total_due="grd")]

ever_paid_log_preds[,{
  pdf2(img_wd%+%"predict_logit_ever_paid_7.pdf")
  yrng<-range(.SD[,!"total_due",with=F])
  par(mfrow=c(2,3),
      mar=c(0,0,0,0),
      oma=c(5.1,4.1,4.1,1.1))
  for (ii in 1:6){
    x<-trt.nms[ii+1L] #skip Control
    ctrlx<-c("Control",x)
    y<-.SD[,paste0(rep(ctrlx,each=3),
                   c("","_lower","_upper")),with=F]
    matplot(total_due,y,axes=F,
            type="l",lty=rep(c(1,2,2),2),
            lwd=rep(c(2,1,1),2),ylim=yrng,
            col=get.col.nm(rep(ctrlx,each=3)))
    tile_axes(ii,2,3,las=1,cex.axis=.6)
    box()
    legend(max(total_grid),.95*yrng[2],legend=ctrlx,
           col=get.col.nm(ctrlx),lwd=2,lty=1,
           y.intersp=.2,bty="n",xjust=1.5,yjust=.5)
    }
  title("Predicted Probability of Ever Paying\n"%+%
          "by Initial Debt",outer=T)
  mtext("Log $ Due",side=1,outer=T,line=2.5,cex=.8)
  mtext("Probability Ever Paid",side=2,outer=T,line=2.5,cex=.8)
  dev.off2()}]

### Paid in Full
logit_paid_full_7<-glm(paid_full~log(total_due)*main_treat,
                       data=data_r2_own,
                       family=binomial(link="logit"))

paid_full_log_preds<-
  dcast(data.table(expand.grid(total_due=total_grid,
                               main_treat=trt.nms)
  )[,l_pred:=predict(logit_paid_full_7,.SD,type="response")],
  total_due~main_treat,value.var="l_pred"
  )[paid_full.lgt.ci,on=c(total_due="grd")]

paid_full_log_preds[,{
  pdf2(img_wd%+%"predict_logit_paid_full_7.pdf")
  yrng<-range(.SD[,!"total_due",with=F])
  par(mfrow=c(2,3),
      mar=c(0,0,0,0),
      oma=c(5.1,4.1,4.1,1.1))
  for (ii in 1:6){
    x<-trt.nms[ii+1L] #skip Control
    ctrlx<-c("Control",x)
    y<-.SD[,paste0(rep(ctrlx,each=3),
                   c("","_lower","_upper")),with=F]
    matplot(total_due,y,axes=F,
            type="l",lty=rep(c(1,2,2),2),
            lwd=rep(c(2,1,1),2),ylim=yrng,
            col=get.col.nm(rep(ctrlx,each=3)))
    tile_axes(ii,2,3,las=1,cex.axis=.6)
    box()
    legend(max(total_grid),.95*yrng[2],legend=ctrlx,
           col=get.col.nm(ctrlx),lwd=2,lty=1,
           y.intersp=.2,bty="n",xjust=1.5,yjust=.5)
  }
  title("Predicted Probability of Paying in Full\n"%+%
          "by Initial Debt",outer=T)
  mtext("Log $ Due",side=1,outer=T,line=2.5,cex=.8)
  mtext("Probability Paid in Full",side=2,outer=T,line=2.5,cex=.8)
  dev.off2()}]

## Probability Repayment by Quartile
data_r2_own[,total_due_quartile:=
              create_quantiles(total_due,4)]
###Ever Paid
print.xtable(xtable(setnames(dcast(
  data_r2_own[,mean(ever_paid),
              keyby=.(main_treat,total_due_quartile)],
  main_treat~total_due_quartile,value.var="V1"),
  c("Treatment","Q"%+%1:4)),
  caption="Probability Ever Paid by Treatment and Debt Quartile",
  label="table_ever_paid_quartile"),
  include.rownames=F)

###Paid Full
print.xtable(xtable(setnames(dcast(
  data_r2_own[,mean(paid_full),
              keyby=.(main_treat,total_due_quartile)],
  main_treat~total_due_quartile,value.var="V1"),
  c("Treatment","Q"%+%1:4)),
  caption="Probability Paid Full by Treatment and Debt Quartile",
  label="table_paid_full_quartile"),
  include.rownames=F)
  