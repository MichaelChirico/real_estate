#Data Analysis
#Philadelphia Real Estate Tax Evasion
#  Round 2
#Michael Chirico
#July 29, 2015

#Setup: Random Seed, Packages,
#  Working Directory, Convenient Functions ####
##Random Seed
###Alternating digits (1,3,...) of
###  my CVS Extra Care card number
set.seed(4746966)

##Packages
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
library(sp)
library(doParallel)

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
data_r2<-{setkey(setnames(setDT(read.xlsx3(
  data_wd%+%"Payments and Balance Penn Letter Experiment_150727.xlsx",
  colIndex=c(2,5,7:15),
  sheetName="DETAILS",header=T,startRow=9,stringsAsFactors=F,
  colClasses=abbr_to_colClass("cidndn","512111"))),
  c("opa_no","treatment","pre_15_balance",
    "paid_full","ever_paid","years_count",
    "period_min","period_max","current_balance",
    "earliest_pmt","total_paid")),opa_no,treatment)}
###Re-set indicators as T/F instead of Y/N
inds<-c("pre_15_balance","paid_full","ever_paid")
data_r2[,(inds):=lapply(.SD,function(x)x=="Y"),.SDcols=inds]
data_r2[,paid_full:=!paid_full]

###Define two alternative treatments:
###  Collapse Big/Small or Focus on Big/Small
data_r2[grepl("Big",treatment),big_small:="Big"]
data_r2[is.na(big_small),big_small:="Small"]

data_r2[,main_treat:=gsub("_.*","",treatment)]
####Reorder main treatments for plotting purposes
trt.nms<-c("Control","Amenities","Moral",
             "Duty","Lien","Sheriff","Peer")
data_r2[,treatment:=factor(treatment,
                           paste0(rep(trt.nms,each=2),
                                  c("_Small_","_Big_"))%+%
                             "Envelope")]
data_r2[,main_treat:=
          factor(main_treat,trt.nms)]

##Original experiment data
###Merge what we need into main data
data_r2<-data_r2[fread("./round_two/round_2_full_data.csv",
                       drop="treatment"),on="opa_no"]

##Property coordinates
data_r2[setDT(read.xlsx3(
  data_wd%+%"req20150709_PennLetter"%+%
    "Experiment_v2_Coordinates.xlsx",
  sheetName="TREATMENTS",colIndex=c(1,4,5),
  colClasses=c("character",rep("numeric",2)))),
  `:=`(longitude=X_LONG,latitude=Y_LAT),
  on=c(opa_no="BRT.NUMBER")]

##Sheriff's Sale property coordinates
sheriffs_delinquent<-
  fread(data_wd%+%"/sheriffs_sales/"%+%
          "delinquent_sold_used_round_2_w_lon_lat.csv",
        select=c("address","longitude","latitude"))

addrs<-"example_address_"%+%1:3
for (addr in addrs){
  ll<-addr%+%c("_longitude","_latitude")
  data_r2[sheriffs_delinquent,
          (ll):=list(i.longitude,i.latitude),
          on=setNames("address",addr)]
  data_r2[!is.na(longitude),addr%+%"_distance":=
            spDists(x=cbind(longitude,latitude),
                    y=Reduce(cbind,mget(ll)),
                    longlat=T,diagonal=T)]
}

data_r2[,sheriff_distance_min:=
          do.call("pmin",mget(addrs%+%"_distance"))]

data_r2[,sheriff_distance_mean:=
          rowMeans(Reduce(cbind,mget(addrs%+%"_distance")))]


##Holdout data
data_holdout<-{
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
            owner1:=owner1,on="opa_no"],fill=T),opa_no)}

###Coordinates for holdout
data_holdout[setDT(read.xlsx3(
  data_wd%+%"req20150709_PennLetter"%+%
    "Experiment_v2_Coordinates.xlsx",
  sheetName="CONTROL",colIndex=c(1,4,5),
  colClasses=c("character",rep("numeric",2)))),
  `:=`(latitude=Y_LAT,longitude=X_LONG),
  on=c(opa_no="BRT.NUMBER")]

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
  data_r2[,.(main_treat=main_treat[1L],
             treatment=treatment[1L],
             big_small=big_small[1L],
             ever_paid=any(ever_paid),
             paid_full=all(paid_full),
             total_paid=sum(total_paid),
             total_due=sum(total_due)),
          by=owner1],treatment)

###Get owner-level version of data, dropping the top
###  a) 5% and b) 10% of accounts by repayment numbers
data_r2_own_x05<-
  data_r2_own[total_paid<quantile(total_paid,.95)]
data_r2_own_x10<-
  data_r2_own[total_paid<quantile(total_paid,.90)]

###Get owner-level version of data for single-owner
###  property subsample
data_r2_own_so<-
  data_r2_own[data_r2[!(flag_multiple_property),
                      .(owner1)],on="owner1"]

###Get owner-level version of holdout data
data_holdout_own<-
  data_holdout[,.(treatment=treatment[1],
                  ever_paid=any(ever_paid),
                  paid_full=all(paid_full),
                  total_paid=sum(total_paid),
                  total_due=sum(total_due)),
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

###Point Estimate CIs ####
###For the standard confidence intervals,
###  we can condense the process by storing
###  the key changing parameters in this list;
###  also store a bunch of plotting parameters
###  since we'll use the result of this list for that
mneppftp<-quote(.(mean(ever_paid),
                  mean(paid_full),
                  mean(total_paid)))
eppftp<-c("ep","pf","tp")
bootlist<-{
  #By owner, big vs. small
  list(o2=list(dt=data_r2_own,tr="big_small",
               exprs=mneppftp,nms=eppftp,fn="2_own",
               tl="Big/Small",lv=c("Small","Big"),nx=.75,
               sp=3,yl=c(2,10),dn=quote(NULL)),
       #By owner, main 7 treatments
       o7=list(dt=data_r2_own,tr="main_treat",
               exprs=quote(.(mean(ever_paid),mean(paid_full),
                             mean(total_paid),
                             median(total_paid[total_paid>0]),
                             mean(total_paid[total_paid>0]))),
               nms=c(eppftp,"md","pp"),fn="7_own",
               tl="Treatment",lv=trt.nms,nx=.75,
               sp=NULL,yl=NULL,dn=quote(NULL)),
       #By owner, main 7 treatments, single-owner properties
       o7so=list(dt=data_r2_own_so,tr="main_treat",
                 exprs=mneppftp,nms=eppftp,fn="7_own_so",
                 tl="Treatment\nSingle-Owner Properties",
                 lv=trt.nms,nx=.75,sp=NULL,
                 yl=NULL,dn=quote(NULL)),
       #By owner, main 7 treatments,
       #  exclude 5% extreme payers
       o75=list(dt=data_r2_own_x05,tr="main_treat",
                exprs=mneppftp,nms=eppftp,fn="7_own_x05",
                tl="Treatment\nExcluding Top 5% of Payers",
                lv=trt.nms,nx=.75,sp=NULL,
                yl=NULL,dn=quote(NULL)),
       #By owner, main 7 treatments,
       #  exclude 10% extreme payers
       o71=list(dt=data_r2_own_x10,tr="main_treat",
                exprs=mneppftp,nms=eppftp,fn="7_own_x10",
                tl="Treatment\nExcluding Top 10% of Payers",
                lv=trt.nms,nx=.75,sp=NULL,
                yl=NULL,dn=quote(NULL)),
       #By owner, full 14 treatments
       o14=list(dt=data_r2_own,tr="treatment",
                exprs=mneppftp,nms=eppftp,fn="14_own",
                tl="Treatment / Big/Small",
                lv=data_r2_own[,levels(treatment)],nx=.5,
                sp=NULL,yl=NULL,dn=quote(rep(c(-1,30),.N))))}

boot.cis<-{
  lapply(bootlist,function(z){
    print(names(z))
    with(z,list("dt"=setnames(setkeyv(
      #First, point estimates from raw data
      dt,tr)[,eval(exprs),by=tr
              ][data.table(t(sapply(paste0(
                dt[,unique(get(tr))]),
                function(w){
                  apply(replicate(
                    #to guarantee equal representation of each
                    #  treatment, block at the treatment level--
                    #  resample N_t observations for each treatment t
                    BB,unlist(dt[.(w)][
                      #calculate point estimate in re-sample
                      sample(.N,.N,T),eval(exprs)])),
                    #CIs are given by the 2.5 &
                    #  97.5 %ile of each measure
                    1,quantile,c(.025,.975))},
                USE.NAMES=T)),keep.rownames=T),
                on=setNames("rn",tr)
                ][,(tr):=factor(get(tr),lv)],
      c(tr,nms,rep(nms,each=2)%+%
          c(".ci.lo",".ci.hi"))),
      "fn"=fn,"tr"=tr,"tl"=tl,"rf"=lv[1L],
      "nx"=nx,"sp"=sp,"yl"=yl,"dn"=dn))})}

###Now add confidence intervals for more complicated
###  scenario--results at the property level,
###  clustering by owner by resampling owners
setkey(data_r2,main_treat)
owners<-sapply(paste0(data_r2[,unique(main_treat)]),
               function(x)data_r2[.(x),unique(owner1)],
               USE.NAMES=T)
setkey(data_r2,owner1)
boot.cis$p7<-{
  list("dt"=setnames(
    data_r2[,eval(mneppftp),by=main_treat
            ][data.table(t(sapply(
              trt.nms,function(x)apply(
                replicate(BB,unlist(
                  data_r2[.(sample(owners[[x]],rep=T)),
                          eval(mneppftp)])),1,
                quantile,c(.025,.975)),
              USE.NAMES=T)),keep.rownames=T),
              on=c(main_treat="rn")],
    c("main_treat",eppftp,rep(eppftp,each=2)%+%
        c(".ci.lo",".ci.hi"))),
    fn="7_prop",tr="main_treat",
    tl="Treatment\nProperty Level, SEs "%+%
      "Clustered by Owner",rf="Control",
    nx=.75,sp=NULL,yl=NULL,dn=quote(NULL))}

###Lastly, for the holdout sample, we simply need to
###  append the holdout results--no need to repeat
###  sampling for the main treatments again
data_holdout_nf<-
  data_holdout[!(flag_main_sample_overlap)]
boot.cis$o8<-{
  list("dt"=rbind(
    fill=T,boot.cis$o7$dt,data.table(
      main_treat="Holdout",
      rbind(setNames(c(unlist(
        data_holdout_nf[,eval(mneppftp)]),
        c(apply(replicate(
          BB,unlist(data_holdout_nf[
            sample(.N,.N,T),eval(mneppftp)])),
          1,quantile,c(.025,.975)))),
        c(eppftp,rep(eppftp,each=2)%+%
            c(".ci.lo",".ci.hi")))))
  )[,main_treat:=
      factor(main_treat,c("Holdout",trt.nms))],
  fn="8_own",tr="main_treat",
  tl="Treatment\nIncluding Holdout Sample",
  rf="Holdout",nx=.75,sp=NULL,
  yl=NULL,dn=quote(NULL))}

###Logit Prediction CIs ####
logit.params<-{
  list(td.ep=list(dn="data_r2_own",ky="main_treat",
                  vr="total_due",kv=trt.nms,
                  ov=quote(ever_paid),nm="total_grid",pd="ep",
                  fn="ever_paid",tl="Ever Paying",
                  st="Initial Debt",xl="Log $ Due",
                  yl="Probability Ever Paid"),
       td.pf=list(dn="data_r2_own",ky="main_treat",
                  vr="total_due",kv=trt.nms,
                  ov=quote(paid_full),nm="total_grid",pd="pf",
                  fn="paid_full",tl="Paying in Full",
                  st="Initial Debt",xl="Log $ Due",
                  yl="Probability Paid in Full"),
       dmn=list(dn="data_r2",ky="main_treat",kv=trt.nms,
                vr="sheriff_distance_min",
                ov=quote(ever_paid),nm="dist_grid",pd="ep",
                fn="dist_min",tl="Ever Paying",
                st="Distance to Sheriff's Sale (min)",
                xl="Log km",yl="Probability Ever Paid"),
       dmu=list(dn="data_r2",ky="main_treat",kv=trt.nms,
                vr="sheriff_distance_mean",
                ov=quote(ever_paid),nm="dist_grid",pd="ep",
                fn="dist_avg",tl="Ever Paying",
                st="Distance to Sheriff's Sale (mean)",
                xl="Log km",yl="Probability Ever Paid"),
       dfr=list(dn="data_r2",ky="main_treat",kv=trt.nms,
                vr="example_address_1_distance",
                ov=quote(ever_paid),nm="dist_grid",pd="ep",
                fn="dist_one",tl="Ever Paying",
                st="Distance to Sheriff's Sale (first)",
                xl="Log km",yl="Probability Ever Paid"))}

system.time({logit.preds<-
  lapply(logit.params,function(x){
    print(names(x))
    with(x,{dt<-get(dn,envir=globalenv())
    grd<-dt[,seq(min(get(vr),na.rm=T),
                 max(get(vr),na.rm=T),
                 length.out=1e3)]
    setkeyv(dt,ky)
    cl <- makeCluster(detectCores())
    registerDoParallel(cl)
    clusterExport(cl,c(dn,"BB"),envir=environment())
    clusterEvalQ(cl,{library("data.table")})
    out<-foreach(i = kv,.combine=rbind) %dopar% {
      outdt<-setnames(data.table(vr=grd),vr)
      print("yo2")
      outdt[,(pd):=dt[.(i)][
        !is.na(get(vr)),
        predict(glm(eval(ov)~log(get(vr)),family=binomial),
                outdt,type="response")]]
      ci.lim<-apply(replicate(
        BB,dt[.(i)][!is.na(get(vr))][
          sample(.N,rep=T),
          predict(glm(eval(ov)~log(get(vr)),family=binomial),
                  outdt,type="response")]),
        1,quantile,c(.025,.975))
      outdt[,c(ky,paste0(pd,c(".ci.lo",".ci.hi"))):=
              list(i,ci.lim[1,],ci.lim[2,])]
      setnames(outdt,vr,nm)
      outdt
    }
    stopCluster(cl)
    list("dt"=out,"fn"=fn,"nm"=nm,"ky"=ky,"kv"=kv,
         "tl"=tl,"st"=st,"xl"=xl,"yl"=yl,"pd"=pd)})})})

##Bar Plots ####
type.params<-{list(list(mfn="bar_plot_ever_paid_",xn="ep",trans=to.pct,
                        mtl="Percent Ever Paid",xlb="Percent",xup=5,
                        tps=c("o2","o7","o7so","o75","o71","p7","o8","o14")),
                   list(mfn="bar_plot_paid_full_",xn="pf",trans=to.pct,
                        mtl="Percent Paid Full",xlb="Percent",xup=5,
                        tps=c("o2","o7","o7so","o75","o71","p7","o8","o14")),
                   list(mfn="bar_plot_aver_paid_",xn="tp",trans=identity,
                        mtl="Average Paid",xlb="$",xup=100,
                        tps=c("o2","o7","o7so","o75","o71","p7","o8","o14")),
                   list(mfn="bar_plot_med_pos_paid_",xn="md",trans=identity,
                        mtl="Median Positive Amount Paid",xlb="$",
                        tps=c("o7"),xup=100),
                   list(mfn="bar_plot_avg_pos_paid_",xn="pp",trans=identity,
                        mtl="Average Positive Amount Paid",xlb="$",
                        tps=c("o7"),xup=100))}

sapply(type.params,
       function(y){
         with(y,sapply(boot.cis[tps],function(lst){
           with(lst,
             dt[order(get(tr)),{pdf2(img_wd%+%mfn%+%fn%+%".pdf")
               par(mar=c(5.1,5.1,4.1,1.6))
               vals<-lapply(mget(xn%+%c("",".ci.lo",".ci.hi")),trans)
               ind<-which(get(tr)==rf)
               x<-barplot(vals[[1]],names.arg=get(tr),ylim=yl,
                          xlim=c(0,nx.mlt(1.05*max(vals[[3]]),xup)),
                          horiz=T,las=1,col=get.col(.N),
                          main=mtl%+%" by "%+%tl,space=sp,
                          xlab=xlb,cex.names=nx,density=eval(dn))
               arrows(vals[[2]],x,vals[[3]],x,code=3,
                      angle=90,length=.07,lwd=2)
               abline(v=c(vals[[2]][ind],vals[[3]][ind]),lty=2)
               dev.off2()}])}))})

##Box and Whisker Plots ####
###Box-and-Whisker Repayment Distribution (among Payers)
pdf2(img_wd%+%"box_whisk_pos_paid_7_own.pdf")
par(mar=c(2.6,5.1,4.1,1.1))
boxplot(total_paid~main_treat,horizontal=T,
        cex.axis=.8,xaxt="n",boxwex=.5,
        data=data_r2_own[total_paid>0],las=1,
        col=get.col(7L),log="x",notch=T,
        main="Distributions of Positive "%+%
          "Payments\nBy Treatment")
axis(side=1,at=10^(0:5),cex.axis=.8,
     labels=paste0("$",formatC(as.integer(10^(0:5)),
                               big.mark=",")))
abline(v=data_r2_own[total_paid>0&
                       main_treat=="Control",
                     median(total_paid)],lty=2)
dev.off2()

##Logit Fit Plots ####
lapply(logit.preds,function(x){
  with(x,dt[,{
    pdf2(img_wd%+%"predict_logit_"%+%fn%+%".pdf")
    yrng<-range(.SD[,!c(nm,ky),with=F])
    par(mfrow=c(2,3),
        mar=c(0,0,0,0),
        oma=c(5.1,4.1,4.1,1.1))
    ctrl<-.SD[get(ky)=="Control",with=F,
              j=pd%+%c("",".ci.lo",".ci.hi")]
    gr<-.SD[1:1000L,get(nm)]
    for (ii in 1:6){
      ctrlx<-kv[c(1,ii+1L)]
      y<-.SD[get(ky)==kv[-1][ii],with=F,
             j=pd%+%c("",".ci.lo",".ci.hi")]
      matplot(log(gr),cbind(ctrl,y),axes=F,type="l",
              lty=rep(c(1,2,2),2),
              lwd=rep(c(2,1,1),2),ylim=yrng,
              col=get.col.nm(rep(ctrlx,each=3)))
      tile_axes(ii,2,3,las=1,cex.axis=.6)
      box()
      legend(max(log(gr)),.95*yrng[2],legend=ctrlx,
             col=get.col.nm(ctrlx),lwd=2,lty=1,
             y.intersp=.2,bty="n",xjust=1.5,yjust=.5)
    }
    title("Predicted Probability of "%+%tl%+%
            "\nby "%+%st,outer=T)
    mtext(xl,side=1,outer=T,line=2.5,cex=.8)
    mtext(yl,side=2,outer=T,line=2.5,cex=.8)
    dev.off2()}])})

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
  

##Geospatial Analysis ####

