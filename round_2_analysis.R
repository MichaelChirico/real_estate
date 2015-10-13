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
gis_wd<-"/media/data_drive/gis_data/PA/"
code_wd<-"./analysis_code/"
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
write.packages(code_wd%+%"logs/round_2_"%+%
                 "analysis_session.txt")

#Convenient Functions 
get.col<-function(st){
  cols<-c(Big="blue",Small="red",Control="blue",Amenities="yellow",
          Moral="cyan",Duty="darkgreen",Lien="red",Sheriff="orchid",
          Peer="orange",Holdout="darkgray")
  cols[as.character(st)]
}

#Data Import ####
##Block I: Main Sample
### total_due is pre-study balance
### current_balance is as of July 22, 2015
### total_paid is the accrual between June 1, 2015
###   and July 22, 2015
mainI<-{setnames(setDT(read.xlsx3(
  data_wd%+%"Payments and Balance Penn Letter Experiment_150727.xlsx",
  colIndex=c(2,5,8,9,13:15),
  sheetName="DETAILS",header=T,startRow=9,stringsAsFactors=F,
  colClasses=abbr_to_colClass("cnDn","4111"))),
  c("opa_no","treat15","paid_full","ever_paid",
    "current_balance","earliest_pmt","total_paid"))}

##Block II: Holdout Sample
holdoutII<-{setnames(setDT(read.xlsx3(
  data_wd%+%"req20150709_PennLetterExperiment_"%+%
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
  data_wd%+%"req20150709_PennLetterExperiment (September 2015 update).xlsx",
  sheet="DETAILS",skip=7,na=c("NULL","-"),
  col_names=c("x","opa_no","x","x","latitude","longitude",
              rep("x",5),"paid_full","ever_paid","pmt_agr",
              rep("x",3),"current_balance","earliest_pmt",
              "total_paid","pmt_agr_type","pmt_agr_status",
              "pmt_agr_start","pmt_agr_amt"),
  #Deleting last row b/c read_excel read too many rows
  col_types=abbr_to_colClass("tnttndnt","42921114"))
  )[,earliest_pmt:=as.Date(earliest_pmt)][-.N,!"x",with=F]

###The following accounts changed OPA #s between samples,
###  See correspondence with Darryl Watson dated:
###    Tue, Sep 15, 2015 at 4:09 PM
###  (should also be able to corroborate with Account ID)
update_opas<-data.table(old=c("151102600","884350465"),
                        new=c("151102610","881577275"))
followupIII[update_opas,opa_no:=i.old,on=c("opa_no"="new")]
                
##Block IV: Main Sample Background Data
mainBGIV<-fread("./round_two/round_2_full_data.csv",drop="treatment")

##Block V: Holdout Sample Background Data
holdBGV<-fread("holdout_sample.csv")

##Block VI: Supplemental Geocoding Data
geosuppVI<-fread(data_wd%+%"round_2_supplement_lon_lat.csv")

##Block VII: Spatial Data for Sheriff's Sale properties
geosherVII<-
  fread(data_wd%+%"/sheriffs_sales/"%+%
          "delinquent_sold_used_round_2_w_lon_lat.csv",
        select=c("address","longitude","latitude"))

##Quilting time!
### Framework:
###  I  III IV VI VII
###  II III  V VI  -
bgvars<-names(mainBGIV)%\%"opa_no"
geovars<-c("latitude","longitude")
updvars<-c("paid_full","ever_paid","current_balance",
           "earliest_pmt","total_paid")
properties<-
  setnames(rbind(mainI,holdoutII),
           updvars,updvars%+%"_jul"
           )[setnames(followupIII,updvars,updvars%+%"_aug"),
             on="opa_no"
             ][rbind(mainBGIV,holdBGV,fill=T),
               (bgvars):=mget("i."%+%bgvars),on="opa_no"
               ][geosuppVI,(geovars):=mget("i."%+%geovars),
                 on="opa_no"]

addrs<-"example_address_"%+%1:3
for (addr in addrs){
  ll<-addr%+%c("_longitude","_latitude")
  properties[geosherVII,(ll):=
               .(i.longitude,i.latitude),
             on=setNames("address",addr)]
  #Should update this if we ever add corresponding
  #  SS/Amenities for holdout properties for comparison
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
###Re-set indicators as T/F instead of Y/N
inds<-c("paid_full_jul","ever_paid_jul",
        "paid_full_aug","ever_paid_aug","pmt_agr")
properties[,(inds):=lapply(.SD,function(x)x=="Y"),.SDcols=inds]
###Paid Full actually stored opposite because 
###  question in data is: "Does property have a balance?"
properties[,paid_full_jul:=!paid_full_jul]
properties[,paid_full_aug:=!paid_full_aug]

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
          opa_no %in% fread("analysis_file_end_only_act.csv",
                            select=c("opa_no"))[,unique(opa_no)]]
### Does this property have any of the
###   tax exemptions excluded in Round 1?
properties[fread(data_wd%+%"prop2015.txt",select=c("PARCEL","XMPT CD")),
        flag_abate_exempt:=`i.XMPT CD`!="",on=c(opa_no="PARCEL")]

###Define alternative treatment sets:
####14 treatments (exclude holdout)
properties[(!holdout),treat14:=treat15]
####2 & 3 treatments (big vs. small (vs. holdout))
properties[(!holdout),treat3:=gsub("(.*)_(.*)_(.*)","\\2",treat15)]
properties[(holdout),treat3:=treat15]
properties[(!holdout),treat2:=treat3]
####7 & 8 treatments (main treatments)
properties[(!holdout),treat7:=gsub("_.*","",treat14)]
properties[(!holdout),treat8:=treat7]
properties[(holdout),treat8:=treat15]
####Reorder main treatments for plotting purposes
trt.nms<-c("Control","Amenities","Moral",
           "Duty","Lien","Sheriff","Peer")
properties[,treat14:=factor(treat14,
                            paste0(rep(trt.nms,each=2),
                                   c("_Small_","_Big_"))%+%
                              "Envelope")]
properties[,treat15:=factor(treat15,
                            c("Holdout",levels(treat14)))]
properties[,treat8:=factor(treat8,c("Holdout",trt.nms))]
properties[,treat7:=factor(treat7,trt.nms)]

properties[,treat3:=factor(treat3,c("Holdout","Small","Big"))]
properties[,treat2:=factor(treat2,c("Small","Big"))]

###Get owner-level version of data, keeping only key analysis variables
owners<-
  properties[,.(treat2 = treat2[1L],
                treat3 = treat3[1L],
                treat7 = treat7[1L],
                treat8 = treat8[1L],
                treat14=treat14[1L],
                treat15=treat15[1L],
                ever_paid_jul=any(ever_paid_jul),
                paid_full_aug=all(paid_full_aug),
                total_paid_jul=sum(total_paid_jul),
                total_paid_aug=sum(total_paid_aug),
                total_due=sum(total_due),
                pmt_agr1=any(pmt_agr),
                pmt_agrA=all(pmt_agr)),
             by=owner1]

###  Subsample Flags
####  a) 1% and b) 5% of accounts by repayment numbers
owners[(!holdout),flag_top01:=total_due>=quantile(total_due,.99)]
owners[(!holdout),flag_top05:=total_due>=quantile(total_due,.95)]

####  Same, now including holdout sample population
owners[,flag_top01_h:=total_due>=quantile(total_due,.99)]
owners[,flag_top05_h:=total_due>=quantile(total_due,.95)]

#Fidelity Checks ####
##Returned Mail Rates by Envelope Size
big_returns<-26+17+75+12+16+4+4+192
small_returns<-766+9
returns<-c(big_returns,small_returns)
xtable(matrix(rbind(returns,100*returns/
                      properties[(!holdout),.N,by=treat2]$N),
              ncol=2,dimnames=list(c("Count","Percentage"),
                                   paste(c("Large","Small"),
                                         "Envelopes"))),
       caption="Returned Mail by Envelope Type",
       label="table:return_env",digits=matrix(c(0,0,0,1,0,1),ncol=3))

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
  label="table:content_fidelity"),include.rownames=F)
                              

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
  list(o2=list(dt=properties_own,tr="big_small",
               exprs=mneppftp,nms=eppftp,fn="2_own",
               tl="Big/Small",lv=c("Small","Big"),nx=.75,
               sp=3,yl=c(2,10),dn=quote(NULL)),
       #By owner, main 7 treatments
       o7=list(dt=properties_own,tr="main_treat",
               exprs=quote(.(mean(ever_paid),mean(paid_full),
                             mean(total_paid),
                             median(total_paid[total_paid>0]),
                             mean(total_paid[total_paid>0]))),
               nms=c(eppftp,"md","pp"),fn="7_own",
               tl="Treatment",lv=trt.nms,nx=.75,
               sp=NULL,yl=NULL,dn=quote(NULL)),
       #By owner, main 7 treatments, single-owner properties
       o7so=list(dt=properties_own_so,tr="main_treat",
                 exprs=mneppftp,nms=eppftp,fn="7_own_so",
                 tl="Treatment\nSingle-Owner Properties",
                 lv=trt.nms,nx=.75,sp=NULL,
                 yl=NULL,dn=quote(NULL)),
       #By owner, main 7 treatments,
       #  exclude 5% extreme payers
       o71=list(dt=properties_own_x01,tr="main_treat",
                exprs=mneppftp,nms=eppftp,fn="7_own_x01",
                tl="Treatment\nExcluding Top 1% of Payers",
                lv=trt.nms,nx=.75,sp=NULL,
                yl=NULL,dn=quote(NULL)),
       #By owner, main 7 treatments,
       #  exclude 10% extreme payers
       o75=list(dt=properties_own_x05,tr="main_treat",
                exprs=mneppftp,nms=eppftp,fn="7_own_x05",
                tl="Treatment\nExcluding Top 5% of Payers",
                lv=trt.nms,nx=.75,sp=NULL,
                yl=NULL,dn=quote(NULL)),
       #By owner, full 14 treatments
       o14=list(dt=properties_own,tr="treatment",
                exprs=mneppftp,nms=eppftp,fn="14_own",
                tl="Treatment / Big/Small",
                lv=properties_own[,levels(treatment)],nx=.5,
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
setkey(properties,main_treat)
owners<-sapply(paste0(properties[,unique(main_treat)]),
               function(x)properties[.(x),unique(owner1)],
               USE.NAMES=T)
setkey(properties,owner1)
boot.cis$p7<-{
  list("dt"=setnames(
    data.table(t(sapply(
      trt.nms,function(x)apply(
        replicate(BB,unlist(
          properties[.(sample(owners[[x]],rep=T)),
                  eval(mneppftp)])),1,
        quantile,c(.025,.975)),
      USE.NAMES=T)),keep.rownames=T
      )[properties[,eval(mneppftp),keyby=main_treat],
        on=c(rn="main_treat")],
    c("main_treat",rep(eppftp,each=2)%+%
        c(".ci.lo",".ci.hi"),eppftp)),
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

##Bar Plots ####
type.params<-{list(list(mfn="bar_plot_ever_paid_",xn="ep",trans=to.pct,
                        mtl="Percent Ever Paid",xlb="Percent",xup=5,
                        tps=c("o2","o7","o7so","o71","o75","p7","o8","o14")),
                   list(mfn="bar_plot_paid_full_",xn="pf",trans=to.pct,
                        mtl="Percent Paid Full",xlb="Percent",xup=5,
                        tps=c("o2","o7","o7so","o751","o75","p7","o8","o14")),
                   list(mfn="bar_plot_aver_paid_",xn="tp",trans=identity,
                        mtl="Average Paid",xlb="$",xup=100,
                        tps=c("o2","o7","o7so","o71","o75","p7","o8","o14")),
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
                          horiz=T,las=1,col=get.col(get(tr)),
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
        data=properties_own[total_paid>0],las=1,
        col=get.col(trt.nms),log="x",notch=T,
        main="Distributions of Positive "%+%
          "Payments\nBy Treatment")
axis(side=1,at=10^(0:5),cex.axis=.8,
     labels=paste0("$",formatC(as.integer(10^(0:5)),
                               big.mark=",")))
abline(v=properties_own[total_paid>0&
                       main_treat=="Control",
                     median(total_paid)],lty=2)
dev.off2()

## Probability Repayment by Quartile
properties_own[,total_due_quartile:=
              create_quantiles(total_due,4)]
###Ever Paid
print.xtable(xtable(setnames(dcast(
  properties_own[,mean(ever_paid),
              keyby=.(main_treat,total_due_quartile)],
  main_treat~total_due_quartile,value.var="V1"),
  c("Treatment","Q"%+%1:4)),
  caption="Probability Ever Paid by Treatment and Debt Quartile",
  label="table_ever_paid_quartile"),
  include.rownames=F)

###Paid Full
print.xtable(xtable(setnames(dcast(
  properties_own[,mean(paid_full),
              keyby=.(main_treat,total_due_quartile)],
  main_treat~total_due_quartile,value.var="V1"),
  c("Treatment","Q"%+%1:4)),
  caption="Probability Paid Full by Treatment and Debt Quartile",
  label="table_paid_full_quartile"),
  include.rownames=F)
  

##Geospatial Analysis ####
###map of Philadelphia for overlay
gmap.phila<-GetMap(center=getGeoCode("Philadelphia, PA"),zoom=11)
phila_azav_quad<-
  readShapePoly(
    gis_wd%+%"Azavea_Quadrant_cartogram_r2.shp")
#Done by copying for now; check GH FR #1310 for updates
phila_azav_quad@data<-setDT(phila_azav_quad@data)
phila_azav_quad@data[,orig:=1:.N]
phila_azav_quad@data<-
  phila_azav_quad@data[
    dcast(properties[,mean(ever_paid),
                  keyby=.(main_treat,azavea_quad)
                  ][CJ(unique(main_treat),
                       unique(phila_azav_quad@data$quadrant))
                    ][is.na(V1),V1:=0
                      ][,.(main_treat,
                           ep.over.control=
                             V1-V1[main_treat=="Control"]),
                        by=azavea_quad][!main_treat=="Control"],
          azavea_quad~main_treat,value.var="ep.over.control"),
    on=c(quadrant="azavea_quad")][order(orig)]

scale.value<-function(x,nn=1000,
                      cols=c("red","white","blue"),
                      rng=range(x,na.rm=T)){
  ramp<-colorRampPalette(cols)(nn)
  xseq<-seq(from=rng[1],to=rng[2],length.out=nn)
  min.or.na<-function(y){
    z<-which.min(abs(xseq-y))
    if (length(z))z else NA
  }
  ramp[sapply(x,min.or.na)]
}

PlotOnStaticMap(gmap.phila)
pdf2(img_wd%+%"cartogram_quadrant_ever_paid.pdf")
ncols<-31
layout(mat=matrix(c(1:7,7,7),byrow=T,nrow=3),
       heights=c(.475,.475,.05))
par(mar=c(0,0,1.1,0),
    oma=c(5.1,4.1,4.1,1.1))
for (trt in trt.nms[-1]){
  plot(phila_azav_quad,col=phila_azav_quad@
         data[,scale.value(get(trt),nn=ncols,rng=c(-.15,.15),
                           cols=c(get.col("Control"),
                                  "white",get.col(trt)))],
       main=trt,cex.main=.9)
  text(coordinates(phila_azav_quad),font=2,cex=.4,
       labels=phila_azav_quad@data[,paste0(quadrant)])
}
par(mar=c(0,0,0.1,0))
plot(NA,type="n",ann=FALSE,xlim=c(1,2),ylim=c(1,2),
     xaxt="n",yaxt="n",bty="n")
xs<-seq(1,2,length.out=ncols+1)
rect(xs[-(ncols+1)],1,
     xs[-1],2,col=colorRampPalette(c("blue","white","black")
                                   )(ncols))
mtext(c("-15%","even","15%"),at=c(1,1.5,2),side=3,adj=c(0,.5,1))
title("Cartogram: Ever Paid by City Sector\n"%+%
        "Percentage above Control",outer=T)
dev.off2()

##Financial Analysis ####
lyx.xtable(xtable(
  properties_own[,.(.N,
             tot_due=sum(total_due),
             ev_pd=mean(ever_paid),pd_fl=mean(paid_full),
             tot_pmt=sum(total_paid)),keyby=main_treat
    ][,.("Treatment"=main_treat,
         "Total Debt Owed"=dol.form(tot_due),
         "Percent Ever Paid"=to.pct(ev_pd,dig=0),
         "Percent Paid in Full"=to.pct(pd_fl,dig=0),
         "Dollars Received"=dol.form(tot_pmt),
         "Percent Debt Received"=to.pct(tot_pmt/tot_due,dig=1),
         "Dollars above Control Per Owner"=
           dol.form(tot_pmt/N-tot_pmt[1]/N[1]),
         "Total Surplus over All Owners"=
           dol.form(tot_pmt-tot_pmt[1]/N[1]*N))],
  caption=c("Summary of Effectiveness of Treatment"),label="table:summary",
  align="|c|p{1.4cm}|p{1.8cm}|p{1.2cm}|p{1.2cm}|"%+%
    "p{1.6cm}|p{1.4cm}|p{2.2cm}|p{1.8cm}|",
  digits=c(rep(0,6),1,0,0)),include.rownames=F)

sapply(list(list(dt=properties_own,fl="7_own",tl="",
                 tr="main_treat",rf="Control"),
            list(dt=properties_own_x01,fl="7_own_x01",
                 tl="\nTop 1% of Payers Removed",
                 tr="main_treat",rf="Control"),
            list(dt=properties_own_x05,fl="7_own_x05",
                 tl="\nTop 5% of Payers Removed",
                 tr="main_treat",rf="Control"),
            list(dt=data_holdout_own,fl="8_own",
                 tl="\nvs. Holdout",tr="main_treat",
                 rf="Holdout"),
            list(dt=data_holdout_own[main_treat%in%
                                       c("Holdout","Control")],
                 fl="8_own_control",
                 tl="\nControl vs. Holdout",tr="main_treat",
                 rf="Holdout"),
            list(dt=properties_own,fl="2_own",
                 tl="\nBig vs. Small",
                 tr="big_small",rf="Small"),
            list(dt=properties_own[main_treat%in%
                                  c("Control","Lien")],
                 fl="7_own_lien",tr="main_treat",
                 tl="\nLien vs. Control",rf="Control"),
            list(dt=properties_own[main_treat%in%
                                  c("Control","Lien","Peer")],
                 fl="7_own_lien.peer",tr="main_treat",
                 tl="\nLien & Peer vs. Control",rf="Control")),
       function(x){
         with(x,{setkeyv(dt,tr)
           lessref<-function(x)x-x[,rf]
           dist<-lessref(sapply(
             dt[,paste0(unique(get(tr)))],
             function(y){tpx<-dt[.(y),total_paid]
             replicate(BB,mean(sample(tpx,rep=T)))})) %*%
             dt[,.N,by=tr]$N
           pdf2(img_wd%+%"histogram_surplus_"%+%fl%+%".pdf")
           hist(dist,xaxt="n",xlab="$ Received above "%+%rf,
                main="Distribution of (Bootstrapped) Total Surplus"%+%
                  "\nAcross All Treatments"%+%tl,col="cyan",breaks=50)
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
date_range<-
  properties[,do.call("seq",c(as.list(range(earliest_pmt,na.rm=T)),
                           "by"="day"))]
date_range2<-date_range[seq(1,length(date_range),by=7)]
pdf2(img_wd%+%"cum_haz_ever_paid.pdf")
matplot(date_range,t(sapply(
  date_range,function(x){
    properties[,to.pct(sum(ever_paid[earliest_pmt<=x],na.rm=T)/.N),keyby=main_treat]$V1
  })),type="l",lty=1,lwd=3,col=get.col(trt.nms),
  main="Cumulative Partial Participation by Treatment",
  xaxt="n",xlab="",ylab="Percent Ever Paid",las=1)
axis(side=1,at=date_range2,labels=format(date_range2,"%B %d"),
     las=1,cex.axis=.75)
legend("topleft",legend=trt.nms,col=get.col(trt.nms),
       lwd=3,y.intersp=.5,bty="n")
dev.off2()

pdf2(img_wd%+%"cum_haz_paid_full.pdf")
matplot(date_range,t(sapply(
  date_range,function(x){
    properties[,to.pct(sum(paid_full[earliest_pmt<=x],na.rm=T)/.N),keyby=main_treat]$V1
  })),type="l",lty=1,lwd=3,col=get.col(trt.nms),
  main="Cumulative FullParticipation by Treatment",
  xaxt="n",xlab="",ylab="Percent Paid Full",las=1)
axis(side=1,at=date_range2,labels=format(date_range2,"%B %d"),
     las=1,cex.axis=.75)
legend("topleft",legend=trt.nms,col=get.col(trt.nms),
       lwd=3,y.intersp=.5,bty="n")
dev.off2()

#NEED TO DO AT OWNER LEVEL
properties_own[,.(0,mean(ever_paid),mean(ever_paid_aug)),keyby=main_treat
            ][,{xmx<-nx.mlt(max(V3),.1)
            plot(0,xaxt="n",yaxt="n",bty="n",pch="",ylab="",
                 xlab="",xlim=c(0,xmx),ylim=c(0,8),
                 main="3-Month Follow-Up Ever Paid Rates")
            yl<-1:.N; yu<-yl+.8
            rect(V1,yl,V2,yu,col=get.col(main_treat))
            rect(V2,yl,V3,yu,col=get.col(main_treat),density=50)
            axis(side=1,at=seq(0,xmx,by=.1),pos=.8)
            axis(side=2,at=.5*(yl+yu),tick=F,las=1,
                 labels=main_treat,pos=0)}]

properties_own[,.(0,mean(paid_full),mean(paid_full_aug)),keyby=main_treat
            ][,{xmx<-nx.mlt(max(V3),.1)
            plot(0,xaxt="n",yaxt="n",bty="n",pch="",ylab="",
                 xlab="",xlim=c(0,xmx),ylim=c(0,8),
                 main="3-Month Follow-Up Paid Full Rates")
            yl<-1:.N; yu<-yl+.8
            rect(V1,yl,V2,yu,col=get.col(main_treat),density=85)
            rect(V2,yl,V3,yu,col=get.col(main_treat))
            axis(side=1,at=seq(0,xmx,by=.1),pos=.8)
            axis(side=2,at=.5*(yl+yu),tick=F,las=1,
                 labels=main_treat,pos=0)}]