# Cartograms for Bob ####
phila_azav_quad<-
  readShapePoly(
    wds["gis"]%+%"Azavea_Quadrant_cartogram_r2.shp")
#Done by copying for now; check GH FR #1310 for updates
phila_azav_quad@data<-setDT(phila_azav_quad@data)
phila_azav_quad@data[,orig:=1:.N]
phila_azav_quad@data<-
  phila_azav_quad@data[
    dcast(properties[(!holdout),
                     .(epj=mean(ever_paid_jul),
                       eps=mean(ever_paid_sep)),
                     keyby=.(treat7,azavea_quad)
                     ][,.(treat7,
                          epj.over.control=
                            epj-epj[treat7=="Control"],
                          eps.over.control=
                            eps-eps[treat7=="Control"]),
                       by=azavea_quad][!treat7=="Control"],
          azavea_quad~treat7,
          value.var=c("epj","eps")%+%".over.control"),
    on=c(quadrant="azavea_quad")][order(orig)]

strz<-function(x){
  ifelse(x<=.01,"***",
         ifelse(x<=.05,"**",
                ifelse(x<=.1,"*","")))
}

region_stars<-
  strz(properties[(!holdout),
                  summary(lm(ever_paid_sep~
                               azavea_quad/treat7)
                  )$coefficients[,4]])

pdf2("~/Desktop/cartogram_bw_for_bob_ever_paid_sept.pdf")
layout(mat=matrix(c(1:7,7,7),byrow=T,nrow=3),
       heights=c(.475,.475,.05))
par(mar=c(0,0,1.1,0),
    oma=c(5.1,4.1,4.1,1.1))
vn<-"eps.over.control_"
for (trt in trt.nms[-1]){
  plot(phila_azav_quad,col=phila_azav_quad@
         data[,scale.value(get(vn%+%trt),nn=ncols,rng=c(-.1,.1),
                           cols=c("white","darkgray"))],
       main=trt,cex.main=.9)
  text(coordinates(phila_azav_quad),font=2,
       labels=phila_azav_quad@data[,to.pct(get(vn%+%trt),dig=1)]%+%
         region_stars[grepl(trt,names(region_stars))])
}
par(mar=c(0,0,0.1,0))
plot(NA,type="n",ann=FALSE,xlim=c(1,2),ylim=c(1,2),
     xaxt="n",yaxt="n",bty="n")
xs<-seq(1,2,length.out=ncols+1)
rect(xs[-(ncols+1)],1,
     xs[-1],2,col=colorRampPalette(c("white","darkgray"))(ncols))
mtext(c("-10%","even","10%"),at=c(1,1.5,2),side=3,adj=c(0,.5,1))
title("Cartogram: Ever Paid (Sep.) by City Sector\n"%+%
        "Percentage above Control",outer=T)
dev.off2()

# Toying with experiment "start" date ####
dates<-seq(as.Date("2015-06-01"),
           as.Date("2015-07-30"),by="day")
dates<-dates[!grepl("Sat|Sun",weekdays(dates))]
expdates<-as.Date(c("2015-06-12","2015-06-23"))
matplot(dates,t(sapply(dates,function(date){
  properties[,sum(earliest_pmt_sep>=date,na.rm=T)/
               (.N-sum(earliest_pmt_sep<date,na.rm=T)),
             keyby=treat3]$V1})),ylab="Ever Paid Point Estimate",
  xlab="Date",xaxt="n",type="l",lty=1,lwd=3,
  col=get.col(c("Holdout","Small","Big")))
axis(1,at=dates,labels=dates)
abline(v=expdates,lwd=3)
text(expdates,1.2*par("usr")[3],pos=4,
     c("DoR -> Mail Room:\n"%+%weekdays(expdates[1]),
       "Lawler Posted:\n"%+%weekdays(expdates[2])))

matplot(dates,t(sapply(dates,function(date){
  properties[,sum(abs(earliest_pmt_sep-date)<=2,na.rm=T)/.N,
             keyby=treat3]$V1})),ylab="% First Payment within 2 days",
  xlab="Date",xaxt="n",type="l",lty=1,lwd=3,
  col=get.col(c("Holdout","Small","Big")))
axis(1,at=dates,labels=dates)
abline(v=expdates,lwd=3)
text(expdates,1.2*par("usr")[3],pos=4,
     c("DoR -> Mail Room:\n"%+%weekdays(expdates[1]),
       "Lawler Posted:\n"%+%weekdays(expdates[2])))

matplot(dates,t(sapply(dates,function(date){
  properties[,sum(abs(earliest_pmt_sep-date)<=1,na.rm=T)/.N,
             keyby=treat3]$V1})),ylab="% First Payment within 2 days",
  xlab="Date",xaxt="n",type="l",lty=1,lwd=3,
  col=get.col(c("Holdout","Small","Big")))
axis(1,at=dates,labels=dates)
abline(v=expdates,lwd=3)
text(expdates,1.2*par("usr")[3],pos=4,
     c("DoR -> Mail Room:\n"%+%weekdays(expdates[1]),
       "Lawler Posted:\n"%+%weekdays(expdates[2])))

# Distance and deterrance ####
sheriffs_delinquent_map<-
  readShapePoints(wds["gis"]%+%"delinquent_sold_year_to_may_15_nad.shp",
                  proj4string=CRS("+init=epsg:2272"))

sheriffs_delinquent_map<-
  spTransform(sheriffs_delinquent_map,CRS("+proj=longlat +datum=WGS84"))

#guess at quantiles of distances among all properties

properties[(!holdout),"ss_within_"%+%c("0300","0500","1000","1500"):=
             as.data.table(t(apply(apply(cbind(longitude,latitude),1,
                            spDistsN1,pts=coordinates(sheriffs_delinquent_map),
                            longlat=TRUE),2,
                            function(propd)sapply(c(.3,.5,1,1.5),
                                                  function(D)sum(propd<=D)))))]

dcast(properties[(!holdout),mean(ever_paid_sep),
           keyby=.(treat7,cut(ss_within_0300,c(0,1,5,10,50),
                           include.lowest=T,right=F))],
      cut~treat7,value.var="V1"
      )[,matplot(1:.N,.SD[,!"cut",wi=F],type="l",lty=1,lwd=3,
                 col=get.col(trt.nms))]

# Randomization block-level SEs for EP 3-month ####

setkey(owners,rand_id)
apply(replicate(5e3,owners[.(sample(unique(rand_id),rep=T)),
                        mean(ever_paid_sep),keyby=treat8
                        ][,{x<-V1[treat8!="Holdout"]
                        trt.nms%=%x-V1[treat8=="Holdout"]}]),
      1,quantile,c(.025,.975))

DT <- data.table( A= c(2,5,4,-2,4), 
                  B= c(1000,1100,1375,1650,1485), 
                  C= c(50,55,68.75,82.5,74.25), 
                  D= c(100,275,275,-165,297))
DT

DT <- data.table(A = c(2, 5, 4, -2, 4),
                 B = c(1000, rep(NA, 4)),
                 C = numeric(5),
                 D = numeric(5))

DT[1, c("C", "D") := .(.05 * B, .05 * A * B)]

sapply(2:nrow(DT),function(nn){
  new_B <- DT[nn - 1L, B + D]
  DT[nn, c("B", "C", "D") := .(new_B, .05 * new_B, .05 * A * new_B)]
})

# Trying to do propensity score matching (rounds 1 & 2) ####
rm(list=ls(all=TRUE))
gc()
library(rpart)
library(data.table)
library(funchir)
library(xlsx)

setwd("~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/")
wds <- c(data="/media/data_drive/real_estate/")

full_data <- fread(wds["data"]%+%"prop2014.txt")[nchar(PARCEL)==9]
round_one<-fread("analysis_file_end_only_act.csv")
round_two<-setnames(setDT(read.xlsx3(
  wds["data"]%+%"Payments and Balance Penn Letter Experiment_150727.xlsx",
  colIndex=c(2,5,8,9,13:15),
  sheetName="DETAILS",header=T,startRow=9,stringsAsFactors=F,
  colClasses=abbr_to_colClass("cnDn","4111"))),
  c("opa_no","treat15","paid_full","ever_paid",
    "current_balance","earliest_pmt","total_paid"))

setnames(full_data, 
         c("OWNER1","CENSUS","ZIP","WD GEO","SALE DATE",
           "SALE PR","SALE TY","UNF","MV","TX LND","TX BLDG",
           "XMPT LND","XMPT BLDG","CAT CD","XMPT CD","BLDG CD",
           "ZONE","LND USE","AREA USE","SITE TYP","FRT","DPT",
           "SHP","TOT AREA","TOP","GRG TYP","GRG SP","VIEW",
           "STORIES","GEN CONST","TYP DWELL","EXT COND",
           "QLT GRD","FLR PLAN","NO RM"),
         c("owner1","census","zip","ward_geo","sale_date",
           "sale_price","sale_type","unfinished","market_value",
           "taxable_land","taxable_bldg","exempt_land",
           "exempt_bldg","category","exempt_code","bldg_code",
           "zoning","land_use","area_use","site_type","frontage",
           "depth","shape","total_area","topography","garage_type",
           "garage_spaces","view","stories","construction",
           "dwell_type","exterior","quality_grade","floor_plan","rooms"))

full_data[,zip5:=substr(zip,1,5)]
full_data[,sale_year:=as.integer(substr(sale_date,1,4))]
full_data[,mult_own:= .N > 1, by = owner1]

full_data[round_one, round_1_treat := i.treatment,
          on = c(PARCEL="opa_no")]

full_data[round_two, round_2_treat := i.treat15,
          on = c(PARCEL="opa_no")]

full_data[ , round_1 := !is.na(round_1_treat)]

full_data[ , round_2 := !is.na(round_2_treat)]

full_data[ , in_sample := (round_1 | round_2)]

nums <- c("sale_year","sale_price","market_value","taxable_land",
          "taxable_bldg","exempt_land","exempt_bldg",
          "frontage","depth","total_area","garage_spaces",
          "stories","rooms")

full_data[,(nums):=lapply(.SD,as.numeric),.SDcols=nums]

fact <- c("census","zip5","ward_geo","sale_type","unfinished",
          "category","exempt_code","bldg_code","zoning","land_use",
          "area_use","site_type","shape","topography","garage_type",
          "view","construction","dwell_type","exterior",
          "quality_grade","floor_plan")

full_data[,(fact):=lapply(.SD,as.factor),.SDcols=fact]

x<-full_data[,propens:=
               predict(glm(in_sample~category+exempt_land+
                             market_value,family=binomial),
                       .SD[,.(in_sample,category,exempt_land,
                              market_value)])]

opas<-full_data[(round_1|round_2)&propens%between%c(-3.1,-2.9),PARCEL]

round_one[opa_no%in%opas,mean(ever_paid),keyby=treatment]
round_one[,mean(ever_paid),keyby=treatment]
round_two[,mean(ever_paid=="Y"),.(gsub("_.*","",treat15))]
round_two[opa_no%in%opas,mean(ever_paid=="Y"),.(gsub("_.*","",treat15))]

# Evaluating marginal effects of # letters received ####

some_letters<-properties[(flag_holdout_overlap|
                            treat15=="Holdout"),
                         .(let_rec=cut(.N-sum(treat15=="Holdout"),
                                       c(0:3,5,10,200),right=F,
                                       labels=c(0,1,2,"3-4","5-9","10+")),
                           ever_paid_sep = any(ever_paid_sep)),
                         by=owner1]
BB<-7000
marg<-
  rbindlist(replicate(
    BB,some_letters[sample(.N,rep=T),
                    mean(ever_paid_sep),
                    keyby=let_rec],
    simplify=FALSE),idcol=TRUE
  )[,c("low","high")%=%as.list(quantile(V1,c(.025,.975))),
    by=let_rec][some_letters[,.(avg=mean(ever_paid_sep)),
                             keyby=let_rec],on="let_rec"]

marg[,{pdf2(wds["imga"]%+%"marginal_effect_of_letters_all.pdf")
  x<-barplot(avg,names.arg=let_rec,col="cyan",xlab="Letters Received",
             ylim=c(0,max(`97.5%`)+.05),ylab="Probability Ever Paid",
             main="Marginal Effects of Receiving Letters")
  arrows(x,`2.5%`,x,`97.5%`,
         angle=90,code=3,lwd=1,length=.05)
  text(x,`97.5%`,pos=3,cex=.6,
       labels="n = "%+%
         prettyNum(some_letters[,.N,keyby=let_rec]$N,
                   big.mark=","))
  dev.off2()}]




some_letters<-
  properties[,if(all(treat15=="Holdout")|
                 (all(treat8%in%
                      c("Holdout","Control"))&
                  any(treat8=="Holdout")))
    .(let_rec=cut(.N-sum(treat8=="Holdout"),
                  c(0:3,5,10,200),right=F,
                  labels=c(0,1,2,"3-4","5-9","10+")),
      ever_paid_sep = any(ever_paid_sep)),
    by=owner1]
BB<-7000
marg<-
  rbindlist(replicate(
    BB,some_letters[sample(.N,rep=T),
                    mean(ever_paid_sep),
                    keyby=let_rec],
    simplify=FALSE),idcol=TRUE
  )[,as.list(quantile(V1,c(.025,.975))),
    by=let_rec][some_letters[,.(avg=mean(ever_paid_sep)),
                             keyby=let_rec],on="let_rec"]

marg[,{pdf2(wds["imga"]%+%"marginal_effect_of_letters_cont.pdf")
  x<-barplot(avg,names.arg=let_rec,col="cyan",xlab="Letters Received",
             ylim=c(0,max(`97.5%`)+.05),ylab="Probability Ever Paid",
             main="Marginal Effects of Receiving Letters"%+%
               "\nControl Overlap Only")
  arrows(x,`2.5%`,x,`97.5%`,
         angle=90,code=3,lwd=1,length=.05)
  text(x,`97.5%`,pos=3,cex=.6,
       labels="n = "%+%
         prettyNum(some_letters[,.N,keyby=let_rec]$N,
                   big.mark=","))
  dev.off2()}]



some_letters<-
  properties[,if(.N==2)
    .(let_rec=.N-sum(treat8=="Holdout"),
      ever_paid_sep = any(ever_paid_sep)),
    by=owner1]
BB<-7000
marg<-
  rbindlist(replicate(
    BB,some_letters[sample(.N,rep=T),
                    mean(ever_paid_sep),
                    keyby=let_rec],
    simplify=FALSE),idcol=TRUE
  )[,as.list(quantile(V1,c(.025,.975))),
    by=let_rec][some_letters[,.(avg=mean(ever_paid_sep)),
                             keyby=let_rec],on="let_rec"]

marg[,{pdf2(wds["imga"]%+%"marginal_effect_of_letters_two_prop.pdf")
  x<-barplot(avg,names.arg=let_rec,col="cyan",xlab="Letters Received",
             ylim=c(0,max(`97.5%`)+.05),ylab="Probability Ever Paid",
             main="Marginal Effects of Receiving Letters"%+%
               "\nOwners of Two Properties")
  arrows(x,`2.5%`,x,`97.5%`,
         angle=90,code=3,lwd=1,length=.05)
  text(x,`97.5%`,pos=3,cex=.6,
       labels="n = "%+%
         prettyNum(some_letters[,.N,keyby=let_rec]$N,
                   big.mark=","))
  dev.off2()}]

# Cumulative Hazards with CIs ####
dt.rng<-owners[(!holdout),{rng<-range(get("earliest_pmt_dec"),na.rm=T)
seq(from=rng[1],to=rng[2],by="day")}]
#For pretty printing, get once/week subset
dt.rng2<-dt.rng[seq(1,length(dt.rng),length.out=7)]
snap.dates<-owners[,do.call("c",sapply(
  .SD,max,na.rm=T,simplify=F)),
  .SDcols="earliest_pmt_"%+%c("jul","sep")]


date.dt <- 
  CJ(treat7 = trt.nms, date = dt.rng,
     unique=TRUE, sorted = FALSE)
cum_haz<-owners[(!holdout),sum(ever_paid_dec)+0.,
                keyby=.(treat7,earliest_pmt_dec)
                ][,.(ep=cumsum(V1[idx<-!is.na(earliest_pmt_dec)]),
                     date=earliest_pmt_dec[idx]),by=treat7
                  ][owners[(!holdout),.N,treat7],ep:=ep/i.N,on="treat7"
                    ][date.dt, on = c("treat7", "date"), roll = TRUE
                      ][,.(treat6=treat7[idx<-treat7!="Control"],
                           ep=ep[idx]-ep[!idx]),by=date]

setkey(owners,rand_id)
randids<-owners[(!holdout),unique(rand_id)]
BB <- 5000
cis <- dcast(rbindlist(lapply(integer(BB), function(...){
  dt <- owners[(!holdout)][.(sample(randids,rep=T))]
  dt[,sum(ever_paid_dec)+0.,keyby=.(treat7,earliest_pmt_dec)
     ][,.(ep=cumsum(V1[idx<-!is.na(earliest_pmt_dec)]),
          date=earliest_pmt_dec[idx]),by=treat7
       ][dt[,.N,treat7],ep:=ep/i.N,on="treat7"
         ][date.dt, on = c("treat7", "date"),roll = TRUE
           ][,.(treat6=treat7[idx<-treat7!="Control"],
                ep=ep[idx]-ep[!idx]),by=date]}),idcol="bootID"
  )[,quantile(ep, c(.025, .975), na.rm=T), by = .(treat6, date)],
  treat6+date~c("low","high")[rowid(treat6,date)],value.var="V1")

dcast(cum_haz[cis,on=c("treat6","date")],
      date~treat6,value.var=c("low","ep","high")
      )[,{pdf2(wds["imga"]%+%"cum_haz_ever_paid_dec_7_own_cis.pdf")
        par(mfrow=c(2,3),
              mar=c(0,0,1.1,0),
              oma=c(7.1,4.1,4.1,1.1))
        ylm <- range(.SD[,!"date",with=F])
        axl <- list(list(at=dt.rng2,las=2,
                         labels=format(dt.rng2,"%b %d")),
                    list())
        for (ii in 1:6){
          tr <- (trt.nms%\%"Control")[ii]
          matplot(date, do.call("cbind",mget(c("low_","ep_","high_")%+%tr)),
                  type="l",lty=c(2,1,2),col=get.col(tr),
                  lwd=3,xaxt="n",yaxt="n",ylim=ylm)
          abline(h = 0, col = "black", lwd = 2)
          abline(v = snap.dates, col = "black", lty = 2)
          tile.axes(ii,2,3,axl)
          title(tr)}
        title("Cumulative Partial Participation (Dec.)"%+%
                "\nRelative to Control",outer=T)
        mtext("Date",side=1,outer=T,line=5.5)
        mtext("Probability Ever Paid vs. Control",
              side=2,outer=T,line=2.5)
        dev.off2()}]



date.dt <- 
  CJ(treat8 = c("Holdout","Control"), date = dt.rng,
     unique=TRUE, sorted = FALSE)
cum_haz<-owners[treat8%in%c("Holdout","Control"),
                sum(ever_paid_dec)+0.,
                keyby=.(treat8,earliest_pmt_dec)
                ][,.(ep=cumsum(V1[idx<-!is.na(earliest_pmt_dec)]),
                     date=earliest_pmt_dec[idx]),by=treat8
                  ][owners[treat8%in%c("Holdout","Control"),.N,treat8],
                    ep:=ep/i.N,on="treat8"
                    ][date.dt, on = c("treat8", "date"), roll = TRUE
                      ][,.(ep=ep[idx<-treat8=="Control"]-ep[!idx]),by=date]

BB <- 5000
cis <- dcast(rbindlist(lapply(integer(BB), function(...){
  dt <- owners[treat8%in%c("Holdout","Control")][sample(.N,rep=T)]
  dt[,sum(ever_paid_dec)+0.,keyby=.(treat8,earliest_pmt_dec)
     ][,.(ep=cumsum(V1[idx<-!is.na(earliest_pmt_dec)]),
          date=earliest_pmt_dec[idx]),by=treat8
       ][dt[,.N,treat8],ep:=ep/i.N,on="treat8"
         ][date.dt, on = c("treat8", "date"),roll = TRUE
           ][,.(ep=ep[idx<-treat8=="Control"]-ep[!idx]),
             by=date]}),idcol="bootID"
)[,quantile(ep, c(.025, .975), na.rm=T), by = date],
date~c("low","high")[rowid(date)],value.var="V1")

cum_haz[cis,on=c("date")
        ][,{pdf2(wds["imga"]%+%"cum_haz_ever_paid_dec_cont_hold_own_cis.pdf")
          matplot(date, do.call("cbind",mget(c("low","ep","high"))),
                  type="l",lty=c(2,1,2),col=get.col("Control"),
                  xaxt="n",lwd=3,xlab="Date",
                  ylab="Probability Ever Paid vs. Holdout")
          axis(side=1,at=dt.rng2,labels=format(dt.rng2,"%b %d"))
          abline(h = 0, col = "black", lwd = 2)
          title("Cumulative Partial Participation (Dec.)"%+%
                  "\nControl vs. Holdout")
          dev.off2()}]



date.dt <- 
  CJ(treat2 = c("Small","Big"), date = dt.rng,
     unique=TRUE, sorted = FALSE)
cum_haz<-owners[(!holdout),
                sum(ever_paid_dec)+0.,
                keyby=.(treat2,earliest_pmt_dec)
                ][,.(ep=cumsum(V1[idx<-!is.na(earliest_pmt_dec)]),
                     date=earliest_pmt_dec[idx]),by=treat2
                  ][owners[(!holdout),.N,treat2],
                    ep:=ep/i.N,on="treat2"
                    ][date.dt, on = c("treat2", "date"), roll = TRUE
                      ][,.(ep=ep[idx<-treat2=="Big"]-ep[!idx]),by=date]

BB <- 5000
cis <- dcast(rbindlist(lapply(integer(BB), function(...){
  dt <- owners[(!holdout)][.(sample(randids,rep=T))]
  dt[,sum(ever_paid_dec)+0.,keyby=.(treat2,earliest_pmt_dec)
     ][,.(ep=cumsum(V1[idx<-!is.na(earliest_pmt_dec)]),
          date=earliest_pmt_dec[idx]),by=treat2
       ][dt[,.N,treat2],ep:=ep/i.N,on="treat2"
         ][date.dt, on = c("treat2", "date"),roll = TRUE
           ][,.(ep=ep[idx<-treat2=="Big"]-ep[!idx]),
             by=date]}),idcol="bootID"
)[,quantile(ep, c(.025, .975), na.rm=T), by = date],
date~c("low","high")[rowid(date)],value.var="V1")

cum_haz[cis,on=c("date")
        ][,{pdf2(wds["imga"]%+%"cum_haz_ever_paid_dec_2_own_cis.pdf")
          matplot(date, do.call("cbind",mget(c("low","ep","high"))),
                  type="l",lty=c(2,1,2),col=get.col("Control"),
                  xaxt="n",lwd=3,xlab="Date",
                  ylab="Probability Ever Paid vs. Small")
          axis(side=1,at=dt.rng2,labels=format(dt.rng2,"%b %d"))
          abline(h = 0, col = "black", lwd = 2)
          title("Cumulative Partial Participation (Dec.)"%+%
                  "\nBig vs. Small")
          dev.off2()}]



pfcs<-c("pfj","pfs","pfd")
cis<-dcast(rbindlist(lapply(integer(BB), function(...){
  dt <- owners[(!holdout)][.(sample(randids, rep = T))]
  dt[,.(pfj=mean(paid_full_jul),
        pfs=mean(paid_full_sep),
        pfd=mean(paid_full_dec)),by=treat7
     ][,.(treat6=treat7[idx<-treat7!="Control"],
          pfj=pfj[idx]-pfj[!idx],
          pfs=pfs[idx]-pfs[!idx],
          pfd=pfd[idx]-pfd[!idx])]}),idcol="bootID"
)[,lapply(.SD,quantile,c(.025,.975)),
  .SDcols=pfcs,by=treat6],
treat6~c("low","high")[rowid(treat6)],
value.var=pfcs)


trt.nms6<-trt.nms%\%"Control"

pdf2(wds["imga"]%+%"cum_haz_paid_full_dec_7_own_cis.pdf")
Pfcs<-"paid_full_"%+%c("jul","sep","dec")
bpx<-owners[(!holdout),lapply(.SD,mean),by=treat7,
            .SDcols=Pfcs
            ][,barplot(do.call("rbind",sapply(Pfcs,function(p)
              (x<-get(p))[idx<-treat7!="Control"]-x[!idx],
              simplify=F)),beside=T,
              col=rep(get.col(trt.nms6),each=3),
              ylim=range(cis[,!"treat6",with=F]),
              density=20*3^(0:2))]
title("Proportion Paid in Full Across Time"%+%
        "\nVersus Control")
legend("topleft",legend=trt.nms6,bty="n",y.intersp=.2,
       col=get.col(trt.nms6),lty=1,lwd=7)
legend("bottomright",bty="n",y.intersp=.2,adj=1,
       legend=owners[(!holdout),
                     paste0(c("J: ","S: ","D: "),
                            format(do.call("c",sapply(
                              .SD,max,na.rm=T,simplify=F)),
                            "%b %d")),
                     .SDcols="earliest_pmt_"%+%
                       c("jul","sep","dec")])
text(bpx,0,labels=rep(c("J","S","D"),6),cex=.5)

cis[,arrows(bpx,do.call("rbind",mget(pfcs%+%"_low")),
            bpx,do.call("rbind",mget(pfcs%+%"_high")),
            angle=90,code=3,lwd=1,length=.05)]
dev.off2()


pdf2(wds["imga"]%+%"cum_haz_paid_full_dec_pairs_own_cis.pdf")
par(mfrow=c(1,2),oma=c(0,0,1,0))
cis<-rbindlist(lapply(integer(BB), function(...){
  dt <- owners[treat8%in%c("Holdout","Control")][sample(.N, rep = T)]
  dt[,.(pfj=mean(paid_full_jul),
        pfs=mean(paid_full_sep),
        pfd=mean(paid_full_dec)),by=treat8
     ][,lapply(.SD[,!"treat8",with=F],diff)]}),idcol="bootID"
)[,lapply(.SD,quantile,c(.025,.975)),
  .SDcols=pfcs]

bpx<-owners[treat8%in%c("Holdout","Control"),
            lapply(.SD,mean),keyby=treat8,.SDcols=Pfcs
            ][,barplot(sapply(.SD[,!"treat8",with=F],diff),
                       space=0,names.arg="",
                       beside=T,col=rep(get.col("Control"),3),
                       ylim=range(cis),density=20*3^(0:2))]
title("Control vs. Holdout")
text(bpx,0,labels=c("J","S","D"))

cis[,arrows(bpx,unlist(.SD[1]),
            bpx,unlist(.SD[2]),
            angle=90,code=3,lwd=1,length=.05)]


cis<-rbindlist(lapply(integer(BB), function(...){
  dt <- owners[(!holdout)][.(sample(randids, rep = T))]
  dt[,.(pfj=mean(paid_full_jul),
        pfs=mean(paid_full_sep),
        pfd=mean(paid_full_dec)),by=treat2
     ][,lapply(.SD[,!"treat2",with=F],diff)]}),idcol="bootID"
)[,lapply(.SD,quantile,c(.025,.975)),
  .SDcols=pfcs]

bpx<-owners[(!holdout),
            lapply(.SD,mean),keyby=treat2,.SDcols=Pfcs
            ][,barplot(sapply(.SD[,!"treat2",with=F],diff),space=0,
                       beside=T,col=rep(get.col("Big"),3),
                       ylim=range(cis),
                       density=20*3^(0:2))]
title("Big vs. Small")
text(bpx,0,labels=c("J","S","D"))

cis[,arrows(bpx,unlist(.SD[1]),
            bpx,unlist(.SD[2]),
            angle=90,code=3,lwd=1,length=.05)]
title("Proportion Paid in Full Across Time",outer=T)
dev.off2()

# Significance of Azavea Quad-level results ####

vre<-"ever_paid_"%+%c("jul","sep","dec")
vrp<-"paid_full_"%+%c("jul","sep","dec")
vrs<-c(vre,vrp)

setkey(properties,rand_id)
randids<-owners[(!holdout),unique(rand_id)]

BB<-5000
marg<-dcast(rbindlist(lapply(integer(BB),function(...)
  properties[(!holdout)][.(sample(randids,rep=T)),
                         lapply(.SD,mean),keyby=.(treat7,azavea_quad),.SDcols=vrs
                         ][,c(list(treat7=treat7[idx<-treat7!="Control"]),
                              lapply(.SD[,!"treat7",with=F],
                                     function(x)x[idx]-x[!idx])),
                           by=azavea_quad]),idcol="BootID"
  )[,c(list(CI=c("low","high")),
       lapply(.SD,quantile,c(.025,.975))),
       by=.(azavea_quad,treat7),.SDcols=vrs],
  azavea_quad+treat7~CI,value.var=vrs
  )[properties[(!holdout),lapply(.SD,mean),
               keyby=.(treat7,azavea_quad),.SDcols=vrs
               ][,c(list(treat7=treat7[idx<-treat7!="Control"]),
                    lapply(.SD[,!"treat7",with=F],
                           function(x)x[idx]-x[!idx])),
                 by=azavea_quad],on=c("azavea_quad","treat7")]

pdf2(wds["imga"]%+%"neighborhood_ever_paid_over_time.pdf")
par(mfrow=c(2,3),mar=c(0,0,1,0),oma=c(5.1,4.1,4.1,1.1))
rng <- 1.03*marg[,range(.SD[,grep("ever_paid",
                                  names(marg)),with=F])]
for (ii in 1:length(aq<-marg[,unique(azavea_quad)])){
  marg[azavea_quad==aq[ii],{
    bpx<-barplot(do.call("rbind",mget(vre)),beside=T,
                 col=rep(get.col(treat7),each=3),
                 density=20*3^(0:2),ylim=rng,yaxt="n")
    if(ii %% 3 == 1) axis(side = 2)
    text(bpx,0,labels=rep(c("J","S","D"),6),cex=.4)
    title(aq[ii],cex.main=.8)
    arrows(bpx,do.call("rbind",mget(vre%+%"_low")),
           bpx,do.call("rbind",mget(vre%+%"_high")),
           angle=90,code=3,lwd=.5,length=.01,lty=1)}]
  box()
}
title("Proportion Ever Paid Over Time"%+%
        "\nBy Azavea Quadrant, vs. Control",
      outer=T)
dev.off2()

pdf2(wds["imga"]%+%"neighborhood_paid_full_over_time.pdf")
par(mfrow=c(2,3),mar=c(0,0,1,0),oma=c(5.1,4.1,4.1,1.1))
rng <- 1.03*marg[,range(.SD[,grep("paid_full",
                                  names(marg)),with=F])]
for (ii in 1:length(aq<-marg[,unique(azavea_quad)])){
  marg[azavea_quad==aq[ii],{
    bpx<-barplot(do.call("rbind",mget(vrp)),beside=T,
                 col=rep(get.col(treat7),each=3),
                 density=20*3^(0:2),ylim=rng,yaxt="n")
    if(ii %% 3 == 1) axis(side = 2)
    text(bpx,0,labels=rep(c("J","S","D"),6),cex=.4)
    title(aq[ii],cex.main=.8)
    arrows(bpx,do.call("rbind",mget(vrp%+%"_low")),
           bpx,do.call("rbind",mget(vrp%+%"_high")),
           angle=90,code=3,lwd=.5,length=.01,lty=1)}]
  box()
}
title("Proportion Paid Full Over Time"%+%
        "\nBy Azavea Quadrant, vs. Control",
      outer=T)
dev.off2()

# paid_full time series ####

payments_o <- payments[order(treat14), .(principal = sum(principal),
                            total_paid = sum(total_paid),
                            rand_id = rand_id[1L]),
                       by = .(owner1, valid)
                       ][owners, c("total_due","treat8") := 
                           .(i.total_due, treat8), on = "owner1"
                         ][,c("cum_paid","paid_full"):=
                             {cp<-cumsum(total_paid)
                             .(cp, cp >= total_due)}, by = owner1]

payments_o[owners,.SD,on="owner1"]

payments_o[owner1 %in% properties[


setkey(properties,treat8)

payments[, treat8_N:=i.N, on = "treat8"]

setkey(payments,treat8,valid)

dt.rng<-payments[treat8!="Holdout",{rng<-range(valid)
seq(from=rng[1],to=rng[2],by="day")}]

png("~/Desktop/pf_cum_hz_prelim.png")
dcast(rbindlist(lapply(dt.rng, function(tt){
  unique(payments[valid<=tt],by="account",fromLast=TRUE
         )[,.(dt=tt,pf=sum(paid_full)),by=treat8]})
  )[properties[,.N,treat8],paid_full:=pf/i.N,on="treat8"],
  dt~treat8,value.var="paid_full"
  )[,matplot(dt,y<-.SD[,!"dt",with=F],type="l",lty=1,lwd=3,
             col=get.col(names(y)))]
legend("topleft",legend=c("Holdout",trt.nms),
       col=get.col(c("Holdout",trt.nms)),lwd=3)
dev.off()

unique(payments,by="account",fromLast=TRUE)[,sum(paid_full),treat8]

properties[unique(payments,by="account",fromLast=TRUE),
           table(paid_full_dec, i.paid_full,
                 useNA="ifany"),
           on="account"]

write.csv(rbind(properties[account%in%unique(payments,by="account",fromLast=TRUE
       )[properties,on="account"][is.na(cum_paid),account]&total_paid_dec!=0,
       .(account,cum_paid=NA,total_paid_jul,total_paid_sep,total_paid_dec)],
unique(payments,by="account",fromLast=TRUE
       )[properties,on="account"
         ][abs(cum_paid-total_paid_dec) > 1,
           .(account,cum_paid,total_paid_jul,
             total_paid_sep,total_paid_dec)]),file="~/Desktop/payment_mismatch.csv",
row.names=F)

unique(payments_o,by="owner1",fromLast=TRUE
       )[,.(payments=as.numeric(sum(paid_full))),keyby=treat8
         ][owners[,.(.N,to.pct(mean(paid_full_dec),dig=1)),treat8],
           `:=`(payments=to.pct(payments/i.N,dig=1),
                main_data=i.V2),on="treat8"]

properties[,.(main_data=to.pct(mean(paid_full_dec),dig=1)),keyby=treat8]


unique(payments_o,by="owner1",fromLast=TRUE
       )[,.(payments=as.numeric(sum(paid_full))),keyby=treat8
         ][owners[,.(.N,own_main=to.pct(mean(paid_full_dec),dig=1)),treat8
                  ],`:=`(payments=to.pct(payments/i.N,dig=1),
                         own_main=i.own_main),on="treat8"][]

dt.rng<-payments_o[,{rng<-range(valid)
seq(from=rng[1],to=rng[2],by="day")}]

png("~/Desktop/pf_cum_hz_prelim_own.png")
dcast(rbindlist(lapply(dt.rng, function(tt){
  unique(payments_o[valid<=tt],by="owner1",fromLast=TRUE
  )[,.(dt=tt,pf=sum(paid_full)),by=treat8]})
)[owners[,.N,treat8],paid_full:=pf/i.N,on="treat8"],
dt~treat8,value.var="paid_full"
)[,matplot(dt,y<-.SD[,!"dt",with=F],type="l",lty=1,lwd=3,
           col=get.col(names(y)))]
legend("topleft",legend=c("Holdout",trt.nms),
       col=get.col(c("Holdout",trt.nms)),lwd=3)
dev.off()

ownsmp<-properties[treat8=="Holdout",sample(unique(owner1),10)]
ownsmp2<-properties[treat8=="Holdout",sample(unique(owner1),5)]
