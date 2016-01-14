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
                        setNames(x-V1[treat8=="Holdout"],
                                 trt.nms)}]),
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

properties[owner1%in%properties[ , if(all(flag_holdout_overlap)&
                 any(treat8=="Control")) owner1,owner1]$V1,.N]

some_letters<-properties[(flag_holdout_overlap|
                            treat15=="Holdout"),
                         .(let_rec=.N-sum(treat15=="Holdout"),
                           ever_paid_sep = any(ever_paid_sep)),
                         by=owner1]

setkey(some_letters,owner1)

cis<-dcast(rbindlist(replicate(
  5000,some_letters[.(sample(owner1,rep=T))
                  ][let_rec<=10,
                    mean(ever_paid_sep),keyby=let_rec],
  simplify=FALSE),idcol=TRUE
)[,quantile(V1,c(.025,.975)),by=let_rec],
let_rec~c("low","high")[rowid(let_rec)],value.var="V1")

png("~/Desktop/number_letters.png")
cis[,matplot(let_rec,cbind(low,high),
             lty=2,type="l",lwd=3,col="black",
             xlab="Letters Received",ylab="Ever Paid (Sep)",
             main="Evidence for Effect of Quantity Received")]
some_letters[let_rec<=10,mean(ever_paid_sep),
             keyby=let_rec
             ][,lines(let_rec,V1,lty=1,lwd=3,col="blue")]
cis[,abline(h=high[1L],lty=3,lwd=2,col="red")]
dev.off()

some_letters[let_rec<=10,
             .(`Percent Ever Paid (Sep.)`=
                 mean(ever_paid_sep)),
             keyby=.(`Letters Received`=let_rec)]