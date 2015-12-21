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