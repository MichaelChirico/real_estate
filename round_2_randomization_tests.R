# Real Estate Tax Experiment Randomization Tests, Round 2
# Michael Chirico
# June 10, 2015

# Setup: Packages, Working Directory, Convenient Functions ####
rm(list=ls(all=T))
gc()
setwd("~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/round_two")
library(data.table)
library(xlsx)
library(maptools)

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

# Data import

data<-fread("round_2_full_data.csv")

## Add shorthand for each treatment
data[,tmt:=paste0(substr(treatment,1,1),
                  ifelse(grepl("Big",treatment),"B","S"))]

# Graphic Tests
rand_path<-"./images/randomization_tests/"
## Bar Plot: (Log) Dollar Value of Debt by Treatment
pdf2(paste0(rand_path,"avg_due_by_trt_bar.pdf"))
data[,mean(log(total_due)),by=tmt
     ][,barplot(V1,names.arg=tmt,
                main="Average Log Due by Treatment",ylab="Log $",las=3,
                col=rep(c("yellow","blue","darkgreen","red",
                          "cyan","orange","orchid"),each=2))]
dev.off2()

## Box Plot: Dollar Value of Debt by Treatment
### First, 14 Treatments
pdf2(paste0(rand_path,"dist_due_by_trt_14_box.pdf"))
boxplot(log(total_due)~tmt,data=data,xlab="Log $",
        main="Box Plots of Log Debt\nBy Treatment",
        col=rep(c("yellow","blue","darkgreen","red",
                  "cyan","orange","orchid"),each=2),
        horizontal=T,las=1,notch=T)
dev.off2()
### Next, 7 Treatments (Eliminate Big/Small Distinction)
pdf2(paste0(rand_path,"dist_due_by_trt_7_box.pdf"))
boxplot(log(total_due)~substr(tmt,1,1),data=data,
        main="Box Plots of Log Debt\nBy Treatment",
        col=c("yellow","blue","darkgreen","red",
              "cyan","orange","orchid"),notch=T,
        horizontal=T,las=1,xlab="Log $")
dev.off2()
### Finally, 14 Treatments (Unique Owner)
pdf2(paste0(rand_path,"dist_due_by_trt_14_unq_own_box.pdf"))
boxplot(log(total_due)~tmt,
        data=data[opa_no %in% data[,if (.N==1) opa_no,by=owner1]$V1],
        xlab="Log $",
        main="Box Plots of Log Debt\nBy Treatment, Unique Owners",
        col=rep(c("yellow","blue","darkgreen","red",
                  "cyan","orange","orchid"),each=2),
        horizontal=T,las=1,notch=T)
dev.off2()

## Bar Plot: Number of Properties and Owners by Treatment
pdf2(paste0(rand_path,"number_prop_own_by_trt_bar.pdf"))
data[,.(.N,uniqueN(owner1)),by=tmt
     ][,barplot(c(rbind(N,V2)),names.arg=rep(tmt,each=2),las=3,
                main="Numbers of Properties and Owners\nBy Treatment",
                col=rep(c("yellow","blue","darkgreen","red",
                          "cyan","orange","orchid"),each=4),
                density=rep(c(-1,20),.N),
                ylim=c(0,1.3*max(N)))]
legend("topleft",legend=c("# Properties","# Owners"),density=c(-1,20),bty="n")
dev.off2()

## Bar Plot: Number of Properties and Owners by Treatment (among Unique Owners)
pdf2(paste0(rand_path,"number_prop_own_by_trt_unq_own_bar.pdf"))
data[opa_no %in% data[,if (.N==1) opa_no,by=owner1]$V1,.N,,by=tmt
     ][,barplot(N,names.arg=tmt,las=3,
                main="Numbers of Properties\nBy Treatment, Unique Owners",
                col=rep(c("yellow","blue","darkgreen","red",
                          "cyan","orange","orchid"),each=2))]
dev.off2()

## Map: Number of Properties by Neighborhood by Treatment
phila_azav<-
  readShapePoly("/media/data_drive/gis_data/PA/Neighborhoods_Philadelphia_with_quadrants.shp")

phila_azav<-phila_azav[phila_azav$MAPNAME %in% data[,unique(azavea_nbhd)],]

data[,n_tmt:=.N,by=tmt]
data[,pct_nbhd:=.N/nrow(data),by=azavea_nbhd]
data[,pct_nbhd_tmt:=.N/n_tmt,by=.(tmt,azavea_nbhd)]

nbhd_data<-dcast(unique(data[,.(azavea_nbhd,tmt,pct_nbhd_tmt)]),
                 azavea_nbhd~tmt,value.var="pct_nbhd_tmt")
setkey(nbhd_data[,names(nbhd_data):=
                   lapply(.SD,function(x)ifelse(is.na(x),0,x))],azavea_nbhd)
nbhd_data[setkey(data,azavea_nbhd),full:=i.pct_nbhd]
mx<-max(nbhd_data[,!"azavea_nbhd",with=F])
upd<-setdiff(names(nbhd_data),"azavea_nbhd")
nbhd_data[,(upd):=lapply(.SD,function(x)x/mx),.SDcols=upd]
setcolorder(nbhd_data,c("azavea_nbhd","full",sort(unique(data$tmt))))

setkey(nbhd_data[setkey(as.data.table(phila_azav@data)[,I:=.I],MAPNAME),
                 ord:=i.I],ord)

for (cols in upd){
  pdf2(paste0(rand_path,"properties_by_nbhd_in_",cols,"_treatment_map.pdf"))
  plot(phila_azav,col=gray(1-nbhd_data[[cols]]),
       main=paste0("Distribution of Properties in ",cols))
  dev.off2()
}

## Map: Number of Owners by Neighborhood by Treatment
own_data<-unique(data,by="owner1")
own_data[,n_tmt:=.N,by=tmt]
own_data[,pct_nbhd:=.N/nrow(own_data),by=azavea_nbhd]
own_data[,pct_nbhd_tmt:=.N/n_tmt,by=.(tmt,azavea_nbhd)]

nbhd_own_data<-dcast(unique(own_data[,.(azavea_nbhd,tmt,pct_nbhd_tmt)],by=NULL),
                     azavea_nbhd~tmt,value.var="pct_nbhd_tmt")
setkey(nbhd_own_data[,names(nbhd_own_data):=
                       lapply(.SD,function(x)ifelse(is.na(x),0,x))],azavea_nbhd)
nbhd_own_data[data,full:=i.pct_nbhd]
mx<-max(nbhd_own_data[,!"azavea_nbhd",with=F])
nbhd_own_data[,(upd):=lapply(.SD,function(x)x/mx),.SDcols=upd]
setcolorder(nbhd_own_data,c("azavea_nbhd","full",sort(unique(own_data$tmt))))

setkey(nbhd_own_data[setkey(as.data.table(phila_azav@data)[,I:=.I],MAPNAME),
                 ord:=i.I],ord)

for (cols in upd){
  pdf2(paste0(rand_path,"owners_by_nbhd_in_",cols,"_treatment_map.pdf"))
  plot(phila_azav,col=gray(1-nbhd_own_data[[cols]]),
       main=paste0("Distribution of Properties in ",cols))
  dev.off2()
}
