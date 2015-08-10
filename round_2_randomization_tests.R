# Real Estate Tax Experiment Randomization Tests, Round 2
# Michael Chirico
# June 10, 2015

# Setup: Packages, Working Directory, Convenient Functions ####
rm(list=ls(all=T))
gc()
setwd("~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency")
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

# Data import ####

data<-fread("./round_two/round_2_full_data.csv")

## Add shorthand for each treatment
data[,tmt:=paste0(substr(treatment,1,1),
                  ifelse(grepl("Big",treatment),"B","S"))
     ][,bs:=gsub(".*(?=(Big)|(Small))|(?<=(Big)|(Small)).*",
                 "",treatment,perl=T)
       ][,tmt7:=gsub("_.*","",treatment)]

data_own<-data[,.(tmt=tmt[1],bs=bs[1],tmt7=tmt7[1],
                  total_due=sum(total_due)),by=owner1]

# Graphic Tests
rand_path<-"./papers_presentations/round_two/images/balance/"
## Log Balance by Letter
pdf2(paste0(rand_path,"dist_log_due_by_trt_7_box.pdf"))
par(mar=c(7.1,6.1,6.1,2.1))
boxplot(log10(total_due)~tmt7,data=data_own,
        main="Box Plots of Log Debt\nBy Treatment",
        col=c("yellow","blue","darkgreen","red",
              "cyan","orange","orchid"),notch=T,
        boxwex=.75,horizontal=T,las=1,xlab="Log_10 $")
dev.off2()

## Log Balance by Envelope
pdf2(paste0(rand_path,"dist_log_due_by_trt_2_box.pdf"))
par(mar=c(7.1,6.1,6.1,2.1))
boxplot(log10(total_due)~bs,data=data_own,
        main="Box Plots of Log Debt\nBy Treatment",
        col=c("blue","red"),notch=T,boxwex=.25,
        horizontal=T,las=1,xlab="Log_10 $")
dev.off2()

## Bar Plot: Number of Properties and Owners by Letter
pdf2(paste0(rand_path,"number_prop_own_by_trt_7_bar.pdf"))
par(mar=c(5.1,4.1,4.1,2.1))
data[,.(.N,uniqueN(owner1)),by=tmt7
     ][,barplot(c(rbind(N,V2)),names.arg=rep(tmt7,each=2),las=3,
                main="Numbers of Properties and Owners\nBy Treatment",
                col=rep(c("yellow","blue","darkgreen","red",
                          "cyan","orange","orchid"),each=2),
                density=rep(c(-1,20),.N),cex.names=.75,
                ylim=c(0,1.3*max(N)))]
legend("top",legend=c("# Properties","# Owners"),
       density=c(-1,20),bty="n",horiz=T,text.width=5)
dev.off2()

## Bar Plot: Number of Properties and Owners by Envelope
pdf2(paste0(rand_path,"number_prop_own_by_trt_2_bar.pdf"))
par(mar=c(5.1,4.1,4.1,2.1))
data[,.(.N,uniqueN(owner1)),by=bs
     ][,barplot(c(rbind(N,V2)),names.arg=rep(bs,each=2),las=3,
                main="Numbers of Properties and Owners\nBy Treatment",
                col=rep(c("blue","red"),each=2),
                density=rep(c(-1,20),.N),
                ylim=c(0,1.3*max(N)))]
legend("top",legend=c("# Properties","# Owners"),
       density=c(-1,20),bty="n",horiz=T,text.width=2)
dev.off2()
