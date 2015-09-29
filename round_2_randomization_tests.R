# Real Estate Tax Experiment Randomization Tests, Round 2
# Michael Chirico
# June 10, 2015

# Setup: Packages, Working Directory, Convenient Functions ####
rm(list=ls(all=T))
gc()
setwd("~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency")
code_wd<-"./analysis_code/"
library(funchir)
library(data.table)
library(xlsx)
library(maptools)
write.packages(code_wd%+%"logs/round_2_"%+%
                 "randomization_tests_session.txt")

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
data_own[,tmt7:=factor(tmt7,c("Control","Amenities","Moral",
                              "Duty","Lien","Sheriff","Peer"))]

# Graphic Tests ####
rand_path<-"./papers_presentations/round_two/images/balance/"
## Log Balance by Letter
pdf2(rand_path%+%"dist_log_due_by_trt_7_box.pdf")
par(mar=c(2.6,5.1,4.1,2.1))
boxplot(total_due~tmt7,data=data_own,
        main="Box Plots of Log Debt\nBy Treatment",
        log="x",xaxt="n",cex.axis=.8,
        col=c("blue","yellow","cyan","darkgreen",
              "red","orchid","orange"),notch=T,
        boxwex=.5,horizontal=T,las=1,xlab="")
axis(side=1,at=10^(1:6),cex.axis=.8,
     labels=paste0("$",formatC(as.integer(10^(1:6)),
                               big.mark=",")))
abline(v=data_own[tmt7=="Control",median(total_due)],lty=2)
dev.off2()

## Log Balance by Envelope
pdf2(rand_path%+%"dist_log_due_by_trt_2_box.pdf")
par(mar=c(2.6,5.1,4.1,2.1))
boxplot(total_due~bs,data=data_own,log="x",xaxt="n",
        main="Box Plots of Log Debt\nBy Envelope Size",
        col=c("blue","red"),notch=T,boxwex=.2,
        horizontal=T,las=1,xlab="")
axis(side=1,at=10^(1:6),cex.axis=.8,
     labels=paste0("$",formatC(as.integer(10^(1:6)),
                               big.mark=",")))
abline(v=data_own[bs=="Small",median(total_due)],lty=2)
dev.off2()

## Bar Plot: Number of Properties and Owners by Letter
data[,.(.N,uniqueN(owner1)),by=tmt7
     ][,{pdf2(rand_path%+%"number_prop_own_by_trt_7_bar.pdf")
       layout(mat=matrix(1:2),heights=c(.8,.2))
       par(mar=c(1,4.1,4.1,2.1))
       x<-barplot(c(rbind(N,V2)),names.arg=rep(tmt7,each=2),las=3,
                  main="Numbers of Properties and Owners\nBy Treatment",
                  col=rep(c("yellow","blue","darkgreen","red",
                            "cyan","orange","orchid"),each=2),
                  density=rep(c(-1,20),.N),cex.names=.75,
                  ylim=c(0,1.3*max(N)))
       par(mar=rep(0,4))
       plot(0,0,type="n",ann=F,axes=F,xlim=range(x))
       legend(max(x)/2,0,bty="n",xjust=.3,
              legend=c("# Properties","# Owners"),
              density=c(-1,20),horiz=T,text.width=5)
       dev.off2()}]

## Bar Plot: Number of Properties and Owners by Envelope
data[,.(.N,uniqueN(owner1)),by=bs
     ][,{pdf2(rand_path%+%"number_prop_own_by_trt_2_bar.pdf")
       layout(mat=matrix(1:2),heights=c(.8,.2))
       par(mar=c(1,4.1,4.1,2.1))
       x<-barplot(c(rbind(N,V2)),names.arg=rep(bs,each=2),las=3,
                  main="Numbers of Properties and Owners\nBy Treatment",
                  col=rep(c("blue","red"),each=2),
                  density=rep(c(-1,20),.N),
                  ylim=c(0,1.3*max(N)))
       par(mar=rep(0,4))
       plot(0,0,type="n",ann=F,axes=F,xlim=range(x))
       legend(max(x)/2,0,bty="n",xjust=.3,
              legend=c("# Properties","# Owners"),
              density=c(-1,20),horiz=T,text.width=1)
       dev.off2()}]