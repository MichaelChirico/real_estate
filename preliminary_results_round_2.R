library(xlsx)
library(data.table)

data<-setkey(setnames(setDT(read.xlsx2(
  paste0("/media/data_drive/real_estate/",
         "Payments and Balance Penn Letter ",
         "Experiment_150727.xlsx"),
  sheetIndex=2,header=T,startRow=9L,
  colClasses="character")),
  c("account_id","opa_no","address","zip","treatment",
    "total_due","pre_2015_balance","current_due",
    "payments_made","year_owed","due_year_min",
    "due_year_max","total_due_july",
    "earliest_payment","total_paid")),opa_no)

orig<-
  setkey(fread(
    "~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/round_two/round_2_full_data.csv"),
    opa_no)

BB<-1e5
ctrl_data<-data[.(unique(opa_no),"Control_Small_Envelope"),total_paid,nomatch=0L]
NN<-length(ctrl_data)
ctrl_mean_bb<-sapply(1:BB,function(x)mean(ctrl_data[sample(NN,rep=T)]))
ctrl_ci_lo<-quantile(ctrl_mean_bb,.025)
ctrl_ci_hi<-quantile(ctrl_mean_bb,.975)

pdf("~/Desktop/by_treatment.pdf")
data[order(tmt),mean(total_paid),by=tmt
     ][,{barplot(V1,names.arg=tmt,
                 main="Average Payment by Treatment",ylab="$",
                 col=rep(c("yellow","blue","darkgreen","red",
                           "cyan","orange","orchid"),each=2),
                 density=rep(c(-1,25),.N/2))
       abline(h=V1[tmt=="CS"])
       abline(h=ctrl_ci_lo,lty=2,col="red")
       abline(h=ctrl_ci_hi,lty=2,col="red")}]
dev.off()

ctrl_data<-data[grepl("Small",treatment),total_paid]
NN<-length(ctrl_data)
ctrl_mean_bb<-sapply(1:BB,function(x)mean(ctrl_data[sample(NN,rep=T)]))
ctrl_ci_lo<-quantile(ctrl_mean_bb,.025)
ctrl_ci_hi<-quantile(ctrl_mean_bb,.975)

pdf("~/Desktop/by_envelope.pdf")
data[,mean(total_paid),by=.(bs=grepl("Big",treatment))
     ][,{barplot(V1,names.arg=ifelse(bs,"Big","Small"),
                 main="Average Payment by Envelope Type",ylab="$",
                 col=c("red","blue"))
       abline(h=ctrl_ci_lo,lty=2,col="red")
       abline(h=ctrl_ci_hi,lty=2,col="red")}]
dev.off()


data2<-data[total_paid<quantile(total_paid,.95)]

BB<-1e5
ctrl_data<-data2[.(unique(opa_no),"Control_Small_Envelope"),total_paid,nomatch=0L]
NN<-length(ctrl_data)
ctrl_mean_bb<-sapply(1:BB,function(x)mean(ctrl_data[sample(NN,rep=T)]))
ctrl_ci_lo<-quantile(ctrl_mean_bb,.025)
ctrl_ci_hi<-quantile(ctrl_mean_bb,.975)

pdf("~/Desktop/by_treatment_no_outliers.pdf")
data2[order(tmt),mean(total_paid),by=tmt
     ][,{barplot(V1,names.arg=tmt,
                 main="Average Payment by Treatment",ylab="$",
                 col=rep(c("yellow","blue","darkgreen","red",
                           "cyan","orange","orchid"),each=2),
                 density=rep(c(-1,25),.N/2))
       abline(h=V1[tmt=="CS"])
       abline(h=ctrl_ci_lo,lty=2,col="red")
       abline(h=ctrl_ci_hi,lty=2,col="red")}]
dev.off()

ctrl_data<-data2[grepl("Control",treatment),total_paid]
NN<-length(ctrl_data)
ctrl_mean_bb<-sapply(1:BB,function(x)mean(ctrl_data[sample(NN,rep=T)]))
ctrl_ci_lo<-quantile(ctrl_mean_bb,.025)
ctrl_ci_hi<-quantile(ctrl_mean_bb,.975)

pdf("~/Desktop/by_envelope_no_outliers.pdf")
data2[,mean(total_paid),by=.(bs=grepl("Big",treatment))
     ][,{barplot(V1,names.arg=ifelse(bs,"Big","Small"),
                 main="Average Payment by Envelope Type",ylab="$",
                 col=c("red","blue"))
       abline(h=ctrl_ci_lo,lty=2,col="red")
       abline(h=ctrl_ci_hi,lty=2,col="red")}]
dev.off()