# Real Estate Tax Experiment Randomization Code Round 2
# Michael Chirico
# May 15, 2015

#Setup: Packages, Working Directory, Set Random Seed ####
rm(list=ls(all=T))
gc()
setwd("~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/")
library(data.table)
library(xlsx)
library(foreign)
library(maptools)
##Set Random Seed:
## Took current time as of this writing and appended the digits
## of a Banana Republic coupon code I found on my desk: 1729749
set.seed(1820749)

#Data Import ####
data<-setnames(fread("/media/data_drive/real_estate/round_two_property_file.csv"),
               c("opa_no","azavea_nbhd","zip","owner1","owner2","mail_address",
                 "mail_city","mail_state","mail_zip","principal",
                 "interest_other","total_due","property_address"))

##Add in spatial data

### First, Sheriff's Sales info
sheriffs_sales<-setkey(fread(paste0("/media/data_drive/real_estate/sheriffs_sales/",
                                    "sheriffs_sales_with_zip_azav.csv")),opa_no)
##Not sure what we'll do with zip codes yet
# zip_sample<-dcast.data.table(
#   sheriffs_sales[.(sheriffs_sales[,sample(opa_no,size=min(.N,5)),by=zip]$V1),
#                  .(zip,address,sale_date)][,I:=1:.N,by=zip],
#   zip~I,value.var = c("address","sale_date"))

azavea_nbhd_sample_ss<-setkey(setnames(dcast.data.table(
  sheriffs_sales[.(sheriffs_sales[,sample(opa_no,size=min(.N,3)),by=azavea_nbhd]$V1),
                 .(azavea_nbhd,address,sale_date)][,I:=1:.N,by=azavea_nbhd],
  azavea_nbhd~I,value.var = c("address","sale_date")),
  c("azavea_nbhd",paste0(rep(c("example_address_","example_sale_date_"),each=3),
                         rep(1:3,2)))),azavea_nbhd
  )[sheriffs_sales[,.N,by=azavea_nbhd],count:=i.N]

azavea_quad_sample_ss<-setkey(setnames(dcast.data.table(
  sheriffs_sales[.(sheriffs_sales[,sample(opa_no,size=3),by=azavea_quad]$V1),
                 .(azavea_quad,address,sale_date)][,I:=1:.N,by=azavea_quad],
  azavea_quad~...+I,value.var = c("address","sale_date")),
  c("azavea_quad",paste0(rep(c("example_address_","example_sale_date_"),each=3),
                         rep(1:3,2)))),azavea_quad
  )[sheriffs_sales[,.N,by=azavea_quad],count:=i.N]

azavea_nbhd_quad_mapping<-
  setkey(setkey(data.table(read.dbf("/media/data_drive/gis_data/PA/Neighborhoods_Philadelphia_with_quadrants.dbf")
                           )[,.(azavea_shp=LISTNAME,quadrant)],azavea_shp
                )[fread(paste0("~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/",
                               "azaveas_mapping_dor_shp.csv")),
                  azavea_nbhd:=i.azavea_dor][,azavea_shp:=NULL],azavea_nbhd)

setkey(data,azavea_nbhd)[azavea_nbhd_quad_mapping,azavea_quad:=i.quadrant]

data[,low_density_sher:=T]
data[sheriffs_sales[,.N,by=azavea_nbhd],low_density_sher:=!i.N>=8]

data[,azavea_sher:=ifelse(low_density_sher,as.character(azavea_quad),as.character(azavea_nbhd))]

data<-rbindlist(list(setkey(data[low_density_sher==T,],azavea_quad)[azavea_quad_sample_ss],
                     setkey(data[low_density_sher==F,],azavea_nbhd)[azavea_nbhd_sample_ss,nomatch=0L]))

### Next, Amenities info
amenities_map<-
  readShapePoints("/media/data_drive/gis_data/PA/amenities_azav_for_geocoding_nad.shp")
phila_azav<-
  readShapePoly("/media/data_drive/gis_data/PA/Neighborhoods_Philadelphia_with_quadrants.shp")

amenities_azav<-
  setkey(as.data.table(amenities_map)[,.(amenity,address,resource)],address
         )[.(as.character(amenities_map@data$address)),
           `:=`(azavea_nbhd=(amenities_map %over% phila_azav)[,"LISTNAME"],
                azavea_quad=(amenities_map %over% phila_azav)[,"quadrant"])
           ][!is.na(azavea_nbhd),]; rm(amenities_map,phila_azav)
#Reset names of Azavea neighborhoods to fit formatting of DoR records
setkey(amenities_azav,azavea_nbhd
       )[setkey(fread(paste0("~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/",
                             "azaveas_mapping_dor_shp.csv")),azavea_shp),
         azavea_nbhd:=i.azavea_dor]

# setkey(amenities_azav,address)
# azavea_nbhd_sample_amen<-setkey(setnames(dcast.data.table(
#   amenities_azav[.(amenities_azav[,sample(address,size=min(.N,2)),by=azavea_nbhd]$V1),
#                  .(azavea_nbhd,amenity)][,I:=1:.N,by=azavea_nbhd],
#   azavea_nbhd~I,value.var = "amenity"),
#   c("azavea_nbhd",paste0("example_amenity_",1:2))),azavea_nbhd)
# 
# azavea_quad_sample_amen<-setkey(setnames(dcast.data.table(
#   amenities_azav[.(amenities_azav[,sample(address,size=2),by=azavea_quad]$V1),
#                  .(azavea_quad,amenity)][,I:=1:.N,by=azavea_quad],
#   azavea_quad~I,value.var = "amenity"),
#   c("azavea_quad",paste0("example_amenity_",1:2))),azavea_quad)

data[,low_density_amen:=T]
setkey(data,azavea_nbhd)[amenities_azav[,.N,by=azavea_nbhd],low_density_amen:=!i.N>=4]

data[,azavea_amen:=ifelse(low_density_sher,as.character(azavea_quad),as.character(azavea_nbhd))]

get_amen_nbhd<-function(nbhd){
  sapply(nbhd,function(x){as.character(amenities_azav[.(x),sample(amenity,1)])})
}

get_amen_quad<-function(quad){
  sapply(quad,function(x){as.character(amenities_azav[.(x),sample(amenity,1)])})
}

setkey(amenities_azav,azavea_quad)
data[low_density_amen==T,paste0("example_amenity_",1:2):=list(get_amen_quad(azavea_quad),get_amen_quad(azavea_quad))]
setkey(amenities_azav,azavea_nbhd)
data[low_density_amen==F,paste0("example_amenity_",1:2):=list(get_amen_nbhd(azavea_nbhd),get_amen_nbhd(azavea_nbhd))]

# data<-rbindlist(list(setkey(data[low_density_amen==T,],azavea_quad)[amenities_azavazavea_quad_sample_amen],
#                      setkey(data[low_density_amen==F,],azavea_nbhd)[amenities_azav,azavea_nbhd_sample_amen,nomatch=0L]))

# Treatment Assignment ####
treatments<-paste0(rep(c("Sheriff","Lien","Moral","Amenities",
                         "Peer","Duty","Control"),each=2),"_",
                   rep(c("Big_Envelope","Small_Envelope"),7))
n_treatments<-length(treatments)
setkey(setorder(data,-total_due)[,grp:=rep(1:ceiling(.N/n_treatments),
                                           each=n_treatments,length.out=.N)
                                 ][,treatment:=sample(treatments,size=.N),
                                   by=grp][,c("grp","treatment"):=
                                             list(NULL,as.factor(treatment))],treatment)

##Separate files for low/high density amenities/disamenities
treatments<-paste0(rep(c("Sheriff_High","Sheriff_Low",
                         "Lien_High","Lien_Low","Moral",
                         "Amenities_High","Amenities_Low",
                         "Peer","Duty","Control"),each=2),"_",
                   rep(c("Big_Envelope","Small_Envelope"),7))

data[grepl("Sheriff",treatment),treatment:=ifelse(low_density_sher,
                                                  gsub("Sheriff","Sheriff_Low",treatment),
                                                  gsub("Sheriff","Sheriff_High",treatment))]
data[grepl("Lien",treatment),treatment:=ifelse(low_density_sher,
                                                  gsub("Lien","Lien_Low",treatment),
                                                  gsub("Lien","Lien_High",treatment))]
data[grepl("Amenities",treatment),treatment:=ifelse(low_density_amen,
                                                  gsub("Amenities","Amenities_Low",treatment),
                                                  gsub("Amenities","Amenities_High",treatment))]
##Need to reset factors
setkey(data[,treatment:=factor(treatment)],treatment)

lapply(treatments,function(x){write.csv(data[.(x)],file=paste0("round_2_sample_",tolower(x),".csv"),row.names=F)})