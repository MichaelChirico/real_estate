#Geocoding
#Philadelphia Real Estate Tax Evasion
#  Round 2
#Michael Chirico
#August 31, 2015

#Setup: Working Directory, Packages, Convenient Functions ####
##Packages
rm(list=ls(all=T))
gc()
setwd("~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/")
data_wd<-"/media/data_drive/real_estate/"
gis_wd<-"/media/data_drive/gis_data/PA/"
library(data.table)
library(maptools)
library(ggmap)

"%+%"<-function(s1,s2)paste0(s1,s2)

#Data Import ####
##Addresses mentioned in any letter in Round 2
to_geocode<-unique(unlist(fread(
  "./round_two/round_2_full_data.csv",
  select="example_address_"%+%1:3)))

##Get OPA Numbers for those addresses for merging
sheriffs_delinquent<-
  fread(data_wd%+%"/sheriffs_sales/"%+%
          "delinquent_sold_year_to_may_15.csv",
        select=c("opa_no","address")
        )[address%in%to_geocode]

#Import shapefiles for delinquent
#  properties & Philadelphia ZIP codes
sheriffs_map_delinquent<-
  readShapePoints(gis_wd%+%"delinquent_sold_year_to_may_15_nad.shp")
phila_zip<-
  readShapePoly(gis_wd%+%"Philadelphia_Zipcodes_Poly201302.shp")

#Analysis ####
##Overlap properties to ZIPs to assign ZIPs; merge
sheriffs_delinquent[as.data.table(cbind(as.character(
  sheriffs_map_delinquent@data$opa_no),
  as.numeric(as.character(
    (sheriffs_map_delinquent %over% phila_zip)[,"CODE"])))),
  "zip":=i.V2,on=c(opa_no="V1")]

##Geocode using ZIPs & addresses 
sheriffs_delinquent[,c("longitude","latitude"):=
                      geocode(paste0(address,", Philadelphia PA ",zip),
                              source="google")]
##Write output
write.csv(sheriffs_delinquent,file=data_wd%+%"/sheriffs_sales/"%+%
            "delinquent_sold_used_round_2_w_lon_lat.csv",
          row.names=F)