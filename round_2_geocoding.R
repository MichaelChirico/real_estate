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
library(getcartr)

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

##Get experiment data for counts
exper.data<-fread("./round_two/round_2_full_data.csv")


#Import shapefiles for delinquent
#  properties & Philadelphia ZIP codes
sheriffs_map_delinquent<-
  readShapePoints(gis_wd%+%"delinquent_sold_year_to_may_15_nad.shp")
phila_zip<-
  readShapePoly(gis_wd%+%"Philadelphia_Zipcodes_Poly201302.shp")
phila_azav<-
  spTransform(readShapePoly(
    gis_wd%+%"Neighborhoods_Philadelphia_with_quadrants.shp",
    proj4string=CRS("+init=epsg:2272")),
    CRS("+proj=longlat +datum=WGS84"))

#Geocoding ####
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

#Shapefile aggregation
phila_azav_quad<-
  unionSpatialPolygons(phila_azav,
                       IDs=phila_azav$quadrant,
                       threshold=1e-5)
quad_df<-phila_azav@data[!duplicated(phila_azav$quadrant),-(1:4)]
phila_azav_quad<-
  SpatialPolygonsDataFrame(phila_azav_quad,
                           quad_df,match.ID="quadrant")
phila_azav_quad@data<-setDT(phila_azav_quad@data)
phila_azav@data<-setDT(phila_azav@data)

#Cartogram Preparation ####
phila_azav@data[exper.data[,.N,by=azavea_nbhd],
                count:=i.N,on=c(MAPNAME="azavea_nbhd")]
phila_azav_carto<-quick.carto(phila_azav,phila_azav$count)

phila_azav_quad@data[exper.data[,.N,by=azavea_quad],
                     count:=i.N,on=c(quadrant="azavea_quad")]
phila_azav_quad_carto<-quick.carto(phila_azav_quad,phila_azav_quad$count)
##Export
writeSpatialShape(phila_azav_carto,
                  gis_wd%+%"Azavea_Neighborhood_cartogram_r2")
writeSpatialShape(phila_azav_quad,
                  gis_wd%+%"Azavea_Quadrants")
writeSpatialShape(phila_azav_quad_carto,
                  gis_wd%+%"Azavea_Quadrant_cartogram_r2")
