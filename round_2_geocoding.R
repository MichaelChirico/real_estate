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
wds<-c(data="/media/data_drive/real_estate/",
       gis="/media/data_drive/gis_data/PA/",
       sher="/media/data_drive/real_estate/sheriffs_sales/",
       code="./analysis_code/")
library(data.table)
library(maptools)
library(ggmap)
library(funchir) #ggmap attaches ggplot which also has a %+% function
library(getcartr)
library(deldir)
library(sp)
library(xlsx)
write.packages(wds["code"]%+%"logs/round_2_"%+%
                 "geocoding_session.txt")

#Copied & personalized from Carson Farmer here:
#  http://carsonfarmer.com/2009/09/voronoi-polygons-with-r/
voronoi = function(layer,box=NULL,proj4string=layer@proj4string,
                   idcol=NULL) {
  crds <- layer@coords
  colnames(crds)<-c("x","y")
  rw <- c(t(box))
  tlz <- tile.list(deldir(data.frame(crds),rw=rw))
  wrap<-function(x)rbind(x, x[1,])
  polys = lapply(tlz,function(tl){
    Polygons(list(Polygon(wrap(cbind(tl$x,tl$y)))), 
             ID=as.character(tl$ptNum))
  })
  SP = SpatialPolygons(polys,proj4string=proj4string)
  DF <- data.frame(crds, idcol = layer@data[idcol],
                   row.names=sapply(slot(SP, 'polygons'),function(x)slot(x, 'ID')))
  
  SpatialPolygonsDataFrame(SP, data = DF)
}

#Data Import ####
##Addresses mentioned in any letter in Round 2
used_sheriff<-unique(unlist(fread(
  "./round_two/round_2_full_data.csv",
  select="example_address_"%+%1:3)))

##Get OPA Numbers for those addresses for merging
sheriffs_delinquent<-
  fread(wds["sher"]%+%
          "delinquent_sold_year_to_may_15.csv",
        select=c("opa_no","address")
        )[address%in%used_sheriff]

##Get experiment data for counts
exper.data<-
  fread("./round_two/round_2_full_data.csv"
        )[setDT(read.xlsx3(
          wds["data"]%+%"req20150709_PennLetter"%+%
            "Experiment_v3_Coordinates.xlsx",
          sheetName="TREATMENT",colIndex=c(2,4,5),
          colClasses=c("character",rep("numeric",2)))),
          `:=`(longitude=X_LONG,latitude=Y_LAT),
          on=c(opa_no="BRT.NUMBER")
          #supplement with coordinates done by hand & flag
          ][fread(wds["data"]%+%"round_2_supplement_lon_lat.csv"),
            `:=`(longitude=i.longitude,latitude=i.latitude),on="opa_no"]

exper.data.map<-
  spTransform(SpatialPointsDataFrame(cbind(
    exper.data$longitude,exper.data$latitude),
    exper.data[,!c("longitude","latitude"),with=F],
    proj4string=CRS("+proj=longlat +datum=WGS84")),
    CRS("+init=epsg:2272"))

#Import shapefiles for delinquent
#  properties & Philadelphia ZIP codes
sheriffs_delinquent_map<-
  readShapePoints(wds["gis"]%+%"delinquent_sold_year_to_may_15_nad.shp",
                  proj4string=CRS("+init=epsg:2272"))
##Restrict to those that actually
##  appear in our experiment
sheriffs_delinquent_map<-
  sheriffs_delinquent_map[
    sheriffs_delinquent_map@data$opa_no %in% 
      sheriffs_delinquent$opa_no,]

#External shapefiles
phila_zip<-
  readShapePoly(wds["gis"]%+%"Philadelphia_Zipcodes_Poly201302.shp",
                proj4string=CRS("+init=epsg:2272"))
phila_azav<-
  spTransform(readShapePoly(
    wds["gis"]%+%"Neighborhoods_Philadelphia_with_quadrants.shp",
    proj4string=CRS("+init=epsg:2272")),
    CRS("+proj=longlat +datum=WGS84"))

phila_ward<-
  readShapePoly(wds["gis"]%+%
                  "PhiladelphiaPoliticalWardDivisions201403.shp",
                proj4string=CRS("+init=epsg:2272"))

phila_ctract<-
  readShapePoly(wds["gis"]%+%
                  "PhiladelphiaCensusTracts2010.shp",
                proj4string=CRS("+init=epsg:2272"))

#Spatial Joins ####
#Add some extra geographic indicators for merging
#  with various other geospatial databases
##Political Ward Division
exper.data.map@data$political_ward<-
  (exper.data.map %over% phila_ward)$DIVISION_N

##Census Tracts
exper.data.map@data$census_tract<-
  (exper.data.map %over% phila_ctract)$GEOID10

##Sheriff's Sale Voronoi Polygons
### ** SEE BELOW **

#Geocoding ####
##Sheriff's Sales
###Overlap properties to ZIPs to assign ZIPs; merge
sheriffs_delinquent[as.data.table(cbind(as.character(
  sheriffs_map_delinquent@data$opa_no),
  as.numeric(as.character(
    (sheriffs_map_delinquent %over% phila_zip)[,"CODE"])))),
  "zip":=i.V2,on=c(opa_no="V1")]

###Geocode using ZIPs & addresses 
sheriffs_delinquent[,c("longitude","latitude"):=
                      geocode(paste0(address,", Philadelphia PA ",zip),
                              source="google")]
###Write output
write.csv(sheriffs_delinquent,file=wds["sher"]%+%
            "delinquent_sold_used_round_2_w_lon_lat.csv",
          row.names=F)

#Shapefile aggregation ####
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

#Voronoi Polygons ####
sheriffs_delinquent_voronoi<-
  voronoi(sheriffs_delinquent_map,
          box=phila_zip@bbox,
          idcol="opa_no")

#Clip Voronoi output to Philadelphia borders
sheriffs_delinquent_voronoi<-
  SpatialPolygonsDataFrame(
    gIntersection(sheriffs_delinquent_voronoi,
                  gUnaryUnion(phila_zip),byid=T,
                  id=sapply(sheriffs_delinquent_voronoi@polygons,
                            slot,name="ID")),
    sheriffs_delinquent_voronoi@data)

sheriffs_delinquent_voronoi@data<-
  setDT(sheriffs_delinquent_voronoi@data)

#Add column to experimental data mapping
#  each property to its nearest Sheriff-Sold
#  property (as mentioned in the letters)
exper.data.map@data$nearest_sheriff<-
  (exper.data.map %over% 
     sheriffs_delinquent_voronoi)$opa_no

sheriffs_delinquent_voronoi@data[
  setDT(exper.data.map@data)[,.N,by=nearest_sheriff],
  count:=i.N,on=c(opa_no="nearest_sheriff")]

##Voronoi cartogram
sheriffs_delinquent_voronoi_carto<-
  quick.carto(sheriffs_delinquent_voronoi,
              sheriffs_delinquent_voronoi$count)

#Data Export ####
##Spatial Joins mappings
sjoins<-c("opa_no","nearest_sheriff","political_ward",
          "census_tract")
write.csv(exper.data.map@data[,sjoins,with=F],
          file=wds["data"]%+%"round_2_spatial_regions.csv",
          quote=TRUE,row.names=FALSE)

##Shapefiles
###Cartograms
writeSpatialShape(phila_azav_carto,
                  wds["gis"]%+%"Azavea_Neighborhood_cartogram_r2")
writeSpatialShape(phila_azav_quad,
                  wds["gis"]%+%"Azavea_Quadrants")
writeSpatialShape(phila_azav_quad_carto,
                  wds["gis"]%+%"Azavea_Quadrant_cartogram_r2")

###Voronoi Polygons
writeSpatialShape(sheriffs_delinquent_voronoi,
                  wds["data"]%+%"round_2_used_sheriff_voronoi")
writeSpatialShape(sheriffs_delinquent_voronoi_carto,
                  wds["sher"]%+%"round_2_sheriffs_voronoi_cartogram")
