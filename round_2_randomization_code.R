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

#Data Import
data<-setnames(fread("/media/data_drive/real_estate/round_two_property_file.csv",
                     drop=c("V10","V11")),
               c("opa_no","azavea_nbhd","zip","owner1","owner2","mail_address",
                 "mail_city","mail_state","mail_zip","total_due",
                 "property_address"))

##Re-cast names in a way that's cleaner to print
to.proper<-function(strings){
  res<-gsub("(?<=\\s|^|-)([A-Z])([A-Z]+)*","\\U\\1\\L\\2",strings,perl=T)
  res<-gsub("\\bMc\\s","Mc",res)
  for (init in c("[A-HJ-UW-Z]","Inc","Assoc","Co","Ch","Civ","Rev",
                 "Corp","Prop","Apts","Assn","St",
                 "Na","Eq","Med","Fed","Vet","Bus","Ltd","Res","Est",
                 "Jr","Sr","Tr","Bros","Sc","Mg","Wm","Un","Dev")){
    res<-gsub(paste0("(?<=\\s|^)(",init,")(?=\\s|$)"),"\\1.",res,perl=T)
  }
  for (abbr in c("[B-DF-HJ-NP-TV-XZ][b-df-hj-np-tv-xz]{1,}",
                 "Op","Ak","Og","Eg","Am","Er","Ir","Itr","Els",
                 "Pa","Ii","Iii","Iv","Sas","Ro","Re","Vii","Sfi",
                 "Ab","Us","Fxa","Fbo","Ne","Ag","Ix","Uc","Dsa",
                 "Amh","Pje","Mod","Xii","Xiv","Xvi","Mla","Ucm",
                 "Gma","Gly","Dja","Mis","Acb","Esb","Epr","Dab",
                 "Vca","Gha","Has","Acm","Usa","Bes","Bta","Sma",
                 "Tca","Jme","Gca","Anc","Abc","Foe","Tna","Fri",
                 "Bor","Mol","Emi","Ame","Ael","Jam","Mba","Bsi",
                 "Arc","Mef","Ams","Nur","Nua","Zlo","Igt","Ama",
                 "Yav","Pha","Nio","Mri","Esp","Rep","Awb","Amr",
                 "Fuh","Atm","Ald","Rko","Syg","Yml","Mmy","Roz",
                 "Efv","Udm","Ahm","Mym","Ent","Jbe","Cme","Mda",
                 "Acp","Bak","Epc","Hew","Nek","Gif","Epdg","Pazy")){
    res<-gsub(paste0("\\b(",abbr,")\\b"),"\\U\\1",res,perl=T)
  }
  for (mist in c("CH","JR","SR","TR","SC","MG","ST",
                 "WM","HH","NG","LTD","MT")){
    m1<-substr(mist,1,1); m2<-substr(mist,2,2)
    res<-gsub(paste0("\\b(",m1,")(",m2,")\\b"),"\\1\\L\\2",res,perl=T)
  }
  res<-gsub("(?<=[0-9])(TH|ST|ND|RD)","\\L\\1",res,perl=T)
  gsub("\\bMc([a-z])","Mc\\U\\1",res,perl=T)
}

data[,paste0("owner",1:2,"_clean"):=lapply(.SD,to.proper),.SDcols=paste0("owner",1:2)]

#Add in spatial data

## Sheriff's Sales Info ####
sheriffs_map_delinquent<-
  readShapePoints("/media/data_drive/gis_data/PA/delinquent_sold_year_to_may_15_nad.shp")
phila_zip<-
  readShapePoly("/media/data_drive/gis_data/PA/Philadelphia_Zipcodes_Poly201302.shp")
phila_azav<-
  readShapePoly("/media/data_drive/gis_data/PA/Neighborhoods_Philadelphia_with_quadrants.shp")

sheriffs_sales<-setkey(setkey(setkey(fread(
  "/media/data_drive/real_estate/sheriffs_sales/delinquent_sold_year_to_may_15.csv"
  ),opa_no)[.(as.character(sheriffs_map_delinquent@data$opa_no)),
            #Spatial join: assign Azavea neighborhood and quadrant to each sale
            `:=`(azavea_nbhd=(sheriffs_map_delinquent %over% phila_azav)[,"LISTNAME"],
                 azavea_quad=(sheriffs_map_delinquent %over% phila_azav)[,"quadrant"])
            #Rename Azavea neighborhoods to match DoR data
            ],azavea_nbhd)[setkey(fread(paste0("~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/",
                      "azaveas_mapping_dor_shp.csv")),azavea_shp),azavea_nbhd:=i.azavea_dor],
  #Format dates pretty for output to letters
  opa_no)[,sale_date:=format(as.Date(sale_date),"%B %d, %Y")]; rm(sheriffs_map_delinquent)

###Now, for each Azavea Neighborhood, pull (at most) 3 sales (dates & addresses) at random
azavea_nbhd_sample_ss<-setkey(setnames(dcast.data.table(
  sheriffs_sales[.(sheriffs_sales[,sample(opa_no,size=min(.N,3)),by=azavea_nbhd]$V1),
                 .(azavea_nbhd,address,sale_date)][,I:=1:.N,by=azavea_nbhd],
  azavea_nbhd~I,value.var = c("address","sale_date")),
  c("azavea_nbhd",paste0(rep(c("example_address_","example_sale_date_"),each=3),
                         rep(1:3,2)))),azavea_nbhd
  )[sheriffs_sales[,.N,by=azavea_nbhd],count:=i.N]

###Repeat for quadrants, which always have >= 3 sales
azavea_quad_sample_ss<-setkey(setnames(dcast.data.table(
  sheriffs_sales[.(sheriffs_sales[,sample(opa_no,size=3),by=azavea_quad]$V1),
                 .(azavea_quad,address,sale_date)][,I:=1:.N,by=azavea_quad],
  azavea_quad~...+I,value.var = c("address","sale_date")),
  c("azavea_quad",paste0(rep(c("example_address_","example_sale_date_"),each=3),
                         rep(1:3,2)))),azavea_quad
  )[sheriffs_sales[,.N,by=azavea_quad],count:=i.N]

###Get Quadrant-Neighborhood mapping to assign in main data
azavea_nbhd_quad_mapping<-
  setkey(setkey(data.table(read.dbf("/media/data_drive/gis_data/PA/Neighborhoods_Philadelphia_with_quadrants.dbf")
                           )[,.(azavea_shp=LISTNAME,quadrant)],azavea_shp
                )[fread(paste0("~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/",
                               "azaveas_mapping_dor_shp.csv")),
                  azavea_nbhd:=i.azavea_dor][,azavea_shp:=NULL],azavea_nbhd)

setkey(data,azavea_nbhd)[azavea_nbhd_quad_mapping,azavea_quad:=i.quadrant]; rm(azavea_nbhd_quad_mapping)

### Identify properties in low-density neighborhoods,
###   defined as those having <8 total local sales
data[,low_density_sher:=T]
data[sheriffs_sales[,.N,by=azavea_nbhd],low_density_sher:=!i.N>=8]

### For simplicity in merging, assign a field taking a neighborhood
###   when high-density and a quadrant when low-density
data[,azavea_sher:=ifelse(low_density_sher,as.character(azavea_quad),as.character(azavea_nbhd))]

### Conditional merge--assign quadrant-level sample
###   properties to low-density properties, neighborhood-level otherwise
data<-rbindlist(list(setkey(data[low_density_sher==T,],azavea_quad)[azavea_quad_sample_ss],
                     setkey(data[low_density_sher==F,],azavea_nbhd)[azavea_nbhd_sample_ss,nomatch=0L]))

rm(sheriffs_sales,azavea_nbhd_sample_ss,azavea_quad_sample_ss)

## Amenities Info ####
amenities_map<-
  readShapePoints("/media/data_drive/gis_data/PA/amenities_azav_for_geocoding_nad.shp")

amenities_azav<-
  setkey(as.data.table(amenities_map)[,.(amenity,address,resource)],address
         )[.(as.character(amenities_map@data$address)),
           #Spatial join: assign Azavea neighborhood and quadrant to each sale
           `:=`(azavea_nbhd=(amenities_map %over% phila_azav)[,"LISTNAME"],
                azavea_quad=(amenities_map %over% phila_azav)[,"quadrant"])
           ][!is.na(azavea_nbhd),]; rm(amenities_map,phila_azav,phila_zip)
###Reset names of Azavea neighborhoods to fit formatting of DoR records
setkey(amenities_azav,azavea_nbhd
       )[setkey(fread(paste0("~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/",
                             "azaveas_mapping_dor_shp.csv")),azavea_shp),
         azavea_nbhd:=i.azavea_dor]

###As for Sheriff's Sales, define which neighborhoods are
###  low-density in that there are few amenities, here <4
data[,low_density_amen:=T]
setkey(data,azavea_nbhd)[amenities_azav[,.N,by=azavea_nbhd],low_density_amen:=!i.N>=4]

data[,azavea_amen:=ifelse(low_density_sher,as.character(azavea_quad),as.character(azavea_nbhd))]

###Function to pull amenities at random, given a
### neighborhood; n is the total number of properties
### in the full sample in the neighborhood
get_amen<-function(kkeys,n){
  amenities_azav[.(kkeys),sample(as.character(amenity),n,replace=T)]
}

setkey(amenities_azav,azavea_quad)
data[low_density_amen==T,paste0("example_amenity_",1:2):=
       lapply(1:2,function(x) get_amen(azavea_quad,.N)),by=azavea_quad]
setkey(amenities_azav,azavea_nbhd)
data[low_density_amen==F,paste0("example_amenity_",1:2):=
       lapply(1:2,function(x) get_amen(azavea_nbhd,.N)),by=azavea_nbhd]

rm(amenities_azav)

# Treatment Assignment ####
treatments<-paste0(rep(c("Sheriff","Lien","Moral","Amenities",
                         "Peer","Duty","Control"),each=2),"_",
                   rep(c("Big_Envelope","Small_Envelope"),7))
## Block randomization on balance due--sort on balance due and
##   assign treatments at random evenly in blocks of n_treatments
setkey(setorder(data,-total_due)[,grp:=rep(1:ceiling(.N/length(treatments)),
                                           each=length(treatments),length.out=.N)
                                 ][,treatment:=sample(treatments,size=.N),
                                   by=grp][,c("grp","treatment"):=
                                             list(NULL,as.factor(treatment))],treatment)

## For pretty output, format total_due as a number with $ and commas:
data[,total_due:=paste0("$",gsub("\\s","",formatC(total_due/100,format="f",big.mark=",",digits=2)))]

## Output individual files for each treatment,
##   sorting by mailing zip for pre-sorting purposes
invisible(lapply(treatments,function(x){
  write.xlsx2(data[.(x)][order(mail_zip)],
            file=paste0("round_2_sample_",tolower(x),".xlsx"),
            row.names=F); gc()}))
