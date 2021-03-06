# Real Estate Tax Experiment Randomization Code Round 2
# Michael Chirico
# May 15, 2015

# Setup: Working Directory, Packages,
#   Convenient Functions, Set Random Seed ####
library(funchir)
library(data.table)
library(xlsx)
library(foreign)
library(maptools)
write.packages('logs/round_two/randomization_code_session.txt')

##Set Random Seed:
## Took current time as of this writing and appended the digits
## of a Banana Republic coupon code I found on my desk: 1729749
set.seed(1820749)

# Data Import ####
data_final = fread('data/2015 Delinquent.csv', drop = c('V8', 'V9'),
                   colClasses = abbr_to_colClass("cnc", "731"))
setnames(data_final,
         c("opa_no", "owner1", "owner2",
           "mail_" %+% c("address", "city", "state", "zip"),
           "total_due", "property_address"))

data_old = fread('data/round_two_property_file.csv',
                 select = c("BRT NUMBER", "AZAVEA NEIGHBORHOOD", "ZIP CODE"))
setnames(data_old, c("opa_no", "azavea_nbhd", "zip"))

data<-setkey(data_old,opa_no
             )[setkey(data_final,opa_no)
               ][,total_due:=total_due/100]; rm(data_final,data_old)

# Hand-code Azavea & Zip for new properties (3)
data["632196220",c("azavea_nbhd","zip"):=list("Bustleton",19115L)]
data["661068112",c("azavea_nbhd","zip"):=list("Morrell Park",19114L)]
data["881035640",c("azavea_nbhd","zip"):=list("Logan Square",19103L)]

# Extract hold-out sample for DoR
holdout_size<-3000L
holdout<-sample(nrow(data),holdout_size)

## DoR wants XLSX format
write.xlsx2(data[holdout], row.names = FALSE,
            file = 'round_two/holdout_sample.xlsx')
## More useful for our own later use to have CSV
write.csv(data[holdout], row.names = FALSE,
          file = 'round_two/holdout_sample.csv')

data<-data[!holdout]; rm(holdout,holdout_size)

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

data[,paste0("owner",1:2,"_clean"):=
       lapply(.SD,to.proper),.SDcols="owner"%+%1:2]

#Add in spatial data

## Sheriff's Sales Info ####
sheriffs_map_delinquent<-
  readShapePoints('data/delinquent_sold_year_to_may_15_nad.shp')
phila_azav<-
  readShapePoly('data/Neighborhoods_Philadelphia_with_quadrants.shp')

sheriffs_sales<-setkey(setkey(setkey(fread(
  'data/sheriffs_sales/delinquent_sold_year_to_may_15.csv'
  ),opa_no)[.(as.character(sheriffs_map_delinquent@data$opa_no)),
            #Spatial join: assign Azavea neighborhood and quadrant to each sale
            `:=`(azavea_nbhd=(sheriffs_map_delinquent %over% phila_azav)[,"LISTNAME"],
                 azavea_quad=(sheriffs_map_delinquent %over% phila_azav)[,"quadrant"])
            #Rename Azavea neighborhoods to match DoR data
            ],azavea_nbhd)[setkey(fread(
                      "azaveas_mapping_dor_shp.csv"),azavea_shp),azavea_nbhd:=i.azavea_dor],
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
  setkey(setkey(data.table(read.dbf('data/Neighborhoods_Philadelphia_with_quadrants.dbf')
                           )[,.(azavea_shp=LISTNAME,quadrant)],azavea_shp
                )[fread('data/azaveas_mapping_dor_shp.csv'),
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
  readShapePoints('data/amenities_azav_for_geocoding_nad.shp')

amenities_azav<-
  setkey(as.data.table(amenities_map)[,.(amenity,address,resource)],address
         )[.(as.character(amenities_map@data$address)),
           #Spatial join: assign Azavea neighborhood and quadrant to each sale
           `:=`(azavea_nbhd=(amenities_map %over% phila_azav)[,"LISTNAME"],
                azavea_quad=(amenities_map %over% phila_azav)[,"quadrant"])
           ][!is.na(azavea_nbhd),]; rm(amenities_map,phila_azav)
###Reset names of Azavea neighborhoods to fit formatting of DoR records
setkey(amenities_azav,azavea_nbhd
       )[setkey(fread("azaveas_mapping_dor_shp.csv"),azavea_shp),
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
setkey(setkey(data,owner1)[data[,sum(total_due),by=owner1][order(-V1)][
  ,rand_id:=rep(1:ceiling(.N/length(treatments)),
            each=length(treatments),length.out=.N)
  ][,treatment:=sample(treatments,size=.N),
    by=rand_id][,treatment:=as.factor(treatment)],
  `:=`(treatment=as.factor(i.treatment),
       rand_id=i.rand_id)],
  treatment)

## Output full data file for future analysis
write.csv(data,file='round_two/round_2_full_data.csv', row.names=FALSE)

## For pretty output, format total_due as a number with $ and commas:
data[,total_due:=paste0("$",gsub("\\s","",formatC(total_due,format="f",big.mark=",",digits=2)))]

## Output individual files for each treatment,
##   sorting by mailing zip for pre-sorting purposes
invisible(lapply(treatments,function(x){
  write.xlsx2(data[.(x)][order(mail_zip)],
            file=paste0('/data/round_2_sample_',tolower(x),'.xlsx'),
            row.names=F); gc()})); rm(treatments)
