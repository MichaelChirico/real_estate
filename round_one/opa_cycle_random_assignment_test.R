##**note: file is a bit decrepit -- 
##    may need extensive tinkering to be useful.
##    will not be updating. **

#Data Exploration
#Philadelphia Real Estate Tax Evasion - Code for Testing Random Assignment of Cycle Numbers
#Michael Chirico
#February 14, 2015
library(data.table)
library(xtable)
library(gplots)
setEPS()

setwd("Desktop/research/Sieg_LMI_Real_Estate_Delinquency/")
rm(list=ls(all=T))
#Convenient Functions
abbr_to_colClass<-function(inits,counts){
  x<-substring(inits,1:nchar(inits),1:nchar(inits))
  types<-ifelse(x=="c","character",
                ifelse(x=="f","factor",
                       ifelse(x=="i","integer",
                              "numeric")))
  rep(types,substring(counts,1:nchar(counts),1:nchar(counts)))
}
prop.table2<-function(...,exclude=if(useNA=="no")c(NA,NaN),useNA=c("no","ifany", "always"),
                      dnn=list.names(...),deparse.level=1,margin=NULL){
  list.names <- function(...) {
    l <- as.list(substitute(list(...)))[-1L]
    nm <- names(l)
    fixup <- if (is.null(nm)) 
      seq_along(l)
    else nm == ""
    dep <- vapply(l[fixup], function(x) switch(deparse.level + 
                                                 1, "", if (is.symbol(x)) as.character(x) else "", 
                                               deparse(x, nlines = 1)[1L]), "")
    if (is.null(nm)) 
      dep
    else {
      nm[fixup] <- dep
      nm
    }
  }
  if (!missing(exclude) && is.null(exclude)) 
    useNA <- "always"
  useNA <- match.arg(useNA)
  prop.table(table(...,exclude=exclude,useNA=useNA,dnn=dnn,deparse.level=1),margin=margin)
}
create_quantiles<-function(x,num,right=F,include.lowest=T){
  cut(x,breaks=quantile(x,probs=seq(0,1,by=1/num)),labels=1:num,right=right,include.lowest=include.lowest)
}

cycle_info<-
  setkey(setnames(fread("/media/data_drive/real_estate/opa_cycles.csv",
                        colClasses=abbr_to_colClass("cncncfncfcn",
                                                    "11215212111")),
                  c("opa_no","cycle","address","address_code","return_mail_count_property",
                    "owner_name",paste("mail_address_line",1:3,sep=""),"mail_city","mail_state",
                    "mail_zip","return_mail_count_address","min_period","max_period","case_status",
                    "bill_suppress_date","total_balance")
                  )[,opa_no:=substr(opa_no,1,nchar(opa_no)-2)
                    ][nchar(opa_no)==8,opa_no:=paste0(0,opa_no)],opa_no)

##UNCLEAR WHY 2164 PROPERTIES ARE MISSING CYCLE INFORMATION... Ignore for now
cycle_info<-cycle_info[cycle!=0,]

cycle_info[is.na(total_balance),total_balance:=0]

cycle_info[,delinquent_ind:=total_balance>0]

cycle_info[is.na(return_mail_count_property),return_mail_count_property:=0]
cycle_info[is.na(return_mail_count_address),return_mail_count_address:=0]


#Bringing in previous data about other property characteristics to test for randomness
opa_data<-setkey(setnames(fread("/media/data_drive/real_estate/prop2014.txt",
                                colClasses=abbr_to_colClass("cfcnfcnfcfcfnfnfnfcfnfnfnfnfcc",
                                                            "446122522212211612121231291221")
                                )[,END:=NULL],c("opa_no","address","owner1","owner2","census_tract",
                                                "zip9","ward_geographic","street_code","house_no","add_suffix",
                                                "unit","add_extension","deed_record_date","sale_date",
                                                "sale_price","sale_type","unfinished","assess_date","mv_date",
                                                "market_value","taxable_land","taxable_building","exempt_land",
                                                "exempt_building","category","exempt_code","exempt_date",
                                                "building_code","zoning","land_use_code","designated_area",
                                                "usage","site_type","frontage","depth","shape","land_area",
                                                "topography","garage_type","garage_spaces_covered",
                                                "garage_spaces_open","view","other_bldgs","stories",
                                                "exterior_finish","dwell_type","exterior_date",
                                                "exterior_condition","quality_grade","year_built",
                                                "year_built_est_flag","floor_plan","rooms","bedrooms",
                                                "bathrooms","basement","basement_area","fireplaces",
                                                "heater","fuel_type","central_air","interior_condition",
                                                "amenities","improvement_type","functional_utility","sewer_connected",
                                                "separate_utilities","liveable_area","off_property_mailing",
                                                "book_and_page","registry_number","cross_reference")),opa_no)

#in accordance with the data dictionary, some properties (gov't buildings, the projects, etc) were assigned
#pseudo-wards, and these numbers are the first two digits of the OPA Number
opa_data[,ward_pseudo:=substr(opa_no,1,2)] 
opa_data[,opa_book:=substr(opa_no,3,3)]
opa_data[,zip5:=substr(zip9,1,5)]
#coerce geographic ward into that indicated by the OPA Number for any property which should have geo ward = pseudo-ward
opa_data[as.numeric(ward_pseudo)<=66&ward_geographic=="",ward_geographic:=ward_pseudo]
#currently upper- and lower-case sale type codes are counted the same
opa_data[tolower(sale_type)==sale_type,sale_type:=toupper(sale_type)]
#form requires other_bldgs and year_built_est be left blank unless Y; similarly for N on sewer_connected
opa_data[other_bldgs %in% c("0","N"),other_bldgs:=""]
opa_data[year_built_est_flag %in% c("0","N"),year_built_est_flag:=""]
opa_data[sewer_connected=="Y",sewer_connected:=""]
#convert nonsense construction years, exterior finish/condition to blank
opa_data[year_built %in% c("0","0  0","0000","2912","2930","3008","3009","4  0"),year_built:=""]
opa_data[exterior_finish=="0",exterior_finish:=""]
opa_data[exterior_condition=="",exterior_condition:=0]
#for uniformity, coerce 0 to N for central air
opa_data[central_air=="0",central_air:="N"]
#census tracts are more detailed (6 digits: one extra prefix and two extra suffix) when released by the census dept. this helps merge them
opa_data[,census_tract:=paste(0,census_tract,sep="")]
#assign (rough) Philly Planning Districts (see dor_data_mayy_plus_philly_layers.qgs)
opa_data[,planning_district:=as.factor("")]
opa_data[ward_geographic %in% c("58"),planning_district:=as.factor("Upper Far NE")]
opa_data[ward_geographic %in% c("57","66"),planning_district:=as.factor("Lower Far NE")]
opa_data[ward_geographic %in% c("56","63","64"),planning_district:=as.factor("Central NE")]
opa_data[ward_geographic %in% c("41","55","62","65"),planning_district:=as.factor("N Delaware")]
opa_data[ward_geographic %in% c("23","35","53","54"),planning_district:=as.factor("Lower NE")]
opa_data[ward_geographic %in% c("25","31","45"),planning_district:=as.factor("River Wards")]
opa_data[ward_geographic %in% c("07","11","13","19","33","37","38","42","43"),planning_district:=as.factor("North")]
opa_data[ward_geographic %in% c("10","17","49","50","61"),planning_district:=as.factor("Upper North")]
opa_data[ward_geographic %in% c("09","12","22","59"),planning_district:=as.factor("Upper NW")]
opa_data[ward_geographic %in% c("21"),planning_district:=as.factor("Lower NW")]
opa_data[ward_geographic %in% c("14","16","18","20","28","29","32","47"),planning_district:=as.factor("Lower N")]
opa_data[ward_geographic %in% c("02","05","08","15","30"),planning_district:=as.factor("Central")]
opa_data[ward_geographic %in% c("52"),planning_district:=as.factor("West Park")]
opa_data[ward_geographic %in% c("03","04","06","34","44","46","60"),planning_district:=as.factor("West")]
opa_data[ward_geographic %in% c("24","27","51"),planning_district:=as.factor("University/SW")]
opa_data[ward_geographic %in% c("40"),planning_district:=as.factor("Lower SW")]
opa_data[ward_geographic %in% c("01","36","39","48"),planning_district:=as.factor("South")]
opa_data[ward_geographic %in% c("26"),planning_district:=as.factor("Lower South")]

#convert numeric variables from strings
tonumeric<-c("sale_price","market_value","taxable_land","taxable_building","exempt_land",
             "exempt_building","frontage","depth","land_area","garage_spaces_covered",
             "garage_spaces_open","stories","year_built","rooms","bedrooms","bathrooms",
             "basement_area","fireplaces")
opa_data[,(tonumeric):=lapply(.SD,as.numeric),.SDcols=tonumeric]
rm(tonumeric)

#convert categorical variables to factors from strings

tofactor<-c("zip9","zip5","census_tract","ward_geographic","street_code","unfinished","category",
            "exempt_code","building_code","zoning","land_use_code","usage","site_type","shape",
            "topography","garage_type","view","other_bldgs","exterior_finish","dwell_type",
            "exterior_condition","quality_grade","year_built_est_flag","floor_plan","basement",
            "heater","fuel_type","central_air","interior_condition","amenities","improvement_type",
            "sewer_connected","separate_utilities","off_property_mailing","ward_pseudo")
opa_data[,(tofactor):=lapply(.SD,as.factor),.SDcols=tofactor]
rm(tofactor)

#Merge the additional info to the cycle dataset

cycle_info<-opa_data[cycle_info]
rm(opa_data)

#**2014 TREATMENT GROUP INCLUDES ONLY CYCLES 33-47**

cycle_info<-cycle_info[cycle %in% 33:47,]

#**INTENDED TREATMENTS WERE AS FOLLOWS**
cycle_info[cycle %in% c(35,37,44),
           c("treatment_int","treatment_int_name"):=list(1,factor("Threat"))]
cycle_info[cycle %in% c(33,40,41,47),
           c("treatment_int","treatment_int_name"):=list(2,factor("Moral"))]
cycle_info[cycle %in% c(34,38,43,46),
           c("treatment_int","treatment_int_name"):=list(3,factor("Peer"))]
cycle_info[cycle %in% c(36,39,42,45),
           c("treatment_int","treatment_int_name"):=list(4,factor("Control"))]
cycle_info[,treatment_int_name:=factor(treatment_int_name,
                                       levels(treatment_int_name)[c(4,2,3,1)])]

#**ACTUAL TREATMENTS WERE AS FOLLOWS**
cycle_info[cycle %in% c(35,36,44),treatment_act:=1]
cycle_info[cycle %in% c(33,40,41,42,47),treatment_act:=2]
cycle_info[cycle %in% c(34,37,38,43,46),treatment_act:=3]
cycle_info[cycle %in% c(39,45),treatment_act:=4]

#**TREATMENT FIDELITY WAS HIGHLY COMPROMISED (>20%) FOR FLAGGED CYCLES**
cycle_info[,fidelity_flag:=0]
cycle_info[cycle %in% c(34:38,42),fidelity_flag:=1]

#Random assignment is done at the owner level, not the property level,
#so eliminate duplicates--2nd, 3rd, etc properties of a given owner

##Not clear how to give so, given fuzzy matching of owner names.
###Approach 1: Given that most of the bloat (single owners
### with many different spellings) is likely due to gov't
### authorities, which are tax exempt in some way, a first proxy
### on eliminating this is to ignore exempt properties

non_exempt<-cycle_info[exempt_code=="",][!duplicated(owner_name),]

###Approach 2: Supposedly randomization was done using the last
### two digits of the ID on file with DOR (SSN, EIN, or
### DOR-generated TEMP ID); we don't have ALL of these IDs, but we
### do have a significant proportion; can check for balance on
### the subset of properties for which we do have the IDs.
###  (incidentically, also all-but eliminates non-exempt IDs)
get_id<-setkey(fread("/media/data_drive/real_estate/dor_data_oct_encrypted.csv",
                     select=c("opa_no","encrypted_id")
                     )[!duplicated(encrypted_id),],opa_no)

cycle_with_encrypt_id<-cycle_info[,.(opa_no,cycle)][get_id][opa_no!="",]

#Add intended ID (mapping from last two digits of secure ID to cycle #)
cycle_with_encrypt_id[,cycle_intended:=ceiling(as.numeric(substr(encrypted_id,7,8))/2)]
#last two digits=00 -> intended cycle 50
cycle_with_encrypt_id[ceiling(as.numeric(substr(encrypted_id,7,8))/2)==0,cycle_intended:=50]

int_act_matrix<-as.matrix(table(cycle_with_encrypt_id[cycle_intended %in% 33:47,
                                                      .(cycle_intended,cycle)]),ncol=15)

write.csv(table(cycle_with_encrypt_id$cycle_intended,cycle_with_encrypt_id$cycle),file="cycle_intended_vs_actual.csv",quote=F)

###Approach 3: Eliminate ALL owners w/ >1 property
### (combined w/ nonexempt to cut down own big-owner bloat again)
cycle_info[,prop_count:=.N,by=owner_name]
only_property_nonexempt<-cycle_info[exempt_code %in% c("","M","N","1","2","8")&prop_count==1,]

setkey(only_property_nonexempt,cycle)

##Tests for randomness
###First 3 sets: Testing randomness of Cycle #
# 1) Chi-Square Testing H0: # of owners is evenly distributed
#     (a) Exclude all exempt properties
#     (b) By encrypted ID
#     (c) By nonexempt single property owners
#****HEREIN USE NONEXEMPT SINGLE PROPERTY OWNERS****
# 2) ANOVA Testing H0: Continuous Variable Means
#     (a) Balance, unconditional on delinquency
#     (b) Balance, conditional on delinquency
#     (c) Market Value of property
#     (d) Property Frontage
#     (e) Property Land Area
#     (f) # Stories
#     (g) # Rooms
# 3) Chi-Square Testing H0: Other descriptives differ by cycle
#     (a) Case assignment
#     (b) Geographic Ward
#     (c) 5-Digit Zip Code (among common types)
#     (d) Planning District
#     (e) Pseudo-Ward (among common types)
#     (f) Sale Type (among common types)
#     (g) Category (Residential/Hotel-Apt/Commercial, etc)
#     (h) Zoning Code (among common types)
###Second 3 tests: Testing randomness of Treatment #
# 4) Chi-Square Testing H0: # of owners is evenly distributed
#     (a) Exclude all exempt properties
#     (b) By encrypted ID
#     (c) By nonexempt single property owners
#****HEREIN USE NONEXEMPT SINGLE PROPERTY OWNERS****
# 5) ANOVA Testing H0: Continuous Variable Means
#     (a) Balance, unconditional on delinquency
#     (b) Balance, conditional on delinquency
#     (c) Market Value of property
#     (d) Property Frontage
#     (e) Property Land Area
#     (f) # Stories
#     (g) # Rooms
# 6) Chi-Square Testing H0: Other descriptives differ by treatment
#     (a) Case assignment
#     (b) Geographic Ward
#     (c) 5-Digit Zip Code (among common types)
#     (d) Planning District
#     (e) Pseudo-Ward (among common types)
#     (f) Sale Type (among common types)
#     (g) Category (Residential/Hotel-Apt/Commercial, etc)
#     (h) Zoning Code (among common types)

p_values<-vector()

#################################################
# 1) (a) NUMBER OF PROPERTIES DIFFERS BY CYCLE? #
#        (Exclude all exempt properties)        #
#################################################
test1a<-chisq.test(table(non_exempt$cycle))
print(test1a)
p_values<-c(p_values,test1a$p.value)
rm(test1a)

png('cycle_count_nonexempt.png')
barplot(table(non_exempt$cycle),col="blue",las=2,
     main="Count of Non-Exempt Owners \n by Mailing Cycle",xlab="Mailing Cycle")
abline(h=mean(table(non_exempt$cycle)),col="red",lty=1)
dev.off()

#################################################
# 1) (b) NUMBER OF PROPERTIES DIFFERS BY CYCLE? #
#        (By encrypted ID)                      #
#################################################

test1b<-chisq.test(table(cycle_with_encrypt_id$cycle))
print(test1b)
p_values<-c(p_values,test1b$p.value)
rm(test1b)

png('cycle_count_encrypt.png')
barplot(table(cycle_with_encrypt_id$cycle),col="blue",las=2,
     main="Count of Encrypted IDs \n by Mailing Cycle",xlab="Mailing Cycle")
abline(h=mean(table(cycle_with_encrypt_id$cycle)),col="red",lty=1)
dev.off()

#################################################
# 1) (c) NUMBER OF PROPERTIES DIFFERS BY CYCLE? #
#        (By nonexempt single property owners)  #
#################################################

test1c<-chisq.test(table(only_property_nonexempt$cycle))
print(test1c)
p_values<-c(p_values,test1c$p.value)
rm(test1c)

png('cycle_count_single_property_nonexempt.png')
barplot(table(only_property_nonexempt$cycle),col="blue",las=2,
     main="Count of Non-Exempt Single-Property Owners \n by Mailing Cycle",xlab="Mailing Cycle")
abline(h=mean(table(only_property_nonexempt$cycle)),col="red",lty=1)
dev.off()

##########################################
# 2) (a) TOTAL BALANCE DIFFERS BY CYCLE? #
##########################################

test2a <- aov(total_balance~cycle,data=only_property_nonexempt)
summary(test2a)
p_values<-c(p_values,summary(test2a)[[1]][["Pr(>F)"]][[1]])
rm(test2a)

png('cycle_total_balance.png')
hist(only_property_nonexempt[,mean(total_balance),by=cycle]$V1,
     col="blue",main="Average Total Balance \n by Mailing Cycle",
     xlab="$ Balance")
dev.off()

###################################################
# 2) (b) TOTAL POSITIVE BALANCE DIFFERS BY CYCLE? #
###################################################

test2b <- aov(total_balance~cycle,data=only_property_nonexempt[total_balance>0,])
summary(test2b)
p_values<-c(p_values,summary(test2b)[[1]][["Pr(>F)"]][[1]])
rm(test2b)

png('cycle_positive_balance.png')
hist(only_property_nonexempt[total_balance>0,mean(log(total_balance)),by=cycle]$V1,
     col="blue",main="Average Log Balance \n by Mailing Cycle, Balance>0",
     xlab="log $ Balance")
dev.off()

##################################################
# 2) (c) PROPERTY MARKET VALUE DIFFERS BY CYCLE? #
##################################################

test2c <- aov(market_value~cycle,data=only_property_nonexempt)
summary(test2c)
p_values<-c(p_values,summary(test2c)[[1]][["Pr(>F)"]][[1]])
rm(test2c)

png('cycle_property_value.png')
hist(only_property_nonexempt[market_value>0,mean(log(market_value)),by=cycle]$V1,
     col="blue",main="Average Log Market Value \n by Mailing Cycle, MV>0",
     xlab="log $")
dev.off()

#####################################
# 2) (d) FRONTAGE DIFFERS BY CYCLE? #
#####################################

#Given possible coding errors in this variable, at least exclude extreme values
frontage_05<-quantile(only_property_nonexempt$frontage,.05)
frontage_95<-quantile(only_property_nonexempt$frontage,.95)

test2d <- aov(frontage~cycle,data=only_property_nonexempt[frontage>frontage_05&frontage<frontage_95,])
summary(test2d)
p_values<-c(p_values,summary(test2d)[[1]][["Pr(>F)"]][[1]])
rm(test2d)

png('cycle_frontage.png')
hist(only_property_nonexempt[frontage>frontage_05&
                               frontage<frontage_95,mean(frontage)/10,by=cycle]$V1,
     col="blue",main="Average Frontage by Mailing Cycle",
     xlab="feet")
dev.off()

######################################
# 2) (e) LAND AREA DIFFERS BY CYCLE? #
######################################

#Given possible coding errors in this variable, at least exclude extreme values
land_area_05<-quantile(only_property_nonexempt$land_area,.05)
land_area_95<-quantile(only_property_nonexempt$land_area,.95)

test2e <- aov(land_area~cycle,
              data=only_property_nonexempt[land_area>land_area_05&
                                             land_area<land_area_95,])
summary(test2e)
p_values<-c(p_values,summary(test2e)[[1]][["Pr(>F)"]][[1]])
rm(test2e)

png('cycle_land_area.png')
hist(only_property_nonexempt[land_area>land_area_05&
                               land_area<land_area_95,mean(land_area)/100,by=cycle]$V1,
     col="blue",main="Average Land Area by Mailing Cycle",
     xlab="square feet")
dev.off()

##############################################
# 2) (f) NUMBER OF STORIES DIFFERS BY CYCLE? #
##############################################

#Given possible coding errors in this variable, at least exclude extreme values
stories_05<-quantile(only_property_nonexempt[stories>=0,]$stories,.05)
stories_95<-quantile(only_property_nonexempt[stories>0,]$stories,.95)

test2f <- aov(stories~cycle,
              data=only_property_nonexempt[stories>=stories_05&
                                             stories<stories_95,])
summary(test2f)
p_values<-c(p_values,summary(test2f)[[1]][["Pr(>F)"]][[1]])
rm(test2f)

png('cycle_stories.png')
hist(only_property_nonexempt[stories>=stories_05&
                               stories<stories_95,mean(stories)/10,by=cycle]$V1,
     col="blue",main="Average # Stories by Mailing Cycle",
     xlab="stories")
dev.off()

############################################
# 2) (g) NUMBER OF ROOMS DIFFERS BY CYCLE? #
############################################

#Given possible coding errors in this variable, at least exclude extreme values
rooms_05<-quantile(only_property_nonexempt[rooms>0,]$rooms,.05)
rooms_95<-quantile(only_property_nonexempt[rooms>0,]$rooms,.95)

test2g <- aov(rooms~cycle,
              data=only_property_nonexempt[rooms>=rooms_05&
                                             rooms<rooms_95,])
summary(test2g)
p_values<-c(p_values,summary(test2g)[[1]][["Pr(>F)"]][[1]])
rm(test2g)

png('cycle_rooms.png')
hist(only_property_nonexempt[rooms>=rooms_05&
                               rooms<rooms_95,mean(rooms)/10,by=cycle]$V1,
     col="blue",main="Average # Rooms by Mailing Cycle",
     xlab="rooms")
dev.off()

###########################################
# 3) (a) CASE ASSIGNMENT DIFFERS BY CYCLE #
###########################################

test3a<-chisq.test(table(only_property_nonexempt$case_status,
                         only_property_nonexempt$cycle))
print(test3a)
p_values<-c(p_values,test3a$p.value)
rm(test3a)

###########################################
# 3) (b) GEOGRAPHIC WARD DIFFERS BY CYCLE #
###########################################

test3b<-chisq.test(table(only_property_nonexempt$ward_geographic,
                         only_property_nonexempt$cycle))
print(test3b)
p_values<-c(p_values,test3b$p.value)
rm(test3b)

############################################
# 3) (c) 5-DIGIT ZIP CODE DIFFERS BY CYCLE #
############################################

#**Many zip codes highly underrepresented, **
#**so select those w/ >1% representation   **
#**in the population                       **
zip_table<-prop.table(table(only_property_nonexempt$zip5))
common_zips<-row.names(zip_table[zip_table>.01])

test3c<-chisq.test(table(as.character(only_property_nonexempt[zip5 %in% common_zips,]$zip5),
                         only_property_nonexempt[zip5 %in% common_zips,]$cycle))
print(test3c)
p_values<-c(p_values,test3c$p.value)
rm(test3c)

#############################################
# 3) (d) PLANNING DISTRICT DIFFERS BY CYCLE #
#############################################

test3d<-chisq.test(table(only_property_nonexempt$planning_district,
                         only_property_nonexempt$cycle))
print(test3d)
p_values<-c(p_values,test3d$p.value)
rm(test3d)

######################################
# 3) (e) PSEUDOWARD DIFFERS BY CYCLE #
######################################

#**Many pseudowards highly underrepresented, **
#**so select those w/ >1% representation     **
#**in the population                         **
pward_table<-prop.table(table(only_property_nonexempt$ward_pseudo))
common_pwards<-row.names(pward_table[pward_table>.01])

test3e<-chisq.test(table(as.character(only_property_nonexempt[ward_pseudo %in% 
                                                   common_pwards,]$ward_pseudo),
                         only_property_nonexempt[ward_pseudo %in% 
                                                   common_pwards,]$cycle))
print(test3e)
p_values<-c(p_values,test3e$p.value)
rm(test3e)

#####################################
# 3) (f) SALE TYPE DIFFERS BY CYCLE #
#####################################

#***To avoid cells w/ 0 counts, only include sale types w/ **
#**>5% representation in the population. These are         **
#** "": (No sale type)                                     **
#**  4: Sales b/w related parties                          **
#**  B: Unfinished improvements                            **
#**  L: Sale deferred for closer review                    **
#**  M: Closer review of sale completed                    **
#**  Z: ? (No description given for Z in dictionary)       **
test3f<-chisq.test(table(only_property_nonexempt[sale_type %in% 
                                                   c("","4","B","L","M","Z"),]$sale_type,
                         only_property_nonexempt[sale_type %in% 
                                                   c("","4","B","L","M","Z"),]$cycle))
print(test3f)
p_values<-c(p_values,test3f$p.value)
rm(test3f)

#########################################
# 3) (g) CATEGORY CODE DIFFERS BY CYCLE #
#########################################

test3g<-chisq.test(table(only_property_nonexempt$category,
                         only_property_nonexempt$cycle))
print(test3g)
p_values<-c(p_values,test3g$p.value)
rm(test3g)

#######################################
# 3) (h) ZONING CODE DIFFERS BY CYCLE #
#######################################

#***To avoid cells w/ 0 counts, only include zoning codes w/   **
#**>5% representation in the population. These are             **
#**  RM1: Residential Multi-Family (Attached, Low Density)     **
#** RSA3: Residential Single-Family (Attached, Medium Density) **
#** RSA5: Residential Single-Family (Attached, High Density)   **

test3h<-chisq.test(table(as.character(only_property_nonexempt[zoning %in% 
                                                   c("RM1","RSA3","RSA5"),]$zoning),
                         only_property_nonexempt[zoning %in% 
                                                   c("RM1","RSA3","RSA5"),]$cycle))
print(test3h)
p_values<-c(p_values,test3h$p.value)
rm(test3h)

##REPEAT THIS USING NOT THE CYCLE BUT THE TREATMENT AS THE ORTHOGONAL COMPARISON
##We know treatments 3&4 overrepresented, so no longer expecting uniform; specifically
p_exp=c(3/15,4/15,4/15,4/15)
#####################################################
# 4) (a) NUMBER OF PROPERTIES DIFFERS BY TREATMENT? #
#        (Exclude all exempt properties)            #
#####################################################

test4a<-chisq.test(table(non_exempt$treatment_int),p=p_exp)
print(test4a)
p_values<-c(p_values,test4a$p.value)
rm(test4a)

png('treatment_count_nonexempt.png')
barplot(table(non_exempt$treatment_int),col="blue",las=1,
        main="Count of Non-Exempt Owners \n by Treatment Group",xlab="Treatment Group")
abline(h=nrow(non_exempt)*3/14,col="red",lty=1)
abline(h=nrow(non_exempt)*4/14,col="red",lty=1)
dev.off()

#####################################################
# 4) (b) NUMBER OF PROPERTIES DIFFERS BY TREATMENT? #
#        (By encrypted ID)                          #
#####################################################

test4b<-chisq.test(table(cycle_with_encrypt_id$treatment_int),p=p_exp)
print(test4b)
p_values<-c(p_values,test4b$p.value)
rm(test4b)

png('treatment_count_encrypt.png')
barplot(table(cycle_with_encrypt_id$treatment_int),col="blue",las=1,
        main="Count of Encrypted IDs \n by Treatment Group",xlab="Treatment Group")
abline(h=nrow(cycle_with_encrypt_id)*3/14,col="red",lty=1)
abline(h=nrow(cycle_with_encrypt_id)*4/14,col="red",lty=1)
dev.off()

#####################################################
# 4) (c) NUMBER OF PROPERTIES DIFFERS BY TREATMENT? #
#        (By nonexempt single property owners)      #
#####################################################

test4c<-chisq.test(table(only_property_nonexempt$treatment_int),p=p_exp)
print(test4c)
p_values<-c(p_values,test4c$p.value)

graphics.off()
dev.new()
postscript('./papers-presentations/images/balance/treatment_count_single_property_nonexempt.eps')
dev.set(which=dev.list()["RStudioGD"])
barplot(table(only_property_nonexempt$treatment_int),col="blue",las=1,
        main="Count of Non-Exempt Single-Property Owners \n by Treatment Group",xlab="Treatment Group",
        ylim=c(0,1.05*nrow(only_property_nonexempt)*4/15))
abline(h=nrow(only_property_nonexempt)*3/15,col="red",lty=1)
abline(h=nrow(only_property_nonexempt)*4/15,col="red",lty=1)
text(2,y=.96*nrow(only_property_nonexempt)*3/15,labels="3/15",col="red")
text(2,y=.96*nrow(only_property_nonexempt)*4/15,labels="4/15",col="red")
dev.copy(which=dev.list()["postscript"])
dev.off(which=dev.list()["postscript"])

##############################################
# 5) (a) TOTAL BALANCE DIFFERS BY TREATMENT? #
##############################################

test5a <- aov(total_balance~treatment_int_name,data=only_property_nonexempt)
summary(test5a)
p_values<-c(p_values,summary(test5a)[[1]][["Pr(>F)"]][[1]])

graphics.off()
dev.new()
postscript('./papers-presentations/images/balance/total_balance_single_property_nonexempt.eps')
dev.set(which=dev.list()["RStudioGD"])
par(mfrow=c(2,2),oma=c(0,0,3,0))
hist(only_property_nonexempt[treatment_int==1
                             &total_balance>0,log10(total_balance)],
     col="red",main="Threat",xlab="Log_10 $",freq=F,breaks=seq(-2,6,by=.25))
hist(only_property_nonexempt[treatment_int==2
                             &total_balance>0,log10(total_balance)],
     col="blue",main="Moral",xlab="Log_10 $",freq=F,breaks=seq(-2,6,by=.25))
hist(only_property_nonexempt[treatment_int==3
                             &total_balance>0,log10(total_balance)],
     col="green",main="Peer",xlab="Log_10 $",freq=F,breaks=seq(-2,6,by=.25))
hist(only_property_nonexempt[treatment_int==4
                             &total_balance>0,log10(total_balance)],
     col="black",main="Control",xlab="Log_10 $",freq=F,breaks=seq(-2,6,by=.25))
title("Distribution of Total Balance by Treatment",outer=T)
dev.copy(which=dev.list()["postscript"])
dev.off(which=dev.list()["postscript"])

xtable(
  matrix(
    as.matrix(
      only_property_nonexempt[,list(mean(total_balance),
                                    sum(total_balance*(total_balance>0))/sum((total_balance>0))),
                              by=treatment_int_name][c(3,1,2,4),.(V1,V2)],ncol=2),ncol=2,
    dimnames=list(c("Threat","Moral","Peer","Control"),
                  c("Average Balance","Average Positive Balance"))))

only_property_nonexempt[total_balance>0,debt_quartile:=create_quantiles(total_balance,4)]
quart_treat_mat<-
  as.matrix(round(only_property_nonexempt[,prop.table2(treatment_int_name,
                                                 debt_quartile,margin=1)],2),ncol=4)

graphics.off()
dev.new()
postscript('./papers-presentations/images/balance/total_balance_quartiles_single_property_nonexempt.eps')
dev.set(which=dev.list()["RStudioGD"])
heatmap.2(x=quart_treat_mat,Rowv=F,Colv=F,dendrogram="none",
          cellnote=quart_treat_mat,notecol="black",trace="none",key=F,
          margins=c(5,7),main="Balance Quartile \n vs. Treatment")
dev.copy(which=dev.list()["postscript"])
dev.off(which=dev.list()["postscript"])

#######################################################
# 5) (b) TOTAL POSITIVE BALANCE DIFFERS BY TREATMENT? #
#######################################################

test5b <- aov(log(total_balance)~treatment_int_name,data=only_property_nonexempt[total_balance>0,])
summary(test5b)
p_values<-c(p_values,summary(test5b)[[1]][["Pr(>F)"]][[1]])

######################################################
# 5) (c) PROPERTY MARKET VALUE DIFFERS BY TREATMENT? #
######################################################

test5c <- aov(market_value~treatment_int_name,data=only_property_nonexempt)
summary(test5c)
p_values<-c(p_values,summary(test5c)[[1]][["Pr(>F)"]][[1]])

only_property_nonexempt[,mv_quartile:=create_quantiles(market_value,4)]
test5c2<-chisq.test(only_property_nonexempt[,table(mv_quartile,treatment_int_name)])
print(test5c2)

graphics.off()
dev.new()
postscript('./papers-presentations/images/balance/market_value_single_property_nonexempt.eps')
dev.set(which=dev.list()["RStudioGD"])
par(mfrow=c(2,2),oma=c(0,0,3,0))
hist(only_property_nonexempt[treatment_int==1
                             &market_value>0,log10(market_value)],
     col="red",main="Threat",xlab="Log_10 $",
     freq=F,breaks=seq(2,9,by=.25),xlim=c(3.5,6))
hist(only_property_nonexempt[treatment_int==2
                             &market_value>0,log10(market_value)],
     col="blue",main="Moral",xlab="Log_10 $",
     freq=F,breaks=seq(2,9,by=.25),xlim=c(3.5,6))
hist(only_property_nonexempt[treatment_int==3
                             &market_value>0,log10(market_value)],
     col="green",main="Peer",xlab="Log_10 $",
     freq=F,breaks=seq(2,9,by=.25),xlim=c(3.5,6))
hist(only_property_nonexempt[treatment_int==4
                             &market_value>0,log10(market_value)],
     col="black",main="Control",xlab="Log_10 $",
     freq=F,breaks=seq(2,9,by=.25),xlim=c(3.5,6))
title("Distribution of Market Value by Treatment",outer=T)
dev.copy(which=dev.list()["postscript"])
dev.off(which=dev.list()["postscript"])

#Table of average values for tests 5a)-c)
xtable(matrix(cbind(only_property_nonexempt[,mean(total_balance),by=treatment_int]$V1,
                    only_property_nonexempt[total_balance>0,mean(total_balance),by=treatment_int]$V1,
                    only_property_nonexempt[,mean(market_value),by=treatment_int]$V1),ncol=3,
              dimnames=list(1:4,c("Avg. Balance","Avg. Positive Balance","Avg. Property Value"))),digits=2)

#Table of median values for tests 5a)-c)
xtable(matrix(cbind(only_property_nonexempt[total_balance>0,median(total_balance),by=treatment_int]$V1,
                    only_property_nonexempt[,median(market_value),by=treatment_int]$V1),ncol=2,
              dimnames=list(1:4,c("Median Balance","Median Prop. Value"))),digits=2)


#########################################
# 5) (d) FRONTAGE DIFFERS BY TREATMENT? #
#########################################

#Given possible coding errors in this variable, at least exclude extreme values
test5d <- aov(frontage~treatment_int,
              data=only_property_nonexempt[frontage>frontage_05&frontage<frontage_95,])
summary(test5d)
p_values<-c(p_values,summary(test5d)[[1]][["Pr(>F)"]][[1]])
rm(test5d)

##########################################
# 5) (e) LAND AREA DIFFERS BY TREATMENT? #
##########################################

#Given possible coding errors in this variable, at least exclude extreme values
test5e <- aov(land_area~treatment_int_name,
              data=only_property_nonexempt[land_area>land_area_05&
                                             land_area<land_area_95,])
summary(test5e)
p_values<-c(p_values,summary(test5e)[[1]][["Pr(>F)"]][[1]])

only_property_nonexempt[,area_quartiles:=create_quantiles(land_area,4)]
test5e2<-chisq.test(only_property_nonexempt[,table(area_quartiles,treatment_int_name)])
print(test5e2)

##################################################
# 5) (f) NUMBER OF STORIES DIFFERS BY TREATMENT? #
##################################################

#Given possible coding errors in this variable, at least exclude extreme values
test5f <- aov(stories~treatment_int,
              data=only_property_nonexempt[stories>=stories_05&
                                             stories<stories_95,])
summary(test5f)
p_values<-c(p_values,summary(test5f)[[1]][["Pr(>F)"]][[1]])
rm(test5f)

################################################
# 5) (g) NUMBER OF ROOMS DIFFERS BY TREATMENT? #
################################################

#Given possible coding errors in this variable,
# exclude extreme values: above 12 & below 2 rooms
test5g <- aov(rooms~treatment_int_name,
              data=only_property_nonexempt[rooms>=20&
                                             rooms<=120,])
summary(test5g)
p_values<-c(p_values,summary(test5g)[[1]][["Pr(>F)"]][[1]])

#Table of average values for tests 5d)-g)
xtable(matrix(cbind(only_property_nonexempt[frontage>frontage_05&
                                              frontage<frontage_95,mean(frontage)/10,by=treatment_int]$V1,
                    only_property_nonexempt[land_area>land_area_05&
                                              land_area<land_area_95,mean(land_area)/100,by=treatment_int]$V1,
                    only_property_nonexempt[stories>=stories_05&
                                              stories<stories_95,mean(stories)/10,by=treatment_int]$V1,
                    only_property_nonexempt[rooms>=rooms_05&
                                              rooms<rooms_95,mean(rooms)/10,by=treatment_int]$V1),ncol=4,
              dimnames=list(1:4,c("Frontage","Land Area","# Stories","# Rooms"))),digits=2)
rm(frontage_05,frontage_95,
   land_area_05,land_area_95,
   stories_05,stories_95,
   rooms_05,rooms_95)

###############################################
# 6) (a) CASE ASSIGNMENT DIFFERS BY TREATMENT #
###############################################

test6a<-chisq.test(table(only_property_nonexempt$case_status,
                         only_property_nonexempt$treatment_int))
print(test6a)
p_values<-c(p_values,test6a$p.value)

###############################################
# 6) (b) GEOGRAPHIC WARD DIFFERS BY TREATMENT #
###############################################

test6b<-chisq.test(table(only_property_nonexempt$ward_geographic,
                         only_property_nonexempt$treatment_int))
print(test6b)
p_values<-c(p_values,test6b$p.value)

################################################
# 6) (c) 5-DIGIT ZIP CODE DIFFERS BY TREATMENT #
################################################
#**Many zip codes highly underrepresented, **
#**so select those w/ >1% representation   **
#**in the population                       **

test6c<-chisq.test(table(as.character(only_property_nonexempt[zip5 %in% common_zips,]$zip5),
                         only_property_nonexempt[zip5 %in% common_zips,]$treatment_int))
print(test6c)
p_values<-c(p_values,test6c$p.value)
rm(test6c,zip_table,common_zips)

#################################################
# 6) (d) PLANNING DISTRICT DIFFERS BY TREATMENT #
#################################################

test6d<-chisq.test(table(only_property_nonexempt$planning_district,
                         only_property_nonexempt$treatment_int))
print(test6d)
p_values<-c(p_values,test6d$p.value)
rm(test6d)

##########################################
# 6) (e) PSEUDOWARD DIFFERS BY TREATMENT #
##########################################

#**Many pseudowards highly underrepresented, **
#**so select those w/ >1% representation     **
#**in the population                         **
test6e<-chisq.test(table(as.character(only_property_nonexempt[ward_pseudo %in% 
                                                                common_pwards,]$ward_pseudo),
                         only_property_nonexempt[ward_pseudo %in% common_pwards,]$treatment_int))
print(test6e)
p_values<-c(p_values,test6e$p.value)
rm(test6e,pward_table,common_pwards)

#########################################
# 6) (f) SALE TYPE DIFFERS BY TREATMENT #
#########################################

#***To avoid cells w/ 0 counts, only include sale types w/ **
#**>5% representation in the population--see above         **

test6f<-chisq.test(table(only_property_nonexempt[sale_type %in% 
                                                   c("","4","B","L","M","Z"),]$sale_type,
                         only_property_nonexempt[sale_type %in% 
                                                   c("","4","B","L","M","Z"),]$treatment_int))
print(test6f)
p_values<-c(p_values,test6f$p.value)
rm(test6f)

#############################################
# 6) (g) CATEGORY CODE DIFFERS BY TREATMENT #
#############################################

test6g<-chisq.test(table(only_property_nonexempt$category,
                         only_property_nonexempt$treatment_int))
print(test6g)
p_values<-c(p_values,test6g$p.value)

#########################################
# 6) (h) SALE TYPE DIFFERS BY TREATMENT #
#########################################

#***To avoid cells w/ 0 counts, only include zoning codes w/   **
#**>5% representation in the population. See above.            **

test6h<-chisq.test(table(as.character(only_property_nonexempt[zoning %in% 
                                                                c("RM1","RSA3","RSA5"),]$zoning),
                         only_property_nonexempt[zoning %in% 
                                                   c("RM1","RSA3","RSA5"),]$treatment_int))
print(test6h)
p_values<-c(p_values,test6h$p.value)
rm(test6h)

##THIRD SET OF TESTS: TESTING CYCLE BALANCE AMONG PROPERTIES
##     FOR WHICH INTENDED CYCLE!=ACTUAL CYCLE

cycle_int_ne_act<-cycle_with_encrypt_id[cycle_intended!=cycle,]

#################################################
# 7) (a) NUMBER OF PROPERTIES DIFFERS BY CYCLE? #
#        (Exclude all exempt properties)        #
#################################################

test7a<-chisq.test(table(cycle_int_ne_act[exempt_code=="",]$cycle))
print(test7a)
p_values<-c(p_values,test7a$p.value)
rm(test7a)

##########################################
# 8) (a) TOTAL BALANCE DIFFERS BY CYCLE? #
##########################################

test8a <- aov(total_balance~cycle,data=cycle_int_ne_act)
summary(test8a)
p_values<-c(p_values,summary(test8a)[[1]][["Pr(>F)"]][[1]])
rm(test8a)

###################################################
# 8) (b) TOTAL POSITIVE BALANCE DIFFERS BY CYCLE? #
###################################################

test8b <- aov(total_balance~cycle,data=cycle_int_ne_act[total_balance>0,])
summary(test8b)
p_values<-c(p_values,summary(test8b)[[1]][["Pr(>F)"]][[1]])
rm(test8b)

##################################################
# 8) (c) PROPERTY MARKET VALUE DIFFERS BY CYCLE? #
##################################################

test8c <- aov(market_value~cycle,data=cycle_int_ne_act)
summary(test8c)
p_values<-c(p_values,summary(test8c)[[1]][["Pr(>F)"]][[1]])
rm(test8c)

#####################################
# 8) (d) FRONTAGE DIFFERS BY CYCLE? #
#####################################

#Given possible coding errors in this variable, at least exclude extreme values
frontage_05<-quantile(cycle_int_ne_act$frontage,.05,na.rm=T)
frontage_95<-quantile(cycle_int_ne_act$frontage,.95,na.rm=T)

test8d <- aov(frontage~cycle,data=cycle_int_ne_act[frontage>frontage_05&frontage<frontage_95,])
summary(test8d)
p_values<-c(p_values,summary(test8d)[[1]][["Pr(>F)"]][[1]])
rm(test8d)

######################################
# 8) (e) LAND AREA DIFFERS BY CYCLE? #
######################################

#Given possible coding errors in this variable, at least exclude extreme values
land_area_05<-quantile(cycle_int_ne_act$land_area,.05,na.rm=T)
land_area_95<-quantile(cycle_int_ne_act$land_area,.95,na.rm=T)

test8e <- aov(land_area~cycle,
              data=cycle_int_ne_act[land_area>land_area_05&
                                      land_area<land_area_95,])
summary(test8e)
p_values<-c(p_values,summary(test8e)[[1]][["Pr(>F)"]][[1]])
rm(test8e)

##############################################
# 8) (f) NUMBER OF STORIES DIFFERS BY CYCLE? #
##############################################

#Given possible coding errors in this variable, at least exclude extreme values
stories_05<-quantile(cycle_int_ne_act[stories>=0,]$stories,.05)
stories_95<-quantile(cycle_int_ne_act[stories>0,]$stories,.95)

test8f <- aov(stories~cycle,
              data=cycle_int_ne_act[stories>=stories_05&
                                      stories<stories_95,])
summary(test8f)
p_values<-c(p_values,summary(test8f)[[1]][["Pr(>F)"]][[1]])
rm(test8f)

############################################
# 8) (g) NUMBER OF ROOMS DIFFERS BY CYCLE? #
############################################

#Given possible coding errors in this variable, at least exclude extreme values
rooms_05<-quantile(cycle_int_ne_act[rooms>0,]$rooms,.05)
rooms_95<-quantile(cycle_int_ne_act[rooms>0,]$rooms,.95)

test8g <- aov(rooms~cycle,
              data=cycle_int_ne_act[rooms>=rooms_05&
                                      rooms<rooms_95,])
summary(test8g)
p_values<-c(p_values,summary(test8g)[[1]][["Pr(>F)"]][[1]])
rm(test8g)

###########################################
# 9) (a) CASE ASSIGNMENT DIFFERS BY CYCLE #
###########################################

test9a<-chisq.test(table(cycle_int_ne_act$case_status,
                         cycle_int_ne_act$cycle))
print(test9a)
p_values<-c(p_values,test9a$p.value)
rm(test9a)

###########################################
# 9) (b) GEOGRAPHIC WARD DIFFERS BY CYCLE #
###########################################

test9b<-chisq.test(table(cycle_int_ne_act$ward_geographic,
                         cycle_int_ne_act$cycle))
print(test9b)
p_values<-c(p_values,test9b$p.value)
rm(test9b)

############################################
# 9) (c) 5-DIGIT ZIP CODE DIFFERS BY CYCLE #
############################################

#**Many zip codes highly underrepresented, **
#**so select those w/ >1% representation   **
#**in the population                       **
zip_table<-prop.table(table(cycle_int_ne_act$zip5))
common_zips<-row.names(zip_table[zip_table>.01])

test9c<-chisq.test(table(as.character(cycle_int_ne_act[zip5 %in% common_zips,]$zip5),
                         cycle_int_ne_act[zip5 %in% common_zips,]$cycle))
print(test9c)
p_values<-c(p_values,test9c$p.value)
rm(test9c)

#############################################
# 9) (d) PLANNING DISTRICT DIFFERS BY CYCLE #
#############################################

test9d<-chisq.test(table(cycle_int_ne_act$planning_district,
                         cycle_int_ne_act$cycle))
print(test9d)
p_values<-c(p_values,test9d$p.value)
rm(test9d)

######################################
# 9) (e) PSEUDOWARD DIFFERS BY CYCLE #
######################################

#**Many pseudowards highly underrepresented, **
#**so select those w/ >1% representation     **
#**in the population                         **
pward_table<-prop.table(table(cycle_int_ne_act$ward_pseudo))
common_pwards<-row.names(pward_table[pward_table>.01])

test9e<-chisq.test(table(as.character(cycle_int_ne_act[ward_pseudo %in% 
                                                         common_pwards,]$ward_pseudo),
                         cycle_int_ne_act[ward_pseudo %in% 
                                            common_pwards,]$cycle))
print(test9e)
p_values<-c(p_values,test9e$p.value)
rm(test9e)

#####################################
# 9) (f) SALE TYPE DIFFERS BY CYCLE #
#####################################

#***To avoid cells w/ 0 counts, only include sale types w/ **
#**>5% representation in the population. These are         **
#** "": (No sale type)                                     **
#**  4: Sales b/w related parties                          **
#**  B: Unfinished improvements                            **
#**  L: Sale deferred for closer review                    **
#**  M: Closer review of sale completed                    **
#**  Z: ? (No description given for Z in dictionary)       **
test9f<-chisq.test(table(cycle_int_ne_act[sale_type %in% 
                                            c("","4","B","L","M","Z"),]$sale_type,
                         cycle_int_ne_act[sale_type %in% 
                                            c("","4","B","L","M","Z"),]$cycle))
print(test9f)
p_values<-c(p_values,test9f$p.value)
rm(test9f)

#########################################
# 9) (g) CATEGORY CODE DIFFERS BY CYCLE #
#########################################

test9g<-chisq.test(table(cycle_int_ne_act$category,
                         cycle_int_ne_act$cycle))
print(test9g)
p_values<-c(p_values,test9g$p.value)
rm(test9g)

#######################################
# 9) (h) ZONING CODE DIFFERS BY CYCLE #
#######################################

#***To avoid cells w/ 0 counts, only include zoning codes w/   **
#**>5% representation in the population. These are             **
#**  RM1: Residential Multi-Family (Attached, Low Density)     **
#** RSA3: Residential Single-Family (Attached, Medium Density) **
#** RSA5: Residential Single-Family (Attached, High Density)   **

test9h<-chisq.test(table(as.character(cycle_int_ne_act[zoning %in% 
                                                         c("RM1","RSA3","RSA5"),]$zoning),
                         cycle_int_ne_act[zoning %in% 
                                            c("RM1","RSA3","RSA5"),]$cycle))
print(test9h)
p_values<-c(p_values,test9h$p.value)
rm(test9h)

##FOURTH SET OF TESTS: IS **ACTUAL** TREATMENT ORTHOGONAL TO OBSERVABLES,
##    AMONG THOSE TREATMENT DAYS THAT WERE NOT COMPROMISED

non_exempt_unc              <-              non_exempt[fidelity_flag==0,]
cycle_with_encrypt_id_unc   <-   cycle_with_encrypt_id[fidelity_flag==0,]
only_property_nonexempt_unc <- only_property_nonexempt[fidelity_flag==0,]

##We know treatments are not evenly represented, so no longer expecting uniform; specifically
p_exp=c(1/9,4/9,2/9,2/9)
######################################################
# 10) (a) NUMBER OF PROPERTIES DIFFERS BY TREATMENT? #
#         (Exclude all exempt properties)            #
######################################################

test10a<-chisq.test(table(non_exempt_unc$treatment_act),p=p_exp)
print(test10a)
p_values<-c(p_values,test10a$p.value)
rm(test10a)

######################################################
# 10) (b) NUMBER OF PROPERTIES DIFFERS BY TREATMENT? #
#         (By encrypted ID)                          #
######################################################

test10b<-chisq.test(table(cycle_with_encrypt_id_unc$treatment_act),p=p_exp)
print(test10b)
p_values<-c(p_values,test10b$p.value)
rm(test10b)

######################################################
# 10) (c) NUMBER OF PROPERTIES DIFFERS BY TREATMENT? #
#         (By nonexempt single property owners)      #
######################################################

test10c<-chisq.test(table(only_property_nonexempt_unc$treatment_act),p=p_exp)
print(test10c)
p_values<-c(p_values,test10c$p.value)
rm(test10c)

###############################################
# 11) (a) TOTAL BALANCE DIFFERS BY TREATMENT? #
###############################################

test11a <- aov(total_balance~treatment_act,data=only_property_nonexempt_unc)
summary(test11a)
p_values<-c(p_values,summary(test11a)[[1]][["Pr(>F)"]][[1]])
rm(test11a)

########################################################
# 11) (b) TOTAL POSITIVE BALANCE DIFFERS BY TREATMENT? #
########################################################

test11b <- aov(total_balance~treatment_act,data=only_property_nonexempt_unc[total_balance>0,])
summary(test11b)
p_values<-c(p_values,summary(test11b)[[1]][["Pr(>F)"]][[1]])
rm(test11b)

#######################################################
# 11) (c) PROPERTY MARKET VALUE DIFFERS BY TREATMENT? #
#######################################################

test11c <- aov(market_value~treatment_act,data=only_property_nonexempt_unc)
summary(test11c)
p_values<-c(p_values,summary(test11c)[[1]][["Pr(>F)"]][[1]])
rm(test11c)

##########################################
# 11) (d) FRONTAGE DIFFERS BY TREATMENT? #
##########################################

#Given possible coding errors in this variable, at least exclude extreme values
test11d <- aov(frontage~treatment_act,
              data=only_property_nonexempt_unc[frontage>frontage_05&frontage<frontage_95,])
summary(test11d)
p_values<-c(p_values,summary(test11d)[[1]][["Pr(>F)"]][[1]])
rm(test11d)

###########################################
# 11) (e) LAND AREA DIFFERS BY TREATMENT? #
###########################################

#Given possible coding errors in this variable, at least exclude extreme values
test11e <- aov(land_area~treatment_act,
              data=only_property_nonexempt_unc[land_area>land_area_05&
                                             land_area<land_area_95,])
summary(test11e)
p_values<-c(p_values,summary(test11e)[[1]][["Pr(>F)"]][[1]])
rm(test11e)

###################################################
# 11) (f) NUMBER OF STORIES DIFFERS BY TREATMENT? #
###################################################

#Given possible coding errors in this variable, at least exclude extreme values
test11f <- aov(stories~treatment_act,
              data=only_property_nonexempt_unc[stories>=stories_05&
                                             stories<stories_95,])
summary(test11f)
p_values<-c(p_values,summary(test11f)[[1]][["Pr(>F)"]][[1]])
rm(test11f)

#################################################
# 11) (g) NUMBER OF ROOMS DIFFERS BY TREATMENT? #
#################################################

#Given possible coding errors in this variable, at least exclude extreme values
test11g <- aov(rooms~treatment_act,
              data=only_property_nonexempt_unc[rooms>=rooms_05&
                                             rooms<rooms_95,])
summary(test11g)
p_values<-c(p_values,summary(test11g)[[1]][["Pr(>F)"]][[1]])
rm(test11g)

###############################################
# 12) (a) CASE ASSIGNMENT DIFFERS BY TREATMENT #
###############################################

test12a<-chisq.test(table(only_property_nonexempt_unc$case_status,
                         only_property_nonexempt_unc$treatment_act))
print(test12a)
p_values<-c(p_values,test12a$p.value)
rm(test12a)

###############################################
# 12) (b) GEOGRAPHIC WARD DIFFERS BY TREATMENT #
###############################################

test12b<-chisq.test(table(only_property_nonexempt_unc$ward_geographic,
                         only_property_nonexempt_unc$treatment_act))
print(test12b)
p_values<-c(p_values,test12b$p.value)
rm(test12b)

################################################
# 12) (c) 5-DIGIT ZIP CODE DIFFERS BY TREATMENT #
################################################
#**Many zip codes highly underrepresented, **
#**so select those w/ >1% representation   **
#**in the population                       **

test12c<-chisq.test(table(as.character(only_property_nonexempt_unc[zip5 %in% common_zips,]$zip5),
                         only_property_nonexempt_unc[zip5 %in% common_zips,]$treatment_act))
print(test12c)
p_values<-c(p_values,test12c$p.value)
rm(test12c,zip_table,common_zips)

#################################################
# 12) (d) PLANNING DISTRICT DIFFERS BY TREATMENT #
#################################################

test12d<-chisq.test(table(only_property_nonexempt_unc$planning_district,
                         only_property_nonexempt_unc$treatment_act))
print(test12d)
p_values<-c(p_values,test12d$p.value)
rm(test12d)

##########################################
# 12) (e) PSEUDOWARD DIFFERS BY TREATMENT #
##########################################

#**Many pseudowards highly underrepresented, **
#**so select those w/ >1% representation     **
#**in the population                         **
test12e<-chisq.test(table(as.character(only_property_nonexempt_unc[ward_pseudo %in% 
                                                                common_pwards,]$ward_pseudo),
                         only_property_nonexempt_unc[ward_pseudo %in% common_pwards,]$treatment_act))
print(test12e)
p_values<-c(p_values,test12e$p.value)
rm(test12e,pward_table,common_pwards)

#########################################
# 12) (f) SALE TYPE DIFFERS BY TREATMENT #
#########################################

#***To avoid cells w/ 0 counts, only include sale types w/ **
#**>5% representation in the population--see above         **

test12f<-chisq.test(table(only_property_nonexempt_unc[sale_type %in% 
                                                   c("","4","B","L","M","Z"),]$sale_type,
                         only_property_nonexempt_unc[sale_type %in% 
                                                   c("","4","B","L","M","Z"),]$treatment_act))
print(test12f)
p_values<-c(p_values,test12f$p.value)
rm(test12f)

#############################################
# 12) (g) CATEGORY CODE DIFFERS BY TREATMENT #
#############################################

test12g<-chisq.test(table(only_property_nonexempt_unc$category,
                         only_property_nonexempt_unc$treatment_act))
print(test12g)
p_values<-c(p_values,test12g$p.value)
rm(test12g)

#########################################
# 12) (h) SALE TYPE DIFFERS BY TREATMENT #
#########################################

#***To avoid cells w/ 0 counts, only include zoning codes w/   **
#**>5% representation in the population. See above.            **

test12h<-chisq.test(table(as.character(only_property_nonexempt_unc[zoning %in% 
                                                                c("RM1","RSA3","RSA5"),]$zoning),
                         only_property_nonexempt_unc[zoning %in% 
                                                   c("RM1","RSA3","RSA5"),]$treatment_act))
print(test12h)
p_values<-c(p_values,test12h$p.value)
rm(test12h)
rm(frontage_05,frontage_95,land_area_05,land_area_95,
   p_exp,rooms_05,rooms_95,stories_05,stories_95)

#Print all p-values in a table here for easy retrieval
print(xtable(matrix(c(NA,p_values[1:3],NA,p_values[4:10],NA,p_values[11:18],
                      NA,p_values[19:21],NA,p_values[22:28],NA,p_values[29:36],
                      rep(NA,3),p_values[37],NA,p_values[38:44],NA,p_values[45:52],
                      NA,p_values[53:55],NA,p_values[56:62],NA,p_values[63:70]),ncol=4,
                    dimnames=list(c("Chi-Squared One-Way Tests","(a) # owners (non-exempt)",
                                    "(b) # owners (encrypted ID)","(c) # owners (NE single property)",
                                    "ANOVA Tests","(a) Balance","(b) Delinquent Balance",
                                    "(c) Market Value","(d) Frontage","(e) Land Area",
                                    "(f) # Stories","(g) # Rooms","Chi-Squared Two-Way Tests",
                                    "(a) Case Status","(b) Geographic Ward","(c) 5-Digit ZIP",
                                    "(d) Planning District","(e) Pseudo-Ward","(f) Sale Type",
                                    "(g) Residential Category","(h) Zoning Code"),
                                  c("Cycle (All)","Int. Treat. (All)",
                                    "Cycle (Mismatched Cycle Ass.)",
                                    "Act. Treat. (High Fidelity)"))),ndigits=2),
      hline.after=c(0,4,12,21))



#Print table of main results for the paper
setkey(only_property_nonexempt,treatment_int)
print.xtable(
  xtable(
    matrix(
      rbind(c(round(only_property_nonexempt[total_balance>0,
                                            mean(total_balance),
                                            by="treatment_int"]$V1,2),
              "ANOVA",round(summary(test5b)[[1]][["Pr(>F)"]][[1]],2)),
            c(round(only_property_nonexempt[,mean(market_value/1e3),
                                            by="treatment_int"]$V1,0),
              "$\\chi^2$",round(test5c2$p.value,2)),
            #square feet calculated in units of 100*ft^2
            c(round(only_property_nonexempt[land_area>land_area_05
                                            &land_area<land_area_95,
                                            mean(land_area/100),
                                            by="treatment_int"]$V1,2),
              "$\\chi^2$",round(test5e2$p.value,2)),
            c(round(only_property_nonexempt[rooms>=20
                                            &rooms<=120,
                                            mean(rooms/10),
                                            by="treatment_int"]$V1,2),
              "ANOVA",round(summary(test5g)[[1]][["Pr(>F)"]][[1]],2)),
            c(rep("",4),"$\\chi^2$",round(test6a$p.value,2)),
            c(rep("",4),"$\\chi^2$",round(test6b$p.value,2)),
            cbind(round(only_property_nonexempt[,prop.table2(category,treatment_int,
                                                             margin=1)
                                                ],2),
                  c("$\\chi^2$",rep("",5)),
                  c(round(test6a$p.value,2),rep("",5))),
            c(round(prop.table2(only_property_nonexempt$treatment_int_name),2),
              "$\\chi^2$",round(test4c$p.value,2)),
            c(round(c(3,4,4,4)/15,2),"","")),ncol=6,
      dimnames=list(c("Postive balance due","Market value ('000)$^{*}$",
                      "Land Area (ft\\textsuperscript{2})$^{*}$","\\# Rooms$^{**}$",
                      "Case Assignment$^{***}$","Political Ward$^{***}$",
                      "Residential","Hotels\\&Apts","Store w. Dwell.",
                      "Commercial","Industrial","Vacant Land",
                      "Distribution of Properties","Expected Distribution"),
                    c("Threat","Moral","Peer","Control","Test","$p$-value")))),
  hline.after=c(-1,0,6,12,13,14),sanitize.text.function=identity,only.contents=T,
  add.to.row=list(pos=list(6),command=c("Category: \\\\ \n")))

#****THE FOLLOWING CODE CAN BE USED TO CREATE MASSIVE TABLES TO HELP PINPOINT THE SOURCES OF NONRANDOMNESS, IF SUSPECTED****
# 
# ##Table 1: average values for amount due & property value, full distributions for 
# ##         case status, geographic ward, zip code, and planning district
# 
# breakout_tables<-matrix(
#   rbind(only_property_nonexempt[,mean(total_balance),by=cycle]$V1,
#         only_property_nonexempt[total_balance>0,mean(total_balance),by=cycle]$V1,
#         only_property_nonexempt[,mean(market_value,na.rm=T),by=cycle]$V1,
#         rep(NA,50),
#         table(only_property_nonexempt$case_status,only_property_nonexempt$cycle),
#         rep(NA,50),
#         table(only_property_nonexempt$ward_geographic,only_property_nonexempt$cycle),
#         rep(NA,50),
#         table(only_property_nonexempt$zip5,only_property_nonexempt$cycle),
#         rep(NA,50),
#         table(only_property_nonexempt$planning_district,only_property_nonexempt$cycle),
#         rep(NA,50),
#         table(only_property_nonexempt$cycle)),
#   ncol=50,dimnames=list(c("Average Balance","Average Balance (among Delinquents)",
#                           "Average Market Value",NA,levels(as.factor(only_property_nonexempt$case_status)),
#                           NA,levels(as.factor(only_property_nonexempt$ward_geographic)),
#                           NA,levels(as.factor(only_property_nonexempt$zip5)),
#                           NA,levels(as.factor(only_property_nonexempt$planning_district)),
#                           NA,"Number of Properties"),paste("Cycle",1:50)))
# 
# write.csv(breakout_tables,file="tables_by_cycle_assignment.csv",quote=F,na="")
#         
# ##Table 2: Median (not average) housing/past due totals, and distributions for exemption codes & type (residential/commercial)
# 
# breakout_tables2.1<-rbind(only_property_nonexempt[total_balance>0,median(total_balance),by=cycle]$V1,
#                           only_property_nonexempt[,median(market_value,na.rm=T),by=cycle]$V1,
#                           rep(NA,50),
#                           table(only_property_nonexempt$exempt_code,only_property_nonexempt$cycle),
#                           rep(NA,50),
#                           table(only_property_nonexempt$category,only_property_nonexempt$cycle),
#                           rep(NA,50),
#                           table(only_property_nonexempt$zoning,only_property_nonexempt$cycle),
#                           rep(NA,50),
#                           table(only_property_nonexempt$exterior_condition,only_property_nonexempt$cycle))
# breakout_tables2.2<-rbind(only_property_nonexempt[total_balance>0,median(total_balance),by=cycle]$V1,
#                           only_property_nonexempt[,median(market_value,na.rm=T),by=cycle]$V1,
#                           rep(NA,50),
#                           table(only_property_nonexempt$exempt_code,only_property_nonexempt$cycle)/
#                             c(table(only_property_nonexempt$cycle)),
#                           rep(NA,50),
#                           table(only_property_nonexempt$category,only_property_nonexempt$cycle)/
#                             c(table(only_property_nonexempt$cycle)),
#                           rep(NA,50),
#                           table(only_property_nonexempt$zoning,only_property_nonexempt$cycle)/
#                             c(table(only_property_nonexempt$cycle)),
#                           rep(NA,50),
#                           table(only_property_nonexempt$exterior_condition,only_property_nonexempt$cycle)/
#                             c(table(only_property_nonexempt$cycle)))
# breakout_tables2.3<-rbind((only_property_nonexempt[total_balance>0,median(total_balance),by=cycle]$V1-
#                              mean(only_property_nonexempt[total_balance>0,median(total_balance),by=cycle]$V1))/
#                             sd(only_property_nonexempt[total_balance>0,median(total_balance),by=cycle]$V1),
#                           (only_property_nonexempt[,median(market_value,na.rm=T),by=cycle]$V1-
#                              mean(only_property_nonexempt[,median(market_value,na.rm=T),by=cycle]$V1))/
#                             sd(only_property_nonexempt[,median(market_value,na.rm=T),by=cycle]$V1),
#                           rep(NA,50),
#                           (table(only_property_nonexempt$exempt_code,only_property_nonexempt$cycle)/
#                              c(table(only_property_nonexempt$cycle))
#                            -rowMeans(table(only_property_nonexempt$exempt_code,only_property_nonexempt$cycle)/
#                                        c(table(only_property_nonexempt$cycle))))/
#                             apply(table(only_property_nonexempt$exempt_code,only_property_nonexempt$cycle)/
#                                     c(table(only_property_nonexempt$cycle)),1,sd),
#                           rep(NA,50),
#                           (table(only_property_nonexempt$category,only_property_nonexempt$cycle)/
#                              c(table(only_property_nonexempt$cycle))
#                            -rowMeans(table(only_property_nonexempt$category,only_property_nonexempt$cycle)/
#                                        c(table(only_property_nonexempt$cycle))))/
#                             apply(table(only_property_nonexempt$category,only_property_nonexempt$cycle)/
#                                     c(table(only_property_nonexempt$cycle)),1,sd),
#                           rep(NA,50),
#                           (table(only_property_nonexempt$zoning,only_property_nonexempt$cycle)/
#                              c(table(only_property_nonexempt$cycle))
#                            -rowMeans(table(only_property_nonexempt$zoning,only_property_nonexempt$cycle)/
#                                        c(table(only_property_nonexempt$cycle))))/
#                             apply(table(only_property_nonexempt$zoning,only_property_nonexempt$cycle)/
#                                     c(table(only_property_nonexempt$cycle)),1,sd),
#                           rep(NA,50),
#                           (table(only_property_nonexempt$exterior_condition,only_property_nonexempt$cycle)/
#                              c(table(only_property_nonexempt$cycle))
#                            -rowMeans(table(only_property_nonexempt$exterior_condition,only_property_nonexempt$cycle)/
#                                        c(table(only_property_nonexempt$cycle))))/
#                             apply(table(only_property_nonexempt$exterior_condition,only_property_nonexempt$cycle)/
#                                     c(table(only_property_nonexempt$cycle)),1,sd))
# 
# breakout_tables2<-matrix(round(cbind(breakout_tables2.1,rep(NA,nrow(breakout_tables2.3)),
#                                      breakout_tables2.2,rep(NA,nrow(breakout_tables2.3)),
#                                      breakout_tables2.3),digits=2),
#                          ncol=3*50+2,
#                          dimnames=list(c("Median Balance (among Delinquents)","Median Market Value",
#                                          "EXEMPT CODE",levels(as.factor(only_property_nonexempt$exempt_code)),
#                                          "CATEGORY","Residential","Hotels & Apts","Store w/ Dwelling",
#                                          "Commercial","Industrial","Vacant Land",
#                                          "ZONING",levels(as.factor(only_property_nonexempt$zoning)),
#                                          "EXTERIOR CONDITION","N/A","New Constr./Rehab.",
#                                          "Above Average","Average","Below Average","Vacant","Sealed"),
#                                        c(rep(c(paste(1:50),NA),2),paste(1:50))))
# 
# write.csv(breakout_tables2,file="tables_by_cycle_assignment2.csv",quote=F,na="")
# 
# #Create counts/percentages files for mapping
# 
# ##PLANNING DISTRICTS
# only_property_nonexempt[,planning_district:as.factor(planning_district)]
# only_property_nonexempt[planning_district=="Upper NW",pd_id:="1"]
# only_property_nonexempt[planning_district=="Upper North",pd_id:="2"]
# only_property_nonexempt[planning_district=="West",pd_id:="3"]
# only_property_nonexempt[planning_district=="Upper Far NE",pd_id:="4"]
# only_property_nonexempt[planning_district=="Lower SW",pd_id:="5"]
# only_property_nonexempt[planning_district=="West Park",pd_id:="6"]
# only_property_nonexempt[planning_district=="Lower N",pd_id:="7"]
# only_property_nonexempt[planning_district=="River Wards",pd_id:="8"]
# only_property_nonexempt[planning_district=="North",pd_id:="9"]
# only_property_nonexempt[planning_district=="Lower NW",pd_id:="10"]
# only_property_nonexempt[planning_district=="Lower South",pd_id:="11"]
# only_property_nonexempt[planning_district=="Lower NE",pd_id:="12"]
# only_property_nonexempt[planning_district=="Central NE",pd_id:="13"]
# only_property_nonexempt[planning_district=="N Delaware",pd_id:="14"]
# only_property_nonexempt[planning_district=="Lower Far NE",pd_id:="16"]
# only_property_nonexempt[planning_district=="South",pd_id:="17"]
# only_property_nonexempt[planning_district=="Central",pd_id:="18"]
# only_property_nonexempt[planning_district=="University/SW",pd_id:="19"]
# 
# setkey(only_property_nonexempt,pd_id)
# pd_table<-matrix(cbind(only_property_nonexempt[cycle==23,.N,by=pd_id]$V1,
#                        only_property_nonexempt[cycle==28,.N,by=pd_id]$V1,
#                        only_property_nonexempt[!(cycle %in% c(23,28)),.N,by=pd_id]$V1,
#                        only_property_nonexempt[cycle==23,.N,by=pd_id]$V1/
#                          nrow(only_property_nonexempt[cycle==23,]),
#                        only_property_nonexempt[cycle==28,.N,by=pd_id]$V1/
#                          nrow(only_property_nonexempt[cycle==28,]),
#                        only_property_nonexempt[!(cycle %in% c(23,28)),.N,by=pd_id]$V1/
#                          nrow(only_property_nonexempt[!(cycle %in% c(23,28)),]),
#                        only_property_nonexempt[,mean(delinquent_ind),by=pd_id]$V1),ncol=7,
#                  dimnames=list(c(only_property_nonexempt[cycle==23,.N,by=pd_id]$pd_id),
#                                c("Count_23","Count_28","Count_REST","Pct_23","Pct_28","Pct_REST","Pct_del")))
# 
# write.csv(pd_table,file="cycle_counts_freq_by_planning_district.csv",quote=F)
# 
# ##GEOGRAPHIC WARDS
# 
# setkey(only_property_nonexempt,ward_geographic)
# ward_table<-matrix(cbind(only_property_nonexempt[cycle==23,.N,by=ward_geographic]$V1,
#                          only_property_nonexempt[cycle==28,.N,by=ward_geographic]$V1,
#                          only_property_nonexempt[!(cycle %in% c(23,28)),.N,by=ward_geographic]$V1,
#                          only_property_nonexempt[cycle==23,.N,by=ward_geographic]$V1/
#                            nrow(only_property_nonexempt[cycle==23,]),
#                          only_property_nonexempt[cycle==28,.N,by=ward_geographic]$V1/
#                            nrow(only_property_nonexempt[cycle==28,]),
#                          only_property_nonexempt[!(cycle %in% c(23,28)),.N,by=ward_geographic]$V1/
#                            nrow(only_property_nonexempt[!(cycle %in% c(23,28)),]),
#                          only_property_nonexempt[,mean(delinquent_ind),by=ward_geographic]$V1),ncol=7,
#                    dimnames=list(c(only_property_nonexempt[cycle==23,.N,by=ward_geographic]$ward_geographic),
#                                  c("Count_23","Count_28","Count_REST","Pct_23","Pct_28","Pct_REST","Pct_del")))
# 
# write.csv(ward_table,file="cycle_counts_freq_by_geog_ward.csv",quote=F)
# 
# ##ZIP CODES
# 
# setkey(only_property_nonexempt,zip5)
# zip_table<-matrix(cbind(only_property_nonexempt[cycle==23,.N,by=zip5]$V1,
#                          only_property_nonexempt[cycle==28,.N,by=zip5]$V1,
#                          only_property_nonexempt[!(cycle %in% c(23,28)),.N,by=zip5]$V1,
#                          only_property_nonexempt[cycle==23,.N,by=zip5]$V1/
#                           nrow(only_property_nonexempt[cycle==23,]),
#                          only_property_nonexempt[cycle==28,.N,by=zip5]$V1/
#                           nrow(only_property_nonexempt[cycle==28,]),
#                          only_property_nonexempt[!(cycle %in% c(23,28)),.N,by=zip5]$V1/
#                            nrow(only_property_nonexempt[!(cycle %in% c(23,28)),])),ncol=6,
#                    dimnames=list(c(only_property_nonexempt[cycle==23,.N,by=zip5]$zip5),
#                                  c("Count_23","Count_28","Count_REST","Pct_23","Pct_28","Pct_REST")))
# 
# write.csv(zip_table,file="cycle_counts_freq_by_zip.csv",quote=F)
# 
# ##Some probits
# probit1<-glm(delinquent_ind~as.factor(cycle),family=binomial(link="probit"),data=only_property_nonexempt)
# probit2<-glm(delinquent_ind~ward_geographic+as.factor(cycle),family=binomial(link="probit"),data=only_property_nonexempt)
# probit3<-glm(delinquent_ind~planning_district+as.factor(cycle),family=binomial(link="probit"),data=only_property_nonexempt)
# probit4<-glm(delinquent_ind~market_value+as.factor(cycle),family=binomial(link="probit"),data=only_property_nonexempt)
# 
# rm(probit1,probit2,probit3,probit4)
# 
# probit5<-glm(delinquent_ind~market_value+planning_district+ward_geographic+as.factor(cycle),
#              family=binomial(link="probit"),data=only_property_nonexempt)
# 
# rm(probit5)
# 
# agg_mean<-mean(only_property_nonexempt$delinquent_ind)
# only_property_nonexempt[,high_del_dist:=mean(delinquent_ind)>agg_mean,by=planning_district]
# 
# probit6<-glm(delinquent_ind~as.factor(cycle)+high_del_dist+high_del_dist*as.factor(cycle),
#              family=binomial(link="probit"),data=only_property_nonexempt)
# 
# rm(probit6)