#Data Cleaning
#Philadelphia Real Estate Tax Evasion
#  Round 2
#Michael Chirico

##Packages
rm(list=ls(all=T))
gc()
#Michael Chirico's function of convenience packages;
#  install via devtools::install_github("MichaelChirico/funchir")
library(funchir)
library(data.table)
library(readxl)
library(xlsx)
setwd(mn <- "~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/")
wds<-c(data = (dwd <- "/media/data_drive/") %+% "real_estate/",
       proj = mn %+% "round_two/", log = mn %+% "logs/round_two/",
       imga = mn %+% "round_two/images/analysis/",
       imgb = mn %+% "round_two/images/balance/",
       gis = dwd %+% "gis_data/PA/",
       sher = dwd %+% "real_estate/sheriffs_sales/",
       cens = dwd %+% "census/")
write.packages(wds["log"] %+% "cleaning_session.txt")

#"metavariables"
trt.nms <- c("Control", "Amenities", "Moral", 
             "Duty", "Peer", "Lien", "Sheriff")
trt.nms8 <- c("Holdout", trt.nms)

mos <- c("jul", "sep", "dec")

#Data Import ####
##Block I: Main Sample
### total_due is pre-study balance
### current_balance is as of July 22, 2015
### total_paid is the accrual between June 1, 2015
###   and July 22, 2015
mainI <- read.xlsx3(
  wds["data"] %+% "Payments and Balance Penn Letter Experiment_150727.xlsx",
  colIndex = c(2L, 5L, 8L, 9L, 13L:15L),
  sheetName = "DETAILS", header = TRUE, startRow = 9L,
  stringsAsFactors = FALSE, colClasses = abbr_to_colClass("cnDn", "4111"))

setnames(setDT(mainI),
         c("opa_no", "treat15", "paid_full_jul", "ever_paid_jul",
           "current_balance_jul", "earliest_pmt_jul", "total_paid_jul"))

##Block II: Holdout Sample
holdoutII <- read.xlsx3(
  wds["data"] %+% "req20150709_PennLetterExperiment_"%+%
    "v2_Commissioners Control Details.xlsx",
  colIndex = c(2L, 8L, 9L, 12L:14L),
  sheetName = "DETAILS", header = TRUE, startRow = 9L,
  stringsAsFactors = FALSE, colClasses = abbr_to_colClass("cnDn", "3111"))

setnames(setDT(holdoutII),
         c("opa_no", "paid_full_jul", "ever_paid_jul",
           "current_balance_jul", "earliest_pmt_jul", "total_paid_jul"))

holdoutII[ , treat15 := "Holdout"]

###The following account changed OPA # between samples,
###  See correspondence with Darryl Watson dated:
###    Tue, Sep 15, 2015 at 4:09 PM
###  (should also be able to corroborate with Account ID)

update_opas <- data.table(old = "057027304",
                          new = "057026500")
holdoutII[update_opas, opa_no := i.old, on = c("opa_no" = "new")]

##Block III: Follow-up Sample (September 2015)
followupIII <- read_excel(
  wds["data"] %+%
    "req20150709_PennLetterExperiment (September 2015 update) v2.xlsx",
  ##** NOTE: I'm using my own branch of readxl here which
  ##   supports multiple NA values; installed via
  ##   devtools::install_github("MichaelChirico/readxl@multiple_na") **
  sheet = "DETAILS", skip = 7L, na = c("NULL", "-"),
  col_names = c("x", "opa_no", "x", "x", "latitude", "longitude",
              rep("x", 5L), "paid_full_sep", "ever_paid_sep", "pmt_agr_sep",
              rep("x", 3L), "current_balance_sep", "earliest_pmt_sep",
              "total_paid_sep", rep("x", 5L)),
  col_types = abbr_to_colClass("btbnbtbndnb", "11225331115"))

setDT(followupIII)

followupIII[ , earliest_pmt_sep := as.Date(earliest_pmt_sep)]

###The following accounts changed OPA #s between samples,
###  See correspondence with Darryl Watson dated:
###    Tue, Sep 15, 2015 at 4:09 PM
###  (should also be able to corroborate with Account ID)
update_opas <- data.table(old = c("151102600", "884350465"),
                          new = c("151102610", "881577275"))
followupIII[update_opas, opa_no := i.old, on = c("opa_no" = "new")]

##Block IV: Follow-up Sample (December 2015)
followupIV <- read_excel(
  wds["data"] %+% "req20150709_PennLetterExperiment (December 2015 update) v2.xlsx",
  #See above about length-2 NA
  sheet = "DETAILS", skip = 7L, na = c("NULL", "-"),
  col_names = c("account", "opa_no" , rep("x", 9L),
              "paid_full_dec", "ever_paid_dec", "pmt_agr_dec",
              rep("x", 3L), "current_balance_dec", "earliest_pmt_dec",
              "total_paid_dec", rep("x", 5L)),
  col_types = abbr_to_colClass("tbtbnb", "293335"))

setDT(followupIV)

followupIV[ , earliest_pmt_dec :=
              as.Date(earliest_pmt_dec, origin = D("1899-12-30"))]

###In addition to the two from the three-month sample,
###  two more properties have updated OPAs.
###  **TO DO: INSERT E-MAIL META DATA FROM DOR CONFIRMATION**
update_opas <-
  data.table(old = c("151102600", "884350465", "882932475", "213155700"),
             new = c("151102610", "881577275", "882932476", "881081460"))
followupIV[update_opas, opa_no := i.old, on = c("opa_no" = "new")]

##Block V: Main Sample Background Data
mainBGV <- fread(wds["proj"] %+% "round_2_full_data.csv", drop = "treatment")

##Block VI: Holdout Sample Background Data
holdBGVI <- fread(wds["proj"] %+% "holdout_sample.csv")

###Block VII: Other Background Data
###  From OPA-issued Property Data CD
###  (certified for 2015, received May 2014)
opabgVII <- fread(wds["data"] %+% "prop2015.txt", select=c("PARCEL", "MV"))
setnames(opabgVII, c("opa_no","assessed_mv"))

##Quilting time!
### Framework:
###  I  III IV V  VII
###  II III IV VI VII
bgvars <- names(mainBGV) %\% "opa_no"

properties <-
  rbind(mainI, holdoutII
        )[followupIII, on = "opa_no"
          ][followupIV, on = "opa_no"
            ][rbind(mainBGV, holdBGVI, fill = TRUE),
              (bgvars) := mget("i." %+% bgvars), on = "opa_no"
              ][!is.na(treat15)]

properties[opabgVII, assessed_mv := as.numeric(i.assessed_mv), on = "opa_no"]

##Data Clean-up
###Account ID with extra whitespace
properties[ , account := gsub("\\s", "", account)]
###Re-set indicators as T/F instead of Y/N
inds <- c("paid_full_jul", "ever_paid_jul",
          "paid_full_sep", "ever_paid_sep",
          "paid_full_dec", "ever_paid_dec")
properties[ , (inds) := lapply(.SD, function(x) x == "Y"), .SDcols = inds]
###Paid Full actually stored opposite because 
###  question in data is: "Does property have a balance?"
pf <- "paid_full_" %+% mos
properties[ , (pf) := lapply(.SD, `!`), .SDcols = pf]

##Define some flags
### Is this a holdout property?
properties[ , holdout := treat15 == "Holdout"]
### Did this owner receive more than one letter?
properties[ , flag_multiple_property := .N > 1L, by = owner1]
### Was this owner in both the holdout sample and the treatment panel?
properties[ , flag_holdout_overlap := any(treat15 == "Holdout") &&
              any(treat15 != "Holdout"), by = owner1]

properties[(!holdout), treat14 := gsub("\\sE.*", "", gsub("_", " ", treat15))]
properties[ , treat14 := 
              factor(treat14, 
                     paste(rep(trt.nms, each = 2L), c("Small","Big")))]

properties[ , treat8 := factor(gsub("_.*", "", treat15), trt.nms8)]
properties[(!holdout), treat7 := factor(treat8)]

###Get owner-level version of data, keeping only key analysis variables

minDate <- function(x) if (all(is.na(x))) as.Date(NA) else min(x)

owners<-{
  properties[order(treat14),
             .(treat7 = treat7[1L],
               treat8 = treat8[1L],
               rand_id = rand_id[1L],
               holdout = all(holdout),
               ever_paid_jul = any(ever_paid_jul),
               ever_paid_sep = any(ever_paid_sep),
               ever_paid_dec = any(ever_paid_dec),
               paid_full_jul = all(paid_full_jul),
               paid_full_sep = all(paid_full_sep),
               paid_full_dec = all(paid_full_dec),
               total_paid_jul = sum(total_paid_jul),
               total_paid_sep = sum(total_paid_sep),
               total_paid_dec = sum(total_paid_dec),
               earliest_pmt_jul = minDate(earliest_pmt_jul),
               earliest_pmt_sep = minDate(earliest_pmt_sep),
               earliest_pmt_dec = minDate(earliest_pmt_dec),
               total_due = sum(total_due),
               assessed_mv = sum(assessed_mv),
               flag_holdout_overlap =
                 flag_holdout_overlap[1L], .N),
             by = owner1]}

###  Subsample Flags
owners[ , unq_own := N == 1]

### Write output
fwrite(owners, wds["data"] %+% "round_two_analysis_owners.csv", quote = TRUE)
fwrite(properties, wds["data"] %+% "round_two_analysis_properties.csv", quote = TRUE)
