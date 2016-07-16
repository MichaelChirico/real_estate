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
wds <- c(data = "/media/data_drive/real_estate/",
         proj = mn %+% "round_two/", 
         log = mn %+% "logs/round_two/")
write.packages(wds["log"] %+% "cleaning_session.txt")

#Specifying order/reference treatments for
#  the 7- and 8-treatment groups
#  (vs. Control and Holdout, resp.)
trt.nms <- c("Control", "Amenities", "Moral", 
             "Duty", "Peer", "Lien", "Sheriff")
trt.nms8 <- c("Holdout", trt.nms)

#Data Import ####
##Block I: Main Sample, July Cross-Section
## * total_paid is the accrual between June 1, 2015 and July 22, 2015
main_jul <- read.xlsx3(
  wds["data"] %+% "Payments and Balance Penn Letter Experiment_150727.xlsx",
  colIndex = c(2L, 5L, 8L, 9L, 15L),
  sheetName = "DETAILS", header = TRUE, startRow = 9L,
  stringsAsFactors = FALSE, colClasses = abbr_to_colClass("cn", "41"))

setDT(main_jul)

setnames(main_jul, c("opa_no", "treat15", "paid_full_jul", 
                     "ever_paid_jul", "total_paid_jul"))

##Block II: Holdout Sample, July Cross-Section
holdout_jul <- read.xlsx3(
  wds["data"] %+% "req20150709_PennLetterExperiment_"%+%
    "v2_Commissioners Control Details.xlsx",
  colIndex = c(2L, 8L, 9L, 14L),
  sheetName = "DETAILS", header = TRUE, startRow = 9L,
  stringsAsFactors = FALSE, colClasses = abbr_to_colClass("cn", "31"))

setDT(holdout_jul)

setnames(holdout_jul, 
         c("opa_no", "paid_full_jul", "ever_paid_jul", "total_paid_jul"))

holdout_jul[ , treat15 := "Holdout"]

###The following account changed OPA # between samples,
###  See correspondence with Darryl Watson dated:
###    Tue, Sep 15, 2015 at 4:09 PM
###  (should also be able to corroborate with Account ID)

update_opas <- data.table(old = "057027304",
                          new = "057026500")
holdout_jul[update_opas, opa_no := i.old, on = c(opa_no = "new")]

##Block III: Full Sample, September Cross-Section
full_sep <- read_excel(
  wds["data"] %+%
    "req20150709_PennLetterExperiment (September 2015 update) v2.xlsx",
  ##** NOTE: I'm using my own branch of readxl here which
  ##   supports multiple NA values; installed via
  ##   devtools::install_github("MichaelChirico/readxl@multiple_na") **
  sheet = "DETAILS", skip = 7L, na = c("NULL", "-"),
  col_names = c("x", "opa_no", rep("x", 9L),
                "paid_full_sep", "ever_paid_sep", rep("x", 6L),
                "total_paid_sep", rep("x", 5L)),
  col_types = abbr_to_colClass("btbtbnb", "1192615"))

setDT(full_sep)

###  **TO DO: INSERT E-MAIL META DATA FROM DOR CONFIRMATION**
update_opas <- data.table(old = c("151102600", "884350465"),
                          new = c("151102610", "881577275"))
full_sep[update_opas, opa_no := i.old, on = c("opa_no" = "new")]

##Block IV: Full Sample, December Cross-Section
full_dec <- read_excel(
  wds["data"] %+% "req20150709_PennLetterExperiment (December 2015 update) v2.xlsx",
  #See above about length-2 NA
  sheet = "DETAILS", skip = 7L, na = c("NULL", "-"),
  col_names = c("account", "opa_no" , rep("x", 9L),
              "paid_full_dec", "ever_paid_dec", 
              rep("x", 6L), "total_paid_dec", rep("x", 5L)),
  col_types = abbr_to_colClass("tbtbnb", "292615"))

setDT(full_dec)

###  **TO DO: INSERT E-MAIL META DATA FROM DOR CONFIRMATION**
update_opas <-
  data.table(old = c("151102600", "884350465", "882932475", "213155700"),
             new = c("151102610", "881577275", "882932476", "881081460"))
full_dec[update_opas, opa_no := i.old, on = c("opa_no" = "new")]

##Block V: Main Sample Background Data
## * total_due is pre-study balance
main_bg <- fread(wds["proj"] %+% "round_2_full_data.csv", 
                 select = c("opa_no", "owner1", "total_due", "rand_id"))

##Block VI: Holdout Sample Background Data
holdout_bg <- fread(wds["proj"] %+% "holdout_sample.csv",
                    select = c("opa_no", "owner1", "total_due"))

###Block VII: Other Background Data
###  From OPA-issued Property Data CD
###  (certified for 2015, received May 2014)
opa_bg <- fread(wds["data"] %+% "prop2015.txt", select = c("PARCEL", "MV"))
setnames(opa_bg, c("opa_no", "assessed_mv"))

##Quilting time!
### Framework:
###  I  III IV V  VII
###  II III IV VI VII
### (blocks stacked vertically will be stitched/appended,
###  and each column involves a merge to the previous column)
bgvars <- c("opa_no", "owner1", "total_due", "rand_id")
properties <-
  rbind(main_jul, holdout_jul
        )[full_sep, on = "opa_no"
          ][full_dec, on = "opa_no"
            #Need fill since holdout_bg lacks rand_id
            ][rbind(main_bg, holdout_bg, fill = TRUE),
              (bgvars) := mget("i." %+% bgvars), on = "opa_no"
              #one of the Excel files is funky and the reader
              #  added a phantom row; this removes that
              ][!is.na(treat15)]

properties[opa_bg, assessed_mv := as.numeric(i.assessed_mv), on = "opa_no"]

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
pf <- "paid_full_" %+% c("jul", "sep", "dec")
properties[ , (pf) := lapply(.SD, `!`), .SDcols = pf]

##Define some flags
### Is this a holdout property?
properties[ , holdout := treat15 == "Holdout"]
### Was this owner in both the holdout sample and the treatment panel?
properties[ , flag_holdout_overlap := any(treat15 == "Holdout") &&
              any(treat15 != "Holdout"), by = owner1]

properties[ , treat8 := factor(gsub("_.*", "", treat15), trt.nms8)]
properties[(!holdout), treat7 := factor(treat8)]

###Get owner-level version of data, keeping only key analysis variables
owners <- 
  #Sort by treat7 within owner1 so that Holdout properties sink to
  #  the bottom (forcing the treatment associated with each
  #  owner to be that of their regular treatment group, if any)
  properties[order(treat7),
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
               total_due = sum(total_due),
               assessed_mv = sum(assessed_mv),
               flag_holdout_overlap =
                 flag_holdout_overlap[1L], .N),
             by = owner1]

###  Subsample Flags
owners[ , unq_own := N == 1]

### Write output
fwrite(owners, wds["data"] %+% "round_two_analysis_owners.csv", quote = TRUE)
fwrite(properties, wds["data"] %+% "round_two_analysis_properties.csv", quote = TRUE)
