#Data Cleaning
#Philadelphia Real Estate Tax Evasion
#  Round 2
#Michael Chirico

##Packages
rm(list=ls(all = TRUE))
gc()
#Michael Chirico's function of convenience packages;
#  install via devtools::install_github("MichaelChirico/funchir")
library(funchir)
#Version 1.9.7+ required for access to the fwrite function
#  (speedy csv writing); for compatibility with data.table
#  versions <=1.9.6, replace fwrite with write.csv from base R.
#  To install the development version, run:
#  install.packages("data.table", type = "source",
#                   repos = "http://Rdatatable.github.io/data.table")
#  If this doesn't work right away, follow the additional
#  instructions here: https://github.com/Rdatatable/data.table/wiki/Installation
library(data.table)
library(readxl)
library(xlsx)
setwd(mn <- "~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/")
wds <- c(data = "/media/data_drive/real_estate/",
         proj = mn %+% "round_two/", 
         log = mn %+% "logs/round_two/")
write.packages(wds["log"] %+% "cleaning_session.txt")

#Data Import ####
##Block I: Main Sample, July Cross-Section
## * total_paid is the accrual between June 1, 2015 and July 22, 2015
main_jul <- read.xlsx3(
  wds["data"] %+% "Payments and Balance Penn Letter Experiment_150727.xlsx",
  colIndex = c(2L, 5L, 8L, 9L), sheetName = "DETAILS", 
  header = TRUE, startRow = 9L, stringsAsFactors = FALSE, 
  colClasses = "character")

setDT(main_jul)

setnames(main_jul, c("opa_no", "treat15", "paid_full_jul", "ever_paid_jul"))

##Block II: Holdout Sample, July Cross-Section
holdout_jul <- read.xlsx3(
  wds["data"] %+% "req20150709_PennLetterExperiment_"%+%
    "v2_Commissioners Control Details.xlsx",
  colIndex = c(2L, 8L, 9L), sheetName = "DETAILS",
  header = TRUE, startRow = 9L, stringsAsFactors = FALSE, 
  colClasses = "character")

setDT(holdout_jul)

setnames(holdout_jul, c("opa_no", "paid_full_jul", "ever_paid_jul"))

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
  sheet = "DETAILS", skip = 7L, na = "-",
  col_names = c("x", "opa_no", rep("x", 9L), "paid_full_sep",
                "ever_paid_sep", rep("x", 12L)),
  col_types = abbr_to_colClass("btbtbb", "119293"))

setDT(full_sep)

###  **TO DO: INSERT E-MAIL META DATA FROM DOR CONFIRMATION**
update_opas <- data.table(old = c("151102600", "884350465"),
                          new = c("151102610", "881577275"))
full_sep[update_opas, opa_no := i.old, on = c(opa_no = "new")]

##Block IV: Full Sample, December Cross-Section
full_dec <- read_excel(
  wds["data"] %+% 
    "req20150709_PennLetterExperiment (December 2015 update) v2.xlsx",
  sheet = "DETAILS", skip = 7L, na = "-",
  col_names = c("account", "opa_no" , rep("x", 9L),
              "paid_full_dec", "ever_paid_dec", 
              rep("x", 5L), "earliest_pmt_dec",
              "total_paid_dec", rep("x", 5L)),
  col_types = abbr_to_colClass("tbtbdnb", "2925115"))

setDT(full_dec)

full_dec[ , earliest_pmt_dec := as.Date(earliest_pmt_dec)]

###  **TO DO: INSERT E-MAIL META DATA FROM DOR CONFIRMATION**
update_opas <-
  data.table(old = c("151102600", "884350465", "882932475", "213155700"),
             new = c("151102610", "881577275", "882932476", "881081460"))
full_dec[update_opas, opa_no := i.old, on = c(opa_no = "new")]

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

###Block VIII: One-Year Follow-Up Data
followup <- read_excel(
  wds["data"] %+%
    "req20150709_PennLetterExperiment (July 2016 update with 2016 " %+% 
    "delinquency, payments, and agreements).xlsx",
  sheet = "DETAILS", skip = 1L, na = "-",
  col_names = c("account", rep("x", 6L), 
                "total_bill_2016", "x", "paid_full_jul16",
                "ever_paid_jul16", rep("x", 6L),
                "earliest_pmt_jul16", rep("x", 6L)),
  col_types = abbr_to_colClass("tbtbtbnb",
                               "16112616"))

setDT(followup)

followup[ , earliest_pmt_jul16 := 
            as.Date(earliest_pmt_jul16, origin = D("1899-12-30"))]

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

#7 properties were dissolved between 2015 & 2016; exclude those
properties[followup[total_bill_2016 != "Consolidation/Subdivision"], 
           `:=`(ever_paid_jul16 = i.ever_paid_jul16,
                paid_full_jul16 = i.paid_full_jul16,
                earliest_pmt_jul16 = i.earliest_pmt_jul16), on = "account"]

##Data Clean-up
###Account ID with extra whitespace
properties[ , account := gsub("\\s", "", account)]
###Re-set indicators as T/F instead of Y/N
ep <- "ever_paid_" %+% c("jul", "sep", "dec", "jul16")
properties[ , (ep) := lapply(.SD, `==`, "Y"), .SDcols = ep]
###Paid Full actually stored opposite because 
###  question in data is: "Does this property have a balance?"
pf <- "paid_full_" %+% c("jul", "sep", "dec", "jul16")
properties[ , (pf) := lapply(.SD, `==`, "N"), .SDcols = pf]

##Define some flags
### Is this a holdout property?
properties[ , holdout := treat15 == "Holdout"]
### Was this owner in both the holdout sample and the treatment panel?
properties[ , flag_holdout_overlap := any(treat15 == "Holdout") &&
              any(treat15 != "Holdout"), by = owner1]

rename <-
  as.list(setNames(c("Holdout", "Control", "Amenities", "Moral", 
                     "Duty", "Peer", "Lien", "Sheriff"),
                   c("Holdout", "Control", "Neighborhood", "Community", 
                     "Duty", "Peer", "Lien", "Sheriff")))

properties[ , treat8 := factor(gsub("_.*", "", treat15), 
                               unlist(rename))]
levels(properties$treat8) <- rename

properties[(!holdout), treat7 := factor(treat8)]

###Get owner-level version of data, keeping only key analysis variables

####Define a randomized anonymous ID for each owner
####  to facilitate public dissemination of results & data
#####Pulled from random.org Sep. 2, 2016 @ 11:38 AM EST
set.seed(65976082)
properties[sample(.N), owner_id := .GRP, by = owner1]

###Write out owner1-owner_id linkage
fwrite(unique(properties[ , .(owner1, owner_id, opa_no)]),
       wds["data"] %+% "round_two_anon_id_link.csv", quote = TRUE)

properties[ , owner1 := NULL]
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
               ever_paid_jul16 = any(ever_paid_jul16),
               paid_full_jul = all(paid_full_jul),
               paid_full_sep = all(paid_full_sep),
               paid_full_dec = all(paid_full_dec),
               paid_full_jul16 = all(paid_full_jul16),
               total_paid_dec = sum(total_paid_dec),
               earliest_pmt_dec = {
                 if (all(is.na(earliest_pmt_dec))) D(NA)
                 else min(earliest_pmt_dec, na.rm = TRUE)},
               earliest_pmt_jul16 = {
                 if (all(is.na(earliest_pmt_jul16))) D(NA)
                 else min(earliest_pmt_jul16, na.rm = TRUE)},
               total_due = sum(total_due),
               assessed_mv = sum(assessed_mv),
               flag_holdout_overlap =
                 flag_holdout_overlap[1L], .N),
             by = owner_id]

###  Subsample Flags
owners[ , unq_own := N == 1]

### Write output
fwrite(owners, wds["data"] %+% "round_two_analysis_owners.csv", quote = TRUE)
