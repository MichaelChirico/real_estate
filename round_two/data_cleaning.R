#Data Cleaning
#Philadelphia Real Estate Tax Evasion
#  Round 2
#Michael Chirico

##Packages
rm(list=ls(all = TRUE))
gc()
library(funchir)
library(data.table)
library(readxl)
library(rgdal)
library(rgeos)
library(sp)
library(maptools)
library(spatstat)
library(splancs)
setwd(mn <- "~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/")
wds <- c(data = "/media/data_drive/real_estate/",
         proj = mn %+% "round_two/", 
         log = mn %+% "logs/round_two/")
write.packages(wds["log"] %+% "cleaning_session.txt")

#Data Import ####
##Block I: Main Sample, July Cross-Section
## * total_paid is the accrual between June 1, 2015 and July 22, 2015

main_jul = read_excel(
  wds["data"] %+% "Payments and Balance Penn Letter Experiment_150727.xlsx",
  sheet = 'DETAILS', skip = 9L, na = "-", 
  col_names = c('x', 'opa_no', 'x', 'x', 'treat15', 'x', 
                'x', 'paid_full_jul', 'ever_paid_jul', rep('x', 6L)),
  col_types = abbr_to_colClass('stststs', '1121226'))

setDT(main_jul)

##Block II: Holdout Sample, July Cross-Section
holdout_jul = read_excel(
  wds["data"] %+% "req20150709_PennLetterExperiment_"%+%
    "v2_Commissioners Control Details.xlsx",
  sheet = 'DETAILS', skip = 9L, na = "-", 
  col_names = c('x', 'opa_no', rep('x', 5L), 
                'paid_full_jul', 'ever_paid_jul', rep('x', 5L)),
  col_types = abbr_to_colClass('ststs', '11525'))

setDT(holdout_jul)

holdout_jul[ , treat15 := "Holdout"]

###The following account changed OPA # between samples,
###  See correspondence with Darryl Watson dated:
###    Tue, Sep 15, 2015 at 4:09 PM
###  (should also be able to corroborate with Account ID)

update_opas <- data.table(old = "057027304",
                          new = "057026500")
holdout_jul[update_opas, opa_no := i.old, on = c(opa_no = "new")]

full_jul = rbind(main_jul, holdout_jul)

##Block III: Full Sample, September Cross-Section
full_sep = read_excel(
  wds["data"] %+%
    "req20150709_PennLetterExperiment (September 2015 update) v2.xlsx",
  sheet = "DETAILS", skip = 8L, na = c('-', 'NULL'),
  col_names = c(`ACCOUNT-ID` = "x", `BRT NUMBER` = "opa_no", 
                `PROP ADDR` = "address", `PZIP5` = "x", 
                `Y_LAT` = 'y', `X_LONG` = 'x', 
                `TREATMENT` = 'x', `ENVELOPE TYPE` = 'x', `MESSAGE TYPE` = 'x',
                `TOTAL DUE (MAY 2015)` = 'x', `PRE 2015 BALANCE` = 'x',
                `CURRENT AMOUNT OWED` = "paid_full_sep",
                `PAYMENT(S) MADE` = "ever_paid_sep", `AGREEMENT` = 'x',
                `# YEARS OWED` = 'x', `EARLIEST YEAR OWED` = 'x',
                `LATEST YEAR OWED` = 'x', `TOTAL DUE (SEPTEMBER 2015)` = 'x',
                `EARLIEST PAYMENT` = 'x', `TOTAL PAID` = 'total_paid_sep',
                `AGREEMENT TYPE` = 'x', `AGREEMENT STATUS` = 'x',
                `AGREEMENT START DATE` = 'x',
                `AGREEMENT AMOUNT` = 'x', `ROW` = 'x'),
  col_types = abbr_to_colClass("stsnstsns", "121252615"))

setDT(full_sep)

### join missing lat/lon geocoded in geocode_missing.R
extra_xy = fread(wds['proj'] %+% 'geocoded_missing.csv',
                 colClasses = list(character = 'opa_no'))
full_sep[extra_xy, c('x', 'y') := .(i.x, i.y), on = 'opa_no']

### get kernel density estimate using these hyperparameters:
n_cells = 50 #number of cuts of x & y direction for grid
kde.eta = 1 #fraction of cell width to use as KDE bandwidth

### use census tracts shapefile to get a boundary file for
###   Philadelphia as a single polygon -- use a very
###   wide width to be sure we don't get any NA
###   from spkernel2d. Probably should let the
###   width depend on eta since I suspect that's what's
###   causing some near-boundary cells to be
###   forced missing (but not worth the extra effort)
phl = gUnaryUnion(gBuffer(
  readOGR('/media/data_drive/gis_data/PA', 
          'PhiladelphiaCensusTracts2010'), 
  width = 1000
))

### delinquent properties as SpatialPoints
del = SpatialPoints(full_sep[ , cbind(x, y)], 
                    proj4string = CRS('+init=epsg:4326'))
### convert to the same CRS as the shapefile
del = spTransform(del, proj4string(phl))

### for spkernel2d, need matrix of boundary coordinates
boundary = phl@polygons[[1L]]@Polygons[[1L]]@coords

xrng = range(boundary[ , 1L])
yrng = range(boundary[ , 2L])
delx = diff(xrng)/n_cells
dely = diff(yrng)/n_cells
### the pixellate specification of a GridTopology
###   is much more intuitive than that for
###   specifying a GT directly
grdtop <- as(as.SpatialGridDataFrame.im(
  pixellate(ppp(xrange = xrng, yrange = yrng),
            eps = c(delx, dely))), "GridTopology")
### keep the GridTopology since it allows for much
###   faster KDE estimation, but ultimately want
###   to spatially join the delinquent property
###   points to this, so convert to polygons
grdSP = as.SpatialPolygons.GridTopology(grdtop)
### for some reason, specifying this in the
###   proj4string argument is an error
proj4string(grdSP) = proj4string(phl)
grdSPDF = SpatialPolygonsDataFrame(
  grdSP, data = data.frame(ID = seq_len(length(grdSP))), match.ID = FALSE
)
grdSPDF$KDE = spkernel2d(del, boundary, kde.eta*mean(delx, dely), grdtop)

### spatial join and assign each point's local KDE;
###   del is guaranteed to have the same order as full_sep
###   since it was created directly from it and then left in that order
full_sep[ , kde := `$`(del %over% grdSPDF, KDE)]

###  **TO DO: INSERT E-MAIL META DATA FROM DOR CONFIRMATION**
update_opas <- data.table(old = c("151102600", "884350465"),
                          new = c("151102610", "881577275"))
full_sep[update_opas, opa_no := i.old, on = c(opa_no = "new")]

##Block IV: Full Sample, December Cross-Section
full_dec = read_excel(
  wds["data"] %+% 
    "req20150709_PennLetterExperiment (December 2015 update) v2.xlsx",
  sheet = "DETAILS", skip = 8L, na = "-",
  col_names = c("account", "opa_no" , rep("x", 9L),
              "paid_full_dec", "ever_paid_dec", 
              rep("x", 5L), "earliest_pmt_dec",
              "total_paid_dec", rep("x", 5L)),
  col_types = abbr_to_colClass("tstsdns", "2925115"))

setDT(full_dec)

full_dec[ , earliest_pmt_dec := as.Date(earliest_pmt_dec)]

###  **TO DO: INSERT E-MAIL META DATA FROM DOR CONFIRMATION**
update_opas <-
  data.table(old = c("151102600", "884350465", "882932475", "213155700"),
             new = c("151102610", "881577275", "882932476", "881081460"))
full_dec[update_opas, opa_no := i.old, on = c(opa_no = "new")]

##Block V: Main Sample Background Data
## * total_due is pre-study balance
bgvars <- c("opa_no", "owner1", "total_due", "rand_id")
bg_main = fread(wds["proj"] %+% "round_2_full_data.csv",
                select = bgvars, colClasses = list(character = 'opa_no'))

##Block VI: Holdout Sample Background Data
bg_holdout = fread(wds["proj"] %+% "holdout_sample.csv",
                   select = c("opa_no", "owner1", "total_due"),
                   colClasses = list(character = 'opa_no'))
#Need fill since holdout_bg lacks rand_id
bg = rbind(bg_main, bg_holdout, fill = TRUE)

nbhds = fread("/media/data_drive/real_estate/round_two_property_file.csv",
              select = c("BRT NUMBER", "AZAVEA NEIGHBORHOOD"),
              col.names = c("opa_no", "azavea"),
              colClasses = list(character = 'BRT NUMBER'))
#There was an issue where the final file we received didn't have
#  the Azavea neighborhoods included, but a slightly earlier
#  version did; these three properties were new to the
#  final version, so we just geocoded them by hand.
nbhds = 
  rbind(nbhds,
        data.table(opa_no = c("632196220", "661068112", "881035640"),
                   azavea = c("Bustleton", "Morrell Park", "Logan Square")))

#only focus on section of city
nbhds[fread(wds["proj"] %+% "azavea_nbhd_quad_mapping.csv"),
      azavea_section := i.azavea_quad, on = c(azavea = "azavea_nbhd")]

bg[nbhds, azavea_section := i.azavea_section, on = "opa_no"]

###Block VII: Other Background Data
###  From OPA-issued Property Data CD
###  (certified for 2015, received May 2014)
opa_bg = fread(wds["data"] %+% "prop2015.txt",
               select = c('PARCEL', 'MV', 'SALE DATE'),
               col.names = c('opa_no', 'assessed_mv', 'sale_date'),
               colClasses = list(character = c('PARCEL', 'SALE DATE'),
                                 numeric = 'MV'))
opa_bg[ , sale_date := as.Date.character(sale_date, format = '%Y%m%d')]
opa_bg[ , tenure := 
          #difftime doesn't have units = 'years' for some reason...
          round(unclass(difftime(as.Date('2015-06-01'), sale_date,
                                 units = 'weeks')/52))]
opa_bg[ , sale_date := NULL]

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
  col_types = abbr_to_colClass("tststsds",
                               "16112616"))

setDT(followup)

##Quilting time!
### Framework:
###  I  III IV V  VII
###  II III IV VI VII
### (blocks stacked vertically have been stitched/appended,
###  and each column involves a merge to the previous column)

properties <- 
  Reduce(function(x, y) x[y, on = "opa_no"],
         list(opa_bg, full_jul, full_sep, full_dec, bg))

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
                   c("Holdout", "Reminder", "Neighborhood", "Community", 
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
               #only really meaningful at the
               #  owner level for unary owners
               azavea_section = azavea_section[1L],
               ever_paid_jul = any(ever_paid_jul),
               ever_paid_sep = any(ever_paid_sep),
               ever_paid_dec = any(ever_paid_dec),
               ever_paid_jul16 = any(ever_paid_jul16),
               paid_full_jul = all(paid_full_jul),
               paid_full_sep = all(paid_full_sep),
               paid_full_dec = all(paid_full_dec),
               paid_full_jul16 = all(paid_full_jul16),
               total_paid_sep = sum(total_paid_sep),
               total_paid_dec = sum(total_paid_dec),
               earliest_pmt_dec = {
                 if (all(is.na(earliest_pmt_dec))) D(NA)
                 else min(earliest_pmt_dec, na.rm = TRUE)},
               earliest_pmt_jul16 = {
                 if (all(is.na(earliest_pmt_jul16))) D(NA)
                 else min(earliest_pmt_jul16, na.rm = TRUE)},
               total_due = sum(total_due),
               assessed_mv = sum(assessed_mv),
               tenure = median(tenure, na.rm = TRUE),
               kde = median(kde),
               flag_holdout_overlap =
                 flag_holdout_overlap[1L], .N),
             by = owner_id]

### Unary Owner Sample Flag
owners[ , unq_own := N == 1]

### Write output
fwrite(owners, wds["data"] %+% "round_two_analysis_owners.csv", quote = TRUE)
