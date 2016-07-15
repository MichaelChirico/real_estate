#Data Analysis
#Philadelphia Real Estate Tax Evasion
#  Round 2
#Michael Chirico

#Note: See e-mail "now we're in a hurry" from cloef@sas.upenn.edu
#  for confirmation that June 12 is the day small envelope
#  versions were sent to the DoR Mail Room.
#   See e-mail "Status" from rmcfadden@lawlerdirect.com
#  or "Next steps?" (from same) on June 22
#  for confirmation that June 23 is the postage date for
#  the large envelopes sent by Lawler Direct.

#Setup: Random Seed, Packages,
#  Working Directory, Convenient Functions ####
## @knitr setup
##Random Seed
###Alternating digits (1,3,...) of
###  my CVS Extra Care card number
set.seed(4746966)

##Packages
rm(list = ls(all = TRUE))
gc()
#Michael Chirico's function of convenience packages;
#  Available on GitHub @ MichaelChirico/funchir
library(funchir)
library(data.table)
library(parallel)
library(lmtest)
library(xtable)
library(texreg)
setwd(mn <- "~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/")
wds <- c(proj = mn %+% "round_two/", log = mn %+% "logs/round_two/",
         imga = mn %+% "round_two/images/analysis/",
         imgb = mn %+% "round_two/images/balance/",
         data = "/media/data_drive/real_estate/")
write.packages(wds["log"] %+% "analysis_session.txt")

#"metavariables"
trt.nms <- c("Control", "Amenities", "Moral", 
             "Duty", "Peer", "Lien", "Sheriff")
trt.nms8 <- c("Holdout", trt.nms)

mos <- c("jul", "sep", "dec")

#number of bootstrap replications in all such exercises
BB <- 5000

#how many clusters does this machine have?
n_clust <- detectCores()

#Convenient Functions 
get.col<-function(st){
  cols <- 
    c(Big="blue", Small="red", Control = "blue", Amenities = "yellow",
      Moral = "cyan", Duty = "darkgreen", Lien = "red", Sheriff = "orchid",
      Peer = "orange", Holdout = "darkgray")
  cols[gsub("\\s.*", "", as.character(st))]
}

#Data import
## Importing directly from cleaned analysis files
##   created with data_cleaning.R.

owners <- fread(wds["data"] %+% "round_two_analysis_owners.csv")[(holdout | rand_id > 2)]

#set factor levels
owners[ , treat8 := factor(treat8, trt.nms8)]
owners[ , treat7 := factor(treat7, trt.nms)]

properties <- fread(wds["data"] %+% "round_two_analysis_properties.csv")

## Payments data
payments <- setDT(readxl::read_excel(
  wds["data"] %+% 
    "req20150709_PennLetterExperiment (December 2015 update) v2.xlsx",
  sheet = "PAYMENT DETAILS", skip = 1L,
  col_names = c("account","period","valid","past_due",
                "principal","total_paid"),
  col_types = abbr_to_colClass("tn", "15"))
  )[ , c("period","valid") :=
      lapply(.(period,valid), as.Date,
             origin = as.Date("1899-12-30"))
    ][format(as.Date(valid) - past_due, "%Y") == 2015
      ][ , account := gsub("\\s*", "", account)
         ][properties, owner1 := i.owner1, on = "account"
           ][ , .(period = max(period),
                  past_due = min(past_due),
                  principal = sum(principal),
                  total_paid = sum(total_paid)),
              by = .(owner1, valid)
              ][is.na(valid), c("valid","total_paid") :=
                  .(as.Date("2015-06-01"), 0L)
                ][owners, treat8 := i.treat8, on = "owner1"]

follow <- 
  fread("head -n -3 " %+% 
          shQuote(wds["data"] %+%
                    paste("Real Estate Current Year",
                          "Delinquency as of 04-06-2016.txt")),
        sep = "|", colClasses=abbr_to_colClass("cnc","359"),
        col.names=c("opa_no", "legal_name", "period",
                    "principal", "interest", "penalty",
                    "other_due", "total_due","address", 
                    "city", "state", "zip5", "zip4",
                    "bldg_code", "bldg_desc", 
                    "bldg_grp1", "bldg_grp2")
        )[properties, owner1 := i.owner1, on = "opa_no"
          ][!is.na(owner1)][owners, treat8 := i.treat8, on = "owner1"]

#Balance on Observables ####
## Full Sample

lmfp <- function(formula){
  #extract F-statistic & DoF from LM call
  mdf <- summary(do.call("lm", list(formula = formula)))$fstatistic
  #copied (ported) from print.summary.lm
  unname(pf(mdf[1L], mdf[2L], mdf[3L], lower.tail = FALSE))
}

pvs <- c(sapply(c(
  `Amount Due (June)` = "total_due",
  `Assessed Property Value` = "assessed_mv",
  `\\% with Unique Owner` = "unq_own",
  `\\% Overlap with Holdout` = "flag_holdout_overlap",
  `\\# Properties per Owner` = "N"),
  function(x) owners[(!holdout), lmfp(get(x) ~ treat7)]),
  `\\# Owners` =
    owners[(!holdout), chisq.test(table(treat7))$p.value])
pvs <- c("$p$-value", round(pvs, 2L))

print.xtable(xtable(cbind(gsub("$", "\\$", t(
  owners[(!holdout),
         .(`Amount Due (June)` = dol.form(mean(total_due)),
           `Assessed Property Value` = 
             dol.form(mean(assessed_mv, na.rm = TRUE)),
           `\\% with Unique Owner` = to.pct(mean(unq_own), 1L),
           `\\% Overlap with Holdout` = to.pct(mean(flag_holdout_overlap), 2L),
           `\\# Properties per Owner` = round(mean(N), 2L),
           `\\# Owners` = prettyNum(.N, big.mark = ",")),
         keyby = .(Variable = treat7)]), fixed = TRUE), pvs),
  caption = "Balance on Observables",
  label = "tbl:balance", align = "|r|lllllll|l|"),
  include.colnames = FALSE, comment = FALSE, 
  sanitize.text.function = identity,
  floating.environment = "sidewaystable", 
  floating = TRUE, hline.after = c(0L, 1L),
  add.to.row =
    list(pos = list(7L),
         command = c("\\hline \n \\multicolumn{9}{l}" %+% 
                       "{\\scriptsize{$p$-values in rows 1-5 are " %+% 
                       "$F$-test $p$-values from regressing each " %+% 
                       "variable on treatment dummies. A $\\chi^2$ " %+% 
                       "test was used for the count of owners.}} \\\\ \n")))

## Unique owner sample
pvs <- c(sapply(c(
  `Amount Due (June)` = "total_due",
  `Assessed Property Value` = "assessed_mv"),
  function(x) owners[(!holdout & unq_own), lmfp(get(x) ~ treat7)]),
  `\\# Owners` =
    owners[(!holdout & unq_own), chisq.test(table(treat7))$p.value])
pvs <- c("$p$-value", round(pvs, 2L))

print.xtable(xtable(cbind(gsub("$", "\\$", t(
  owners[(!holdout & unq_own),
         .(`Amount Due (June)` = dol.form(mean(total_due)),
           `Assessed Property Value` = 
             dol.form(mean(assessed_mv, na.rm = TRUE)),
           `\\# Owners` = prettyNum(.N, big.mark = ",")),
         keyby = .(Variable = treat7)]), fixed = TRUE), pvs),
  caption = "Balance on Observables (Unique Owners)",
  label = "tbl:balance_unq_own", align = "|r|lllllll|l|"),
  include.colnames = FALSE, comment = FALSE, 
  sanitize.text.function = identity,
  floating.environment = "sidewaystable", 
  floating = TRUE, hline.after = c(0L, 1L),
  add.to.row =
    list(pos = list(4L),
         command = c("\\hline \n \\multicolumn{9}{l}" %+% 
                       "{\\scriptsize{$p$-values in rows 1-5 are " %+% 
                       "$F$-test $p$-values from regressing each " %+% 
                       "variable on treatment dummies. A $\\chi^2$ " %+% 
                       "test was used for the count of owners.}} \\\\ \n")))

## Treated vs. Holdout Comparison

pvs_full <- c("$p$-value", sapply(c(
  `Amount Due (June)` = "total_due",
  `Assessed Property Value` = "assessed_mv",
  `\\% with Unique Owner` = "unq_own",
  `\\# Properties per Owner` = "N"),
  function(x) owners[ , round(lmfp(get(x) ~ holdout), 2L)]),
  `\\# Owners` = "")

pvs_unq <- 
  c("$p$-value", 
    `Amount Due (June)` = 
      owners[(unq_own), round(lmfp(total_due ~ holdout), 2L)],
    `Assessed Property Value` = 
      owners[(unq_own), round(lmfp(assessed_mv ~ holdout), 2L)],
    `\\% with Unique Owner` = "",
    `\\# Properties per Owner` =  "",
    `\\# Owners` = "")

hold_bal <- 
  cbind(gsub("$", "\\$", t(
    owners[ , .(`Amount Due (June)` = dol.form(mean(total_due)),
                `Assessed Property Value` = 
                  dol.form(mean(assessed_mv, na.rm = TRUE)),
                `\\% with Unique Owner` = to.pct(mean(unq_own), 1L),
                `\\# Properties per Owner` = round(mean(N), 2L),
                `\\# Owners` = prettyNum(.N, big.mark = ",")),
            by = .(Variable = c("Treated", "Holdout")[holdout + 1L])]),
    fixed = TRUE), pvs_full,
    gsub("$", "\\$", t(
      owners[(unq_own), 
             .(`Amount Due (June)` = dol.form(mean(total_due)),
               `Assessed Property Value` = 
                 dol.form(mean(assessed_mv, na.rm = TRUE)),
               `\\% with Unique Owner` = "",
               `\\# Properties per Owner` = "",
               `\\# Owners` = prettyNum(.N, big.mark = ",")),
             by = .(Variable = c("Treated", "Holdout")[holdout + 1L])]),
      fixed = TRUE), pvs_unq)

print(xtable(hold_bal, caption = "Balance between Holdout and Treated Samples",
             label = "tbl:bal_hold", align = c("|r|rrr|rrr|")),
      caption.placement = "top", comment = FALSE, include.colnames = FALSE, 
      sanitize.text.function = identity, hline.after = 6,
      add.to.row = list(pos = list(0),
                        command = "\\hline\n & " %+% 
                          "\\multicolumn{3}{c|}{Full Sample} & " %+% 
                          "\\multicolumn{3}{c|}{Unique Owners} " %+% 
                          "\\\\\n\\hline\n")) 

#Point Estimate CIs ####
# @knitr analysis_bs
#For the standard confidence intervals,
#  we can condense the process by storing
#  the key changing parameters in this list;
#  also store a bunch of plotting parameters
#  since we'll use the result of this list for that
outvar <- parse(text = paste0(
  "setNames(list(", 
  paste(paste0("mean(100*", c("ever_paid_", "paid_full_"), 
               rep(mos, each = 2L), ")"), 
        collapse = ","), "), outn)"))

delq <- quote(c(Treatment = list(treat8[-1L]),
                lapply(.SD, function(x) x[-1L] - x[1L])))

outn <- c("ep", "pf") %+% c(1, 1, 3, 3, 6, 6)

DT <- owners[(unq_own)]

boot <- 
  rbindlist(lapply(integer(BB), function(...)
    #calculate point estimate in re-sample
    DT[sample(.N, rep = TRUE), eval(outvar),
       keyby = treat8][ , eval(delq), .SDcols = outn]))

star <- function(x){
  q1.5.10 <- quantile(x, c(.005, .025, .05, .95, .975, .995))
  if (!0 %between% q1.5.10[c(1, 6)]) return("***")
  if (!0 %between% q1.5.10[c(2, 5)]) return("**")
  if (!0 %between% q1.5.10[c(3, 4)]) return("*")
  return("")
}

tbl_info <- 
  rbind(
    DT[ , eval(outvar), keyby = treat8
        ][ , c(Treatment = list(treat8), lapply(.SD, function(x) 
          round(c(x[1L], x[-1L] - x[1L]), 1) %+% "")), .SDcols = outn
          ][boot[ , lapply(.SD, star), by = Treatment, .SDcols = outn],
            (outn) := lapply(outn, function(jj) 
              get(jj) %+% get("i." %+% jj)), on = "Treatment"],
    boot[ , lapply(.SD, function(x) round(sd(x), 1)), by = Treatment], 
    idcol = "type")[type == 2, (outn) := lapply(.SD, function(x)
      paste0("(", x, ")")), .SDcols = outn
      ][order(Treatment)][type == 2, Treatment := ""
                          ][ , !"type", with = FALSE]

print(xtable(
  tbl_info[ , c("Treatment", sort(grep("1|3", names(tbl_info), value = TRUE))),
            with = FALSE], caption = "Linear Probability Model Estimates",
  align = "llllll", label = "tbl:marg_3mo"),
  include.rownames = FALSE, include.colnames = FALSE,
  comment = FALSE, hline.after = c(-1, 1), caption.placement = "top", 
  add.to.row = 
    list(pos = list(0, 15), 
         command = 
           c(" & \\multicolumn{2}{c}{Ever Paid} & " %+% "
             \\multicolumn{2}{c}{Paid in Full} \\\\\n" %+% 
               " & One Month & Three Months & One Month & Three Months \\\\\n",
             "\n \\hline \n \\multicolumn{5}{l}{\\scriptsize{" %+% 
               "Holdout values are in levels; " %+% 
               "remaining figures are relative to Holdout}} \\\\ \n")))

print(xtable(
  tbl_info[ , c("Treatment", "ep6", "pf6"), with = FALSE], 
  caption = "Linear Probability Model Estimates: 6 Months",
  align = "llll", label = "tbl:marg_6mo"),
  include.rownames = FALSE, include.colnames = FALSE,
  comment = FALSE, hline.after = c(-1, 1), caption.placement = "top", 
  add.to.row = 
    list(pos = list(0, 15), 
         command = 
           c(" & Ever Paid & Paid in Full \\\\\n",
             "\n \\hline \n \\multicolumn{3}{l}{\\scriptsize{" %+% 
               "Holdout values are in levels; " %+% 
               "remaining figures are relative to Holdout}} \\\\ \n")))
  

## Cumulative Partial Participation ####
## @knitr analysis_ch_ep
### get range of dates for day-by-day averages
dt.rng <- owners[(!holdout & unq_own),{
  rng <- range(earliest_pmt_dec, na.rm = TRUE)
  seq(from = rng[1L], to = rng[2L], by = "day")}]
#For pretty printing, get once/week subset
dt.rng2 <- dt.rng[seq(1L, length(dt.rng), length.out = 7L)]

date.dt <- 
  #not all treatments saw activity on each day,
  #  so we'll have to "fill-in-the-blanks" with this
  CJ(treat8 = c("Holdout","Control"), date = dt.rng,
     unique = TRUE, sorted = FALSE)
owners[(unq_own), hold_cont := treat8 %in% c("Holdout","Control")]
cum_haz <- 
  owners[(hold_cont),
         #total entrants by day, treatment
         sum(ever_paid_dec) + 0., #convert to numeric
         keyby = .(treat8, earliest_pmt_dec)
         #running total of entrants; take care to eliminate NAs
         ][ , .(ep = cumsum(V1[idx <- !is.na(earliest_pmt_dec)]),
                date = earliest_pmt_dec[idx]), by=treat8
            #merge to get the denominator
            ][owners[(hold_cont), .N, treat8],
              ep := ep/i.N, on = "treat8"
              ][date.dt, on = c("treat8", "date"), roll = TRUE
                #express relative to holdout
                ][ , .(ep = ep[idx <- treat8 == "Control"] -
                         ep[!idx]), by = date]

cis <- dcast(rbindlist(lapply(integer(BB), function(...){
  dt <- owners[(hold_cont)][sample(.N, rep = TRUE)]
  dt[ , sum(ever_paid_dec) + 0., keyby = .(treat8, earliest_pmt_dec)
      ][ , .(ep = cumsum(V1[idx <- !is.na(earliest_pmt_dec)]),
             date = earliest_pmt_dec[idx]), by = treat8
         ][dt[ , .N, treat8], ep := ep/i.N, on = "treat8"
           ][date.dt, on = c("treat8", "date"), roll = TRUE
             ][ , .(ep = ep[idx <- treat8 == "Control"] - ep[!idx]),
                by = date]}), idcol = "bootID"
  #extract for 95% CI
  )[ , quantile(ep, c(.025, .975), na.rm = TRUE), by = date],
  date ~ c("low", "high")[rowid(date)], value.var = "V1")

#Regression Tables ####
# @knitr analysis_reg
reg_vars <- c("ever_paid", "paid_full", "total_paid")
reg_all <- levels(interaction(reg_vars, mos, sep="_"))

reg_j <- 
  parse(text = 
          paste0(".(", paste(paste0(
            reg_all, "=list(", sapply(reg_all, function(x) 
              if (grepl("total", x)) 
                paste0("lm(", x," ~ treat7)")
              else paste0("glm(", x, " ~ treat7, ",
                          "family = binomial)")), ")"), 
            collapse = ","), ")"))

coef_j = 
  parse(text = 
          paste0(".(", paste(paste0(
            reg_all, "=", sapply(reg_all, function(x) 
              if (grepl("total", x)) 
                paste0("lm.fit(model.matrix(", x,
                       " ~ treat7), ",x,")")
              else paste0("glm.fit(model.matrix(", x, 
                          " ~ treat7), ",x,
                          ', start = starts[["',x,'"]],',
                          "family=binomial())")), 
            "$coefficients"), 
            collapse = ","), ")"))

setkey(owners, rand_id)
setindex(owners, holdout)
DT <- owners[(!holdout)]
regs <- owners[(!holdout), eval(reg_j)]
starts <- 
  lapply(reg_all[!grepl("total", reg_all)], 
         function(rr) regs[[rr]][[1L]]$coefficients)
RBs <- owners[(!holdout), unique(rand_id)]

tmp <- tempfile()
progress <- txtProgressBar(1, BB, style = 3L)

cl <- makeCluster(n_clust, outfile = "")
invisible(clusterEvalQ(cl, library(data.table)))
clusterExport(cl, c("DT", "RBs", "coef_j", "starts", 
                    "progress", "tmp"),
              envir = environment())
boot_dist <- rbindlist(parLapply(cl, 1:BB, function(bb){
  cat("\n", file = tmp, append = TRUE)
  setTxtProgressBar(
    progress, as.integer(system(paste("wc -l <", tmp), intern = TRUE)))
  DT[.(sample(RBs, rep = TRUE)), eval(coef_j)]}))
stopCluster(cl); close(progress); unlink(tmp)

reg_info <- 
  rbind(regs, boot_dist[ , lapply(.SD, function(x)
    list(var(matrix(x, ncol = 7, byrow = TRUE))))]
  )[ , lapply(.SD, function(x)
    c(x, list(sqrt(diag(x[[2]])))))
    ][ , lapply(.SD, function(x) 
      c(x, list(coeftest(x[[1]], vcov = x[[2]])[ , 4])))]

#now for single-owner samples
##exclude total_paid for single owner analysis
DT <- owners[(!holdout & unq_own)]
regs <- owners[(!holdout & unq_own), eval(reg_j)]
starts <- 
  lapply(reg_all, function(rr) regs[[rr]][[1L]]$coefficients)
RBs <- owners[(!holdout & unq_own), unique(rand_id)]

tmp <- tempfile()
progress <- txtProgressBar(1, BB, style = 3L)

cl <- makeCluster(n_clust, outfile = "")
invisible(clusterEvalQ(cl, library(data.table)))
clusterExport(cl, c("DT", "RBs", "coef_j", "starts", "progress", "tmp"),
              envir = environment())
boot_dist <- rbindlist(parLapply(cl, 1:BB, function(bb){
  cat("\n", file = tmp, append = TRUE)
  setTxtProgressBar(
    progress, as.integer(system(paste("wc -l <", tmp), intern = TRUE)))
  DT[.(sample(RBs, rep = TRUE)), eval(coef_j)]}))
stopCluster(cl); close(progress); unlink(tmp)

reg_info_so <- 
  rbind(regs, boot_dist[ , lapply(.SD, function(x)
    list(var(matrix(x, ncol = 7, byrow = TRUE))))]
  )[ , lapply(.SD, function(x)
    c(x, list(sqrt(diag(x[[2]])))))
    ][ , lapply(.SD, function(x) 
      c(x, list(coeftest(x[[1]], vcov = x[[2]])[ , 4])))]


###Difference in Mean Tests
rename_coef<-function(obj){
  nms <- names(obj$coefficients)
  idx <- !grepl("treat7", nms)
  #Add XXX to coefficients we'll exclude
  nms[idx] <- nms[idx] %+% "XXX"
  nms <- gsub("treat7", "", nms)
  names(obj$coefficients) <- nms
  obj
}

proper <- c(ever_paid="Ever Paid",
            paid_full="Paid Full",
            total_paid="Total Paid")
abbr <- c(ever_paid="ep", paid_full="pf", total_paid="tp")
invisible(lapply(reg_vars, function(rr){
  cat("\n\n\n\n***********************\n", proper[rr])
  x <- capture.output(texreg(c(lapply(
    tp <- paste(rr, mos, sep="_"), function(mo)
      rename_coef(reg_info[[mo]][[1]])),
    lapply(tp, function(mo)
      rename_coef(reg_info_so[[mo]][[1]]))),
    custom.model.names=
      rep(c("One Month","Three Months","Six Months"), 2),
    caption=
      "Estimated Average Treatment Effects: " %+% proper[rr],
    override.se=
      c(lapply(tp, function(mo) reg_info[[mo]][[3]]),
        lapply(tp, function(mo) reg_info_so[[mo]][[3]])),
    override.pval=
      c(lapply(tp, function(mo) reg_info[[mo]][[4]]),
        lapply(tp, function(mo) reg_info_so[[mo]][[4]])),
    caption.above=TRUE, stars=c(.01,.05,.1),
    label="tbl:reg7_" %+% abbr[rr], 
    include.rsquared=FALSE, include.adjrs=FALSE, 
    include.rmse=FALSE, include.aic=FALSE, 
    include.bic=FALSE, include.deviance=FALSE,
    #Exclude Intercept
    omit.coef="XXX$", float.pos="htbp", 
    sideways = TRUE, use.packages = FALSE))
  
  ## add vertical lines to separate populations
  x[5] <- "\\begin{tabular}{|l| c c c| c c c| }"
  
  ## add multicolumn to distinguish populations
  x[6] <- x[6] %+% "\n & \\multicolumn{3}{c}{All Owners} & " %+% 
    "\\multicolumn{3}{|c|}{Single-Property Owners} \\\\"
  
  ## print again
  cat(x, sep = "\n")}))

#Now for main vs. holdout
reg_all <- c("paid_full_", "ever_paid_") %+% 
  rep(mos, each = 2)

reg_j <- 
  parse(text = 
          paste0(".(", paste(paste0(
            reg_all, "=list(glm(", reg_all,
            " ~ treat8, family = binomial))"), 
            collapse = ","), ")"))

coef_j = 
  parse(text = 
          paste0(".(", paste(paste0(
            reg_all, "=", sapply(reg_all, function(x) 
              paste0("glm.fit(model.matrix(", x, 
                     " ~ treat8), ",x,
                     ', start = starts[["',x,'"]],',
                     "family=binomial())")), 
            "$coefficients"), 
            collapse = ","), ")"))

DT <- owners[(unq_own)]
regs <- owners[(unq_own), eval(reg_j)]
starts <- 
  lapply(reg_all, function(rr) regs[[rr]][[1L]]$coefficients)

tmp <- tempfile()
progress <- txtProgressBar(1, BB, style = 3L)

cl <- makeCluster(n_clust, outfile = "")
invisible(clusterEvalQ(cl, library(data.table)))
clusterExport(cl, c("DT", "coef_j", "starts", "progress", "tmp"),
              envir = environment())
boot_dist <- rbindlist(parLapply(cl, 1:BB, function(bb){
  cat("\n", file = tmp, append = TRUE)
  setTxtProgressBar(
    progress, as.integer(system(paste("wc -l <", tmp), intern = TRUE)))
  DT[sample(.N, rep = TRUE), eval(coef_j)]}))
stopCluster(cl); close(progress); unlink(tmp)

reg_info_8 <- 
  rbind(regs, boot_dist[ , lapply(.SD, function(x)
    list(var(matrix(x, ncol = 8, byrow = TRUE))))]
  )[ , lapply(.SD, function(x)
    c(x, list(sqrt(diag(x[[2]])))))
    ][ , lapply(.SD, function(x) 
      c(x, list(coeftest(x[[1]], vcov = x[[2]])[ , 4])))]


###Difference in Mean Tests
rename_coef<-function(obj){
  nms <- names(obj$coefficients)
  idx <- !grepl("treat8", nms)
  #Add XXX to coefficients we'll exclude
  nms[idx] <- nms[idx] %+% "XXX"
  nms <- gsub("treat8", "", nms)
  names(obj$coefficients) <- nms
  obj
}

three_months <- sort(grep("jul|sep", reg_all, value = TRUE))
x <- capture.output(print(texreg(
  lapply(three_months, function(mo)
    rename_coef(reg_info_8[[mo]][[1]])),
  custom.model.names=
    rep(c("One Month", "Three Months"), 2L),
  caption = "Logistic Model Estimates",
  override.se = lapply(three_months, function(mo) reg_info_8[[mo]][[3]]),
  override.pval = lapply(three_months, function(mo) reg_info_8[[mo]][[4]]),
  caption.above = TRUE, stars = c(.01, .05, .1), label = "tbl:reg8_3mo", 
  include.rsquared = FALSE, include.adjrs = FALSE, include.rmse = FALSE,
  include.aic = FALSE, include.bic = FALSE, include.deviance = FALSE,
  #Exclude Intercept
  omit.coef = "XXX$", float.pos="htbp")))

cat(x[1:5], "\\hline", 
    " & \\multicolumn{2}{c}{Ever Paid} & \\multicolumn{2}{c}{Paid Full} \\\\",
    x[6:length(x)], sep = "\n")

x <- capture.output(texreg(lapply(expression(
  total_paid_jul, total_paid_sep),
  function(mo) { x <- owners[(unq_own), lm(eval(mo) ~ treat8)]
    names(x$coefficients) <- 
      gsub("treat8", "", names(x$coefficients))
    names(x$coefficients)[1L] <- "Holdout"; x}),
  custom.model.names = c("One Month", "Three Months"),
  include.rsquared = FALSE, include.adjrs = FALSE,
  include.rmse = FALSE, caption = "Revenue Regressions",
  float.pos = "htbp", stars = c(.01, .05, .1), 
  label = "tbl:rev_reg_3mo", caption.above = TRUE))

cat(x[1:10], "\\hline", x[11:length(x)], sep = "\n")

x <- capture.output(texreg({ 
  x <- owners[(unq_own), lm(total_paid_dec ~ treat8)]
  names(x$coefficients) <- 
    gsub("treat8", "", names(x$coefficients))
  names(x$coefficients)[1L] <- "Holdout"; x},
  custom.model.names = "Six Months",
  include.rsquared = FALSE, include.adjrs = FALSE,
  include.rmse = FALSE, caption = "Revenue Regression (Six Months)",
  float.pos = "htbp", stars = c(.01, .05, .1), 
  label = "tbl:rev_reg_6mo", caption.above = TRUE))

cat(x[1:10], "\\hline", x[11:length(x)], sep = "\n")

#Cost-Benefit: Estimating returns to speed-up ####
## Parameters of estimate
##  - Date when properties were distributed to co-council
##  - Proportion of converted debt distributed to co-council
params <- list(switch_date = D("2015-08-15"),
               lawyer_rate = .06)
delta <- 
  #count those having paid by hand-off
  owners[earliest_pmt_dec <= params$switch_date,
         .N, keyby = treat8
         ][owners[ , .N, keyby = treat8], 
           #what's the difference in percentage
           #  of owners ever paid in each 
           #  treatment/control vs. holdout?
           .(treat8[-1], N[-1]/i.N[-1] - N[1]/i.N[1]), 
           on = "treat8"][ , setNames(V2, V1)]

repaid_by_switch <- 
  owners[earliest_pmt_dec <= params$switch_date &
           !treat8 == "Holdout"]

setkey(repaid_by_switch, treat7)

dist_benefits <- 
  sapply(integer(BB), function(...)
    sum(sapply(trt.nms, function(tr)
      #going treatment-by-treatment, pull out 
      #  properties in quantity roughly equal to the 
      #  difference in treatment effect vs. holdout 
      #  by the switch date
      repaid_by_switch[.(tr)][
        sample(.N, round(delta[tr] * .N)), 
        #find what would have been the lawyers'
        #  take of the total amount paid by
        #  these properties
        params$lawyer_rate * 
          sum(total_paid_jul)])))

pdf2(wds["imga"] %+% "hist_benefit_acceleration.pdf")
x <- hist(dist_benefits, breaks = 20, freq = FALSE,  
          main = "Distribution of Simulated Benefits to Acceleration",
          col = "lightblue", xlab = "$", yaxt = "n", ylab = "")
abline(v = md <- median(dist_benefits), col = "red", lty = 2, lwd = 3)
mtext(side = 2L, text = "Density")
text(md, max(x$density), pos = 4,
     labels = "Median Benefit: " %+% dol.form(md))
dev.off2()

##Regression-based approach
setkey(owners, treat8)
setindex(owners, treat8, assessed_mv)
regs <- 
  owners[assessed_mv > 0, 
         .(list(reg=lm(total_paid_jul ~ log(assessed_mv) + 
                         log(total_due) + residential + 
                         phila_mailing + log(N)))), keyby = treat8]

params$lawyer_rate * sum(sapply(trt.nms, function(tr)
  sum((x <- owners[.(tr)][assessed_mv > 0])[ , total_paid_jul] - 
        predict(regs[.("Holdout"), V1][[1]], x))))

params$lawyer_rate * sum(sapply(trt.nms, function(tr)
  sum(predict(regs[.(tr), V1][[1]], x <- owners[.(tr)][assessed_mv > 0]) - 
        predict(regs[.("Holdout"), V1][[1]], x))))

##Back-of-the-envelope approach

### @ 3 months
print(xtable(
  owners[(unq_own), .(.N, mean(ever_paid_sep)), keyby = treat8
         ][ , .(Treatment = treat8[-1], 
                `Impact Per Letter` = 
                  dol.form(x <-  (V2[-1] - V2[1]) * 
                             owners[(unq_own & total_paid_dec > 0), 
                                    median(total_paid_dec)],  dig = 2L), 
                `Total Impact` = dol.form(N[-1] * x))],
  caption = "Estimated Impact on Revenue: 3 Months",
  label = "rev_ep3", align = "rrrr"),
  include.rownames = FALSE, comment = FALSE, caption.placement = "top")

### @ 6 months
print(xtable(
  owners[(unq_own), .(.N, mean(ever_paid_dec)), keyby = treat8
         ][ , .(Treatment = treat8[-1], 
                `Impact Per Letter` = 
                  dol.form(x <-  (V2[-1] - V2[1]) * 
                             owners[(unq_own & total_paid_dec > 0), 
                                    median(total_paid_dec)],  dig = 2L), 
                `Total Impact` = dol.form(N[-1] * x))],
  caption = "Estimated Impact on Revenue: 6 Months",
  label = "rev_ep6", align = "rrrr"),
  include.rownames = FALSE, comment = FALSE, caption.placement = "top")

# Alternative Ever Paid: @ August 10th ####


         
texreg(lapply(D("2015-07-22", "2015-08-10", "2015-09-24"),
              function(dd){
                #will throw a warning on the first
                #  pass of the lapply loop
                owners[ , ever_paid_temp := NULL]
                owners[payments[valid <= dd, sum(total_paid), 
                                by = owner1],
                       ever_paid_temp := i.V1 > 0, on = "owner1"]
                owners[is.na(ever_paid_temp), ever_paid_temp := FALSE]
                x <- owners[(unq_own), lm(ever_paid_temp ~ treat8)]
                names(x$coefficients) <- 
                  gsub("treat8", "", names(x$coefficients))
                names(x$coefficients)[1L] <- "Holdout"; x}),
  custom.model.names = c("One Month", "Aug 10", "Three Months"),
  caption.above = TRUE, float.pos = "htbp",
  caption = "Ever Paid, Using Payments File",
  include.rsquared = FALSE, include.adjrs = FALSE,
  include.rmse = FALSE, stars = c(.01, .05, .1),
  label = "reg:ep_pmts")

# 2016 Follow-Up
owners[follow, total_due_2016 := i.total_due, on = "owner1"]
owners[ , paid_full_2016 := is.na(total_due_2016) | 
          total_due_2016 < .1]

texreg(lapply(expression((unq_own), (unq_own & ever_paid_dec)),
              function(y){
                x <- owners[eval(y), lm(paid_full_2016 ~ treat8)]
                names(x$coefficients) <- 
                  gsub("treat8", "", names(x$coefficients))
                names(x$coefficients)[1L] <- "Holdout"; x}),
       include.rsquared = FALSE, include.rmse = FALSE,
       include.adjrs = FALSE, float.pos = "htbp",
       custom.model.names = c("Full Sample", "Among 2015 Ever-Paid"),
       stars = c(.01, .05, .1), caption.above = TRUE,
       caption = "Effects on 2016 Full Payment", label = "reg:2016_pf")
