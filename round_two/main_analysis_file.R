#Data Analysis
#Philadelphia Real Estate Tax Evasion
#  Round 2
#Michael Chirico

# Setup: Packages, Directories, Data Import ####

##Packages
rm(list = ls(all = TRUE))
gc()
###Michael Chirico's package of convenience functions;
###  Available on GitHub @ MichaelChirico/funchir
library(funchir)
library(data.table) #for everything
library(parallel) #for bootstrap
library(lmtest) #for standard errors
library(xtable) #for table output
library(texreg) #for regression output

##Directories
setwd(mn <- "~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/")
wds <- c(log = mn %+% "logs/round_two/",
         data = "/media/data_drive/real_estate/")
write.packages(wds["log"] %+% "analysis_session.txt")

#Data import
#  Importing directly from cleaned analysis files created with data_cleaning.R.

owners <- fread(wds["data"] %+% "round_two_analysis_owners.csv"
                #exclude top 2 randomization blocks
                )[(holdout | rand_id > 2)]

##set factor levels (thereby, reference group)
owners[ , treat8 := factor(treat8, c("Holdout", "Control", "Amenities", "Moral", 
                                     "Duty", "Peer", "Lien", "Sheriff"))]
owners[ , treat7 := factor(treat7, c("Control", "Amenities", "Moral", 
                                     "Duty", "Peer", "Lien", "Sheriff"))]

# TABLE 1: Balance - Treated vs. Holdout Comparison (Unique Owners) ####
lmfp <- function(formula){
  #extract F-statistic & DoF from LM call
  mdf <- summary(do.call("lm", list(formula = formula)))$fstatistic
  #copied (ported) from print.summary.lm
  unname(pf(mdf[1L], mdf[2L], mdf[3L], lower.tail = FALSE))
}

hold_bal <- 
  cbind(gsub("$", "\\$", t(
    owners[(unq_own), 
           .(`Amount Due (June)` = dol.form(mean(total_due)),
             `Assessed Property Value` = 
               dol.form(mean(assessed_mv, na.rm = TRUE)),
             `\\# Owners` = prettyNum(.N, big.mark = ",")),
           by = .(Variable = c("Treated", "Holdout"
                               )[holdout + 1L])]), fixed = TRUE),
    c("$p$-value", 
      `Amount Due (June)` = 
        owners[(unq_own), round(lmfp(total_due ~ holdout), 2L)],
      `Assessed Property Value` = 
        owners[(unq_own), round(lmfp(assessed_mv ~ holdout), 2L)],
      `\\# Owners` = ""))

print(xtable(hold_bal, caption = "Balance between Holdout and Treated Samples",
             label = "bal_hold", align = c("lrrc")),
      caption.placement = "top", comment = FALSE, include.colnames = FALSE, 
      sanitize.text.function = identity, hline.after = c(0L, 4L))

# TABLE 2: Balance  - Comparison by Treatment, Full & Unique Owner Sample ####
p_tex <- function(x) c("$p$-value", round(x, 2L))

{cat("\\begin{sidewaystable}[ht]",
    "\\centering", 
    "\\caption{Balance on Observables}",
    "\\label{balance}",
    "\\begin{tabular}{lrrrrrrrc}", 
    "\\hline",
    "\\multicolumn{9}{c}{Unique Owners} \\\\", sep = "\n")

print.xtable(xtable(cbind(t(
  owners[(!holdout & unq_own),
         .(`Amount Due (June)` = dol.form(mean(total_due), tex = TRUE),
           `Assessed Property Value` = 
             dol.form(mean(assessed_mv, na.rm = TRUE), tex = TRUE),
           `\\# Owners` = prettyNum(.N, big.mark = ",")),
         keyby = .(Variable = treat7)]), 
  p_tex(c(sapply(c(
    `Amount Due (June)` = "total_due",
    `Assessed Property Value` = "assessed_mv"),
    function(x) owners[(!holdout & unq_own), lmfp(get(x) ~ treat7)]),
    `\\# Owners` =
      owners[(!holdout & unq_own), chisq.test(table(treat7))$p.value])))),
  include.colnames = FALSE, comment = FALSE, 
  sanitize.text.function = identity, only.contents = TRUE,
  floating = TRUE, hline.after = c(0L, 1L))

cat("\\hline",
    "\\multicolumn{9}{c}{Unique and Multiple Owners} \\\\", sep = "\n")

print.xtable(xtable(cbind(t(
  owners[(!holdout),
         .(`Amount Due (June)` = dol.form(mean(total_due), tex = TRUE),
           `Assessed Property Value` = 
             dol.form(mean(assessed_mv, na.rm = TRUE), tex = TRUE),
           `\\% with Unique Owner` = to.pct(mean(unq_own), 1L),
           `\\% Overlap with Holdout` = to.pct(mean(flag_holdout_overlap), 2L),
           `\\# Properties per Owner` = round(mean(N), 2L),
           `\\# Owners` = prettyNum(.N, big.mark = ",")),
         keyby = .(Variable = treat7)]), 
  p_tex(c(sapply(c(
    `Amount Due (June)` = "total_due",
    `Assessed Property Value` = "assessed_mv",
    `\\% with Unique Owner` = "unq_own",
    `\\% Overlap with Holdout` = "flag_holdout_overlap",
    `\\# Properties per Owner` = "N"),
    function(x) owners[(!holdout), lmfp(get(x) ~ treat7)]),
    `\\# Owners` =
      owners[(!holdout), chisq.test(table(treat7))$p.value])))),
  include.colnames = FALSE, comment = FALSE, 
  sanitize.text.function = identity, 
  only.contents = TRUE, hline.after = c(0L, 1L))

cat("\\hline",
    "\\multicolumn{9}{l}" %+% 
      "{\\scriptsize{$p$-values in rows 1-5 are " %+% 
      "$F$-test $p$-values from regressing each " %+% 
      "variable on treatment dummies. A $\\chi^2$ " %+% 
      "test was used for the count of owners.}} \\\\",
    "\\end{tabular}",
    "\\end{sidewaystable}", sep = "\n")}

# TABLE 3: Regression - Ever Paid/Paid Full @ 1 & 3 Months, LPM ####
rename_coef <- function(obj){
  nms <- names(obj$coefficients)
  nms[nms == "(Intercept)"] <- "Holdout"
  nms <- gsub("treat8", "", nms)
  names(obj$coefficients) <- nms
  obj
}

tbl <- capture.output(texreg(lapply(lapply(expression(
  `One Month` = ever_paid_jul, `Three Months` = ever_paid_sep,
  `One Month` = paid_full_jul, `Three Months` = paid_full_sep),
  function(x) owners[(unq_own), lm(I(100 * eval(x)) ~ treat8)]), rename_coef),
  stars = c(.01, .05, .1), include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L, label = "pc_lin",
  caption = "Short Term Linear Probability Model Estimates",
  custom.note = "%stars. Holdout values in levels; " %+% 
    "remaining figures relative to this"))

## Replace Holdout SEs with horizontal rule, add header for EP vs. PF
idx <- grep("^Holdout", tbl)

tbl[idx] <- gsub("\\^\\{[*]*\\}", "", tbl[idx])

tbl <- c(tbl[1L:(idx - 3L)],
         " & \\multicolumn{2}{c}{Ever Paid} & " %+% 
           "\\multicolumn{2}{c}{Paid in Full} \\\\",
         tbl[c(idx - 2L, idx)],
         "\\hline", tbl[(idx + 1L):length(tbl)])

cat(tbl, sep = "\n")

# TABLE 4: Regression - Ever Paid/Paid Full @ 1 & 3 Months, Logistic ####
tbl <- capture.output(texreg(lapply(lapply(expression(
  `One Month` = ever_paid_jul, `Three Months` = ever_paid_sep,
  `One Month` = paid_full_jul, `Three Months` = paid_full_sep),
  function(x) 
    owners[(unq_own), glm(eval(x) ~ treat8, family = binomial)]), rename_coef),
  stars = c(.01, .05, .1), include.rsquared = FALSE, caption.above = TRUE,
  include.aic = FALSE, include.bic = FALSE, include.deviance = FALSE,
  omit.coef = "Holdout", digits = 2L, label = "sh_logit",
  caption = "Short Term Logistic Model Estimates"))

idx <- grep("^\\\\begin\\{tabular\\}", tbl)

tbl <- c(tbl[1L:(idx + 1L)], 
         " & \\multicolumn{2}{c}{Ever Paid} & " %+% 
           "\\multicolumn{2}{c}{Paid in Full} \\\\",
         tbl[(idx + 2L):length(tbl)])

cat(tbl, sep = "\n")

# TABLE 5: Revenue - Per-Letter Impact @ 3 Months ####
print(xtable(
  owners[(unq_own), .(.N, mean(ever_paid_sep)), keyby = treat8
         ][ , .(Treatment = treat8[-1L], 
                `Impact Per Letter` = 
                  dol.form(x <-  (V2[-1L] - V2[1L]) * 
                             owners[(unq_own & total_paid_dec > 0), 
                                    median(total_paid_dec)],  dig = 2L), 
                `Total Impact` = dol.form(N[-1L] * x))],
  caption = "Estimated Short Term Impact on Revenue (3 Months)",
  label = "sh_rev", align = "rlcc"),
  include.rownames = FALSE, comment = FALSE, caption.placement = "top")

# TABLE 6: Regression - Ever Paid @ 1 & 3 Months, Logistic, vs. Control ####
rename_coef <- function(obj){
  nms <- names(obj$coefficients)
  nms[nms == "(Intercept)"] <- "Control"
  nms <- gsub("treat7", "", nms)
  names(obj$coefficients) <- nms
  obj
}

tbl <- capture.output(texreg(lapply(c(lapply(expression(
  `One Month` = ever_paid_jul,
  `Three Months` = ever_paid_sep),
  function(x) 
    owners[(!holdout), glm(eval(x) ~ treat7, family = binomial)]),
  lapply(expression(`One Month` = ever_paid_jul,
                    `Three Months` = ever_paid_sep),
         function(x) 
           owners[(!holdout & unq_own), 
                  glm(eval(x) ~ treat7, family = binomial)])), rename_coef),
  omit.coef = "Control", include.aic = FALSE, include.bic = FALSE,
  include.deviance = FALSE, stars = c(.001, .05, .1),
  caption = "Robustness Analysis: Multiple Owners",
  label = "sh_logit_rob", caption.above = TRUE))

idx <- grep("^\\\\begin\\{tabular\\}", tbl)

tbl <- c(tbl[1L:(idx + 1L)], 
         " & \\multicolumn{2}{c}{All Owners} & " %+% 
           "\\multicolumn{2}{c}{Single-Property Owners} \\\\",
         tbl[(idx + 2L):length(tbl)])

cat(tbl, sep = "\n")

# TABLE 7: Regression - Ever Paid/Paid Full @ 6 Months, LPM ####
rename_coef <- function(obj){
  nms <- names(obj$coefficients)
  nms[nms == "(Intercept)"] <- "Holdout"
  nms <- gsub("treat8", "", nms)
  names(obj$coefficients) <- nms
  obj
}

tbl <- capture.output(texreg(lapply(lapply(expression(
  `Ever Paid` = ever_paid_dec, `Paid in Full` = paid_full_dec),
  function(x) owners[(unq_own), lm(I(100 * eval(x)) ~ treat8)]), rename_coef),
  stars = c(.01, .05, .1), include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L, label = "lg_pc_lin",
  caption = "Long Term Linear Probability Model Estimates"))

idx <- grep("^Holdout", tbl)

tbl[idx] <- gsub("\\^\\{[*]*\\}", "", tbl[idx])

tbl[c(idx - 1L, idx)] <- tbl[c(idx, idx - 1L)]

idx <- grep("^\\\\end\\{tabular\\}", tbl)

tbl[idx - 1L] <- tbl[idx - 1L] %+% " \\\\"

tbl <- c(tbl[1L:(idx - 1L)],
         "\\multicolumn{3}{l}{\\scriptsize{" %+% 
           "Holdout values in levels; remaining figures relative to this}}",
         tbl[idx:length(tbl)])

cat(tbl, sep = "\n")

# TABLE 8: Revenue - Per-Letter Impact @ 6 Months ####
print(xtable(
  owners[(unq_own), .(.N, mean(ever_paid_dec)), keyby = treat8
         ][ , .(Treatment = treat8[-1L], 
                `Impact Per Letter` = 
                  dol.form(x <-  (V2[-1L] - V2[1L]) * 
                             owners[(unq_own & total_paid_dec > 0), 
                                    median(total_paid_dec)],  dig = 2L), 
                `Total Impact` = dol.form(N[-1L] * x))],
  caption = "Estimated Long Term Impact on Revenue",
  label = "lg_rev", align = "rlcc"),
  include.rownames = FALSE, comment = FALSE, caption.placement = "top")

# TOTAL PAID @ 1 & 3 Months ####
rename_coef <- function(obj){
  nms <- names(obj$coefficients)
  nms[nms == "(Intercept)"] <- "Holdout"
  nms <- gsub("treat8", "", nms)
  names(obj$coefficients) <- nms
  obj
}

tbl <- capture.output(texreg(lapply(lapply(expression(
  `One Month` = total_paid_jul, `Three Months` = total_paid_sep),
  function(x) owners[(unq_own), lm(eval(x) ~ treat8)]), rename_coef),
  stars = c(.01, .05, .1), include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L, label = "rev_short",
  caption = "Short Term Linear Probability Model Estimates: Revenue",
  custom.note = "%stars. Holdout values in levels; " %+% 
    "remaining figures relative to this"))

## Replace Holdout SEs with horizontal rule
idx <- grep("^Holdout", tbl)

tbl[idx] <- gsub("\\^\\{[*]*\\}", "", tbl[idx])

tbl <- c(tbl[1L:idx], "\\hline", tbl[(idx + 2L):length(tbl)])

cat(tbl, sep = "\n")

# TOTAL PAID @ 6 Months ####
tbl <- capture.output(texreg(lapply(lapply(
  expression(`Total Paid` = total_paid_dec),
  function(x) owners[(unq_own), lm(eval(x) ~ treat8)]), rename_coef),
  stars = c(.01, .05, .1), include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L, label = "rev_long",
  caption = "Long Term Linear Probability Model Estimates: Revenue"))

idx <- grep("^Holdout", tbl)

tbl[idx] <- gsub("\\^\\{[*]*\\}", "", tbl[idx])

tbl[idx + 1L] <- "\\hline"

cat(tbl, sep = "\n")


# Further analysis ####

## Alternative Ever Paid: @ August 10th
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

## 2016 Follow-Up (@ March)
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
