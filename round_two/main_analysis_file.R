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
library(xtable) #for table output
library(texreg) #for regression output

##Directories
setwd(mn <- "~/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/")
wds <- c(log = mn %+% "logs/round_two/",
         data = "/media/data_drive/real_estate/"); rm(mn)
write.packages(wds["log"] %+% "analysis_session.txt")

##Specialized Functions
###Get the p-value on the full-regression F-test of an OLS call
lmfp <- function(formula){rename_coef <- function(obj){
  nms <- names(obj$coefficients)
  nms[nms == "(Intercept)"] <- "Reminder"
  nms <- gsub("treat7", "", nms)
  names(obj$coefficients) <- nms
  obj
}
  #extract F-statistic & Degrees of Freedom from LM call
  mdf <- summary.lm(do.call("lm", list(formula = formula)))$fstatistic
  #copied (ported) from print.summary.lm
  unname(pf(mdf[1L], df1 = mdf[2L], df2 = mdf[3L], lower.tail = FALSE))
}

###Round a p-value to two digits (we only use two-digit rounding
###  in this paper), append with a TeX-marked-up label
p_tex <- function(x) c("$p$-value", round(x, 2L))

###Specially tailor the lm object for pretty printing
rename_coef <- function(obj, nn){
  nms <- names(obj$coefficients)
  nms[nms == "(Intercept)"] <- if (nn == 7) "Reminder" else "Holdout"
  #treatment dummies take the form treat8Reminder, etc -- 
  #  eliminate the treat8 part for digestibility
  nms <- gsub("treat" %+% nn, "", nms)
  names(obj$coefficients) <- nms
  obj
}

#Data import
#  Importing directly from cleaned analysis files created with data_cleaning.R.
owners <- fread(wds["data"] %+% "round_two_analysis_owners.csv"
                #exclude top 2 randomization blocks
                )[(holdout | rand_id > 2)]

##set factor levels (and, thereby, the reference group)
owners[ , treat8 := 
          factor(treat8, c("Holdout", "Reminder", "Lien", "Sheriff",
                           "Neighborhood", "Community", "Peer", "Duty"))]
owners[ , treat7 := 
          factor(treat7, c("Reminder", "Lien", "Sheriff",
                           "Neighborhood", "Community", "Peer", "Duty"))]

owners[ , earliest_pmt_dec := 
          as.Date(earliest_pmt_dec, format = "%Y-%m-%d")]

# TABLE 1: Balance on Observables (Unary Owners) ####
##Print Table Header
### *surround with {} so the table all prints together*
{cat("\\begin{sidewaystable}[ht]",
    "\\centering", 
    "\\caption{Balance on Observables (Unary Owners)}",
    "\\label{balance}",
    "\\vspace{10mm}",
    "\\begin{tabular}{lrrrrrrrrc}", 
    "\\hline", sep = "\n")

print.xtable(xtable(cbind(t(
  owners[(unq_own),
         c(.(`Amount Due (June)` = dol.form(mean(total_due), tex = TRUE)),
           .(`Assessed Property Value` = 
                  dol.form(mean(assessed_mv, na.rm = TRUE), tex = TRUE)),
           as.list(table(azavea_section)),
           .(`\\# Owners` = prettyNum(.N, big.mark = ","))),
         keyby = .(Variable = treat8)]), 
  p_tex(c(sapply(c(
    `Amount Due (June)` = "total_due",
    `Assessed Property Value` = "assessed_mv"),
    function(x) owners[(unq_own), lmfp(get(x) ~ treat8)]),
    owners[(unq_own), c(round(chisq.test(table(
      azavea_section, treat8))$p.value, 2L),
      rep(NA, uniqueN(azavea_section) - 1L))],
    #not targeting owners, so exclude
    `\\# Owners` = NA)))),
  include.colnames = FALSE, comment = FALSE, 
  #setting sanitize.text.function prevents xtable from
  #  commenting out the math markup (especially $). This
  #  is also why we use tex = TRUE for dol.form.
  sanitize.text.function = identity, only.contents = TRUE,
  floating = TRUE, hline.after = c(0L, 1L, 3L, 9L))

cat("\\hline",
    "\\multicolumn{10}{l}" %+% 
      "{\\scriptsize{$p$-values in rows 1-2 are " %+% 
      "$F$-test $p$-values from regressing each " %+% 
      "variable on treatment dummies. A $\\chi^2$ " %+% 
      "test was used for the geographic distribution.}} \\\\",
    "\\end{tabular}",
    "\\end{sidewaystable}", sep = "\n")}

# TABLE 2: Short-term Linear Probability Model Estimates ####
tbl <- capture.output(texreg(lapply(lapply(expression(
  `One Month` = ever_paid_jul, `Three Months` = ever_paid_sep,
  `One Month` = paid_full_jul, `Three Months` = paid_full_sep),
  #Multiply indicator by 100 so the units are in %ages already
  function(x) owners[(unq_own), lm(I(100 * eval(x)) ~ treat8)]), 
  rename_coef, nn = 8), stars = c(.01, .05, .1), 
  include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L, 
  label = "sh_lin", float.pos = 'htb',
  caption = "Short-Term Linear Probability Model Estimates",
  custom.note = "%stars. Holdout values in levels; " %+% 
    "remaining figures relative to this"))

## Replace Holdout SEs with horizontal rule, 
##   eliminate significance for intercept,
##   add header for Ever Paid vs. Paid in Full
idx <- grep("^Holdout", tbl)

tbl[idx] <- gsub("\\^\\{[*]*\\}", "", tbl[idx])

tbl <- c(tbl[1L:(idx - 3L)],
         " & \\multicolumn{2}{c}{Ever Paid} & " %+% 
           "\\multicolumn{2}{c}{Paid in Full} \\\\",
         tbl[c(idx - 2L, idx)],
         "\\hline", tbl[(idx + 2L):length(tbl)])

cat(tbl, sep = "\n")

# TABLE 3: Short-term Reults: Relative to Generic Reminder ####
tbl <- capture.output(texreg(lapply(lapply(expression(
  `One Month` = ever_paid_jul, `Three Months` = ever_paid_sep,
  `One Month` = paid_full_jul, `Three Months` = paid_full_sep),
  #Multiply indicator by 100 so the units are in %ages already
  function(x) owners[(!holdout & unq_own), lm(I(100 * eval(x)) ~ treat7)]), 
  rename_coef, nn = 7), stars = c(.01, .05, .1), 
  include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L, 
  label = "sh_lpm_rob", float.pos = 'htb',
  caption = "Short-term Results: Relative to Generic Reminder",
  custom.note = "%stars. Reminder values in levels; " %+% 
    "remaining figures relative to this"))

## Replace Reminder SEs with horizontal rule, 
##   eliminate significance for intercept,
##   add header for Ever Paid vs. Paid in Full
idx <- grep("^Reminder", tbl)

tbl[idx] <- gsub("\\^\\{[*]*\\}", "", tbl[idx])

tbl <- c(tbl[1L:(idx - 3L)],
         " & \\multicolumn{2}{c}{Ever Paid} & " %+% 
           "\\multicolumn{2}{c}{Paid in Full} \\\\",
         tbl[c(idx - 2L, idx)],
         "\\hline", tbl[(idx + 2L):length(tbl)])

cat(tbl, sep = "\n")

# TABLE 4: Long-Term Linear Probability Model Estimates ####
tbl <- capture.output(texreg(lapply(lapply(expression(
  `Ever Paid` = ever_paid_dec, `Paid in Full` = paid_full_dec,
  `Ever Paid` = ever_paid_jul16, `Paid in Full` = paid_full_jul16),
  function(x) owners[(unq_own), lm(I(100 * eval(x)) ~ treat8)]),
  rename_coef, nn = 8), stars = c(.01, .05, .1), 
  include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L,
  label = "ltmpme", float.pos = 'htb',
  caption = "Long-Term Linear Probability Model Estimates",
  custom.note = "%stars. Holdout values in levels; " %+% 
    "remaining figures relative to this"))

## Replace Holdout SEs with horizontal rule, add header for EP vs. PF
idx <- grep("^Holdout", tbl)

tbl[idx] <- gsub("\\^\\{[*]*\\}", "", tbl[idx])

tbl <- c(tbl[1L:(idx - 3L)],
         " & \\multicolumn{2}{c}{Six Months} & " %+% 
           "\\multicolumn{2}{c}{Subsequent Tax Cycle} \\\\",
         tbl[c(idx - 2L, idx)],
         "\\hline", tbl[(idx + 2L):length(tbl)])

cat(tbl, sep = "\n")

# TABLE 5: Three Month Impact of Collection ``Nudges"* ####
## **TO DO: CONFORM TO AER SUBMISSION**
note = 
  paste('\\scriptsize* Sample Size are the number of single property',
        'tax payers in the treatment group.  Total Taxes Owed is the',
        ' total taxes owed by single property tax payers in the',
        'treatment group. New Payers equals the new payers after three',
        'months computed as the estimated increase in rate of compliance of',
        'those receiving the letter over those in the holdout sample as',
        'reported in Table 2; for example, for the reminder letter the number',
        'of new payers equals 95 = .039 x2,419.  Revenue per letter for each',
        'treatment equals the median new revenue collected from those who',
        'received a treatment letter and made some payment (=$738/letter)',
        'times the three month increase in compliance from each treatment',
        'letter; for example for the reminder letter the median estimated',
        'revenue per letter equals $28.79 = .039x$738.  New revenues for',
        'each treatment equals the revenue/letter times the number of single',
        'owner properties receiving a treatment letter: for example, for the',
        'reminder letter the estimated total new revenues equals $69,643 =',
        '$28.79x2,419.  New % of Taxes Paid equals New Revenues Divided by',
        'Total Taxes Owed; for example, for the reminder letter .023 =',
        '$69,643/$3,038,000.')
print(xtable(
  #Use keyby to make sure the output is sorted and Holdout comes first
  owners[(unq_own), .(.N, ep = mean(ever_paid_sep), 
                      owed = sum(total_due)/1e6), keyby = treat8
         #Express relative to Holdout
         ][ , {
           hld = .SD[1L]
           .SD[-1L, {
             #treatment delta
             delta = ep - hld$ep
             np = round(delta*N)
             #Get median positive payment by December
             mpp = owners[(unq_own & total_paid_sep > 0), 
                          median(total_paid_sep)]
             rev_per = delta * mpp
             newrev = N * rev_per
             .(Treatment = c(paste0(treat8), 'Totals'), 
                `Sample Size` = prettyNum(c(N, sum(N)), big.mark = ','),
                `Total Taxes Owed` = sprintf('$%.3f M', c(owed, sum(owed))),
                `New Payers` = c(np, sum(np)),
                `Revenue/Letters` = c(dol.form(rev_per,  dig = 2L), '-'), 
                `New Revenues` = dol.form(c(newrev, sum(newrev))),
                `New % of Taxes Paid` = 
                  round(c(newrev/owed, sum(newrev)/sum(owed)), 3L))}]}],
  caption = "Three Month Impact of Collection ``Nudges''*",
  label = "sh_rev", align = "rlcccccc",
  digits = c(0, 0, 0, 0, 0, 0, 0, 3)), hline.after = c(0L, 1L),
  add.to.row = list(pos = list(7L, 8L),
                    command = c('\\hline\n   \\hline\n',
                                paste0('\\hline\n',
                                       '\\multicolumn{7}{p{1\\textwidth}}{',
                                       note, '}\n'))),
  table.placement = 'htb', include.rownames = FALSE,
  comment = FALSE, caption.placement = "top")

# TABLE A1: Robustness Analysis: Relative to Reminder (All Owners) ####
tbl <- capture.output(texreg(lapply(lapply(expression(
  `One Month` = ever_paid_jul, `Three Months` = ever_paid_sep,
  `One Month` = paid_full_jul, `Three Months` = paid_full_sep),
  #Multiply indicator by 100 so the units are in %ages already
  function(x) owners[(!holdout), lm(I(100 * eval(x)) ~ treat7)]), 
  rename_coef, nn = 7), stars = c(.01, .05, .1), 
  include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L, 
  label = "sh_lpm_mult", float.pos = 'htbp',
  caption = "Robustness Analysis: Relative to Reminder (Multiple Owners)",
  custom.note = "%stars. Reminder values in levels; " %+% 
    "remaining figures relative to this"))

## Replace Reminder SEs with horizontal rule, 
##   eliminate significance for intercept,
##   add header for Ever Paid vs. Paid in Full
idx <- grep("^Reminder", tbl)

tbl[idx] <- gsub("\\^\\{[*]*\\}", "", tbl[idx])

tbl <- c(tbl[1L:(idx - 3L)],
         " & \\multicolumn{2}{c}{Ever Paid} & " %+% 
           "\\multicolumn{2}{c}{Paid in Full} \\\\",
         tbl[c(idx - 2L, idx)],
         "\\hline", tbl[(idx + 2L):length(tbl)])

cat(tbl, sep = "\n")

# Table A2: Balance on Observables ####
{cat("\\begin{sidewaystable}[ht]",
    "\\centering", 
    "\\caption{Balance on Observables}",
    "\\label{balance2}",
    "\\begin{tabular}{lrrrrrrrc}", 
    "\\hline",
    "\\multicolumn{9}{c}{Unary Owners} \\\\", sep = "\n")

##Top Section: Unique Owners Only
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
  #exclude table header since we're combining two tables;
  #  setting sanitize.text.function prevents xtable from
  #  commenting out the math markup (especially $). This
  #  is also why we use tex = TRUE for dol.form.
  sanitize.text.function = identity, only.contents = TRUE,
  floating = TRUE, hline.after = c(0L, 1L))

##Bottom Section: Exclude Holdout Only
cat("\\hline",
    "\\multicolumn{9}{c}{Unary and Multiple Owners} \\\\", sep = "\n")

print.xtable(xtable(cbind(t(
  owners[(!holdout),
         .(`Amount Due (June)` = dol.form(mean(total_due), tex = TRUE),
           `Assessed Property Value` = 
             dol.form(mean(assessed_mv, na.rm = TRUE), tex = TRUE),
           `\\% with Unary Owner` = to.pct(mean(unq_own), 1L),
           `\\% Overlap with Holdout` = to.pct(mean(flag_holdout_overlap), 2L),
           `\\# Properties per Owner` = round(mean(N), 2L),
           `\\# Owners` = prettyNum(.N, big.mark = ",")),
         keyby = .(Variable = treat7)]), 
  p_tex(c(sapply(c(
    `Amount Due (June)` = "total_due",
    `Assessed Property Value` = "assessed_mv",
    `\\% with Unary Owner` = "unq_own",
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

# TABLE A3: Short-Term Logistic Model Estimates (Unary Owners) ####
tbl <- capture.output(texreg(lapply(lapply(expression(
  `One Month` = ever_paid_jul, `Three Months` = ever_paid_sep,
  `One Month` = paid_full_jul, `Three Months` = paid_full_sep),
  #Multiply indicator by 100 so the units are in %ages already
  function(x) owners[(unq_own), glm(eval(x) ~ treat8, family = binomial)]), 
  rename_coef, nn = 8), stars = c(.01, .05, .1), 
  include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L, 
  label = "sh_logit", float.pos = 'htbp',
  caption = "Short-Term Logistic Model Estimates (Unary Owners)",
  custom.note = "%stars. Holdout values in levels; " %+% 
    "remaining figures relative to this"))

## Replace Holdout SEs with horizontal rule, 
##   eliminate significance for intercept,
##   add header for Ever Paid vs. Paid in Full
idx <- grep("^Holdout", tbl)

tbl[idx] <- gsub("\\^\\{[*]*\\}", "", tbl[idx])

tbl <- c(tbl[1L:(idx - 3L)],
         " & \\multicolumn{2}{c}{Ever Paid} & " %+% 
           "\\multicolumn{2}{c}{Paid in Full} \\\\",
         tbl[c(idx - 2L, idx)],
         "\\hline", tbl[(idx + 2L):length(tbl)])

cat(tbl, sep = "\n")

# TABLE A4: Logit Estimates Including Multiple Owners ####
## **TO DO: TABLE A4 **