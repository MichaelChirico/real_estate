#Data Analysis
#Philadelphia Real Estate Tax Evasion
#  Round 2
#Michael Chirico

# Setup: Packages, Directories, Data Import ####

##Packages
rm(list = ls(all = TRUE))
gc()
library(funchir) #convenience functions
library(plm) #for clustered SEs
library(pglm) #for clustered logit SEs
library(data.table) #for everything
library(xtable) #for table output
library(texreg) #for regression output
library(sandwich) #for robust SEs
library(lmtest) #for testing
write.packages('logs/round_two/analysis_session.txt')

tf = 'round_two/tables.tex'

##Exclude the top two blocks?
excludeTopBlocks = FALSE

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
owners <- fread('data/round_two_analysis_owners.csv')

#exclude top 2 randomization blocks
if (excludeTopBlocks) owners = owners[(holdout | rand_id > 2)]

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
cat("\\begin{sidewaystable}[ht]",
    "\\centering", 
    "\\caption{Balance on Observables (Unary Owners)}",
    "\\label{balance}",
    "\\vspace{10mm}",
    "\\begin{tabular}{lrrrrrrrrc}", 
    "\\hline", sep = "\n", file = tf)

tbl = owners[(unq_own),
              c(list(`Amount Due (June)` = 
                       dol.form(mean(total_due), tex = TRUE),
                     `Amount Due (June)` = 
                       dol.form(sd(total_due), tex = TRUE),
                    `Assessed Property Value` = 
                      dol.form(mean(assessed_mv, na.rm = TRUE), tex = TRUE),
                    `Assessed Property Value` = 
                      dol.form(sd(assessed_mv, na.rm = TRUE), tex = TRUE),
                    `Ownership Tenure (Years)` = 
                      round(mean(tenure, na.rm = TRUE), 1L),
                    `Ownership Tenure (Years)` = 
                      round(sd(tenure, na.rm = TRUE), 1L)),
               as.list(to.pct(table(azavea_section)/.N, dig = 0L)),
               list(`\\# Owners` = prettyNum(.N, big.mark = ","))),
             keyby = .(Variable = treat8)]

tbl = cbind(t(tbl), 
            p_tex(c(sapply(c(
              `Amount Due (June)` = "total_due",
              `Amount Due (June)` = "total_due",
              `Assessed Property Value` = "assessed_mv",
              `Assessed Property Value` = "assessed_mv",
              `Ownership Tenure (Years)` = 'tenure',
              `Ownership Tenure (Years)` = 'tenure'),
              function(x) owners[(unq_own), lmfp(get(x) ~ treat8)]),
              owners[(unq_own), c(round(chisq.test(table(
                azavea_section, treat8))$p.value, 2L),
                rep(NA, uniqueN(azavea_section) - 1L))],
              #not targeting owners, so exclude
              `\\# Owners` = NA)))

sdrows = duplicated(rownames(tbl))
pvcol = grep('$p$-value', tbl[1L, ], fixed = TRUE)
pctrows = grepl('Philadelphia|City', rownames(tbl))
tbl[sdrows, -pvcol] = paste0('(', tbl[sdrows, -pvcol], ')')
tbl[pctrows, -pvcol] = paste0(tbl[pctrows, -pvcol], '\\%')
tbl[sdrows, pvcol] = ''
tbl = cbind(rownames(tbl), tbl)
tbl[sdrows, 1L] = NA
dimnames(tbl) = list(NULL, NULL)

seprows = grep('Center City', rownames(tbl)) + c(-1L, 5L)
print.xtable(xtable(tbl), include.rownames = FALSE,
             include.colnames = FALSE, comment = FALSE, 
             #setting sanitize.text.function prevents xtable from
             #  commenting out the math markup (especially $). This
             #  is also why we use tex = TRUE for dol.form.
             sanitize.text.function = identity, only.contents = TRUE,
             floating = TRUE, hline.after = c(0L, 1L, seprows),
             file = tf, append = TRUE)

cat("\\hline",
    "\\multicolumn{10}{l}" %+% 
      "{\\scriptsize{$p$-values in rows 1-2 are " %+% 
      "$F$-test $p$-values from regressing each " %+% 
      "variable on treatment dummies. A $\\chi^2$ " %+% 
      "test was used for the geographic distribution. " %+% 
      "Standard deviations in parentheses.}} \\\\",
    "\\end{tabular}",
    "\\end{sidewaystable}", sep = "\n", file = tf, append = TRUE)

# TABLE 2: Short-term Linear Probability Model Estimates ####
regs = lapply(expression(
  #Multiply indicator by 100 so the units are in %ages already
  `One Month` = 100*ever_paid_jul, `Three Months` = 100*ever_paid_sep,
  `One Month` = 100*paid_full_jul, `Three Months` = 100*paid_full_sep,
  `One Month` = total_paid_jul, `Three Months` = total_paid_sep),
  function(x) 
    rename_coef(owners[(unq_own), lm(eval(x) ~ treat8)], 8))
ses = lapply(regs, function(r) sqrt(diag(vcovHC(r))))
pvals = lapply(regs, function(r)
  coeftest(r, vcovHC(r))[ , 'Pr(>|t|)'])
tbl <- capture.output(texreg(
  regs, stars = c(.01, .05, .1), 
  override.se = ses, override.pvalues = pvals,
  include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L, 
  label = "sh_lin", float.pos = 'htb',
  caption = "Short-Term Linear Probability Model Estimates",
  custom.note = "%stars. Robust standard errors. " %+% 
    "Holdout values in levels; remaining figures relative to this"))

## Replace Holdout SEs with horizontal rule, 
##   eliminate significance for intercept,
##   add header for Ever Paid vs. Paid in Full
idx <- grep("^Holdout", tbl)

tbl[idx] <- gsub("\\^\\{[*]*\\}", "", tbl[idx])

tbl <- c(tbl[1L:(idx - 3L)],
         " & \\multicolumn{2}{c}{Ever Paid} & " %+% 
           "\\multicolumn{2}{c}{Paid in Full} & " %+%
           "\\multicolumn{2}{c}{Total Paid} \\\\",
         tbl[c(idx - 2L, idx)],
         "\\hline", tbl[(idx + 2L):length(tbl)])

cat(tbl, sep = "\n", file = tf, append = TRUE)

# TABLE 3: Short-term Reults: Relative to Generic Reminder ####
powners_unq_all = 
  pdata.frame(owners[!holdout & unq_own], 
              index = 'rand_id', drop.index = FALSE)
regs = lapply(expression(
  `One Month` = 100*ever_paid_jul, `Three Months` = 100*ever_paid_sep,
  `One Month` = 100*paid_full_jul, `Three Months` = 100*paid_full_sep,
  `One Month` = total_paid_jul, `Three Months` = total_paid_sep),
  #Multiply indicator by 100 so the units are in %ages already
  function(x) 
    plm(eval(x) ~ treat7, data = powners_unq_all, model = 'pooling'))
# per the guide here:
#  http://www.richard-bluhm.com/clustered-ses-in-r-and-stata-2/
n_clust = uniqueN(powners_unq_all$rand_id)
n_obs = nrow(powners_unq_all)
#since all vs. treat7, there are 7 degrees of
#  freedom lost in each regression (hence n_obs- 7)
dof_adj = n_clust/(n_clust - 1) * (n_obs - 1)/(n_obs - 7)
ses = lapply(regs, function(r) 
  sqrt(diag(dof_adj * vcovHC(r, type = 'HC0', cluster = 'group', 
                             adjust = TRUE))))
pvals = lapply(regs, function(r)
  coeftest(r, dof_adj * vcovHC(r, type = 'HC0', cluster = 'group', 
                             adjust = TRUE))[ , 'Pr(>|t|)'])
#rename now since vcovHC somehow recovers the
#  original coefficient names, causing conflict
#  and leading to empty p values
regs = lapply(regs, rename_coef, 7L)
tbl <- capture.output(texreg(
  regs, stars = c(.01, .05, .1), 
  override.se = ses, override.pvalues = pvals,
  include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L, 
  label = "sh_lpm_rob", float.pos = 'htb',
  caption = "Short-term Results: Relative to Generic Reminder",
  custom.note = "%stars. Standard errors clustered by block." %+% 
    "Reminder values in levels; remaining figures relative to this"))

## Replace Reminder SEs with horizontal rule, 
##   eliminate significance for intercept,
##   add header for Ever Paid vs. Paid in Full
idx <- grep("^Reminder", tbl)

tbl[idx] <- gsub("\\^\\{[*]*\\}", "", tbl[idx])

tbl <- c(tbl[1L:(idx - 3L)],
         " & \\multicolumn{2}{c}{Ever Paid} & " %+% 
           "\\multicolumn{2}{c}{Paid in Full} & " %+% 
           "\\multicolumn{2}{c}{Total Paid} \\\\",
         tbl[c(idx - 2L, idx)],
         "\\hline", tbl[(idx + 2L):length(tbl)])

cat(tbl, sep = "\n", file = tf, append = TRUE)

# TABLE 4: Long-Term Linear Probability Model Estimates ####
regs = lapply(expression(
  `Ever Paid` = 100*ever_paid_dec, `Paid in Full` = 100*paid_full_dec,
  `Total Paid` = total_paid_dec, `Ever Paid` = 100*ever_paid_jul16,
  `Paid in Full` = 100*paid_full_jul16, `Total Paid` = total_paid_jul16),
  function(x) rename_coef(owners[(unq_own), lm(eval(x) ~ treat8)], 8))
ses = lapply(regs, function(r) sqrt(diag(vcovHC(r))))
pvals = lapply(regs, function(r)
  coeftest(r, vcovHC(r))[ , 'Pr(>|t|)'])
tbl <- capture.output(texreg(
  regs, stars = c(.01, .05, .1), 
  override.se = ses, override.pvalues = pvals,
  include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L,
  label = "ltmpme", float.pos = 'htb',
  caption = "Long-Term Linear Model Estimates",
  custom.note = "%stars. Robust standard errors. " %+%
    "Holdout values in levels; remaining figures relative to this"))

## Replace Holdout SEs with horizontal rule, add header for EP vs. PF
idx <- grep("^Holdout", tbl)

tbl[idx] <- gsub("\\^\\{[*]*\\}", "", tbl[idx])

tbl <- c(tbl[1L:(idx - 3L)],
         " & \\multicolumn{3}{c}{Six Months} & " %+% 
           "\\multicolumn{3}{c}{Subsequent Tax Cycle} \\\\",
         tbl[c(idx - 2L, idx)],
         "\\hline", tbl[(idx + 2L):length(tbl)])

cat(tbl, sep = "\n", file = tf, append = TRUE)

# TABLE 5: Three Month Impact of Collection ``Nudges"* ####
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
        'received a treatment letter and made some payment (=\\$738/letter)',
        'times the three month increase in compliance from each treatment',
        'letter; for example for the reminder letter the median estimated',
        'revenue per letter equals \\$28.79 = .039x\\$738.  New revenues for',
        'each treatment equals the revenue/letter times the number of single',
        'owner properties receiving a treatment letter: for example, for the',
        'reminder letter the estimated total new revenues equals \\$69,643 =',
        '\\$28.79x2,419. New \\% of Taxes Paid equals New Revenues Divided by',
        'Total Taxes Owed; for example, for the reminder letter .023 =',
        '\\$69,643/\\$3,038,000.')
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
  comment = FALSE, caption.placement = "top",
  file = tf, append = TRUE)

# TABLE A1: Robustness Analysis: Relative to Reminder (All Owners) ####
powners_all = 
  pdata.frame(owners[(!holdout)], index = 'rand_id', drop.index = FALSE)
regs = lapply(expression(
  `One Month` = 100*ever_paid_jul, `Three Months` = 100*ever_paid_sep,
  `One Month` = 100*paid_full_jul, `Three Months` = 100*paid_full_sep,
  `One Month` = total_paid_jul, `Three Months` = total_paid_sep),
  #Multiply indicator by 100 so the units are in %ages already
  function(x) 
    plm(eval(x) ~ treat7, data = powners_all, model = 'pooling'))
n_clust = uniqueN(powners_all$rand_id)
n_obs = nrow(powners_all)
#since all vs. treat7, there are 7 degrees of
#  freedom lost in each regression (hence n_obs- 7)
dof_adj = n_clust/(n_clust - 1) * (n_obs - 1)/(n_obs - 7)
ses = lapply(regs, function(r) 
  sqrt(diag(dof_adj * vcovHC(r, type = 'HC0', cluster = 'group', 
                             adjust = TRUE))))
pvals = lapply(regs, function(r)
  coeftest(r, dof_adj * vcovHC(r, type = 'HC0', cluster = 'group', 
                             adjust = TRUE))[ , 'Pr(>|t|)'])
regs = lapply(regs, rename_coef, 7L)
tbl <- capture.output(texreg(
  regs, stars = c(.01, .05, .1), 
  override.se = ses, override.pvalues = pvals,
  include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L, 
  label = "sh_lpm_mult", float.pos = 'htbp',
  caption = "Robustness Analysis: Relative to Reminder (Multiple Owners)",
  custom.note = "%stars. Standard errors clustered by block." %+% 
    "Reminder values in levels; remaining figures relative to this"))

## Replace Reminder SEs with horizontal rule, 
##   eliminate significance for intercept,
##   add header for Ever Paid vs. Paid in Full
idx <- grep("^Reminder", tbl)

tbl[idx] <- gsub("\\^\\{[*]*\\}", "", tbl[idx])

tbl <- c(tbl[1L:(idx - 3L)],
         " & \\multicolumn{2}{c}{Ever Paid} & " %+% 
           "\\multicolumn{2}{c}{Paid in Full} & " %+% 
           "\\multicolumn{2}{c}{Total Paid} \\\\",
         tbl[c(idx - 2L, idx)],
         "\\hline", tbl[(idx + 2L):length(tbl)])

cat(tbl, sep = "\n", file = tf, append = TRUE)

# Table A2: Balance on Observables ####
cat("\\begin{sidewaystable}[htbp]",
    "\\centering", 
    "\\caption{Balance on Observables}",
    "\\label{balance2}",
    "\\begin{tabular}{lrrrrrrrc}", 
    "\\hline",
    "\\multicolumn{9}{c}{Unary Owners} \\\\", 
    sep = "\n", file = tf, append = TRUE)

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
  floating = TRUE, hline.after = c(0L, 1L),
  file = tf, append = TRUE)

##Bottom Section: Exclude Holdout Only
cat("\\hline",
    "\\multicolumn{9}{c}{Unary and Multiple Owners} \\\\", 
    sep = "\n", file = tf, append = TRUE)

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
  only.contents = TRUE, hline.after = c(0L, 1L),
  file = tf, append = TRUE)

cat("\\hline",
    "\\multicolumn{9}{l}" %+% 
      "{\\scriptsize{$p$-values in rows 1-5 are " %+% 
      "$F$-test $p$-values from regressing each " %+% 
      "variable on treatment dummies. A $\\chi^2$ " %+% 
      "test was used for the count of owners.}} \\\\",
    "\\end{tabular}",
    "\\end{sidewaystable}", sep = "\n", file = tf, append = TRUE)

# TABLE A3: Short-Term Logistic Model Estimates (Unary Owners) ####
regs = lapply(expression(
  `One Month` = ever_paid_jul, `Three Months` = ever_paid_sep,
  `One Month` = paid_full_jul, `Three Months` = paid_full_sep),
  function(x) owners[(unq_own), glm(eval(x) ~ treat8, family = binomial)])
ses = lapply(regs, function(r) sqrt(diag(vcovHC(r))))
pvals = lapply(regs, function(r)
  coeftest(r, vcovHC(r))[ , 'Pr(>|z|)'])
regs = lapply(regs, rename_coef, 8L)
tbl <- capture.output(texreg(
  regs, stars = c(.01, .05, .1),
  override.se = ses, override.pvalues = pvals,
  include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L, 
  label = "sh_logit", float.pos = 'htbp',
  caption = "Short-Term Logistic Model Estimates (Unary Owners)",
  custom.note = "%stars. Robust standard errors. " %+% 
    "Holdout values in levels; remaining figures relative to this"))

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

cat(tbl, sep = "\n", file = tf, append = TRUE)

# TABLE A4: Logit Estimates Including Multiple Owners ####
# TABLE A4: Logit Estimates Including Multiple Owners ####
tbl <- capture.output(texreg(lapply(
  list(`One Month` = 
         owners[ , glm(ever_paid_jul ~ treat7, family = binomial)],
       `Three Months` = 
         owners[ , glm(ever_paid_sep ~ treat7, family = binomial)],
       `One Month` = 
         owners[(unq_own), glm(ever_paid_jul ~ treat7, family = binomial)],
       `Three Months` = 
         owners[(unq_own), glm(ever_paid_sep ~ treat7, family = binomial)]),
  rename_coef, nn = 7), stars = c(.01, .05, .1), 
  include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 2L, 
  label = "sh_logit_rob", float.pos = 'htbp', omit.coef = 'Reminder',
  caption = "Logit Estimates Including Multiple Owners"))

idx = grep('One Month', tbl, fixed = TRUE) - 1L

tbl <- c(tbl[1L:idx],
         " & \\multicolumn{2}{c}{All Owners} & " %+% 
           "\\multicolumn{2}{c}{Unary Owners} \\\\",
         tbl[(idx + 1L):length(tbl)])

cat(tbl, sep = "\n", file = tf, append = TRUE)

# SANDBOX ####
owners[(unq_own), debt_quartile := create_quantiles(total_due, 4)]

rename_coef = function(obj) {
  nm = names(obj$coefficients)
  int = grep('Intercept', nm)
  qint = grep('.*quartile[1-4]$', nm)
  keep = grep('quartile.*(Lien|Sheriff)', nm)
  nm[-c(int, qint, keep)] = 'x'
  nm[int] = 'Holdout in Quartile 1'
  nm[qint] = gsub('.*quartile([1-4])$', 
                  'Holdout in Quartile \\1', nm[qint])
  nm[keep] = gsub('.*quartile([1-4]):.*8(.*)$', 
                  '\\2 in Quartile \\1', nm[keep])
  names(obj$coefficients) = nm
  obj
}

tbl = capture.output({
  owners[(unq_own), {
    regs = lapply(expression(
      `1 Month` = 100*ever_paid_jul, `3 Months` = 100*ever_paid_sep,
      `6 Months` = 100*ever_paid_dec, `1 Month` = total_paid_jul,
      `3 Months` = total_paid_sep, `6 Months` = total_paid_dec),
      function(dep_var) 
        rename_coef(lm(eval(dep_var) ~ debt_quartile/treat8)))
    ses = lapply(regs, function(r) sqrt(diag(vcovHC(r))))
    pvals = lapply(regs, function(r)
      coeftest(r, vcovHC(r))[ , 'Pr(>|t|)'])
    texreg(regs, omit.coef = 'x', stars = c(.01, .05, .1),
           override.se = ses, override.pvalues = pvals,
           include.rsquared = FALSE, caption.above = TRUE,
           include.adjrs = FALSE, include.rmse = FALSE,
           digits = 1L, label = 'tbl:lpm_hetero',
           float.pos = 'htbp',
           caption = 'Treatment Effect Heterogeneity by Debt Quantile',
           custom.note = "\\parbox{.75\\linewidth}{%stars. Holdout values " %+% 
             "for first quartile in levels; other holdout figures are " %+% 
             "relative to this and remaining figures are " %+% 
             "treatment effects for the stated treatment vs. holdout " %+% 
             "owners in the same quartile.}")
    }]
})

## Replace Holdout SEs with horizontal rule, add header for EP vs. TP
idx1 <- grep("Month", tbl) - 1L
idx2 <- grep("^Holdout.*Quartile\\s1", tbl)

tbl[idx2] <- gsub("\\^\\{[*]*\\}", "", tbl[idx2])

tbl <- c(tbl[1L:idx1],
         paste(" & \\multicolumn{3}{c}{Ever Paid} &",
               "\\multicolumn{3}{c}{Total Paid}", "\\\\"),
         tbl[(idx1 + 1L):idx2], "\\hline",
         tbl[(idx2 + 2L):length(tbl)])

cat(tbl, sep = "\n", file = tf, append = TRUE)
