#Data Analysis
#Philadelphia Real Estate Tax Evasion
#  Round 2
#Michael Chirico

# Setup: Packages, Directories, Data Import ####

##Packages
library(funchir)    # convenience functions
library(plm)        # for cluster-robust SEs
library(pglm)       # for clustered logit SEs
library(data.table) # for everything
library(xtable)     # for table output
library(texreg)     # for regression output
library(sandwich)   # for robust SEs
library(lmtest)     # for testing using robust SEs
library(magrittr)   # for beautifying hyperfunctional code

write.packages('logs/round_two/analysis_session.txt')

tex_file = 'round_two/tables.tex'

##Exclude the top two blocks?
excludeTopBlocks = FALSE

##Specialized Functions
###Get the p-value on the full-regression F-test of an OLS call
lmfp <- function(formula){
  #extract F-statistic & Degrees of Freedom from LM call
  mdf <- summary.lm(do.call(lm, list(formula = formula)))$fstatistic
  #copied (ported) from print.summary.lm
  unname(pf(mdf[1L], df1 = mdf[2L], df2 = mdf[3L], lower.tail = FALSE))
}

###Round a p-value to two digits (we only use two-digit rounding
###  in this paper), append with a TeX-marked-up label
p_tex <- function(x) c("$p$-value", round(x, 2L))

###Specially tailor the lm object for pretty printing
rename_coef <- function(obj, nn, name_only = FALSE){
  nms <- names(obj$coefficients)
  nms[nms == "(Intercept)"] <- if (nn == 7) "Reminder" else "Holdout"
  #treatment dummies take the form treat8Reminder, etc -- 
  #  eliminate the treat8 part for digestibility
  nms <- gsub("treat" %+% nn, "", nms)
  if (name_only) return(nms)
  names(obj$coefficients) <- nms
  obj
}

rename_coef_qtl = function(obj) {
  nm = names(obj$coefficients)
  int = grep('Intercept', nm)
  qint = grep('.*quartile[1-4]$', nm)
  treat = grep('treat8', nm)
  nm[int] = 'Holdout in Quartile 1'
  nm[qint] = gsub('.*quartile([1-4])$', 
                  'Holdout in Quartile \\1', nm[qint])
  nm[treat] = gsub('.*quartile([1-4]):.*8(.*)$', 
                  '\\2 in Quartile \\1', nm[treat])
  names(obj$coefficients) = nm
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

# TABLE 1: Balance on Observables ####
##Print Table Header
### *surround with {} so the table all prints together*
cat("\\begin{sidewaystable}[htbp]",
    "\\centering", 
    "\\caption{Balance on Observables}",
    "\\label{balance}",
    "\\vspace{10mm}",
    "\\begin{tabular}{lrrrrrrrrc}", 
    sep = "\n", file = tex_file)

tbl = owners[(unq_own), {
  c(list(`Amount Due` = dol.form(mean(total_due), tex = TRUE),
         `Amount Due` = dol.form(sd(total_due), tex = TRUE),
         `Prop. Value` = 
           dol.form(floor(mean(assessed_mv, na.rm = TRUE)/1000), tex = TRUE),
         `Prop. Value` = 
           dol.form(floor(sd(assessed_mv, na.rm = TRUE)/1000), tex = TRUE),
         `Years Tenure` = round(mean(tenure, na.rm = TRUE), 1L),
         `Years Tenure` = round(sd(tenure, na.rm = TRUE), 1L)),
    as.list(to.pct(table(azavea_section)/.N, dig = 0L)),
    list(`\\# Owners` = prettyNum(.N, big.mark = ",")))
  }, keyby = .(Variable = treat8)]
#relegate treatment names to footnote for brevity
v = tbl$Variable
tbl[ , Variable := seq_along(Variable)]
  

tbl = cbind(
  t(tbl), 
  p_tex(c(
    c(`Amount Due` = "total_due",
      `Prop. Value` = "assessed_mv", 
      `Years Tenure` = 'tenure') %>% 
      sapply(function(x) owners[(unq_own), lmfp(get(x) ~ treat8)]) %>% 
      rep(each = 2L),
    owners[(unq_own), {
      pv = table(azavea_section, treat8) %>% 
        chisq.test %>% extract2('p.value') %>% round(2L) %>%
        c(rep(NA_real_, uniqueN(azavea_section) - 1L))
    }],
    #not targeting owners, so exclude
    `\\# Owners` = NA_real_
  ))
)

sdrows = which(duplicated(rownames(tbl)))
pvcol = grep('$p$-value', tbl[1L, ], fixed = TRUE)
pctrows = grepl('Philadelphia|City', rownames(tbl))
tbl[sdrows, -pvcol] = sprintf('(%s)', tbl[sdrows, -pvcol])
tbl[pctrows, -pvcol] = paste0(tbl[pctrows, -pvcol], '\\%')
tbl[sdrows, pvcol] = ''
tbl = cbind(gsub('Philadelphia', 'Philly', 
                 rownames(tbl), fixed = TRUE), tbl)
tbl[sdrows, 1L] = NA_character_
dimnames(tbl) = list(NULL, NULL)

seprows = grep('Center City', rownames(tbl)) + c(-1L, 5L)
print.xtable(xtable(tbl), include.rownames = FALSE,
             include.colnames = FALSE, comment = FALSE, 
             #setting sanitize.text.function prevents xtable from
             #  commenting out the math markup (especially $). This
             #  is also why we use tex = TRUE for dol.form.
             sanitize.text.function = identity, only.contents = TRUE,
             floating = TRUE, hline.after = c(0L, 1L, seprows),
             file = tex_file, append = TRUE)

cat(
  "\\hline",
  "\\multicolumn{10}{l}{\\scriptsize{$p$-values in rows 1-2 are $F$-test",
  "    $p$-values from regressing each variable on treatment dummies. A",
  "    $\\chi^2$ test was used for the geographic distribution. }} \\\\",
  sprintf("\\multicolumn{10}{l}{\\scriptsize{ %s. %s }} \\\\",
          "Standard deviations in parentheses",
          "Property values are reported in \\$1000. "),
  sprintf("\\multicolumn{10}{l}{\\scriptsize{%s}} \\\\",
          paste(paste(unclass(v), v, sep = ': '), collapse = ', ')),
  "\\end{tabular}",
  "\\end{sidewaystable}", 
  sep = "\n", file = tex_file, append = TRUE
)

# TABLE 2: Short-term Linear Probability Model Estimates ####
regs = lapply(expression(
  #Multiply indicator by 100 so the units are in %ages already
  `One` = 100*ever_paid_jul, `Three` = 100*ever_paid_sep,
  `One` = 100*paid_full_jul, `Three` = 100*paid_full_sep,
  `One` = total_paid_jul, `Three` = total_paid_sep),
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
  label = "sh_lin", float.pos = 'htbp',
  caption = "Short-Term Linear Probability Model Estimates",
  custom.note = "%stars. Robust standard errors."))

## move label to the top of the table
lbl_idx = grep('\\label', tbl, fixed = TRUE)
cap_idx = grep('\\caption', tbl, fixed = TRUE)
tbl[cap_idx] = paste0(tbl[cap_idx], tbl[lbl_idx])
tbl = tbl[-lbl_idx]

## add quantifier row (split for horizontal brevity)
idx = grep('One.*Three', tbl)
tbl = c(tbl[1:idx], 
        ' & Month & Months & Month & Months & Month & Months \\\\',
        tbl[(idx + 1):length(tbl)])

## Replace Holdout SEs with horizontal rule, 
##   add $ symbol to denote currency in total paid Holdout,
##   eliminate significance for intercept,
##   add header for Ever Paid vs. Paid in Full
idx <- grep("^Holdout", tbl)

tbl[idx] <- gsub("\\^\\{[*]*\\}", "", tbl[idx])
row = strsplit(tbl[idx], ' & ')[[1L]]
row[6:7] = paste0('\\$', row[6:7])
tbl[idx] = paste(row, collapse = ' & ')

tbl <- c(tbl[1L:(idx - 4L)],
         " & \\multicolumn{2}{c}{Ever Paid} & " %+% 
           "\\multicolumn{2}{c}{Paid in Full} & " %+% 
           "\\multicolumn{2}{c}{Total Paid} \\\\",
         "\\hline", tbl[(idx - 3L):idx],
         "\\hline", 
         gsub("Neighborhood", "Neighbor.", 
              tbl[(idx + 2L):length(tbl)], fixed = TRUE))

## add second row to custom note
idx = grep("end{tabular}", tbl, fixed = TRUE)

tbl = c(tbl[1:(idx - 2L)],
        paste(tbl[idx - 1L], '\\\\'),
        sprintf("\\multicolumn{7}{l}{\\scriptsize{%s}}",
                paste("Holdout values in levels;",
                      "remaining figures relative to this.")),
        tbl[idx:length(tbl)])

cat(tbl, sep = "\n", file = tex_file, append = TRUE)

# TABLE 3: Long-Term Linear Model Estimates ####
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
  label = "lt_lin", float.pos = 'htbp',
  caption = "Long-Term Linear Model Estimates",
  custom.note = "%stars. Robust standard errors. " %+%
    "Holdout values in levels; remaining figures relative to this."))

# float label to the top
lbl_idx = grep('\\label', tbl, fixed = TRUE)
cap_idx = grep('\\caption', tbl, fixed = TRUE)
tbl[cap_idx] = paste0(tbl[cap_idx], tbl[lbl_idx])
tbl = tbl[-lbl_idx]

## Replace Holdout SEs with horizontal rule, add header for EP vs. PF
idx <- grep("^Holdout", tbl)

tbl[idx] <- gsub("\\^\\{[*]*\\}", "", tbl[idx])

tbl <- c(tbl[1L:(idx - 3L)],
         " & \\multicolumn{3}{c}{Six Months} & " %+% 
           "\\multicolumn{3}{c}{Subsequent Tax Cycle} \\\\",
         tbl[c(idx - 2L, idx)],
         "\\hline", tbl[(idx + 2L):length(tbl)])

## add second row to custom note
idx = grep("end{tabular}", tbl, fixed = TRUE)

tbl = c(tbl[1:(idx - 2L)],
        paste(tbl[idx - 1L], '\\\\'),
        sprintf("\\multicolumn{7}{l}{\\scriptsize{%s}}",
                paste("Change in sample size between long-term",
                      "and subsequent year results reflects",
                      "property dissolution for three properties.")),
        tbl[idx:length(tbl)])

cat(tbl, sep = "\n", file = tex_file, append = TRUE)

# TABLE 4: Treatment Effect Heterogeneity by Debt Quantile #### 
## ** note -- the SEs in this table are robust,
##            even though this is not mentioned in the footnote **
owners[(unq_own), debt_quartile := create_quantiles(total_due, 4L)]

tbl = capture.output({
  owners[(unq_own), {
    regs = lapply(expression(
      `One` = 100*ever_paid_jul, `Three` = 100*ever_paid_sep,
      `Six` = 100*ever_paid_dec, `One` = total_paid_jul,
      `Three` = total_paid_sep, `Six` = total_paid_dec),
      function(outcome) 
        rename_coef_qtl(lm(eval(outcome) ~ debt_quartile/treat8)))
    ses = lapply(regs, function(r) sqrt(diag(vcovHC(r))))
    pvals = lapply(regs, function(r)
      coeftest(r, vcovHC(r))[ , 'Pr(>|t|)'])
    texreg(regs, omit.coef = 'x', stars = c(.01, .05, .1),
           override.se = ses, override.pvalues = pvals,
           include.rsquared = FALSE, caption.above = TRUE,
           include.adjrs = FALSE, include.rmse = FALSE, 
           longtable = TRUE, use.packages = FALSE,
           digits = 1L, label = 'lpm_hetero',
           caption = 'Treatment Effect Heterogeneity by Debt Quantile',
           custom.note = "\\parbox{.75\\linewidth}{%stars. Holdout values " %+% 
             "for first quartile in levels; other holdout figures are " %+% 
             "relative to this and remaining figures are " %+% 
             "treatment effects for the stated treatment vs. holdout " %+% 
             "owners in the same quartile.}")
    }]
})

## add quantifier row (split for horizontal brevity), add header for EP vs. TP
### jacknife the info into one element here because it's a bit manual otherwise
idx = grep('One.*Three', tbl)
tbl[idx] = paste(sep = '\n', tbl[idx],
                 ' & Month & Months & Months & Month & Months & Months \\\\',
                 '\\hline',
                 paste(" & \\multicolumn{3}{c}{Ever Paid} &",
                       "\\multicolumn{3}{c}{Total Paid} \\\\"))

## continuation text for multi-page split
idx1 = grep('\\endfirsthead', tbl, fixed = TRUE)
tbl = c(head(tbl, idx1),
        paste('\\multicolumn{7}{c}{\\tablename\\',
              '\\thetable\\ -- \\textit{Continued from previous page}} \\\\'),
        tail(tbl, -idx1))

idx2 = grep('\\endhead', tbl, fixed = TRUE) + 1L
tbl[idx2] = 
  paste(tbl[idx2], '\\multicolumn{7}{r}{\\textit{Continued on next page}} \\\\')

idx3 = grep('\\endfoot', tbl, fixed = TRUE)
tbl = c(head(tbl, idx3),
        '\\hline', '\\endlastfoot',
        tail(tbl, -idx3))

## Replace Holdout SEs with horizontal rule
idx <- grep("^Holdout.*Quartile\\s1", tbl)

tbl[idx] <- gsub("\\^\\{[*]*\\}", "", tbl[idx])

tbl <- c(head(tbl, idx - 1L), 
         "\\hline", tbl[idx], "\\hline",
         tail(tbl, -(idx + 1L)))

## Move footer
idx1 = grep("$^{***}p<0.01$", tbl, fixed = TRUE)
idx2 = grep("\\end{longtable}", tbl, fixed = TRUE) - 1L
tbl = c(head(tbl, idx1 - 1L),
        tbl[(idx + 1L):idx2],
        '\\hline', tbl[idx1], tail(tbl, -idx2))

cat(tbl, sep = "\n", file = tex_file, append = TRUE)

# TABLE 5: Liquidity Constraints and Payment Agreements ####
tbl <- owners[(unq_own), {
  capture.output(texreg(
    rename_coef(lm(I(100 * agreement) ~ treat8), 8L),
    stars = c(.01, .05, .1), 
    include.rsquared = FALSE, caption.above = TRUE,
    include.adjrs = FALSE, include.rmse = FALSE, digits = 1L,
    label = "payment", float.pos = 'htbp',
    caption = "Liquidity Constraints and Payment Agreements"))
}]

# float label to the top
lbl_idx = grep('\\label', tbl, fixed = TRUE)
cap_idx = grep('\\caption', tbl, fixed = TRUE)
tbl[cap_idx] = paste0(tbl[cap_idx], tbl[lbl_idx])
tbl = tbl[-lbl_idx]

## drop the dummy model name
idx = grep('Model 1', tbl, fixed = TRUE) + 1L
tbl = c(head(tbl, idx - 2L), tail(tbl, -idx))

## non-\scriptsize note -- continue on next line
idx = grep('scriptsize', tbl, fixed = TRUE)
tbl[idx] = gsub('\\scriptsize{', '', tbl[idx], fixed = TRUE)
tbl[idx] = gsub('}}', '. } \\\\', tbl[idx], fixed = TRUE)
tbl = c(head(tbl, idx),
        sprintf('\\multicolumn{2}{l}{%s}',
                'Holdout values in levels; remaining figures relative to this'),
        tail(tbl, -idx))

## Replace Holdout SEs with horizontal rule, add header for EP vs. PF
idx <- grep("^Holdout", tbl)

tbl[idx] <- gsub("\\^\\{[*]*\\}", "", tbl[idx])

tbl <- c(head(tbl, idx), "\\hline", 
         tail(tbl, -(idx + 1L)))

cat(tbl, sep = "\n", file = tex_file, append = TRUE)

# TABLE 6: Revenue Implications ####
## use a format to be fed to sprintf with actual values, since some of
##   these exact numbers depend on the data specification
note_fmt = 
  paste('\\scriptsize* Sample Size is the number of single property',
        'tax payers in the treatment group.  Total Taxes Owed is the',
        'total taxes owed by single property tax payers in the',
        'treatment group. New Payers equals the new payers after three',
        'months computed as the estimated increase in rate of compliance of',
        'those receiving the letter over those in the holdout sample as',
        'reported in Table 2; for example, for the reminder letter the number',
        # {new_payers} / {delta} / {sample_size}
        'of new payers equals %d = %.3f x %s.  Revenue per letter for each',
        'treatment equals the median new revenue collected from those who',
        # {payment}
        'received a treatment letter and made some payment (=\\$%.0f/letter)',
        'times the three month increase in compliance from each treatment',
        'letter; for example for the reminder letter the median estimated',
        # {per_letter} / {delta} / {payment}
        'revenue per letter equals \\$%.2f = %.3f x \\$%.0f.  New revenues for',
        'each treatment equals the revenue/letter times the number of single',
        'owner properties receiving a treatment letter: for example, for the',
        'reminder letter the estimated total new revenues equals',
        # {new_rev} / {per_letter} / {sample_size}
        '\\$%s = \\$%.2f x %s.  New \\%% of Taxes Paid equals New Revenues',
        'divided by Total Taxes Owed; for example, for the reminder letter',
        # {pct_tax} / {new_rev} / {owed}
        '%.1f = \\$%s / \\$%s.')
#Get median positive payment by December
MEDIAN_POSITIVE_PAYMENT = 
  owners[(unq_own & total_paid_sep > 0), median(total_paid_sep)]
tbl = capture.output(print(xtable(
  #Use keyby to make sure the output is sorted and Holdout comes first
  owners[(unq_own), keyby = treat8,
         .(N = .N, ep3 = mean(ever_paid_sep),
           ep6 = mean(ever_paid_dec), owed = sum(total_due)), 
         #Express relative to Holdout
         ][ , melt(.SD, id.vars = c('treat8', 'N', 'owed'), value.name = 'ep')
            ][ , keyby = variable, {
              ep_holdout = .SD[1L, ep]
              param_env = environment()
              jval = .SD[-1L, {
                #treatment delta
                delta = ep - ep_holdout
                new_payers = round(delta*N)
                rev_per_letter = delta * MEDIAN_POSITIVE_PAYMENT
                new_revenue = N * rev_per_letter
                NEW_REVENUE = sum(new_revenue)
                OWED = sum(owed)
                # store these parameters for the footnote
                param_env$params = list(
                  sample_size = prettyNum(N[1L], big.mark = ','),
                  delta = delta[1L],
                  new_payers = new_payers[1L],
                  payment = MEDIAN_POSITIVE_PAYMENT,
                  per_letter = rev_per_letter[1L],
                  new_rev = prettyNum(round(new_revenue[1L]), big.mark = ','),
                  owed = prettyNum(round(owed[1L]), big.mark = ','),
                  pct_tax = 100*new_revenue[1L]/owed[1L]
                )
                .(
                  Treatment = c(as.character(treat8), 'Totals'), 
                  `Sample` = prettyNum(c(N, sum(N)), big.mark = ','),
                  `Total Taxes` = sprintf('$%.3f M', c(owed, OWED)/1e6),
                  `New` = c(new_payers, sum(new_payers)),
                  `Revenue/` = c(dol.form(rev_per_letter,  dig = 2L), '-'), 
                  `New` = dol.form(c(new_revenue, NEW_REVENUE)),
                  `New % of Taxes` = 
                    round(100*c(new_revenue/owed, NEW_REVENUE/OWED), 1L)
                )
              }]
              # only use the parameter values
              #   from 3 month estimates to illustrate
              if (.BY$variable == 'ep3') REMINDER_PARAMS <<- param_env$params
              jval
            }][ , !'variable'],
  caption = "Revenue Implications",
  label = "rev", align = "rlcccccc", only.contents = TRUE,
  digits = c(rep(0L, 7L), 1L)), hline.after = c(-1L, 0L, 7L, 8L),
  table.placement = 'htbp', include.rownames = FALSE,
  comment = FALSE, caption.placement = "top"))

# float label to the top
lbl_idx = grep('\\label', tbl, fixed = TRUE)
cap_idx = grep('\\caption', tbl, fixed = TRUE)
tbl[cap_idx] = paste0(tbl[cap_idx], tbl[lbl_idx])
tbl = tbl[-lbl_idx]

# add second header row from saving horizontal space
idx1 = grep('Treatment.*Total Taxes', tbl)
idx2 = grep('Reminder', tbl, fixed = TRUE)[2L] - 1L
idx3 = grep('end{tabular}', tbl, fixed = TRUE) - 1L

REMINDER_PARAMS$fmt = 
  sprintf('\\multicolumn{7}{p{1\\textwidth}}{%s}', note_fmt)
header_continue = ' & Size & Owed & Payers & Letters & Revenues & Paid \\\\'
tbl = c(head(tbl, idx1 - 1L), 
        '\\multicolumn{7}{c}{Based on 3 Months Estimates} \\\\', '\\hline',
        tbl[idx1], header_continue,
        tbl[(idx1 + 1L):idx2], 
        # duplicate header here
        '\\multicolumn{7}{c}{Based on 6 Months Estimates} \\\\', '\\hline',
        tbl[idx1], header_continue, '\\hline',
        tbl[(idx2 + 1L):idx3], 
        do.call(sprintf, REMINDER_PARAMS[c(
          'fmt', 'new_payers', 'delta', 'sample_size', 'payment',
          'per_letter', 'delta', 'payment', 'new_rev', 'per_letter', 
          'sample_size', 'pct_tax', 'new_rev', 'owed'
        )]),
        tail(tbl, -idx3))

cat(tbl, sep = '\n', file = tex_file, append = TRUE)
