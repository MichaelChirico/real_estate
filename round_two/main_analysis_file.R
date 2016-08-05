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
  nms[nms == "(Intercept)"] <- "Control"
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
  nms[nms == "(Intercept)"] <- if (nn == 7) "Control" else "Holdout"
  #treatment dummies take the form treat8Control, etc -- 
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
owners[ , treat8 := factor(treat8, c("Holdout", "Control", "Amenities", "Moral", 
                                     "Duty", "Peer", "Lien", "Sheriff"))]
owners[ , treat7 := factor(treat7, c("Control", "Amenities", "Moral", 
                                     "Duty", "Peer", "Lien", "Sheriff"))]

# TABLE 1: Balance - Treated vs. Holdout Comparison (Unique Owners) ####
hold_bal <- 
  cbind(t(
    owners[(unq_own), 
           .(`Amount Due (June)` = dol.form(mean(total_due), tex = TRUE),
             `Assessed Property Value` = 
               dol.form(mean(assessed_mv, na.rm = TRUE)),
             `\\# Owners` = prettyNum(.N, big.mark = ",")),
           by = .(Variable = c("Treated", "Holdout")[holdout + 1L])]),
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
##Print Table Header
### *surround with {} so the table all prints together*
{cat("\\begin{sidewaystable}[ht]",
    "\\centering", 
    "\\caption{Balance on Observables}",
    "\\label{balance}",
    "\\begin{tabular}{lrrrrrrrc}", 
    "\\hline",
    "\\multicolumn{9}{c}{Unique Owners} \\\\", sep = "\n")

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
tbl <- capture.output(texreg(lapply(lapply(expression(
  `One Month` = ever_paid_jul, `Three Months` = ever_paid_sep,
  `One Month` = paid_full_jul, `Three Months` = paid_full_sep),
  #Multiply indicator by 100 so the units are in %ages already
  function(x) owners[(unq_own), lm(I(100 * eval(x)) ~ treat8)]), 
  rename_coef, nn = 8), stars = c(.01, .05, .1), 
  include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L, label = "pc_lin",
  caption = "Short Term Linear Probability Model Estimates",
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
         "\\hline", tbl[(idx + 1L):length(tbl)])

cat(tbl, sep = "\n")

# TABLE 4: Revenue - Per-Letter Impact @ 3 Months ####
print(xtable(
  #Use keyby to make sure the output is sorted and Holdout comes first
  owners[(unq_own), .(.N, mean(ever_paid_sep)), keyby = treat8
         #Express relative to Holdout
         ][ , .(Treatment = treat8[-1L], 
                `Impact Per Letter` = 
                  dol.form(x <-  (V2[-1L] - V2[1L]) * 
                             #Get median positive payment by December
                             owners[(unq_own & total_paid_dec > 0), 
                                    median(total_paid_dec)],  dig = 2L), 
                `Total Impact` = dol.form(N[-1L] * x))],
  caption = "Estimated Short Term Impact on Revenue (3 Months)",
  label = "sh_rev", align = "rlcc"),
  include.rownames = FALSE, comment = FALSE, caption.placement = "top")

# TABLE 5: Regression - Ever Paid @ 1 & 3 Months, LPM, vs. Control ####
tbl <- capture.output(texreg(lapply(c(lapply(expression(
  `One Month` = ever_paid_jul, 
  `Three Months` = ever_paid_sep),
  function(x) owners[(!holdout), lm(eval(x) ~ treat7)]),
  lapply(expression(`One Month` = ever_paid_jul, 
                    `Three Months` = ever_paid_sep),
         function(x) owners[(!holdout & unq_own), lm(eval(x) ~ treat7)])),
  rename_coef, nn = 7), omit.coef = "Control", 
  include.rsquared = FALSE, include.rmse = FALSE,
  include.adjrs = FALSE, stars = c(.001, .05, .1),
  caption = "Robustness Analysis: Multiple Owners",
  label = "sh_lpm_rob", caption.above = TRUE))

idx <- grep("^\\\\begin\\{tabular\\}", tbl)

tbl <- c(tbl[1L:(idx + 1L)], 
         " & \\multicolumn{2}{c}{All Owners} & " %+% 
           "\\multicolumn{2}{c}{Single-Property Owners} \\\\",
         tbl[(idx + 2L):length(tbl)])

cat(tbl, sep = "\n")

# TABLE 6: Regression - Ever Paid/Paid Full @ 6 & 12 Months, LPM ####
tbl <- capture.output(texreg(lapply(lapply(expression(
  `Six Months` = ever_paid_dec, `Tax Year 2016` = ever_paid_jul16,
  `Six Months` = paid_full_dec, `Tax Year 2016` = paid_full_jul16),
  function(x) owners[(unq_own), lm(I(100 * eval(x)) ~ treat8)]),
  rename_coef, nn = 8), stars = c(.01, .05, .1), 
  include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L, label = "lg_pc_lin",
  caption = "Long Term Linear Probability Model Estimates",
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

# TABLE 7: Revenue - Per-Letter Impact @ 6 Months ####
print(xtable(
  owners[(unq_own), .(.N, mean(ever_paid_dec)), keyby = treat8
         ][ , .(Treatment = treat8[-1L], 
                `Impact Per Letter` = 
                  dol.form(x <-  (V2[-1L] - V2[1L]) * 
                             owners[(unq_own & total_paid_dec > 0), 
                                    median(total_paid_dec)],  dig = 2L), 
                `Total Impact` = dol.form(N[-1L] * x))],
  caption = "Estimated Six-Month Impact on Revenue",
  label = "lg_rev", align = "rlcc"),
  include.rownames = FALSE, comment = FALSE, caption.placement = "top")

####ADDENDUM
## Cumulative Partial Participation
### get range of dates for day-by-day averages
dt.rng <- owners[(!holdout & unq_own), {
  rng <- range(earliest_pmt_jul16, na.rm = TRUE)
  seq(from = rng[1L], to = rng[2L], by = "day")}]
#For pretty printing, get once/week subset
dt.rng2 <- dt.rng[seq(1L, length(dt.rng), length.out = 7L)]

date.dt <- 
  #not all treatments saw activity on each day,
  #  so we'll have to "fill-in-the-blanks" with this
  CJ(treat8 = owners[ , levels(treat8)], date = dt.rng,
     unique = TRUE, sorted = FALSE)
cum_haz <- 
  owners[(unq_own),
         #total entrants by day, treatment
         sum(ever_paid_jul16) + 0., #convert to numeric
         keyby = .(treat8, earliest_pmt_jul16)
         #running total of entrants; take care to eliminate NAs
         ][ , .(ep = cumsum(V1[idx <- !is.na(earliest_pmt_jul16)]),
                date = earliest_pmt_jul16[idx]), by = treat8
            #merge to get the denominator
            ][owners[(unq_own), .N, treat8],
              ep := ep/i.N, on = "treat8"
              ][date.dt, on = c("treat8", "date"), roll = TRUE
                #express relative to holdout
                ][is.na(ep), ep := 0
                  ][ , .(treat8 = treat8[idx <- treat8 != "Holdout"],
                         ep = ep[idx] - ep[!idx]), by = date]

### plot
get.col <- function(st){
  cols <- c(Big = "blue", Small = "red", Control = "blue", 
            Amenities = "yellow", Moral = "cyan", 
            Duty = "darkgreen", Lien = "red", Sheriff = "orchid",
            Peer = "orange", Holdout = "darkgray")
  cols[gsub("\\s.*", "", as.character(st))]
}
dcast(cum_haz, date ~ treat8, value.var = "ep")[ , {
  matplot(date, do.call("cbind", 
                        mget(nms <- 
                               owners[(!holdout), levels(factor(treat8))])),
          type = "l", lty = 1L, col = get.col(nms),
          xaxt = "n", lwd = 3L, las = 2L,
          ylab = "Probability Ever Paid vs. Holdout")
    axis(side = 1L, at = dt.rng2, labels = format(dt.rng2, "%b %d"))
    abline(h = 0, col = "black", lwd = 2L)
    title("Cumulative Partial Participation\n" %+%
            "vs. Holdout (Unique Owners Only)")}]

owners[(unq_own), .N, keyby = earliest_pmt_jul16
       ][-1L][ , .(earliest_pmt_jul16, cumsum(N))
               ][ , plot(earliest_pmt_jul16, V2, type = "l", lty = 1L,
                         lwd = 3L, xlab = "Date", ylab = "# Participants",
                         main = "Partial Participation for the 2016 Tax Year",
                         xaxt = "n")]
axis(side = 1L, at = dt.rng2, labels = as.character(dt.rng2))
abline(v = D("2016-03-01"), lty = 2L, col = "red", lwd = 3L)
