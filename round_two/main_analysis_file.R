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

# TABLE 1: Balance  - Comparison by Treatment ####
##Print Table Header
### *surround with {} so the table all prints together*
{cat("\\begin{sidewaystable}[ht]",
    "\\centering", 
    "\\caption{Balance on Observables (Unary Owners)}",
    "\\label{balance}",
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

# TABLE 2: Regression - Ever Paid/Paid Full @ 1 & 3 Months, LPM ####
tbl <- capture.output(texreg(lapply(lapply(expression(
  `One Month` = ever_paid_jul, `Three Months` = ever_paid_sep,
  `One Month` = paid_full_jul, `Three Months` = paid_full_sep),
  #Multiply indicator by 100 so the units are in %ages already
  function(x) owners[(unq_own), lm(I(100 * eval(x)) ~ treat8)]), 
  rename_coef, nn = 8), stars = c(.01, .05, .1), 
  include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L, label = "pc_lin",
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

# TABLE 3: Revenue - Per-Letter Impact @ 3 Months ####
print(xtable(
  #Use keyby to make sure the output is sorted and Holdout comes first
  owners[(unq_own), .(.N, mean(ever_paid_sep)), keyby = treat8
         #Express relative to Holdout
         ][ , .(Treatment = treat8[-1L], 
                `Impact Per Letter` = 
                  dol.form(x <-  (V2[-1L] - V2[1L]) * 
                             #Get median positive payment by December
                             owners[(unq_own & total_paid_sep > 0), 
                                    median(total_paid_sep)],  dig = 2L), 
                `Total Impact` = dol.form(N[-1L] * x))],
  caption = "Estimated Three-Month Impact on Revenue",
  label = "sh_rev", align = "rlcc"),
  include.rownames = FALSE, comment = FALSE, caption.placement = "top")

# TABLE 4: Regression - Ever Paid @ 1 & 3 Months, LPM, vs. Reminder ####
tbl <- capture.output(texreg(lapply(lapply(expression(
  `One Month` = ever_paid_jul, `Three Months` = ever_paid_sep,
  `One Month` = paid_full_jul, `Three Months` = paid_full_sep),
  #Multiply indicator by 100 so the units are in %ages already
  function(x) owners[(!holdout & unq_own), lm(I(100 * eval(x)) ~ treat7)]), 
  rename_coef, nn = 7), stars = c(.01, .05, .1), 
  include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L, label = "sh_lpm_rob",
  caption = "Robustness Analysis: Relative to Reminder",
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

# TABLE 5: Regression - Ever Paid/Paid Full @ 6 & 12 Months, LPM ####
tbl <- capture.output(texreg(lapply(lapply(expression(
  `Ever Paid` = ever_paid_dec, `Paid in Full` = paid_full_dec,
  `Ever Paid` = ever_paid_jul16, `Paid in Full` = paid_full_jul16),
  function(x) owners[(unq_own), lm(I(100 * eval(x)) ~ treat8)]),
  rename_coef, nn = 8), stars = c(.01, .05, .1), 
  include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L, label = "lg_pc_lin",
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

# TABLE 6: Revenue - Per-Letter Impact @ 6 Months ####
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

#Extra: Cumulative Hazard vs. Hold-out ####
trt.nms = owners[ , levels(treat8)]
get.col<-function(st){
  cols <- 
    c(Big="blue", Small="red", Reminder = "blue", Neighborhood = "yellow",
      Community = "cyan", Duty = "darkgreen", Lien = "red", Sheriff = "orchid",
      Peer = "orange", Holdout = "darkgray")
  cols[gsub("\\s.*", "", as.character(st))]
}

dt.rng<-owners[(unq_own),{rng<-range(earliest_pmt_dec,na.rm=T)
seq(from=rng[1],to=rng[2],by="day")}]
#For pretty printing, get once/week subset
dt.rng2<-dt.rng[seq(1,length(dt.rng),length.out=7)]

date.dt <- 
  CJ(treat8 = trt.nms, date = dt.rng,
     unique=TRUE, sorted = FALSE)
cum_haz<-owners[(unq_own),sum(ever_paid_dec)+0.,
                keyby=.(treat8,earliest_pmt_dec)
                ][,.(ep=cumsum(V1[idx<-!is.na(earliest_pmt_dec)]),
                     date=earliest_pmt_dec[idx]),by=treat8
                  ][owners[(unq_own),.N,treat8],ep:=ep/i.N,on="treat8"
                    ][date.dt, on = c("treat8", "date"), roll = TRUE
                      ][,.(treat7=treat8[idx<-treat8!="Holdout"],
                           ep=ep[idx]-ep[!idx]),by=date]

owners[(unq_own),sum(ever_paid_dec)+0.,
                keyby=.(rec = treat8 == "Holdout",earliest_pmt_dec)
                ][,.(ep=cumsum(V1[idx<-!is.na(earliest_pmt_dec)]),
                     date=earliest_pmt_dec[idx]),by=rec
                  ][owners[(unq_own),.N,by = .(rec = treat8 == "Holdout")],ep:=ep/i.N,on="rec"
                    ]

owners[(unq_own),sum(ever_paid_dec)+0.,
                keyby=earliest_pmt_dec
                ][,.(ep=cumsum(V1[idx<-!is.na(earliest_pmt_dec)]),
                     date=earliest_pmt_dec[idx])
                  ][month(date) %in% 8:9]
dtunqown = owners[(unq_own)]
BB <- 5000
cis <- dcast(rbindlist(lapply(integer(BB), function(...){
  dt <- dtunqown[sample(.N,rep=T)]
  dt[,sum(ever_paid_dec)+0.,keyby=.(treat8,earliest_pmt_dec)
     ][,.(ep=cumsum(V1[idx<-!is.na(earliest_pmt_dec)]),
          date=earliest_pmt_dec[idx]),by=treat8
       ][dt[,.N,treat8],ep:=ep/i.N,on="treat8"
         ][date.dt, on = c("treat8", "date"),roll = TRUE
           ][,.(treat7=treat8[idx<-treat8!="Holdout"],
                ep=ep[idx]-ep[!idx]),by=date]}),idcol="bootID"
  )[,quantile(ep, c(.025, .975), na.rm=T), by = .(treat7, date)],
  treat7+date~c("low","high")[rowid(treat7,date)],value.var="V1")

dcast(cum_haz[cis,on=c("treat7","date")],
      date~treat7,value.var=c("low","ep","high")
      )[,{pdf2("~/Desktop/cum_haz_ever_paid_dec_8_own_cis.pdf")
        par(mfrow=c(2,4),
              mar=c(0,0,1.1,0),
              oma=c(7.1,4.1,4.1,1.1))
        ylm <- range(.SD[,!"date",with=F])
        axl <- list(x = list(at=dt.rng2,las=2,
                         labels=format(dt.rng2,"%b %d")),
                    y = list())
        for (ii in 1:7){
          tr <- (trt.nms%\%"Holdout")[ii]
          matplot(date, do.call("cbind",mget(c("low_","ep_","high_")%+%tr)),
                  type="l",lty=c(2,1,2),col=get.col(tr),
                  lwd=3,xaxt="n",yaxt="n",ylim=ylm)
          abline(h = 0, col = "black", lwd = 2)
          if (ii == 4L) axis(side = 1L, at = dt.rng2, las = 2L,
                            labels = format(dt.rng2, "%b %d"))
          else tile.axes(ii,2,4,axl)
          title(tr)}
        title("Cumulative Partial Participation (Dec.)"%+%
                "\nRelative to Holdout",outer=T)
        mtext("Date",side=1,outer=T,line=5.5)
        mtext("Probability Ever Paid vs. Holdout",
              side=2,outer=T,line=2.5)
        dev.off2()}]

#Table 1, but for comparing full & unary-owner samples
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

#versions of LPM tables with logit specification
##table2
tbl <- capture.output(texreg(lapply(lapply(expression(
  `One Month` = ever_paid_jul, `Three Months` = ever_paid_sep,
  `One Month` = paid_full_jul, `Three Months` = paid_full_sep),
  #Multiply indicator by 100 so the units are in %ages already
  function(x) owners[(unq_own), glm(eval(x) ~ treat8, family = binomial)]), 
  rename_coef, nn = 8), stars = c(.01, .05, .1), 
  include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L, label = "pc_lin",
  caption = "Short-Term Logistic Model Estimates",
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

##table 4
tbl <- capture.output(texreg(lapply(c(lapply(expression(
  `One Month` = ever_paid_jul, 
  `Three Months` = ever_paid_sep),
  function(x) owners[(!holdout), glm(eval(x) ~ treat7, family = binomial)]),
  lapply(expression(`One Month` = ever_paid_jul, 
                    `Three Months` = ever_paid_sep),
         function(x) owners[(!holdout & unq_own), 
                            glm(eval(x) ~ treat7, family = binomial)])),
  rename_coef, nn = 7), omit.coef = "Reminder", 
  include.rsquared = FALSE, include.rmse = FALSE,
  include.adjrs = FALSE, stars = c(.001, .05, .1),
  caption = "Robustness Analysis: Multiple Owners",
  label = "sh_lpm_rob", caption.above = TRUE))

idx <- grep("^\\\\begin\\{tabular\\}", tbl)

tbl <- c(tbl[1L:(idx + 1L)], 
         " & \\multicolumn{2}{c}{All Owners} & " %+% 
           "\\multicolumn{2}{c}{Unary Owners} \\\\",
         tbl[(idx + 2L):length(tbl)])

cat(tbl, sep = "\n")

##table 5
tbl <- capture.output(texreg(lapply(lapply(expression(
  `Six Months` = ever_paid_dec, `Tax Year 2016` = ever_paid_jul16,
  `Six Months` = paid_full_dec, `Tax Year 2016` = paid_full_jul16),
  function(x) owners[(unq_own), glm(eval(x) ~ treat8, family = binomial)]),
  rename_coef, nn = 8), stars = c(.01, .05, .1), 
  include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L, label = "lg_pc_lin",
  caption = "Long-Term Linear Probability Model Estimates",
  custom.note = "%stars. Holdout values in levels; " %+% 
    "remaining figures relative to this"))

## Replace Holdout SEs with horizontal rule, add header for EP vs. PF
idx <- grep("^Holdout", tbl)

tbl[idx] <- gsub("\\^\\{[*]*\\}", "", tbl[idx])

tbl <- c(tbl[1L:(idx - 3L)],
         " & \\multicolumn{2}{c}{Ever Paid} & " %+% 
           "\\multicolumn{2}{c}{Paid in Full} \\\\",
         tbl[c(idx - 2L, idx)],
         "\\hline", tbl[(idx + 2L):length(tbl)])

cat(tbl, sep = "\n")

# Short-term Estimates for All Owners
tbl <- capture.output(texreg(lapply(lapply(expression(
  `One Month` = ever_paid_jul, `Three Months` = ever_paid_sep,
  `One Month` = paid_full_jul, `Three Months` = paid_full_sep),
  #Multiply indicator by 100 so the units are in %ages already
  function(x) owners[(!holdout), lm(I(100 * eval(x)) ~ treat7)]), 
  rename_coef, nn = 7), stars = c(.01, .05, .1), 
  include.rsquared = FALSE, caption.above = TRUE,
  include.adjrs = FALSE, include.rmse = FALSE, digits = 1L, label = "sh_lpm_mult",
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
