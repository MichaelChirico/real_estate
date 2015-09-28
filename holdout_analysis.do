clear
import excel "U:\Projects\DOR Experiment\Round 2\Copy of req20150709_PennLetterExperiment_v2_Commissioners Control Details.xlsx", sheet("DETAILS") cellrange(A9:N3008) firstrow allstring
 
merge 1:1 BRTNUMBER using "U:\Projects\DOR Experiment\Round 2\brt_owner.dta", keepusing(owner1)
gsort owner1 - _merge
bysort owner1 : gen control_only = 1 if _merge[_N]==3
 
keep if _merge==3
 
destring TOTALPAID TOTALDUEJULY2015, replace
bysort owner1: gen owner_paid = sum(TOTALPAID)
 
bysort owner1: gen owner_count = _N
 
duplicates drop owner1, force
 
gen nonzeropayment  = 1 if owner_paid>0
 
gen fullpayment = 1 if TOTALDUEJULY2015==0
 
mvencode nonzeropayment fullpayment, mv(.=0)