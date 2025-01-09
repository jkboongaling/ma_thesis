// Change working directory
cd "C:\Users\JKB\Documents\WFH\Personal\DEMO\300\out\data"

// Get data
use "who_mdb.dta", clear

// Year 1999 to 2003
keep if Country == 3300 & List == "104"

gen Cause3 = substr(Cause, 1, 3)

// Verify the conversion
// list Cause Cause3 if !missing(Cause)

// Check invalid codes
// Cause "AAA" refers to total deaths from all causes combined
icd10 check Cause3, version(2012) generate(prob1) summary


// Get data
use "who_mdb.dta", clear
keep if Country == 3300 & List == "09B"

icd9 check Cause, generate(prob2) list
