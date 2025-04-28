// Change working directory
cd "C:\Users\JKB\Documents\WFH\Personal\DEMO\300\out\data"

// Get data
use "who_mdb_104.dta", clear

// Check for coding problems in cause of death variable
// icd10 check Cause, version(2016) generate(problem16) summary

// A90, A91, I84
// icd10 lookup A90 A91 I84, version(2012)

// Cause "AAA" refers to total deaths from all causes combined
keep if Cause != "AAA"

// COD Categories
icd10 generate infectious = Cause, range(A00/B99)
icd10 generate cancers = Cause, range(C00/D48)
icd10 generate diabetes = Cause, range(E10/E14)
icd10 generate ischemic = Cause, range(I20/I25)
icd10 generate stroke = Cause, range(I60/I64)
icd10 generate cvd_oth = Cause, range(I00/I99)
	replace cvd_oth = . if ischemic == 1 | stroke == 1
icd10 generate respiratory = Cause, range(J00/J99)
icd10 generate suicide = Cause, range(X60/X84)
icd10 generate transport = Cause, range(V01/V99)
icd10 generate ext_oth = Cause, range(V01/Y98)
	replace ext_oth = . if suicide == 1 | transport == 1
icd10 generate covid = Cause, range(U071/U072)
	
gen cod10 = 12
replace cod10 = 1 if infectious == 1
replace cod10 = 2 if cancers == 1
replace cod10 = 3 if diabetes == 1
replace cod10 = 4 if ischemic == 1
replace cod10 = 5 if stroke == 1
replace cod10 = 6 if cvd_oth == 1
replace cod10 = 7 if respiratory == 1
replace cod10 = 8 if suicide == 1
replace cod10 = 9 if transport == 1
replace cod10 = 10 if ext_oth == 1
replace cod10 = 11 if covid == 1

gen cod5 = .
replace cod5 = 3 if cod10 == 1
replace cod5 = 2 if cod10 == 2
replace cod5 = 5 if cod10 == 3
replace cod5 = 1 if cod10 == 4
replace cod5 = 1 if cod10 == 5
replace cod5 = 1 if cod10 == 6
replace cod5 = 5 if cod10 == 7
replace cod5 = 4 if cod10 == 8
replace cod5 = 4 if cod10 == 9
replace cod5 = 4 if cod10 == 10
replace cod5 = 5 if cod10 == 11
replace cod5 = 5 if cod10 == 12

lab def COD10 ///
1  "Infectious diseases" ///
2  "Neoplasms" ///
3  "Diabetes" ///
4  "Ischemic heart disease" ///
5  "Stroke" ///
6  "Other cardiovascular diseases" ///
7  "Respiratory diseases" ///
8  "Suicide" ///
9  "Transport accidents" ///
10 "Other external causes" ///
11 "COVID-19" ///
12 "Others" 

lab val cod10 COD10

lab def COD5 ///
1 "Circulatory" ///
2 "Neoplasm" ///
3 "Infection" ///
4 "External" ///
5 "Others"

lab val cod5 COD5

drop infectious - covid
save "mdb_104_cod.dta", replace
// export delimited mdb_104_cod.csv, replace
