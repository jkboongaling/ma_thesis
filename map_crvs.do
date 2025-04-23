// Change working directory
cd "C:\Users\JKB\Documents\WFH\Personal\DEMO\300\out\data"

// Get data
use "px_crvs.dta", clear

// Check for coding problems in cause of death variable
// icd10 check cod, version(2016) generate(problem16) summary

// A90, A91, I84
// icd10 lookup A90 A91 I84, version(2012)

// COD Categories
icd10 generate infectious = cod, range(A00/B99)
icd10 generate cancers = cod, range(C00/D48)
icd10 generate diabetes = cod, range(E10/E14)
icd10 generate ischemic = cod, range(I20/I25)
icd10 generate stroke = cod, range(I60/I64)
icd10 generate cvd_oth = cod, range(I00/I99)
	replace cvd_oth = . if ischemic == 1 | stroke == 1
icd10 generate respiratory = cod, range(J00/J99)
icd10 generate suicide = cod, range(X60/X84)
icd10 generate injuries_oth = cod, range(V01/Y98)
	replace injuries_oth = . if suicide == 1

gen cod10 = 10
replace cod10 = 1 if infectious == 1
replace cod10 = 2 if cancers == 1
replace cod10 = 3 if diabetes == 1
replace cod10 = 4 if ischemic == 1
replace cod10 = 5 if stroke == 1
replace cod10 = 6 if cvd_oth == 1
replace cod10 = 7 if respiratory == 1
replace cod10 = 8 if suicide == 1
replace cod10 = 9 if injuries_oth == 1

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
replace cod5 = 5 if cod10 == 10

lab def COD10 ///
1 "Infectious diseases" ///
2 "Neoplasms" ///
3 "Diabetes" ///
4 "Ischemic heart disease" ///
5 "Stroke" ///
6 "Other cardiovascular diseases" ///
7 "Respiratory diseases" ///
8 "Suicide" ///
9 "Other external causes" ///
10 "Others" 

lab val cod10 COD10

lab def COD5 ///
1 "Circulatory" ///
2 "Neoplasm" ///
3 "Infection" ///
4 "External" ///
5 "Others"

lab val cod5 COD5

lab def SEX ///
1 "Male" ///
2 "Female"

lab val sex SEX

drop infectious - injuries_oth
save "cod.dta", replace
