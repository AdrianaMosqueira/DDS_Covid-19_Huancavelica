clear all
set more off
cd "C:\WBG\Otros\Desarroio\Repos\unfpa-mm-y-pf\data"

** Pregnancy data

local years 2000 2004 2005 2006 2007 2008 2009 2010 2011 2012 ///
	2013 2014 2015 2016 2017 2018 2019

foreach year in `years' {
	
	import spss "raw/`year'/REC41.sav", clear

	gen year = `year'
	local to_keep CASEID MIDX year M3A M3B M3C
	
	keep `to_keep'
	tempfile y_`year'
	save `y_`year'', replace

}

clear
foreach year in `years' {
	
	append using `y_`year''
	
}

replace CASEID = strtrim(CASEID)

* HH ID
gen HHID = ""
split CASEID, limit(1)
replace HHID = CASEID1 if year >= 2009
replace HHID = substr(CASEID, 1, strlen(CASEID) -3) if year <= 2008
drop CASEID1

* Parto profesinal
gen parto_prof = 0 if M3A != . & M3B != . & M3C != .
*replace parto_prof = 0 if M3A != . & M3B != . & year == 2000
// 2000 no registro bien la variable M3C, por eso no la incluimos
replace parto_prof = 1 if (M3A == 1 | M3B == 1 | M3C == 1) & year != 2000
*replace parto_prof = 1 if (M3A == 1 | M3B == 1 | M3C == 1)

compress _all
save "intermediate/endes/pregnancy.dta", replace