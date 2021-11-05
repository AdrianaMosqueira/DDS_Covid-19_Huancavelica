clear all
set more off
cd "C:\WBG\Otros\Desarroio\Repos\unfpa-mm-y-pf\data"

** Female data

local years 2000 2004 2005 2006 2007 2008 2009 2010 2011 2012 ///
	2013 2014 2015 2016 2017 2018 2019

foreach year in `years' {
	
	if `year' == 2004 {
		import spss "raw/`year'/REC01_11.sav", clear
	}
	else {
		import spss "raw/`year'/REC0111.sav", clear
	}

	gen year = `year'
	local to_keep CASEID V005 V106 V012 V015 year
	
	if `year' == 2014 | `year' == 2016 | `year' == 2004 {
		ren v005 V005
	}
	
	if `year' == 2004 {
		ren caseid CASEID
		ren v106 V106
		ren v015 V015
		ren v012 V012
	}
	
	
	keep `to_keep'
	tempfile y_`year'
	save `y_`year'', replace

}

clear
foreach year in `years' {
	
	append using `y_`year''
	
}

replace CASEID = strtrim(CASEID)

* Education level
gen sin_educacion = V106 == 0 if V106 != .
gen primaria      = V106 == 1 if V106 != .
gen secundaria    = V106 == 2 if V106 != .
gen superior      = V106 == 3 if V106 != .

* Reproductive age
gen edad_reproductiva = V012 >= 15 & V012 <= 49 if V012 != .

compress _all
save "intermediate/endes/female.dta", replace