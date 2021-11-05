clear all
set more off
cd "C:\WBG\Otros\Desarroio\Repos\unfpa-mm-y-pf\data"

** Contraception data

local years 2000 2004 2005 2006 2007 2008 2009 2010 2011 2012 ///
	2013 2014 2015 2016 2017 2018 2019

foreach year in `years' {
    
	if `year' == 2000 {
		import spss "raw/`year'/REC567_1.sav", clear
	}
	else if `year' == 2004 | `year' == 2010 {
		import spss "raw/`year'/REC516171.sav", clear
	}
	else {
		import spss "raw/`year'/RE516171.sav", clear		
	} 

	gen year = `year'
	local to_keep CASEID V501 V502 V504 year

	keep `to_keep'
	tempfile y_`year'
	save `y_`year'', replace

}

clear
foreach year in `years' {
	
	append using `y_`year''
	
}

replace CASEID = strtrim(CASEID)

* Actualmente unida
gen en_union = V502 == 1 if V502 != .

compress _all
save "intermediate/endes/relationships.dta", replace