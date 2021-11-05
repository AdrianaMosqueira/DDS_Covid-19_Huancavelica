clear all
set more off
cd "C:\WBG\Otros\Desarroio\Repos\unfpa-mm-y-pf\data"

** Contraception data

local years 2000 2004 2005 2006 2007 2008 2009 2010 2011 2012 ///
	2013 2014 2015 2016 2017 2018 2019

foreach year in `years' {
    
	if `year' == 2000 | `year' == 2005 | `year' == 2007 {
		import spss "raw/`year'/REC22312.sav", clear
	}
	else if `year' == 2004 {
		import spss "raw/`year'/REC223132.sav", clear
	}
	else if `year' == 2012 {
		import spss "raw/`year'/RE212232.sav", clear
	}
	else {
		import spss "raw/`year'/RE223132.sav", clear
	}
	
	gen year = `year'
	local to_keep CASEID V312 V313 V208 V209 V238 year

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

* Use of contraception
gen usa_anticonceptivo = V312 != 0 if V312 != .
gen usa_metodo_moderno = V313 == 3 if V313 != .

compress _all
save "intermediate/endes/contraception.dta", replace