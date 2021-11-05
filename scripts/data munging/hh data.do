clear all
set more off
cd "C:\WBG\Otros\Desarroio\Repos\unfpa-mm-y-pf\data"

** HH data

local years 2000 2004 2005 2006 2007 2008 2009 2010 2011 2012 ///
	2013 2014 2015 2016 2017 2018 2019

foreach year in `years' {
	
	import spss "raw/`year'/RECH0.sav", clear
	
	gen year = `year'
	local to_keep year HHID HV025 HV005 HV015

	if `year' == 2014 | `year' == 2016 | `year' == 2004 {
		ren hv005 HV005
	}
	
	if `year' == 2004 {
		ren hhid HHID
		ren hv025 HV025
		ren hv015 HV015
	}
	
	keep `to_keep'
	tempfile y_`year'
	save `y_`year'', replace
	
}

clear
foreach year in `years' {
	
	append using `y_`year''
	
}

replace HHID = strtrim(HHID)

* Ambito
gen urbano = HV025 == 1 if HV025 != .
gen rural =  HV025 == 2 if HV025 != .

compress _all
save "intermediate/endes/hh.dta", replace