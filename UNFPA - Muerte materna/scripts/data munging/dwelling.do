clear all
set more off
cd "C:\WBG\Otros\Desarroio\Repos\unfpa-mm-y-pf\data"

** Dwelling data

local years 2000 2004 2005 2006 2007 2008 2009 2010 2011 2012 ///
	2013 2014 2015 2016 2017 2018 2019

foreach year in `years' {
	
	if `year' == 2004 {
		import spss "raw/`year'/RECH2_H3.sav", clear
	}
	else {
		import spss "raw/`year'/RECH23.sav", clear
	}
	
	gen year = `year'
	local to_keep HHID SHREGION year
	
	* Agregando variable de quintil de pobreza
	* Esta variable no estuvo en el 2000
	if `year' != 2000 {
		local to_keep `to_keep' HV270
	}
	
	if `year' == 2005 {
		ren hhid HHID
		ren shregion SHREGION
		ren hv270 HV270
	}
	
	if `year' == 2016 {
		ren hv270 HV270
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

* Region
gen lima_metropolitana = SHREGION == 1
gen selva = 			 SHREGION == 4
gen resto_del_pais =     SHREGION == 2 | SHREGION == 3

* Quintil de pobreza
forvalues i = 1/5 {
	gen quintil`i'= HV270 == `i' if HV270 != .
}

compress _all
save "intermediate/endes/dwelling.dta", replace