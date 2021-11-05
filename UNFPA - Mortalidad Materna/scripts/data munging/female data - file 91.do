clear all
set more off
cd "C:\WBG\Otros\Desarroio\Repos\unfpa-mm-y-pf\data"

* Female data

local years 2005 2006 2007 2008 2009 2010 2011 2012 ///
	2013 2014 2015 2016 2017 2018 2019
	
local years 2007 2008 2009 2010 2011 2012 ///
	2013 2014 2015 2016 2017 2018 2019

foreach year in `years' {
	
	import spss "raw/`year'/REC91.sav", clear

	gen year = `year'
	local to_keep CASEID S119 year
	
	if `year' <= 2009 {
		ren S118A S119
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

* Lengua materna
gen castellano = 0 if S119 != .
replace castellano = 1 if S119 == 1 & year <= 2016 & castellano == 0
replace castellano = 1 if S119 == 10 & year >= 2017 & castellano == 0
gen lengua_nativa = 0 if S119 != .
replace lengua_nativa = 1 if (S119 >= 2 & S119 <= 4) & year <= 2016 & lengua_nativa == 0
replace lengua_nativa = 1 if (S119 >= 1 & S119 <= 9) & year >= 2017 & lengua_nativa == 0

compress _all
save "intermediate/endes/female2.dta", replace