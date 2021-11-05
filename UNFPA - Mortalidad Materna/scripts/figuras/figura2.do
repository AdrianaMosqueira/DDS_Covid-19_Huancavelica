clear all
set more off
cd "C:\WBG\Otros\Desarroio\Repos\unfpa-mm-y-pf\data"

* Base data >> pregnancy
use "intermediate/endes/pregnancy.dta"

* Female individual data
merge m:1 year CASEID using "intermediate/endes/female.dta", keep(master matched) nogen

* hh data
merge m:1 year HHID using "intermediate/endes/hh.dta", keep(master matched) nogen

* dwelling
merge m:1 year HHID using "intermediate/endes/dwelling.dta", keep(master matched) nogen

** Professional-assisted labor by year/group
gen total = 1
local groups total urbano rural lima_metropolitana selva resto_del_pais ///
	sin_educacion primaria secundaria superior

preserve
foreach group in `groups' {

	restore, preserve
	collapse parto_prof if `group' == 1 [iweight = V005], by(year)
	ren parto_prof `group'
	tempfile temp_`group'
	save `temp_`group'', replace

}

restore, not
clear
set obs 17
gen year = 2000 if _n == 1
replace year = _n + 2002 if _n != 1

foreach group in `groups' {
	merge 1:1 year using `temp_`group'', nogen
}

export delimited using "intermediate/figura2.csv", delimiter(",") replace