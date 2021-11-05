clear all
set more off
cd "C:\WBG\Otros\Desarroio\Repos\unfpa-mm-y-pf\data"

** Probar un filtro de edad

* Base data >> contraception
use "intermediate/endes/contraception.dta"

* Relationship data
merge 1:1 year CASEID using "intermediate/endes/relationships.dta", keep(master matched) nogen

* Female individual data
merge 1:1 year CASEID using "intermediate/endes/female.dta", keep(master matched) nogen

* (more) Female individual data
merge 1:1 year CASEID using "intermediate/endes/female2.dta", keep(master matched) nogen

* hh data
merge m:1 year HHID using "intermediate/endes/hh.dta", keep(master matched) nogen

* dwelling
merge m:1 year HHID using "intermediate/endes/dwelling.dta", keep(master matched) nogen

** Uses contraception by year/group
gen total = 1
local groups total urbano rural lima_metropolitana selva resto_del_pais ///
	sin_educacion primaria secundaria superior quintil1 quintil2 ///
	quintil3 quintil4 quintil5 castellano lengua_nativa

preserve
foreach group in `groups' {

	restore, preserve
	collapse usa_metodo_moderno if `group' == 1 & edad_reproductiva == 1 & en_union == 1 [iweight = V005], by(year)
	ren usa_metodo_moderno `group'
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

export delimited using "intermediate/figura17.csv", delimiter(",") replace