clear all
set more off
cd "C:\WBG\Otros\Desarroio\Repos\unfpa-mm-y-pf\data"

* Base data >> contraception
use "intermediate/endes/contraception.dta"

* Female individual data
merge 1:1 year CASEID using "intermediate/endes/female.dta", keep(master matched) nogen

* hh data
merge m:1 year HHID using "intermediate/endes/hh.dta", keep(master matched) nogen

** Age groups
gen edad_10_14 = V012 >= 10 & V012 <= 14 if V012 != .
gen edad_15_19 = V012 >= 15 & V012 <= 19 if V012 != .
gen edad_20_24 = V012 >= 20 & V012 <= 24 if V012 != .

collapse V209 [iw = V005], by(year)
ren V209 tasa_fecundidad_adolescente
replace tasa_fecundidad_adolescente = tasa_fecundidad_adolescente * 1000

*export delimited using "intermediate/figura9.csv", delimiter(",") replace