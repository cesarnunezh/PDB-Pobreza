clear all

*global data "C:\Users\Usuario\OneDrive - VIDENZA\2024\PdB_Pobreza\Enaho panel\Sumaria"
*global output "C:\Users\Usuario\OneDrive - VIDENZA\2024\PdB_Pobreza\Enaho panel\Sumaria\output"

global data "C:\Users\User\OneDrive - MIGRACIÓN VIDENZA\1. Proyectos\1. Proyectos actuales\23. Artículos PDB\2. PDB - Pobreza Urbana\2. Data\1. Bases\1. ENAHOPanel"
global output "C:\Users\User\OneDrive - MIGRACIÓN VIDENZA\1. Proyectos\1. Proyectos actuales\23. Artículos PDB\2. PDB - Pobreza Urbana\2. Data\1. Bases\0. Output"

use "$data\sumaria_2018_2022_panel.dta", clear
keep if hpanel_21_22 == 1 | hpanel_20_21 == 1 | hpanel_19_20 == 1 | hpanel_18_19 == 1
rename facpanel1819 facpanel_19
rename facpanel1920 facpanel_20
rename facpanel2021 facpanel_21
rename facpanel2122 facpanel_22

save "$output\base1.dta", replace

use "$data\sumaria_2014_2018_panel.dta", clear
keep if hpan1718 == 1 | hpan1617 == 1 | hpan1516 == 1 | hpan1415 == 1
rename facpanel1718 facpanel_18
rename facpanel1617 facpanel_17
rename facpanel1516 facpanel_16
rename facpanel1415 facpanel_15

save "$output\base2.dta", replace

* Append 2014-2022
use "$output\base1.dta", clear 
append using "$output\base2.dta"

keep numpanh conglome vivienda ///
 pobreza_* ///
 percepho_* ///
 inghog1d_* /// 
 inghog2d_* ///
 gashog1d_* ///
 ingmo1hd_* ///
 ingmo2hd_* ///
 estrsocial_* ///
 mieperho_* ///
 estrato_* ///
 dominio_* ///
 facpanel_* ///
 factor07_* ///
 hpanel_18_19 hpanel_19_20 hpanel_20_21 hpanel_21_22 hpan1415 hpan1516 hpan1617 hpan1718 hpan1112 hpan1314 facpanel_14 facpanel_12
 
* Generación de variables de transición de pobreza    

foreach x of numlist 11/22 {
	gen pobre_`x' = (pobreza_`x' == 1 | pobreza_`x' == 2 )
	replace pobre_`x' = . if (pobreza_`x' == . | pobreza_`x' == . )
}


gen transicion_pobreza = . 

replace transicion_pobreza = 0 if (hpan1415 == 1 & pobre_14==0 & pobre_15==0) | (hpan1516 == 1 & pobre_15==0 & pobre_16==0) | (hpan1617 == 1 & pobre_16==0 & pobre_17==0) | (hpan1718 == 1 & pobre_17==0 & pobre_18==0) | (hpanel_18_19 == 1 & pobre_18==0 & pobre_19==0) | (hpanel_19_20 == 1 & pobre_19==0 & pobre_20==0) | (hpanel_20_21 == 1 & pobre_20==0 & pobre_21==0) | (hpanel_21_22 == 1 & pobre_21==0 & pobre_22==0) // no pobre a no pobre

replace transicion_pobreza = 1  if (hpan1415 == 1 & pobre_14==1 & pobre_15==1) | (hpan1516 == 1 & pobre_15==1 & pobre_16==1) | (hpan1617 == 1 & pobre_16==1 & pobre_17==1) | (hpan1718 == 1 & pobre_17==1 & pobre_18==1) | (hpanel_18_19 == 1 & pobre_18==1 & pobre_19==1) | (hpanel_19_20 == 1 & pobre_19==1 & pobre_20==1) | (hpanel_20_21 == 1 & pobre_20==1 & pobre_21==1) | (hpanel_21_22 == 1 & pobre_21==1 & pobre_22==1) // pobre a pobre

replace transicion_pobreza = 2  if (hpan1415 == 1 & pobre_14==0 & pobre_15==1) | (hpan1516 == 1 & pobre_15==0 & pobre_16==1) | (hpan1617 == 1 & pobre_16==0 & pobre_17==1) | (hpan1718 == 1 & pobre_17==0 & pobre_18==1) | (hpanel_18_19 == 1 & pobre_18==0 & pobre_19==1) | (hpanel_19_20 == 1 & pobre_19==0 & pobre_20==1) | (hpanel_20_21 == 1 & pobre_20==0 & pobre_21==1) | (hpanel_21_22 == 1 & pobre_21==0 & pobre_22==1) // no pobre a pobre

replace transicion_pobreza = 3  if (hpan1415 == 1 & pobre_14==1 & pobre_15==0) | (hpan1516 == 1 & pobre_15==1 & pobre_16==0) | (hpan1617 == 1 & pobre_16==1 & pobre_17==0) | (hpan1718 == 1 & pobre_17==1 & pobre_18==0) | (hpanel_18_19 == 1 & pobre_18==1 & pobre_19==0) | (hpanel_19_20 == 1 & pobre_19==1 & pobre_20==0) | (hpanel_20_21 == 1 & pobre_20==1 & pobre_21==0) | (hpanel_21_22 == 1 & pobre_21==1 & pobre_22==0)  // pobre a no pobre

label define transicion_pobreza 0 "Mantiene no pobre" 1 "Mantiene pobre" 2 "Caida en pobreza" 3 "Salida de pobreza"
label values transicion_pobreza transicion_pobreza

foreach x of numlist 14/22 {
	gen gpc_`x' = gashog1d_`x' / mieperho_`x'
}

foreach z in gpc_ percepho_ {
	foreach x of numlist 14/21 {
		local y = `x' + 1
		if `x' < 18 {
			table transicion_pobreza if hpan`x'`y' == 1, stat(mean `z'`x' `z'`y')
		}
		else {
			table transicion_pobreza if hpanel_`x'_`y' == 1, stat(mean `z'`x' `z'`y')
		}

	}
}


foreach x of numlist 14/21 {
	local y = `x' + 1
	if `x' < 18 {
		table transicion_pobreza if hpan`x'`y' == 1 [iw = facpanel_`y']
	}
	else {
		table transicion_pobreza if hpanel_`x'_`y' == 1 [iw = facpanel_`y']
	}

}