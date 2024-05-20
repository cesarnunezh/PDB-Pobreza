clear all

*global data "C:\Users\Usuario\OneDrive - VIDENZA\2024\PdB_Pobreza\Enaho panel\Sumaria"
*global output "C:\Users\Usuario\OneDrive - VIDENZA\2024\PdB_Pobreza\Enaho panel\Sumaria\output"

global data "C:\Users\User\OneDrive - MIGRACIÓN VIDENZA\1. Proyectos\1. Proyectos actuales\23. Artículos PDB\2. PDB - Pobreza Urbana\2. Data\1. Bases\1. ENAHOPanel"
global output "C:\Users\User\OneDrive - MIGRACIÓN VIDENZA\1. Proyectos\1. Proyectos actuales\23. Artículos PDB\2. PDB - Pobreza Urbana\2. Data\1. Bases\0. Output"

* MODULO 100
{
use "$data\enaho01-2014-2018-100-panel.dta", clear
keep if hpan1416 == 1 | hpan1517 == 1 | hpan1618 == 1
*keep if hpan1718 == 1 | hpan1617 == 1 | hpan1516 == 1 | hpan1415 == 1
save "$output\mod100-1.dta", replace

use "$data\enaho01-2018-2022-100-panel.dta", clear
keep if hpan1820 == 1 | hpan1921 == 1 | hpan2022 == 1
*keep if hpan1819 == 1 | hpan1920 == 1 | hpan2021 == 1 | hpan2122 == 1
save "$output\mod100-2.dta", replace

* Append 2014-2022
use "$output\mod100-1.dta", clear 
append using "$output\mod100-2.dta"

keep numpanh conglome vivienda p101_* p102_* p103_* p103a_* p104a_* p105a_* p106a_* p110_* p110a1_* p110a_* p111a_* p1121_* p113a_* p1141_* p1142_* p1143_* p1144_* p1145_* nbi1_* nbi2_* nbi3_* nbi4_* nbi5_* longitud_* latitud_* altitud_* hpan* facpanel1820 facpanel1921 facpanel2022 facpanel1416 facpanel1517 facpanel1618

save "$output\mod100-panel.dta", replace
}

* SUMARIA 
{
use "$data\sumaria_2018_2022_panel.dta", clear
keep if hpanel_18_20 == 1 | hpanel_19_21 == 1 | hpanel_20_22 == 1 
*keep if hpanel_21_22 == 1 | hpanel_20_21 == 1 | hpanel_19_20 == 1 | hpanel_18_19 == 1
rename facpanel1819 facpanel_19
rename facpanel1920 facpanel_20
rename facpanel2021 facpanel_21
rename facpanel2122 facpanel_22

save "$output\base1.dta", replace

use "$data\sumaria_2014_2018_panel.dta", clear
keep if hpan1416 == 1 | hpan1517 == 1 | hpan1618 == 1
*keep if hpan1718 == 1 | hpan1617 == 1 | hpan1516 == 1 | hpan1415 == 1
rename facpanel1718 facpanel_18
rename facpanel1617 facpanel_17
rename facpanel1516 facpanel_16
rename facpanel1415 facpanel_15

save "$output\base2.dta", replace

* Append 2014-2022
use "$output\base1.dta", clear 
append using "$output\base2.dta"

keep numpanh conglome vivienda conglome_* vivienda_* hogar_* ///
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
 factor07_*

 save "$output\sumaria-panel.dta", replace
}

merge 1:1 numpanh conglome vivienda using "$output\mod100-panel.dta" 

reshape long pobreza_ percepho_ inghog1d_ inghog2d_ gashog1d_ ingmo1hd_ ingmo2hd_ estrsocial_ mieperho_ estrato_ dominio_ facpanel_ factor07_ p101_ p102_ p103_ p103a_ p104a_ p105a_ p106a_ p110_ p110a1_ p110a_ p111a_ p1121_ p113a_ p1141_ p1142_ p1143_ p1144_ p1145_ nbi1_ nbi2_ nbi3_ nbi4_ nbi5_ conglome_ vivienda_ hogar_, i(numpanh conglome vivienda) j(año)
 
gen paredBC_ = (p102_ != 1 & p102_ != 2)
gen pisoBC_ = (p103_ == 6 | p103_ == 7)
gen techoBC_ = (p103a_ >= 4)
 
recode año (14 = 2014) (15 = 2015) (16 = 2016) (17 = 2017) (18 = 2018)(19 = 2019) (20 = 2020) (21 = 2021) (22 = 2022)

recode estrato_ (1/5=1 "Urbano") (6/8=2 "Rural"), gen(area)
la var area "Area de residencia"
tab area

gen residencia=.
replace residencia = 1 if dominio_==8
replace residencia = 2 if dominio_!=8
label define residencia 1"Lima Metropolitana" 2"No Lima Metropolitana"
label values residencia residencia

sort numpanh conglome vivienda año
recode pobreza_ (1 = 1) (2 = 1) (3 = 0)
label define pobreza_ 0"No pobre" 1"Pobre"
label values pobreza_ pobreza_

drop if pobreza_==.

bysort numpanh conglome vivienda (año) : gen pobreza_anterior = pobreza_[_n-2]
label define pobreza_anterior 0"No pobre" 1"Pobre"
label values pobreza_anterior pobreza_anterior

gen transicion_pobreza = .
replace transicion_pobreza = 0 if pobreza_anterior == 0 & pobreza_==0 // no pobre a no pobre
replace transicion_pobreza = 1 if pobreza_anterior == 1 & pobreza_==1 // pobre a pobre
replace transicion_pobreza = 2 if pobreza_anterior == 0 & pobreza_==1 // no pobre a pobre
replace transicion_pobreza = 3 if pobreza_anterior == 1  & pobreza_==0 // pobre a no pobre
label define transicion_pobreza 0 "No pobre a no pobre" 1 "Pobre a pobre" 2 "No pobre a pobre" 3 "Pobre a no pobre"
label values transicion_pobreza transicion_pobreza

order numpanh conglome vivienda año pobreza_ pobreza_anterior transicion_pobreza


// Hogares 
tab año transicion_pobreza  [iw=facpanel_]
tab año transicion_pobreza if area==1 [iw=facpanel_]
tab año transicion_pobreza if area==2 [iw=facpanel_]

// Personas
gen factor_personas = facpanel_ * mieperho
tab año transicion_pobreza [iw=factor_personas]
tab año transicion_pobreza if area==1 [iw=factor_personas]
tab año transicion_pobreza if area==2 [iw=factor_personas]

gen gpc_ = gashog1d_ / (mieperho_*12)

table año transicion_pobreza, stat(mean gpc_)

gen caidaPobreza = (transicion_pobreza == 2)
replace caidaPobreza = . if transicion_pobreza ==.