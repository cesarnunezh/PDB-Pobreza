
********************************************************************************
*** "Propuesta Metodológica para la medición de la 
***  Pobreza Multidimensional en la Población en General
********************************************************************************

***Bases requeridas
*) enaho01-2021-100.dta   ->  1: Características de la Vivienda y del Hogar
*) enaho01-2021-200.dta   ->  2: Características de los Miembros del Hogar
*) enaho01a-2021-300.dta  ->  3. Educación
*) enaho01a-2021-400.dta  ->  4. Salud
*) enaho01a-2021-500.dta  ->  5. Empleo e Ingresos
*) sumaria-2021.dta		  -> 34. Sumarias (Variables Calculadas) 
*) enaho01-2021-800a.dta  -> 84. Participación Ciudadana
*) enaho01b-2021-2.dta    -> 85. Gobernabilidad, Democracia y Transparencia

global ao=2022
global L="F"

cd ""
global enaho "${L}:\aaa\1.ENAHO\\${ao}"
global temp  "${L}:\aaa\Temporal"


********************************************************************************
* A. Construcción de la base de datos
********************************************************************************

use "$enaho/enaho01-${ao}-200", clear
keep aÑo-estrato codperso p201 p203 p204 p205 p206 p207 p208* facpob07
keep if p204==1

merge m:1 conglome vivienda hogar using "$enaho/enaho01-${ao}-100.dta", keepusing(p101 p102 p103* p104 p110* p111* p112* p113* p114* result)
tab result _merge, m // Mantener solo completa e incompleta
drop if _merge==2
drop _merge 

merge m:1 conglome vivienda hogar using "$enaho/sumaria-${ao}.dta", keepusing(pobreza mieperho linea linpe factor07)
drop _merge

merge 1:1 conglome vivienda hogar codperso using "$enaho/enaho01a-${ao}-300.dta", keepusing(p300* p301* p302* p306 p307 p308*)
tab p208a _merge, m // 
tab p204 _merge, m  //   
keep if p204==1		//   
drop _merge

merge 1:1 conglome vivienda hogar codperso using "$enaho/enaho01a-${ao}-400", keepusing(p402* p403* p409* p419*)
tab p204 _merge, m //  
keep if p204==1	   //  
drop _merge

merge 1:1 conglome vivienda hogar codperso using "$enaho/enaho01a-${ao}-500.dta", keepusing(p513* p514* p518* p519* p520* p521* p558* p556* ocu500)
tab p208a _merge, m // 
tab p204 _merge, m  // 
keep if p204==1		// 
drop _merge

merge m:1 conglome vivienda hogar using "$enaho/enaho01-${ao}-800a.dta", keepusing(p801_19 p806)
drop _merge

merge m:1 conglome vivienda hogar using "$enaho/enaho01b-${ao}-2.dta", force keepusing(p37 p39b1)
drop _merge

drop if p205 == 1
// dropear a aquellas personas que se encuentran ausentes del hogar 30 días o más

save "${temp}/enaho${ao}.dta", replace



********************************************************************************
*** B. Indicadores
********************************************************************************

use "${temp}/enaho${ao}.dta", clear
*keep aÑo mes conglome vivienda hogar p101 p102 p103* p104 p110* p111* p112* p113* p203 p204 p205 p206 p207 p208* p301* p302* p306 p307 p308* p402* p403* p409* p419* p513* p514* p518* p519* p520* p521* p558* p556* ocu500 p801* p806 p37 p39b1
*-------------------------------------------------------------------------------
*** Dimensión: SALUD
*-------------------------------------------------------------------------------

*** Indicador 1: Atención médica
gen     problema_salud = 1 if p4021 == 1 | p4022 == 1 | p4023 == 1 | p4024 == 1
replace problema_salud = 0 if p4021 == 0 & p4022 == 0 & p4023 == 0 & p4024 == 0

gen     razon_fuerza = 1 if p4091==1 | p4092==1 | p4093==1 | p4094==1 | p4096==1 | p4097==1 | p4098==1 | p4099==1 | p40910==1 | p40911==1
replace razon_fuerza = 0 if p4091==0 & p4092==0 & p4093==0 & p4094==0 & p4096==0 & p4097==0 & p4098==0 & p4099==0 & p40910==0 & p40911==0

gen     atencion = 1 if problema_salud==1 & (p40310==1 | p40311==1 | p40313==1 | (p40314==1 & razon_fuerza==1))
replace atencion = 0 if (problema_salud==0) | (problema_salud==1 & p40310==0 & p40311==0 & p40313==0 & (p40314==0 | (p40314==1 & razon_fuerza==0)))

* Nivel hogar
bys conglome vivienda hogar: egen num_sin_atencion_hog = total(atencion==1)
bys conglome vivienda hogar: egen    num_atencion_miss = total(atencion==.)

gen     atencion_hog = 1 if num_sin_atencion_hog>=1 & num_sin_atencion_hog!=.
replace atencion_hog = 0 if num_sin_atencion_hog==0 & num_atencion_miss==0



*** Indicador 2: Seguro de salud
gen     seguro = 1 if p4191==2 & p4192==2 & p4193==2 & p4194==2 & p4195==2 & p4196==2 & p4197==2 & p4198==2 
replace seguro = 0 if p4191==1 | p4192==1 | p4193==1 | p4194==1 | p4195==1 | p4196==1 | p4197==1 | p4198==1

* Nivel hogar
bys conglome vivienda hogar: egen num_sin_seguro_hog = total(seguro==1)
bys conglome vivienda hogar: egen    num_seguro_miss = total(seguro==.)

gen     seguro_hog = 1 if num_sin_seguro_hog>=1 & num_sin_seguro_hog!=.
replace seguro_hog = 0 if num_sin_seguro_hog==0 & num_seguro_miss==0



*-------------------------------------------------------------------------------
*** Dimensión: EDUCACIÓN
*-------------------------------------------------------------------------------

*** Indicador 3: Asistencia y rezago escolar
gen     grado = p308b if p308a!=2 & p308a!=7 & p308a!=. 
replace grado = p308c if p308a==2 | p308a==7

gen brecha_educ = p208a - grado

gen     educ5_19 = 1 if (p208a>=5 & p208a<20) & p308a==2 & brecha_educ>=8 & brecha_educ!=.
replace educ5_19 = 0 if (p208a>=5 & p208a<20) & p308a==2 & brecha_educ<8
replace educ5_19 = 1 if (p208a>=5 & p208a<20) & p308a==3 & brecha_educ>=14 & brecha_educ!=.
replace educ5_19 = 0 if (p208a>=5 & p208a<20) & p308a==3 & brecha_educ<14
replace educ5_19 = 1 if (p208a>=5 & p208a<20) & p308a==1 & brecha_educ>=5 & brecha_educ!=.
replace educ5_19 = 0 if (p208a>=5 & p208a<20) & p308a==1 & brecha_educ<5

replace educ5_19 = 1 if (p208a>=5 & p208a<20) & p301a<=5 & (p306==2 | p307==2)
replace educ5_19 = 0 if (p208a>=5 & p208a<20) & p301a>5 & p301a!=12 & p301a!=.

replace educ5_19 = 1 if (p208a>=5 & p208a<20) & p301a==12 & (p301b<6 | p301c<6) & (p306==2 | p307==2)
replace educ5_19 = 0 if (p208a>=5 & p208a<20) & p301a==12 & (((p301b<6 | p301c<6) & p307==1) | (p301b==6 | p301c==6))

* Nivel hogar
bys conglome vivienda hogar: egen num_sin_educ5_19_hog = total(educ5_19==1)
bys conglome vivienda hogar: egen    num_educ5_19_miss = total(educ5_19==. & p208a>=5 & p208a<20)
bys conglome vivienda hogar: egen             num_5_19 = total(p208a>=5 & p208a<20)

gen     asistencia_rezago_hog = 1 if num_sin_educ5_19_hog>=1 & num_sin_educ5_19_hog!=.
replace asistencia_rezago_hog = 0 if num_sin_educ5_19_hog==0 & num_educ5_19_miss==0
replace asistencia_rezago_hog = 0 if num_5_19==0



*** Indicador 4: Logro educativo en adultos de 20 años o más
gen año_enc = ${ao}
gen nacimiento = año_enc - p208a
gen edad_1994 = 1994 - nacimiento

gen     logro_20= 1 if p208a>=20 & edad_1994>=13 & p301a<4 & ((p306==2 | (p306==1 & p307==2) | p302a==2))
replace logro_20= 0 if p208a>=20 & edad_1994>=13 & ((p301a>=4 & p301a!=12) | ((p301a<4) & (p307==1 | p302a==1)))

replace logro_20= 1 if p208a>=20 & edad_1994 <13 & p301a<6 & ((p306==2 | (p306==1 & p307==2)) | p302a==2)
replace logro_20= 0 if p208a>=20 & edad_1994 <13 & ((p301a>= 6 & p301a!=12) | ((p301a<6) & (p307==1 | p302a==1)))

replace logro_20= 1 if p208a>=20 & p301a==12 & (p301b<6 | p301c<6) & (p306==2 | p307==2)
replace logro_20= 0 if p208a>=20 & p301a==12 & (((p301b<6 | p301c<6) & p307==1) | (p301b==6 | p301c==6))

* Nivel hogar
bys conglome vivienda hogar: egen  num_sin_logro_20 = total(logro_20 == 1)
bys conglome vivienda hogar: egen num_logro_20_miss = total(logro_20==. & p208a>=20 & p208a!=.)
bys conglome vivienda hogar: egen        num_20_mas = total(p208a>=20 & p208a!=.)

gen     logro_hog = 1 if num_sin_logro_20>=1 & num_sin_logro_20!=.
replace logro_hog = 0 if num_sin_logro_20==0 & num_logro_20_miss==0
replace logro_hog = 0 if num_20_mas==0



*-------------------------------------------------------------------------------
*** Dimensión: VIVIENDA
*-------------------------------------------------------------------------------


*** Indicador 5: Materiales de la vivienda
recode p102 1/2=0 3/9=1, gen(paredes)
recode p103 1/5=0 6/7=1, gen(piso)
recode p103a 1/4=0 5/8=1, gen(techo)
recode p101 1/5=0 6/8=1, gen(tipo_vivienda)

gen     materiales = 1 if paredes==1 | piso==1 | techo==1 | tipo_vivienda==1
replace materiales = 0 if paredes==0 & piso==0 & techo==0 & tipo_vivienda==0



*** Indicador 6: Hacinamiento
bys conglome vivienda: egen mieperviv = total(p204==1)

gen personas_hab = mieperviv/p104

gen     hacinamiento = 1 if personas_hab>=3 & personas_hab!=.
replace hacinamiento = 0 if personas_hab<3



*-------------------------------------------------------------------------------
*** Dimensión: AGUA Y SANEAMIENTO 
*-------------------------------------------------------------------------------

*** Indicador 7: Agua
recode p110 1/3=1 4/8=0, gen(red_pub_agua)

gen agua = 1 if (red_pub_agua==0) | (red_pub_agua==1 & ((p110c==2) | (p110c1<24)))
replace agua = 0 if red_pub_agua == 1 & p110c == 1 & p110c1 == 24

*** Indicador 8: Saneamiento
recode p111a 1/4=0 5/9=1, gen(saneamiento)



*-------------------------------------------------------------------------------
*** Dimensión: ENERGÍA
*-------------------------------------------------------------------------------

*** Indicador 9: Electricidad
recode p1121 0=1 1=0, gen(electricidad)


*** Indicador 10: Combustible para cocinar
gen combustible = 1 if p113a==5 | p113a==6 | p113a==7 | p113a==9
replace combustible = 0 if p113a==1 | p113a==2 | p113a==3 | p1138==1



*-------------------------------------------------------------------------------
*** Dimensión: EMPLEO Y PREVISIÓN SOCIAL
*-------------------------------------------------------------------------------

*** Indicador 11: Empleo (Desempleo, desaliento, subempleo por horas)
gen     horas = p513t if p514==2 & p519==1
replace horas = p513t + p518 if p514==1 & p519==1
replace horas = p520 if p519==2

gen     subempleo = 1 if ocu500==1 & horas<35 & p521==1 & p521a==1
replace subempleo = 0 if ocu500==1 & ((horas>=35 & horas!=.) | (horas<35 & (p521==2 | p521a==2)))

gen     empleo = 1 if ocu500==2 | ocu500==3 | (ocu500==1 & subempleo==1)
replace empleo = 0 if ocu500==4 | (ocu500==1 & subempleo==0)

* Nivel hogar
bys conglome vivienda hogar: egen num_sin_empleo_hog = total(empleo==1)
bys conglome vivienda hogar: egen    num_empleo_miss = total(empleo==. & p208a>=14 & p208a!=.)

gen     empleo_hog = 1 if num_sin_empleo_hog>=1 & num_sin_empleo_hog!=.
replace empleo_hog = 0 if num_sin_empleo_hog==0 & num_empleo_miss==0



*** Indicador 12: Pensión (p558b1:mes, p558b2: año)
gen taporte=abs(p558b2 - ${ao}) if p558b2!=.
tab taporte p558b2 

recode p558a5 5=1, gen(no_afiliado)

destring mes, replace

gen     no_aporta = 1 if taporte >= 2 & taporte!=.		// 2 años a mas atras
replace no_aporta = 1 if taporte == 1 & mes >  p558b1   // Año pasado, si aporto hasta antes del mes de la encuesta de este año (mas de 1 año)
replace no_aporta = 0 if taporte == 1 & mes <= p558b1	// Año pasado, si aporto hasta despues del mes de la encuesta de este año (menos de 1 año)
replace no_aporta = 0 if taporte == 0

gen     no_recibe_pension=1 if p5564a==2 & p5565a==2 & p5567a==2
replace no_recibe_pension=0 if p5564a==1 | p5565a==1 | p5567a==1

* 14-64
gen     pension_14_64 = 1 if (p208a>=14 & p208a<=64) & ocu500==1 & (no_afiliado==1 | no_aporta==1)
replace pension_14_64 = 0 if (p208a>=14 & p208a<=64) & ((ocu500==1 & no_afiliado==0 & no_aporta==0) | (ocu500==2 | ocu500==3 | ocu500==4))

* 65+
gen     pension_65 = 1 if (p208a>=65 & p208a!=.) & (no_recibe_pension==1) & ((ocu500==1 & (no_afiliado==1 | no_aporta==1)) | (ocu500==2 | ocu500==3 | ocu500==4))
replace pension_65 = 0 if (p208a>=65 & p208a!=.) & ((no_recibe_pension==0) | ((no_recibe_pension==1) & (ocu500==1 & (no_afiliado==0 & no_aporta==0))))

* Total de 14 a más
gen     pension = 1 if (p208a>=14 & p208a<=64 & pension_14_64==1) | (p208a>=65 & p208a!=. & pension_65==1)
replace pension = 0 if (p208a>=14 & p208a<=64 & pension_14_64==0) | (p208a>=65 & p208a!=. & pension_65==0)

* Nivel hogar
bys conglome vivienda hogar: egen num_sin_pension_hog = total(pension==1)
bys conglome vivienda hogar: egen    num_pension_miss = total(pension==. & p208a>=14 & p208a!=.)

gen     pension_hog = 1 if num_sin_pension_hog>=1 & num_sin_pension_hog!=.
replace pension_hog = 0 if num_sin_pension_hog==0 & num_pension_miss==0



*-------------------------------------------------------------------------------
*** Dimensión: CONECTIVIDAD
*-------------------------------------------------------------------------------

*** Indicador 13: Pertenencia a redes
recode p801_19 19=1, gen(no_participa)

gen redes = 1 if no_participa==1 & (p806!=5 & p806!=.)
replace redes = 0 if no_participa==0 | (no_participa==1 & p806==5)


*** Indicador 14: Tecnologías de la información y la comunicación (Tel.fijo, Tel.cel e Internet)
gen comunicacion = 1 if p1141==0 & p1142==0 & p1144==0
replace comunicacion = 0 if p1141==1 | p1142==1 | p1144==1


*****************************
* Variables dedesagregación
*****************************

gen nacional=1
lab def nacional 1"Nacional"
lab val nacional nacional

recode estrato 1/5=0 6/8=1, gen(rural)
label define estrato_label 0 "Urbano" 1 "Rural"
label values rural estrato_label

recode p207 1=0 2=1, gen(mujer)
label define sexo_label 0 "Hombre" 1 "Mujer"
label values mujer sexo_label

gen     grupo_edad = 1 if p208a <= 4
replace grupo_edad = 2 if p208a >= 5 & p208a <= 17
replace grupo_edad = 3 if p208a >= 18 & p208a <= 59
replace grupo_edad = 4 if p208a >= 60 
label define edad_label 1 "0 a 4" 2 "5 a 17" 3 "18 a 59" 4 "60+"
label values grupo_edad edad_label

gen cod_region = substr(ubigeo, 1, 2)
destring cod_region, gen(region)
label define region_label 1 "Amazonas" 2 "Áncash" 3 "Apurímac" 4 "Arequipa" 5 "Ayacucho" 6 "Cajamarca" 7 "Callao" 8 "Cusco" 9 "Huancavelica" 10 "Huánuco" 11 "Ica" 12 "Junín" 13 "La_Libertad" 14 "Lambayeque" 15 "Lima" 16 "Loreto" 17 "Madre_de_Dios" 18 "Moquegua" 19 "Pasco" 20 "Piura" 21 "Puno" 22 "San_Martín" 23 "Tacna" 24 "Tumbes" 25 "Ucayali"
label values region region_label

recode dominio 1/3=1 4/6=2 7=3 8=4, gen(dominio_geo)
labe define dominio_label 1 "Costa" 2 "Sierra" 3 "Selva" 4 "Lima Metropolitana"
label values dominio_geo dominio_label

gen     dominio_rur = 1 if dominio_geo == 1 & rural == 0
replace dominio_rur = 2 if dominio_geo == 1 & rural == 1
replace dominio_rur = 3 if dominio_geo == 2 & rural == 0
replace dominio_rur = 4 if dominio_geo == 2 & rural == 1
replace dominio_rur = 5 if dominio_geo == 3 & rural == 0
replace dominio_rur = 6 if dominio_geo == 3 & rural == 1
replace dominio_rur = 7 if dominio_geo == 4
label define dominio_rural_label 1 "Costa urbana" 2 "Costa rural" 3 "Sierra urbana" 4 "Sierra rural" 5 "Selva urbana" 6 "Selva rural" 7 "Lima Metropolitana"
label values dominio_rur dominio_rural_label

recode p300a 1/3=1 4/9=0 10/15=1, gen(indigena_lengua)
recode p558c 1/3=1 9=1 4/8=0, gen(indigena_auto)

gen     indigena = 1 if indigena_lengua==1 | indigena_auto==1
replace indigena = 0 if (indigena_lengua==0 & indigena_auto==0) | (indigena_lengua==0 & indigena_auto==.) | (indigena_lengua==. & indigena_auto==0)

svyset conglome [iw=factor07], strata(estrato) || vivienda



**************************
* CÁLCULOS PARA EL IPM
**************************

* generar score de privación *
gen c_i = 1/14 * (atencion_hog + seguro_hog + asistencia_rezago_hog + logro_hog + materiales + hacinamiento + agua + saneamiento + electricidad + combustible + empleo_hog + pension_hog + redes + comunicacion)

* Cálculo del H (Tasa de Recuento, H, o Incidencia de la Pobreza)
forvalues i = 6(1)6 {
gen     pobre_`i'_14 = 1 if c_i >= `i'/14 & c_i!=.
replace pobre_`i'_14 = 0 if c_i <  `i'/14
}

* Cálculo del A (Intensidad de la Pobreza entre los Pobres)
forvalues i = 6(1)6 {
gen cc_k`i'_14 = c_i if pobre_`i'_14 == 1
}

* Cálculo del M0 (Tasa de Recuento Ajustada)
forvalues i = 6(1)6 {
gen     ck_`i'_14 = c_i if pobre_`i'_14 == 1
replace ck_`i'_14 = 0   if pobre_`i'_14 == 0
}


***Cruce con pobreza
fre pobreza
recode pobreza 2=1 3=0, gen(pobre_mon)
clonevar pobre_mul=pobre_6_14

gen pmon_npmul  =(pobre_mon==1 & pobre_mul==0) & pobre_mon!=. & pobre_mul!=.
gen npmon_npmul =(pobre_mon==0 & pobre_mul==0) & pobre_mon!=. & pobre_mul!=.
gen npmon_pmul  =(pobre_mon==0 & pobre_mul==1) & pobre_mon!=. & pobre_mul!=.
gen pmon_pmul   =(pobre_mon==1 & pobre_mul==1) & pobre_mon!=. & pobre_mul!=.

***HH: TABULADOS

svyset conglome [iw=factor07], strata(estrato) || vivienda
	

*** ESTIMACIONES PARA CADA PUNTO DE CORTE POR SUBGRUPO ***

*** Nacional
* H
foreach nivel in nacional rural dominio_rur mujer grupo_edad region indigena {
svy: mean pobre_6_14, over(`nivel')
estat cv
}


* M0
foreach nivel in nacional rural dominio_rur mujer grupo_edad region indigena {
svy: mean ck_6_14, over(`nivel')
estat cv
}

