********************************************************************************
* Proyecto:	PDB IV - Pobreza urbana
* Objetivo:	Realizar estimaciones de la pobreza multidimensional urbana, contrastar con las cifras de pobreza monetaria e identificar 
* Encargados: CN/JP
********************************************************************************

*	0. Directorios

	global bd "C:\Users\User\OneDrive - MIGRACIÓN VIDENZA\1. Proyectos\1. Proyectos actuales\23. Artículos PDB\2. PDB - Pobreza Urbana\2. Data\1. Bases\2. ENAHO Anual"
    *global bd "C:\Users\LENOVO\OneDrive - VIDENZA\Proyectos activos\2. PDB - Pobreza Urbana\2. Data\1. Bases\2. ENAHO Anual"

*	1. Calculo de variables a nivel de hogar
{
	use "$bd\modulo100.dta", clear
	merge 1:1 anio conglome vivienda hogar using "$bd\sumaria.dta", keepusing(ipc* gpc* gpg* percepho mieperho pobre* quintil* decil*)
	drop if _merge==2
	drop _merge
	
*	Dimensión: Salud 

*	Atención médica
/*Al menos uno de sus miembros presentó, en las últimas cuatro semanas, alguna de los siguientes problemas:
	- síntoma o malestar,
	- enfermedad,
	- recaída de enfermedad crónica,
	- accidente,
y, adicionalmente, para consultar por dicho problema, se atendió en un lugar no adecuado, como:
	- una farmacia o botica;
	- su propio domicilio;
	- otro lugar distinto de puesto de salud MINSA, centro de salud MINSA, centro o puesto de salud CLAS, posta, policlínico EsSalud, hospital MINSA, hospital del seguro (EsSalud), hospital de las FF.AA. y/o Policía Nacional, consultorio médico particular, clínica particular;
o no buscó atención médica por alguna de las siguientes razones*:
	- no tuvo dinero,
	- se encuentra lejos,
	- demoran mucho en atender,
	- no confía en los médicos,
	- prefiere curarse con remedios caseros,
	- no tiene seguro,
	- se auto recetó o repitió receta anterior,
	- falta de tiempo,
	- por el maltrato del personal de salud,
	- responde "otro".
*/
	preserve
	use "$bd\modulo400.dta", clear
	gen mieperho=1 if (p204==1 & p203!=8 & p203!=9) & codinfor!="00"
	
	gen prob_salud=0 if p4021!=. 
	replace prob_salud=1 if (p4021==1 | p4022==1 | p4023==1 | p4024==1 | p4026==1) //presentó algún problema de salud
	gen     razon_fuerza=1 if p4091==1 | p4092==1 | p4093==1 | p4094==1 | p4096==1 | p4097==1 | p4098==1 | p4099==1 | p40910==1 | p40911==1 
	replace razon_fuerza=0 if p4091==0 & p4092==0 & p4093==0 & p4094==0 & p4096==0 & p4097==0 & p4098==0 & p4099==0 & p40910==0 & p40911==0
	
	gen     priv1=1 if prob_salud==1 & (p40310==1 | p40311==1 | p40313==1 | (p40314==1 & razon_fuerza==1)) 
	replace priv1=0 if (prob_salud==0) | (prob_salud==1 & p40310==0 & p40311==0 & p40313==0 & (p40314==0 | (p40314==1 & razon_fuerza==0)))
	
	collapse (sum) priv1 , by(anio conglome vivienda hogar)
	replace priv1 = 1 if priv1 > 1
	
	tempfile priv1
	save `priv1'
	restore
	
	merge 1:1 anio conglome vivienda hogar using `priv1'
	drop _merge
	
*	Seguro de salud

	preserve
	use "$bd\modulo400.dta", clear
	foreach x of varlist p4191 p4192 p4193 p4194 p4195 p4196 p4197{
		replace `x' = 0 if `x' ==2
	}
	gen priv2 = (p4191==0 & p4192==0 & p4193==0 & p4194==0 & p4195==0 & p4196== 0 & p4197==0)
	replace priv2 = 0 if p4198 ==1
	replace priv2 = . if (p4191==. & p4192==. & p4193==. & p4194==. & p4195==. & p4196== . & p4197==.)
	
	
	collapse (sum) priv2 , by(anio conglome vivienda hogar)
	replace priv2 = 1 if priv2 > 1
	
	tempfile priv2
	save `priv2'
	restore
	
	merge 1:1 anio conglome vivienda hogar using `priv2'
	drop _merge
	

*	Dimensión: Educación
*	Asistencia y rezago escolar
/*Al menos un niño o adolescente (de entre 5 y 19 años)** se encuentra en alguna de las siguientes situaciones:
	- no asiste a ningún centro educativo y no ha terminado la Educación Básica Regular (EBR)
	- está rezagado en el sistema educativo regular en más de dos años en correspondencia con su edad
	- si no ha completado la Educación Básica Especial (EBE) y actualmente no asiste a ningún tipo de centro
educativo
*/
	preserve 
	use "$bd\modulo300.dta", clear 
	keep if  p208a >= 5 & p208a <= 19 
	
*	No centro educativo y no EBR	
	gen privedu1 = (p307 == 2 & p301a < 6)
	
*	Rezago
	gen edad_max = 0 if p307 == 1
	replace edad_max = 7 if p308a == 1 &  p308b == 3 //inicial 5
	replace edad_max = 8 if p308a == 2 &  p308c == 1 //1 prim
	replace edad_max = 9 if p308a == 2 &  p308c == 2 //2 prim
	replace edad_max = 10 if p308a == 2 &  p308c == 3 //3 prim
	replace edad_max = 11 if p308a == 2 &  p308c == 4 //4 prim
	replace edad_max = 12 if p308a == 2 &  p308c == 5 //5 prim
	replace edad_max = 13 if p308a == 2 &  p308c == 6 //6 prim
	replace edad_max = 14 if p308a == 3 &  p308b == 1 //1 sec
	replace edad_max = 15 if p308a == 3 &  p308b == 2 //2 sec
	replace edad_max = 16 if p308a == 3 &  p308b == 3 //3 sec
	replace edad_max = 17 if p308a == 3 &  p308b == 4 //4 sec
	replace edad_max = 18 if p308a == 3 &  p308b == 5 //5 sec
	
	gen privedu2 = (((p208 >= edad_max & mes > "04") | (p208 >= edad_max - 1 & mes <= "04")) & p307 == 1 ) // EBR
	
*	No EBE y no centro educativo
	gen no_ebe = (p301a==12 & (p301b !=6 | p301b !=5)) | (p301a==12 & (p301c !=6 | p301c !=5))
	gen privedu3 = (p307==2  & no_ebe ==1 )
	
*	Privación educación 
	gen priv3=0
	replace priv3 = 1 if privedu1 == 1 | privedu2 == 1 | privedu3 == 1
	
	collapse (sum) priv3 , by(anio conglome vivienda hogar)
	replace priv3 = 1 if priv3 > 1
	
	tempfile priv3
	save `priv3'
	restore
	
	merge 1:1 anio conglome vivienda hogar using `priv3'
	drop _merge
	
	
*	Logro educativo
/*Al menos una persona de 20 años o más* no alcanzó un nivel educativo mínimo y actualmente no asiste a ningún
centro educativo ni reciben ningún programa de alfabetización; es decir: 
	- # Personas que tenían 13 años o más en 1994, no cuentan con primaria completa y actualmente no asisten a ningún centro educativo ni reciben ningún programa de alfabetización
	- # Personas que nacieron en el año 1994 en adelante o personas que tenían 12 años o menos en 1994, no cuentan con secundaria completa y actualmente no asisten a ningún centro educativo ni reciben ningún programa de alfabetización
*/
	preserve
	use "$bd\modulo300.dta", clear 
	keep if  p208a >= 20 
	gen edad94 = p208a - (anio - 1994)
	gen no_logro1 = 0
	replace no_logro1 = 1 if (edad94 >= 13) & p301a < 4 & (p306 == 2 | (p306 == 1 & p307 == 2) | p302a == 2)
	replace no_logro1 = 0 if (edad94 >= 13) & ((p301a >= 4 & p301a != 12) | ((p301a < 4) & (p307 == 1 | p302a == 1)))
	gen no_logro2 = 0
	replace no_logro2 = 1 if (edad94 < 13) & p301a < 6 & (p306 == 2 | (p306 == 1 & p307 == 2) | p302a == 2)
	replace no_logro2 = 0 if edad94 < 13 & ((p301a >= 6 & p301a != 12) | ((p301a < 6) & (p307 == 1 | p302a == 1)))
	gen no_logro3 = 0
	replace no_logro3 = 1 if p301a==12 & (p301b<6 | p301c<6) & (p306==2 | p307==2)
	replace no_logro3 = 0 if p301a==12 & (((p301b<6 | p301c<6) & p307==1) | (p301b==6 | p301c==6))

	gen priv4=0
	replace priv4= 1 if no_logro1 ==1 | no_logro2==1 
	
	collapse (sum) priv4 , by(anio conglome vivienda hogar)
	replace priv4 = 1 if priv4 > 1
	
	
	tempfile priv4
	save `priv4'
	restore
	
	merge 1:1 anio conglome vivienda hogar using `priv4'
	drop _merge
	
*	Dimensión: Vivienda
*	Materiales de la vivienda
/*La vivienda tiene alguna de las siguientes características:
	- el material de las paredes exteriores es predominantemente adobe, tapia, quincha, piedra con barro, triplay/calamina/estera, u "otro" distinto de ladrillo, bloque de cemento, piedra o sillar con cal o cemento, o madera;
	- el material de los techos es predominantemente caña o estera con torta de barro o cemento, triplay/estera/carrizo, paja/hoja de palmera/similares, u "otro" distinto de concreto armado, madera, tejas, planchas de calamina, fibra de cemento o similares;
	- el material de los pisos es predominantemente tierra u "otro" distinto de parquet o madera pulida, láminas asfálticas, vinílicos (o similares), losetas, terrazos (o similares), madera o cemento; 
	- la vivienda es improvisada, o es un local no destinado para habitación humana, o es de "otro" tipo distinto de casa independiente, departamento en edificio, vivienda en quinta, vivienda en casa de vecindad, choza o cabaña.*/

	gen priv5 = (p102 != 1 & p102 != 2) | (p103a > 4) | (p103 == 6 | p103 == 7) | (p101 >= 6)
	
*	Hacinamiento
	bys anio conglome vivienda : egen mieperviv = total(mieperho)
	gen hacinam = mieperviv/p104
	gen priv6 = (hacinam>=3)
		
*	Dimensión: Agua y Saneamiento
*	Agua
/*	El agua que utilizan en su hogar proviene de:
	- camión-cisterna u otro similar,
	- pozo (agua subterránea),
	- manantial o puquio,
	- río, acequia, lago, laguna,
	- responde "otro".
	- O proviene de la red pública dentro de la vivienda; la red pública fuera de la vivienda, pero dentro del edificio; o un pilón o pileta de uso público; pero no tiene acceso al agua todos los días de la semana y las 24 horas del día.*/
	
	gen priv7 = (p110 >= 4) | (p110 < 4 & p110c == 2) | (p110 < 4 & p110c == 1 & p110c1 != 24)
	
*	Saneamiento
/*El baño o servicio higiénico de su hogar está conectado a*:
	- pozo ciego o negro,
	- río, acequia, canal o similar,
	- campo abierto o al aire libre,
	- responde "otro"
*Las categorías que se consideran como no privación son red pública de desagüe dentro de la vivienda; red pública de desagüe fuera de la vivienda, pero dentro del edificio; letrina con tratamiento; pozo séptico, tanque séptico, o biodigestor.*/
	gen priv8 = (p111a >= 5)


*	Dimensión: Energía
*	Electricidad
/*El tipo de alumbrado al que tiene acceso no es eléctrico, sino es por:
	- petróleo/gas (lámpara),
	- vela,
	- generador,
	- responde "otro",
	- no utiliza.*/
	gen priv9 = (p1123 == 1 | p1124 == 1 | p1125 == 1 | p1126 == 1 | p1127 == 1)
	replace priv9 = 0 if p1121 ==1
*	Combustible para cocinar
/*El combustible que utiliza con mayor frecuencia para cocinar los alimentos es*:
	- carbón;
	- leña;
	- bosta, estiércol;
	- responde "otro".

	No se considera como privación si no se cocina en el hogar, o si responde electricidad, gas (balón GLP), o gas natural (sistema de tuberías).*/
	gen priv10 = (p113a == 5 | p113a == 6 | p113a == 9 | p113a ==7)

*	Dimensión: Empleo y previsión social
*	Empleo
/*Al menos una persona de 14 años o más se encuentran en alguna de las siguientes situaciones:
	- está en situación de desempleo (persona que se encuentra desocupada, pero que tiene deseos y disponibilidad para trabajar, y busca trabajo activamente);
	- está en situación de desaliento (persona que se encuentra desocupada, sí tiene deseos y disponibilidad para trabajar, pero no busca trabajo activamente);
	- está en situación de subempleo por horas (persona que se encuentra ocupada, pero que trabaja menos de 35 horas a la semana, y sí tiene deseos y disponibilidad para trabajar más horas).*/
	preserve
	use "$bd\modulo500.dta", clear
	keep if p208a >= 14
	egen horas = rowtotal(i513t i518)
	gen priv11 =  0
	replace priv11 = 1 if (ocu500 == 2) | (ocu500 == 3) 
	replace priv11 = 1 if ocu500 == 1 & p519 == 1 & (horas <35) & (p521 == 1 & p521a == 1)
	replace priv11 = 1 if ocu500 == 1 & (p519 == 2 & i520 <35) & (p521 == 1 & p521a == 1)
	collapse (sum) priv11, by(anio conglome vivienda hogar)
	replace priv11 = 1 if priv11 > 1
	
	tempfile priv11
	save `priv11'
	restore
	
	merge 1:1 anio conglome vivienda hogar using `priv11'
	drop _merge

*	Pensión
/* Al menos una persona de 14 años o más se encuentran en alguna de las siguientes situaciones:
	- tiene entre 14 y 64 años, se encuentra ocupada, pero no está afiliada a ningún sistema de pensiones o no ha aportado a dicho sistema hace un año o más;
	- tiene 65 años o más, se encuentra ocupada, pero no está afiliada a ningún sistema de pensiones o no ha aportado a dicho sistema hace un año o más, y no recibe ningún tipo de pensión (jubilación/cesantía; viudez, orfandad o sobrevivencia; Pensión 65);
	- tiene 65 años o más, se encuentra desocupada y no recibe ningún tipo de pensión (jubilación/cesantía; viudez, orfandad o sobrevivencia; Pensión 65).*/ 
	
	preserve
	use "$bd\modulo500.dta", clear
	keep if p208a >= 14
	destring mes, replace
	gen date1 = ym(anio, mes)
	gen date2 = ym(p558b2, p558b1)
	gen diff = date1 - date2
	gen priv12 = 0
	replace priv12 = 1 if (p208a >=14 & p208a <65) & (ocu500 == 1) & (p558a5 == 5 | (diff > 12))
	replace priv12 = 1 if (p208a >= 65) & (ocu500 == 1) & (p558a5 == 5 | diff > 12) & (p5564a == 2 & p5565a == 2 & p5567a == 2)
	replace priv12 = 1 if (p208a >= 65) & (ocu500 != 1) & (p5564a == 2 & p5565a == 2 & p5567a == 2)
	
	collapse (sum) priv12, by(anio conglome vivienda hogar)
	replace priv12 = 1 if priv12 > 1
	
	tempfile priv12
	save `priv12'
	restore
	
	merge 1:1 anio conglome vivienda hogar using `priv12'
	drop _merge

	
*	Dimensión: Conectividad
*	Pertenencia a redes
/*Ninguno de los miembros del hogar pertenece a ninguno de los siguientes tipos de organización o asociación*:
	- clubes y asociaciones deportivas,
	- agrupación o partido político,
	- clubes culturales (danza, música, etc.),
	- asociación vecinal/junta vecinal,
	- ronda campesina,
	- asociación de regantes,
	- asociación profesional,
	- asociación de trabajadores o sindicato,
	- club de madres,
	- asociación de padres de familia (APAFA),
	- vaso de leche,
	- comedor popular,
	- Comité Local Administrativo de Salud (CLAS),
	- Concejo de Coordinación Local Distrital (CCLD),
	- comunidad campesina,
	- asociación agropecuaria,
	- participación en la preparación de desayuno y/o almuerzo escolar,
	- otro/a.

*No se considera como privación si los miembros del hogar no participan en ninguna organización o asociación porque el jefe de hogar o cónyuge afirma que "no le interesa" participar.*/
	preserve
	use "$bd\modulo800a.dta", clear
	egen no_org = rowtotal(p801_1 p801_2 p801_3 p801_4 p801_5 p801_6 p801_7 p801_8 p801_9 p801_10  p801_11 p801_12 p801_13 p801_15 p801_16 p801_17 p801_18 p801_20), m
	  
	gen priv13 = (no_org==0) & p806!=5
	replace priv13 = . if no_org ==.
	
	destring anio , replace
	
	tempfile priv13
	save `priv13'
	restore
	
	merge 1:1 anio conglome vivienda hogar using `priv13'
	drop if _merge == 2
	drop _merge

*	Tecnologías de la información y la comunicación
/*No tiene acceso a ninguna de las siguientes tecnologías:
	- teléfono fijo,
	- celular,
	- internet*/	
	gen priv14 = (p1141 == 0 & p1142 == 0 & p1144 == 0)

}

* 	2. Cálculo de IPM = H (Incidencia o tasa de recuento de la pobreza multidimensional) x A

	tempfile privaciones
	save `privaciones'

	use "$bd\modulo200.dta", clear
	merge m:1 anio conglome vivienda hogar using `privaciones', keepusing(priv*)
/*El IPM propuesto se construye en base al método Alkire Foster e identifica a una persona como pobre multidimensional si tiene carencias en al menos 3 de las 7 dimensiones, o 6 de los 14 indicadores ponderados. */



* Creación de variable de dimensiones

	gen dim1 = (priv1 == 1 | priv2 == 1)
	gen dim2 = (priv3 == 1 | priv4 == 1)
	gen dim3 = (priv5 == 1 | priv6 == 1)
	gen dim4 = (priv7 == 1 | priv8 == 1)
	gen dim5 = (priv9 == 1 | priv10 == 1)
	gen dim6 = (priv11 == 1 | priv12 == 1)
	gen dim7 = (priv13 == 1 | priv14 == 1)

foreach var of varlist priv*  {
	tab `var' [iw = facpob07] if anio ==2022
}	
* Creación de variable de incidencia
	
	egen nDim = rowtotal(dim*), m
	egen nPriv = rowtotal(priv*), m
	gen hIPM = (nDim >=3 | nPriv >= 6)
	replace hIPM = . if nDim == . | nPriv ==. 

* Tabulación

	gen area = (estrato <6)
	
	table anio area  [iw = facpob07], stat(mean hIPM)