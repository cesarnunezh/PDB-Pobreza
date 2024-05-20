********************************************************************************
* Proyecto:	PDB IV - Pobreza urbana
* Objetivo:	Realizar estimaciones de la pobreza multidimensional urbana, contrastar con las cifras de pobreza monetaria e identificar 
* Encargados: CN/MG
********************************************************************************
*	1. Calculo de variables a nivel de hogar
{
	use "$temp\modulo100.dta", clear
	merge 1:1 anio conglome vivienda hogar using "$temp\sumaria.dta", keepusing(ipc* gpc* gpg* percepho mieperho pobre* quintil* decil*)
	keep if _m==3
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
	use "$temp\modulo400.dta", clear
	restore
	
/*# Al menos un miembro sufrió: síntoma o malestar, enfermedad, recaida de enfermedad crónica, accidente*/
#* (p4021 == 1 | p4022 == 1 | p4023 == 1 | p4024 == 1) 
# y, adicionalmente, para consultar por dicho problema, se atendió en un lugar no adecuado,
# ((p40310 == 1 | p40311 == 1 | p40312 == 1 | p40313 == 1) 
# o no buscó atención médica por alguna de las siguientes razones
# (p4091 == 1 | p4092 == 1 | p4093 == 1 | p4094== 1 |  p4096== 1 |  p4097== 1 |  p4098== 1 |  p4099== 1 |  p40910== 1 |  p40911== 1)
#  No se considera como privación si la persona responde que el problema de salud "no era grave" o que "no era necesario buscar atención".
# p4095== 1

*	Seguro de salud
#* Al menos uno de sus miembros no se encuentra afiliado a ningún tipo de seguro de salud
#* p4191-p4197 == 1
# No se considera como privación si la persona responde "otro".
# p4198==1

*	Dimensión: Educación

*	Asistencia y rezago escolar
# Primer filtro por edad
# p208a >= 5 & p208a <= 19 
# No asiste a ningún centro educativo y no ha terminado la Educación Básica Regular (EBR)
# (p306 == 2 | p307 == 2) & p301a < 6 
# está rezagado en el sistema educativo regular en más de dos años en correspondencia con su edad
# Idea: crear variable edadMax para cada nivel y compararlo a la edad de la persona (ejm: edad máxima para grado 1 primaria es 9) P308A P304A
# si no ha completado la Educación Básica Especial (EBE) y actualmente no asiste a ningún tipo de centro educativo 
# PENDIENTE
*	Logro educativo
# Personas que tenían 13 años o más en 1994, no cuentan con primaria completa y actualmente no asisten a ningún centro educativo ni reciben ningún programa de alfabetización
# ((p208a - (anio - 1994)) >= 13) & p301a < 4 & (p306 == 2 | p307 == 2) & p302 == 2

# Personas que nacieron en el año 1994 en adelante o personas que tenían 12 años o menos en 1994, no cuentan con secundaria completa y actualmente no asisten a ningún centro educativo ni reciben ningún programa de alfabetización
# ((p208a - (anio - 1994)) < 13) & p301a < 6 & (p306 == 2 | p307 == 2) & p302 == 2

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
	use "$temp\modulo500.dta", clear
	keep if p208a >= 14
	egen horas = rowtotal(i513t i518)
	gen priv11 = (ocu500 == 2) | (ocu500 == 3) | ((ocu500 == 1) & ((p519 == 1 & (horas <35)) | (p519 == 2 & i520 <35)) & (p521 == 1 & p521a == 1))
	collapse (sum) priv11, by(anio conglome vivienda hogar)
	replace priv11 = 1 if priv11 > 1
	
	tempfile priv11
	save `priv11'
	restore
	
	merge 1:1 anio conglome vivienda hogar using `priv11'

*	Pensión
/* Al menos una persona de 14 años o más se encuentran en alguna de las siguientes situaciones:
	- tiene entre 14 y 64 años, se encuentra ocupada, pero no está afiliada a ningún sistema de pensiones o no ha aportado a dicho sistema hace un año o más;
	- tiene 65 años o más, se encuentra ocupada, pero no está afiliada a ningún sistema de pensiones o no ha aportado a dicho sistema hace un año o más, y no recibe ningún tipo de pensión (jubilación/cesantía; viudez, orfandad o sobrevivencia; Pensión 65);
	- tiene 65 años o más, se encuentra desocupada y no recibe ningún tipo de pensión (jubilación/cesantía; viudez, orfandad o sobrevivencia; Pensión 65).*/ 
	
	preserve
	use "$temp\modulo500.dta", clear
	keep if p208a >= 14
	destring mes, replace
	gen date1 = ym(anio, mes)
	gen date2 = ym(p558b2, p558b1)
	gen diff = date1 - date2
	gen priv12 = (p208a >=14 & p208a <65) & (ocu500 == 1) & (p558a5 == 5 | (diff > 12))
	replace priv12 = 1 if (p208a >= 65) & (ocu500 == 1) & (p558a5 == 5 | diff > 12) & (p5564a == 2)
	replace priv12 = 1 if (p208a >= 65) & (ocu500 == 2) & (p5564a == 2 & p5565a == 2 & p5567a == 2)
	
	collapse (sum) priv12, by(anio conglome vivienda hogar)
	replace priv12 = 1 if priv12 > 1
	
	tempfile priv12
	save `priv12'
	restore
	
	merge 1:1 anio conglome vivienda hogar using `priv12'

	
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
	p801_20 == 20 & p806 == 5

*	Tecnologías de la información y la comunicación
/*No tiene acceso a ninguna de las siguientes tecnologías:
	- teléfono fijo,
	- celular,
	- internet*/	
	gen priv14 = (p1141 == 0 & p1142 == 0 & p1144 == 0)

}	