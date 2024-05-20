################################################################################
# Objetivo: Unión de los módulos de la ENAHO para el periodo 2007-2022
# Proyecto:  GiZ Pobreza Urbana
# 
# Estructura:
# 0. Librerías
# 1. Unión de bases
# 2. Creación de variables de interés
################################################################################

# 0. Librerías y direcciones ----
dirEnaho <- "C:/Users/User/OneDrive - MIGRACIÓN VIDENZA/1. Proyectos/1. Proyectos actuales/GiZ Pobreza Urbana/1. Productos/0. Insumos/1. Bases de datos/ENAHO"
#dirEnaho <- "/etc/data/"
library(tidyverse)
library(haven)
library(dplyr)

# 1. Unión de bases ------------------------------------------------------------

## 1.1. A nivel de hogar -----
setwd(dirEnaho)

#solo carga columnas con col_select
mod18 <- read_dta("modulo18.dta",
                  col_select = c(anio, mes, conglome, vivienda, hogar, ubigeo, dominio, estrato, p612n, p612))

mod18 <- mutate(mod18, p612= case_when(p612 == 2 ~ 0,
                                       TRUE ~ 1 ))

mod18h <- mod18 %>%
  group_by(anio, mes, conglome, vivienda, hogar, ubigeo, dominio, estrato) %>%
  pivot_wider(
    names_from = p612n,
    values_from = p612,
    names_prefix = "equipment_") %>% 
  mutate(nActivos = sum(c_across(starts_with("equipment")), na.rm = TRUE)) %>% 
  rename(radio = "equipment_1",
         tv_color = "equipment_2",
         tv_bn = "equipment_3",
         equipo_sonido = "equipment_4",
         dvd = "equipment_5",
         video_grabadora = "equipment_6",
         computadora = "equipment_7",
         plancha = "equipment_8",
         licuadora = "equipment_9",
         cocina_gas = "equipment_10",
         cocina_kerosene = "equipment_11",
         refrigerador = "equipment_12",
         lavadora = "equipment_13",
         microondas = "equipment_14",
         maq_coser = "equipment_15",
         bicicleta = "equipment_16",
         auto = "equipment_17",
         moto = "equipment_18",
         triciclo = "equipment_19",
         mototaxi = "equipment_20",
         camion = "equipment_21") %>% 
  select(-starts_with("equipment"))
rm(mod18)

mod34 <- read_dta("sumaria.dta") %>% 
  select(anio, mes, conglome, vivienda, hogar, ubigeo, dominio, estrato, 
         percepho, mieperho, gashog2d, linpe, linea, gashog1d, pobrezav, pobreza,
         ipcr_0, gpgru0, ingtexhd, inghog1d)

baseHogares <- mod34 %>% 
  left_join(mod18h, by = c("anio", "mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato"))
rm(mod34, mod18h)

mod1 <- read_dta("modulo100.dta",col_select = c(anio, mes, conglome, vivienda, hogar, ubigeo, dominio, estrato,
                                                fecent, result, p101, p102, p103, p103a, p104, p104a, p105a, p110,
                                                p110a, p110a1, p110g, p110c, p110c1, p110c2, p110c3, p111, p111a, 
                                                p1121, p1122, p1123, p1124, p1126, p1125, p1127, p1138, p113a, p1142,
                                                p1144, p1145, factor07, ticuest01, altitud, latitud, longitud, matches("^nbi")))
baseHogares <- baseHogares %>% 
  left_join(mod1, by = c("anio", "mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato"))
rm(mod1)

mod800a <- read_dta("modulo800a.dta", 
                    col_select = c(anio, mes, conglome, vivienda, hogar, ubigeo, dominio, estrato, matches("^p801_")))
mod800a$anio <- as.numeric(mod800a$anio)
baseHogares <- baseHogares %>%
  left_join(mod800a, by = c("anio", "mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato")) 
rm(mod800a)

mod37 <- read_dta("modulo37.dta")
baseHogares <- baseHogares %>%
  left_join(mod37, by = c("anio", "mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato")) 
rm(mod37)

# Área de residencia --> Nos quedamos con el ámbito urbano
baseHogares <- baseHogares %>% 
  mutate(area = case_when(estrato < 6 ~ 1,
                          TRUE ~ 0)) %>% 
  filter(area == 1)

setwd(dirEnaho)
write_dta(data = baseHogares, "base_trabajo/baseHogares.dta")

## 1.2. A nivel de persona -----------------------------------------------------
setwd(dirEnaho)

mod2 <- read_dta("modulo200.dta", col_select = c(anio, mes, conglome, vivienda, hogar, ubigeo, dominio, estrato, codperso,
                                                 p204, p205, p206, p203, p203a, p207, p209, p208a, p209, facpob07)) 
mod3 <- read_dta("modulo300.dta", 
                 col_select = c(anio, mes, conglome, vivienda, hogar, ubigeo, dominio, estrato, codperso, p307, p306, p300a,
                                p301b, p301c, p301a, p302, p308c1, p308c2, p310))
mod4 <- read_dta("modulo400.dta", 
                 col_select = c(anio, mes, conglome, vivienda, hogar, ubigeo, dominio, estrato, codperso, matches("^p401"),
                                p4191, p4192, p4193, p4194, p4195, p4196, p4197, p4198, p420a, p420b,matches("^p407g")))
mod5 <- read_dta("modulo500.dta", 
                 col_select = c(anio, mes, conglome, vivienda, hogar, ubigeo, dominio, estrato, codperso, fac500a, 
                                p500n, p500i, p501, p502, p503, matches("^p504"), p506r4, p507, p511a, p512a, p512b, p519, p521, p521a, 
                                p552, p521a, p521, ocu500, p546, ocupinf, i513t, i518, i524a1, d529t, i520,
                                i530a, d536, i538a1, d540t, i541a, d543, d544t, matches("^p560t_"), matches("^p560a_"),
                                matches("^p560a1_"), p558d2_1, p558d2_2, p518, p513a, p545, p547, p548, p549, p550, p558c, 
                                matches("^p558e1_"), matches("^p5564")))

mod77 <- read_dta("modulo77.dta", 
                  col_select = c(anio, mes, conglome, vivienda, hogar, ubigeo, dominio, estrato, codperso,
                                 e1, e1a1, e1b, e2, e5, e13a, e13b, e13c))

mod851 <- read_dta("modulo851.dta", 
                   col_select = c(anio, mes, conglome, vivienda, hogar, ubigeo, dominio, estrato, codperso, matches("^p22_1_"),
                                  p1_04, p1_05))
mod851$anio <- as.numeric(mod851$anio)

mod852 <- read_dta("modulo852.dta", 
                   col_select = c(anio, mes, conglome, vivienda, hogar, ubigeo, dominio, estrato, codperso,  
                                  p24_1, p24_2, p24_3, p24_5, p24_6, p24_7, p24_8, p24_9,
                                  p40_1, p40_2, p40_3, p40_4, p40_5, p40_6, p45_1, p45_2))
mod852$anio <- as.numeric(mod852$anio)

basePersonas <- mod2 %>% 
  left_join(mod3, by = c("anio", "mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato", "codperso")) %>% 
  left_join(mod4, by = c("anio", "mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato", "codperso")) %>% 
  left_join(mod5, by = c("anio", "mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato", "codperso")) %>% 
  left_join(mod77, by = c("anio", "mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato", "codperso")) %>%
  left_join(mod851, by = c("anio", "mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato", "codperso")) %>%
  left_join(mod852, by = c("anio", "mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato", "codperso"))

rm(mod2, mod3, mod4, mod5, mod77, mod851, mod852)

basePersonas <- basePersonas %>% 
  mutate(area = case_when(estrato < 6 ~ 1,
                          TRUE ~ 0)) %>% 
  filter(area == 1)

setwd(dirEnaho)
write_dta(data = basePersonas, "base_trabajo/basePersonas.dta")

# 2. Creación de variables de interés ------------------------------------------
## 2.1. A nivel de hogar ----
setwd(dirEnaho)
baseHogares <- read_dta("base_trabajo/baseHogares.dta")

# Departamento
baseHogares$dpto = trunc(as.numeric(baseHogares$ubigeo)/10000)
baseHogares$dpto[baseHogares$dpto==15 & baseHogares$dominio!=8] <- 26
dptoLabel <-  c("Amazonas", "Ancash", "Apurimac", "Arequipa", "Ayacucho", 
                "Cajamarca", "Callao", "Cusco", "Huancavelica", "Huanuco", 
                "Ica", "Junin", "La Libertad", "Lambayeque", "Lima Metropolitana", 
                "Loreto", "Madre de Dios", "Moquegua", "Pasco", "Piura", 
                "Puno", "San Martin", "Tacna", "Tumbes", "Ucayali", "Lima Provincia")
baseHogares$dpto <- factor(baseHogares$dpto, levels = 1:26, labels = dptoLabel)

# Provincia - ubigeo
baseHogares <-  baseHogares %>%
  mutate(provubigeo = trunc(as.numeric(ubigeo)/100))

# Variables de acceso a TICs
baseHogares <- baseHogares %>%
  rename(telCelu = p1142, 
         internet = p1144)

#Variable de activos prioritarios
baseHogares <- baseHogares %>%
  mutate(nActivosPrioritarios = rowSums(select(., computadora, equipo_sonido, lavadora, licuadora, microondas, plancha, refrigerador, tv_color)))


# variables de acceso a agua
baseHogares <- baseHogares %>% 
  mutate(agua = case_when(p110 == 1 | p110 ==2 ~ 1,
                          p110 == NA ~ NA,
                          TRUE ~ 0),
         aguaPotable = case_when(p110a1 == 1 & (p110 == 1 | p110 == 2) ~ 1,
                                 p110a1 == NA | p110 == NA ~ NA,
                                 TRUE ~ 0),
         aguaSegura = case_when(p110a == 1 ~ 1,
                                p110a == NA ~ NA,
                                TRUE ~ 0),
         aguaCisterna = case_when(p110g == 4 ~ 1,
                                  p110g == NA ~ NA,
                                  TRUE ~ 0),
         desague = case_when(p111a == 1 | p111a ==2 ~ 1,
                             p111a == NA ~ NA,
                             TRUE ~ 0),
         electricidad = case_when(p1121 == 1  ~ 1,
                                  p111a == NA ~ NA,
                                  TRUE ~ 0),
         tresServicios = case_when(agua == 1 & desague == 1 & electricidad == 1 ~ 1,
                                   agua == NA | desague == NA | electricidad == NA ~ NA,
                                   TRUE ~ 0),
         agua24Horas = case_when(agua == 1 & p110c1 == 24 ~ 1,
                                 agua == NA ~ NA,
                                 TRUE ~ 0),
         aguaHoras = case_when(p110c == 1 ~ p110c1,
                               p110c == 2 ~ p110c2 * p110c3 / 7,
                               TRUE ~ NA),
         nServicios = agua + desague + electricidad + internet + telCelu)

# variables de pobreza y vulnerabilidad
baseHogares <-baseHogares %>% 
  mutate(pobre = case_when(pobreza <= 2 ~ 1,
                           pobreza == NA ~ NA,
                           TRUE ~ 0),
         vulnerableNoPobre = case_when(pobrezav == 3 ~ 1,
                                       pobrezav == NA ~ NA,
                                       TRUE ~ 0))

baseHogares <-baseHogares %>% 
  mutate(nbi1 = case_when(nbi1 == 1 ~ 1,
                          nbi1 == NA ~ NA,
                          TRUE ~ 0),
         nbi2 = case_when(nbi2 == 1 ~ 1,
                          nbi2 == NA ~ NA,
                          TRUE ~ 0),
         nbi3 = case_when(nbi3 == 1 ~ 1,
                          nbi3 == NA ~ NA,
                          TRUE ~ 0),
         nbi4 = case_when(nbi4 == 1 ~ 1,
                          nbi4 == NA ~ NA,
                          TRUE ~ 0),
         nbi5 = case_when(nbi5 == 1~ 1,
                          nbi5 == NA ~ NA,
                          TRUE ~ 0)) %>% 
  mutate(nbis = rowSums(select(., matches("nbi")), na.rm = TRUE))

# variables de tipo de vivienda
baseHogares <-  baseHogares %>% 
  mutate(vivBajaCalidad = case_when(p101 == 5 | p101 == 6 | p101 == 7 ~ 1,
                                    p101 == NA ~ NA,
                                    TRUE ~ 0))

# variables de ocupación de vivienda
baseHogares <-  baseHogares %>%
  mutate(vivInvasion = case_when(p105a == 3 ~ 1,
                                 p105a == NA ~ NA,
                                 TRUE ~ 0),
         vivCedida = case_when(p105a == 6 ~ 1,
                               p105a == NA ~ NA,
                               TRUE ~ 0))

# variables de material de la vivienda 
baseHogares <-  baseHogares %>% 
  mutate(paredLadrillo = case_when(p102 == 1 ~ 1,
                                   p102 == NA ~ NA,
                                   TRUE ~ 0),
         pisoTierra = case_when(p103 == 6 ~ 1,
                                p103 == NA ~ NA,
                                TRUE ~ 0),
         pisoCemento = case_when(p103 == 5 ~ 1,
                                 p103 == NA ~ NA,
                                 TRUE ~ 0),
         pisoTierraCemento = case_when(p103 == 6 | p103 == 5 ~ 1,
                                       p103 == NA ~ NA,
                                       TRUE ~ 0),
         techoDebil = case_when(p103a >= 4 ~ 1,
                                p103a == NA ~ NA,
                                TRUE ~ 0))

# variable de uso de combustible en viviendas
baseHogares <-  baseHogares %>% 
  mutate(combustibleCocina = case_when(p113a == 5 | p101 == 6 ~ 1,
                                       p113a == NA ~ NA,
                                       TRUE ~ 0))

# Variables de características sociodemográficas del hogar 
baseHogares <-  baseHogares %>% 
  mutate(pctPerceptores = percepho/mieperho,
         hacinam = mieperho/p104a) %>% 
  rename(gasPC = gpgru0, ingPC = ipcr_0)

baseHogares <-  baseHogares %>%
  mutate(pctIngExt = ingtexhd/inghog1d)

# Variables de características del jefe del hogar
basePersonas <- read_dta("base_trabajo/basePersonas.dta")
baseJefesHogar <- basePersonas %>% 
  filter(p203 == 1) %>% 
  mutate(nivEducJH = case_when(p301a <= 3 ~ 1, 
                               p301a == 4 | p301a == 5 ~ 2,
                               p301a == 6 | p301a == 7 | p301a ==9 ~ 3,
                               p301a == 8 | p301a == 10 | p301a == 11 ~ 4,
                               p301a == NA ~ NA,
                               TRUE ~ 0),
         educSupJH = case_when(p301a == 8 | p301a == 10 | p301a == 11 ~ 1,
                               p301a == NA ~ NA,
                               TRUE ~ 0),
         educSecJH = case_when(p301a == 6 | p301a == 7 | p301a ==9 ~ 1,
                               p301a == NA ~ NA,
                               TRUE ~ 0),
         lengJH = case_when(p300a == 4 ~ 1,
                            p300a <= 3 | p300a >= 10 ~ 2,
                            p300a >= 5 & p300a <=7 ~ 3,
                            p300a == NA ~ NA,
                            TRUE ~ 4),
         mujerJH = case_when(p207 == 2 ~ 1,
                             TRUE ~ 0),
         hombreJH = case_when(p207 == 1 ~ 1,
                              TRUE ~ 0),
         edadJH = p208a,
         grupoEdadJH = case_when(edadJH < 25 ~ 1,
                                 edadJH >=25 & edadJH < 45 ~ 2,
                                 edadJH >=45 & edadJH < 65 ~ 3,
                                 edadJH == NA ~ NA,
                                 TRUE ~ 4),
         jovenJH = case_when(grupoEdadJH == 1 ~ 1,
                             grupoEdadJH == NA ~ NA,
                             TRUE ~ 0),
         adultoJovJH = case_when(grupoEdadJH == 2 ~ 1,
                                 grupoEdadJH == NA ~ NA,
                                 TRUE ~ 0),
         adultoJH = case_when(grupoEdadJH == 3 ~ 1,
                              grupoEdadJH == NA ~ NA,
                              TRUE ~ 0),
         adultoMayorJH = case_when(grupoEdadJH == 4 ~ 1,
                                   grupoEdadJH == NA ~ NA,
                                   TRUE ~ 0),
         estadoCivilJH = p209,
         convivienteJH = case_when(p209 == 1 ~ 1,
                                  p209 == NA ~ NA,
                                  TRUE ~ 0),
         casadoJH = case_when(p209 == 2 ~ 1,
                             p209 == NA ~ NA,
                             TRUE ~ 0),
         empInfJH = case_when(ocupinf ==1 ~ 1,
                              ocupinf == NA ~ NA,
                              TRUE ~ 0),
         sectorJH = case_when(p506r4 < 1000 ~ 1,
                              p506r4 > 1000 & p506r4 <= 4399 ~ 2,
                              p506r4 == NA ~ NA,
                              TRUE ~ 3),
         sectorJHCom = case_when(((p506r4 >= 4600 & p506r4 <= 4699) | (p506r4 >= 4500 & p506r4 <= 4599) | (p506r4 >= 4700 & p506r4 <= 4799)) ~ 1,
                                 TRUE ~ 0),
         sectorJHRest = case_when(p506r4>=5500 & p506r4<=5699 ~ 1,
                                  TRUE ~ 0),
         sectorJHHog = case_when(p506r4>=9700 & p506r4<=9799 ~ 1,
                                 TRUE ~ 0),
         sinContratoJH = case_when(p511a == 7 ~ 1,
                                   p511a == NA ~ NA,
                                   TRUE ~ 0),
         indepJH = case_when(p507 ==2 ~ 1,
                             p507 == NA ~ NA,
                             TRUE ~ 0),
         numTrabJH = p512b,
         tamaEmpJH = case_when(p512b <= 10 ~ 1,
                               (p512a == 1 & p512b > 10) | p512a == 2 ~ 2,
                               p512a == NA ~ NA, 
                               TRUE ~ 3),
         empPeqJH = case_when(tamaEmpJH == 1 ~ 1,
                              tamaEmpJH == NA ~ NA,
                              TRUE ~ 0),
         empMedJH = case_when(tamaEmpJH == 2 ~ 1,
                              tamaEmpJH == NA ~ NA,
                              TRUE ~ 0),
         empGrandeJH = case_when(tamaEmpJH == 3 ~ 1,
                                 tamaEmpJH == NA ~ NA,
                                 TRUE ~ 0),
         anioEducJH = case_when(p301a <= 2 ~ 0,
                                p301a == 3 & p301c != NA ~ p301c,
                                p301a == 3 & p301c == NA ~ p301b,
                                p301a == 4 ~ 6,
                                p301a == 5 ~ 6 + p301b,
                                p301a == 6 ~ 6 + 5,
                                p301a == 7 & p301b <= 3 ~ 6 + 5 + p301b,
                                (p301a == 7 & p301b > 3) | p301a == 8 ~ 6 + 5 + 3,
                                p301a == 9 & p301b <= 5 ~ 6 + 5 + p301b,
                                (p301a == 9 & p301b > 5) | p301a == 10 ~ 6 + 5 + 5,
                                p301a == 11 ~ 6 + 5 + 5 + p301b,
                                TRUE ~ 0),
         jh65mas = case_when(grupoEdadJH == 4 ~ 1,
                             TRUE ~ 0),
         jh25menos = case_when(grupoEdadJH == 1 ~ 1,
                               TRUE ~ 0),
         origenJH = case_when(p558c <= 3 | p558c == 7 | p558c == 9 ~ 1,
                              p558c == 4 ~ 2,
                              p558c == 5 ~ 3,
                              p558c == 6 ~ 4,
                              TRUE ~ 0),
         indigenaJH = case_when(p558c <= 3 | p558c == 7 | p558c == 9 ~ 1,
                                p558c == NA ~ NA,
                                TRUE ~ 0),
         mestizoJH = case_when(p558c == 6 ~ 1,
                               p558c == NA ~ NA,
                               TRUE ~ 0)) %>% 
  select(anio, mes, conglome, vivienda, hogar, ubigeo, dominio, estrato, 
         nivEducJH, lengJH, mujerJH, hombreJH, edadJH, grupoEdadJH, estadoCivilJH, casadoJH, convivienteJH, 
         empInfJH, sectorJH, sectorJHCom, sectorJHRest, sectorJHHog, sinContratoJH, indepJH, tamaEmpJH, numTrabJH, anioEducJH,  
         p401g1, p401g2, p401g, p401f, jh65mas, jh25menos, educSecJH, educSupJH, jovenJH, adultoJovJH, adultoJH, adultoMayorJH,
         empPeqJH, empMedJH, empGrandeJH, origenJH, indigenaJH, mestizoJH)

baseHogares <-  baseHogares %>% 
  left_join(baseJefesHogar, by= c("anio", "mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato"))  %>% 
  mutate(migranteJH = case_when(p401g1 == 2 & dpto != trunc(p401g2/10000) ~ 1,
                                p401g1 == NA ~ NA,
                                TRUE ~ 0),
         migrenteJHprov = case_when(p401g1 == 2 & provubigeo != trunc(p401g2/100) ~ 1,
                                    p401g1 == NA ~ NA,
                                    TRUE ~ 0),
         migrante5aniosJH = case_when(p401f == 2 & dpto != trunc(p401g/10000) ~ 1,
                                      p401f == NA ~ NA,
                                      TRUE ~ 0),
         migrante5aniosJHprov = case_when(p401f == 2 & provubigeo != trunc(p401g/100) ~ 1,
                                          p401f == NA ~ NA,
                                          TRUE ~ 0)) %>% 
  select(-c(p401g1, p401g2, p401g, p401f)) 
rm(baseJefesHogar)

baseHogares <-  baseHogares %>%
  mutate(hogarVasoLeche = case_when(p701_01 == 1 ~ 1,
                                    p701_01 == NA ~ NA,
                                    TRUE ~ 0),
         hogarComedor = case_when(p701_02 == 1 ~ 1,
                                  p103 == NA ~ NA,
                                  TRUE ~ 0),
         hogarQaliWarma = case_when(p701_03 == 1 | p701_04 == 1 ~ 1,
                                    p701_03 == NA | p701_04 == NA ~ NA,
                                    TRUE ~ 0),
         hogarCunaMas = case_when(p701_05 == 1 | p710_01 == 1 | p710_02 == 1 ~ 1,
                                  p701_05 == NA | p710_01 == NA | p710_02 == NA ~ NA,
                                  TRUE ~ 0),
         hogarJuntos = case_when(p710_04 == 1 ~ 1,
                                 p710_04 == NA ~ NA,
                                 TRUE ~ 0),
         hogarPension65 = case_when(p710_05 == 1 ~ 1,
                                    p710_05 == NA ~ NA,
                                    TRUE ~ 0),
         hogarJovProd = case_when(p710_07 == 1 ~ 1,
                                  p710_07 == NA ~ NA,
                                  TRUE ~ 0),
         hogarTrabajaPeru = case_when(p710_08 == 1 ~ 1,
                                      p710_08 == NA ~ NA,
                                      TRUE ~ 0),
         hogarImpulsaPeru = case_when(p710_09 == 1 ~ 1,
                                      p710_09 == NA ~ NA,
                                      TRUE ~ 0),
         hogarBeca18 = case_when(p710_10 == 1 ~ 1,
                                 p710_10 == NA ~ NA,
                                 TRUE ~ 0),
         programasSociales = hogarVasoLeche + hogarComedor + hogarQaliWarma + hogarCunaMas + hogarJuntos + hogarPension65 + hogarJovProd + hogarTrabajaPeru + hogarImpulsaPeru + hogarBeca18)

# variables de participación ciudadana

baseHogares <-baseHogares %>%
  mutate(p801a_1 = case_when(p801_1 == 1 ~ 1, TRUE ~ 0),
         p801a_2 = case_when(p801_2 == 2 ~ 1, TRUE ~ 0),
         p801a_3 = case_when(p801_3 == 3 ~ 1, TRUE ~ 0),
         p801a_4 = case_when(p801_4 == 4 ~ 1, TRUE ~ 0),
         p801a_5 = case_when(p801_5 == 5 ~ 1, TRUE ~ 0),
         p801a_6 = case_when(p801_6 == 6 ~ 1, TRUE ~ 0),
         p801a_7 = case_when(p801_7 == 7 ~ 1, TRUE ~ 0),
         p801a_8 = case_when(p801_8 == 8 ~ 1, TRUE ~ 0),
         p801a_9 = case_when(p801_9 == 9 ~ 1, TRUE ~ 0),
         p801a_10 = case_when(p801_10 == 10 ~ 1, TRUE ~ 0),
         p801a_11 = case_when(p801_11 == 11 ~ 1, TRUE ~ 0),
         p801a_12 = case_when(p801_12 == 12 ~ 1, TRUE ~ 0),
         p801a_13 = case_when(p801_13 == 13 ~ 1, TRUE ~ 0),
         p801a_14 = case_when(p801_14 == 14 ~ 1, TRUE ~ 0),
         p801a_15 = case_when(p801_15 == 15 ~ 1, TRUE ~ 0),
         p801a_16 = case_when(p801_16 == 16 ~ 1, TRUE ~ 0),
         p801a_17 = case_when(p801_17 == 17 ~ 1, TRUE ~ 0),
         p801a_18 = case_when(p801_18 == 18 ~ 1, TRUE ~ 0),
         p801a_19 = case_when(p801_19 == 19 ~ 1, TRUE ~ 0),
         p801a_20 = case_when(p801_20 == 20 ~ 1, TRUE ~ 0)) %>% 
  mutate(participacion2 = rowSums(select(., starts_with("p801a_"))),
         participacion = case_when(participacion2 > 0 ~ 1, TRUE ~ 0)) 
         
# Variables de composición del hogar

basePH <-  basePersonas %>%
  mutate(jefe = case_when(p203 == 1 ~ 1,
                          TRUE ~ 0),
         conyugue = case_when(p203 == 2 & p204 == 1 ~ 1,
                              TRUE ~ 0),
         hijo = case_when(p203 == 3 & p204 == 1 ~ 1,
                          TRUE ~ 0),
         otroFam = case_when(p204 == 1 & ((p203 >=4 & p203 <= 7) | p203 == 11) ~ 1,
                             TRUE ~ 0),
         otroNoFam = case_when(p204 == 1 & (p203 >= 8 & p203 <= 10) ~ 1,
                               TRUE ~ 0),
         personaDep = case_when(p208a < 14 | p208a > 64 ~ 1,
                                p208a == NA ~ NA,
                                TRUE ~ 0)) %>%
  group_by(anio, mes, conglome, vivienda, hogar, ubigeo, dominio, estrato) %>% 
  summarise(jefeH = max(jefe, na.rm = TRUE),
            conyugueH = max(conyugue, na.rm = TRUE),
            hijoH = max(hijo, na.rm = TRUE),
            otroFamH = max(otroFam, na.rm = TRUE),
            otroNoFamH = max(otroNoFam, na.rm = TRUE),
            nDepenH = sum(personaDep, na.rm = TRUE))

baseHogares <- baseHogares %>%
  left_join(basePH, by= c("anio", "mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato"))
rm(basePH)

baseHogares <- baseHogares %>%
  mutate(tipoHogar = case_when(otroFamH == 0 & otroNoFamH == 0 ~ 1,
                               otroFamH == 1 & otroNoFamH == 0 ~ 2,
                               otroNoFamH == 1 ~ 3,
                               conyugueH == 0 & hijoH == 0 & otroFamH == 0 & otroNoFamH == 0 & mieperho == 1 ~ 4,
                               conyugueH == 0 & hijoH == 0 & (otroFamH == 1 | otroNoFamH == 1) ~ 5,
                               TRUE ~ NA),
         hogarConHijo = case_when(hijoH == 1 ~ 1,
                                  TRUE ~ 0),
         hogarMonoParent = case_when(conyugueH == 0 & hijoH == 1 ~ 1,
                                     TRUE ~ 0),
         hogarUniFem = case_when(tipoHogar == 4 & mujerJH == 1 ~ 1,
                                 TRUE ~ 0),
         hogarUniMayor = case_when (tipoHogar == 4 & edadJH >= 60 ~ 1,
                                    TRUE ~ 0),
         hogarNuclear = case_when(otroFamH == 0 & otroNoFamH == 0 ~ 1,
                                  TRUE ~ 0),
         hogarExtendido = case_when(otroFamH == 1 & otroNoFamH == 0 ~ 1,
                                    TRUE ~ 0))

baseHogares$tipoHogar <- c("Nuclear", "Extendido", "Compuesto", "Unipersonal", "Sin núcleo")[baseHogares$tipoHogar]

baseHogares <- baseHogares %>%
  mutate(ratioDependencia = nDepenH/mieperho)

setwd(dirEnaho)
write_dta(data = baseHogares, "base_trabajo/baseHogaresFinal.dta")

## 2.2. A nivel de persona -----------------------------------------------------
setwd(dirEnaho)
basePersonas <- read_dta("base_trabajo/basePersonas.dta")

# Departamento
basePersonas$dpto = trunc(as.numeric(basePersonas$ubigeo)/10000)
basePersonas$dpto[basePersonas$dpto==15 & basePersonas$dominio!=8] <- 26
dptoLabel <-  c("Amazonas", "Ancash", "Apurimac", "Arequipa", "Ayacucho", 
                "Cajamarca", "Callao", "Cusco", "Huancavelica", "Huanuco", 
                "Ica", "Junin", "La Libertad", "Lambayeque", "Lima Metropolitana", 
                "Loreto", "Madre de Dios", "Moquegua", "Pasco", "Piura", 
                "Puno", "San Martin", "Tacna", "Tumbes", "Ucayali", "Lima Provincia")
basePersonas$dpto <- factor(basePersonas$dpto, levels = 1:26, labels = dptoLabel)

# Provincia - ubigeo
basePersonas <-  basePersonas %>%
  mutate(provubigeo = trunc(as.numeric(ubigeo)/100))

#variables de características del individuo
basePersonas <- basePersonas %>%
  mutate(nivEduc = case_when(p301a <= 3 ~ 1, 
                             p301a == 4 | p301a == 5 ~ 2,
                             p301a == 6 | p301a == 7 | p301a ==9 ~ 3,
                             p301a == 8 | p301a == 10 | p301a == 11 ~ 4,
                             TRUE ~ 0),
         leng = case_when(p300a == 4 ~ 1,
                          p300a <= 3 | p300a >= 10 ~ 2,
                          p300a >= 5 & p300a <=7 ~ 3,
                          p300a == NA ~ NA,
                          TRUE ~ 4),
         mujer = case_when(p207 == 2 ~ 1,
                           TRUE ~ 0),
         hombre = case_when(p207 == 1 ~ 1,
                            TRUE ~ 0),
         edad = p208a,
         grupoEdad = case_when(edad < 25 ~ 1,
                               edad >=25 & edad < 45 ~ 2,
                               edad >=45 & edad < 65 ~ 3,
                               edad == NA ~ NA,
                               TRUE ~ 4),
         estadoCivil = p209,
         conviviente = case_when(p209 == 1 ~ 1,
                                   p209 == NA ~ NA,
                                   TRUE ~ 0),
         casado = case_when(p209 == 2 ~ 1,
                              p209 == NA ~ NA,
                              TRUE ~ 0),
         anioEduc = case_when(p301a <= 2 ~ 0,
                              p301a == 3 & p301c != NA ~ p301c,
                              p301a == 3 & p301c == NA ~ p301b,
                              p301a == 4 ~ 6,
                              p301a == 5 ~ 6 + p301b,
                              p301a == 6 ~ 6 + 5,
                              p301a == 7 & p301b <= 3 ~ 6 + 5 + p301b,
                              (p301a == 7 & p301b > 3) | p301a == 8 ~ 6 + 5 + 3,
                              p301a == 9 & p301b <= 5 ~ 6 + 5 + p301b,
                              (p301a == 9 & p301b > 5) | p301a == 10 ~ 6 + 5 + 5,
                              p301a == 11 ~ 6 + 5 + 5 + p301b,
                              TRUE ~ 0),
         educSup = case_when(p301a == 8 | p301a == 10 | p301a == 11 ~ 1,
                               p301a == NA ~ NA,
                               TRUE ~ 0),
         educSec = case_when(p301a == 6 | p301a == 7 | p301a ==9 ~ 1,
                               p301a == NA ~ NA,
                               TRUE ~ 0),
         origen = case_when((p558c <= 3 | p558c == 7 | p558c == 9) ~ 1,
                            p558c == 4 ~ 2,
                            p558c == 5 ~ 3,
                            p558c == 6 ~ 4,
                            TRUE ~ 0),
         indigena = case_when(p558c <= 3 | p558c == 7 | p558c == 9 ~ 1,
                                p558c == NA ~ NA,
                                TRUE ~ 0),
         mestizo = case_when(p558c == 6 ~ 1,
                               p558c == NA ~ NA,
                               TRUE ~ 0))

# Tenencia de seguro de salud
basePersonas <-basePersonas %>% 
  mutate(segEssalud = case_when(p4191 == 1 ~ 1,
                                p4191 == NA ~ NA,
                                TRUE ~ 0),
         segPriv = case_when(p4192 == 1 ~ 1,
                             p4192 == NA ~ NA,
                             TRUE ~ 0),
         segEps = case_when(p4193 == 1 ~ 1,
                            p4193 == NA ~ NA,
                            TRUE ~ 0),
         segFfaa = case_when(p4194 == 1 ~ 1,
                             p4194 == NA ~ NA,
                             TRUE ~ 0),
         segSis = case_when(p4195 == 1 ~ 1,
                            p4195 == NA ~ NA,
                            TRUE ~ 0),
         segUniv = case_when(p4196 == 1 ~ 1,
                             p4196 == NA ~ NA,
                             TRUE ~ 0),
         segEsc = case_when(p4197 == 1 ~ 1,
                            p4197 == NA ~ NA,
                            TRUE ~ 0),
         segOtro = case_when(p4198 == 1 ~ 1,
                             p4198 == NA ~ NA,
                             TRUE ~ 0),
         algunSeg = case_when(rowSums(select(., starts_with("seg"))) > 1 ~ 1,
                              TRUE ~ 0),
         difSegSis = case_when(segEssalud == 1 | segPriv == 1 | segEps == 1 | segFfaa == 1 | segUniv == 1 | segEsc == 1 | segOtro == 1 ~ 1,
                               TRUE ~ 0))

#Dimensión: Empleo e ingresos
basePersonas <- basePersonas %>%
  mutate(desemp = case_when(ocu500 == 2 ~ 1,
                            ocu500 == NA ~ NA,
                            TRUE ~ 0),
         inactivo = case_when(ocu500 == 3 | ocu500 == 4 ~ 1,
                              ocu500 == NA ~ NA,
                              TRUE ~ 0),
         empInf = case_when(ocupinf == 1 ~ 1,
                            ocupinf == NA ~ NA,
                            TRUE ~ 0),
         sinContrato = case_when(p511a == 7 ~ 1,
                                 p511a == NA ~ NA,
                                 TRUE ~ 0),
         hrsTrb = p513a + p518,
         sector = case_when(p506r4 < 1000 ~ 1,
                              p506r4 > 1000 & p506r4 <= 4399 ~ 2,
                              p506r4 == NA ~ NA,
                              TRUE ~ 3),
         sectorCom = case_when(((p506r4 >= 4600 & p506r4 <= 4699) | (p506r4 >= 4500 & p506r4 <= 4599) | (p506r4 >= 4700 & p506r4 <= 4799)) ~ 1,
                                 TRUE ~ 0),
         sectorRest = case_when(p506r4>=5500 & p506r4<=5699 ~ 1,
                                  TRUE ~ 0),
         sectorHog = case_when(p506r4>=9700 & p506r4<=9799 ~ 1,
                                 TRUE ~ 0),
         numTrab = p512b,
         tamaEmp = case_when(p512b <= 10 ~ 1,
                               (p512a == 1 & p512b > 10) | p512a == 2 ~ 2,
                               p512a == NA ~ NA, 
                               TRUE ~ 3),
         empPeq = case_when(tamaEmp == 1 ~ 1,
                              tamaEmp == NA ~ NA,
                              TRUE ~ 0),
         empMed = case_when(tamaEmp == 2 ~ 1,
                              tamaEmp == NA ~ NA,
                              TRUE ~ 0),
         empGrande = case_when(tamaEmp == 3 ~ 1,
                                 tamaEmp == NA ~ NA,
                                 TRUE ~ 0),
         d529t = case_when(d529t == 999999 ~ NA,
                           TRUE ~ d529t),
         d536 = case_when(d536 == 999999 ~ NA,
                          TRUE ~ d536),
         d540t = case_when(d540t == 999999 ~ NA,
                           TRUE ~ d540t),
         d543 = case_when(d543 == 999999 ~ NA,
                          TRUE ~ d543),
         d544t = case_when(d544t == 999999 ~ NA,
                           TRUE ~ d544t),
         ingLabPrin = case_when(ocu500 == 1 ~ (i524a1 + d529t + i530a + d536)/12,
                                TRUE ~ NA),
         ingLabSec = case_when(ocu500 == 1 ~ (i538a1 + d540t + i541a + d543)/12,
                               TRUE ~ NA),
         ingNoLab = d544t/12,
         ingTot = sum(ingLabPrin, ingLabSec, ingNoLab, na.rm = TRUE))

## Subempleo
#Merge de hogares y personas
baseHogares <- read_dta("base_trabajo/baseHogaresFinal.dta")
baseIMR <- baseHogares %>%
  mutate(domgeo = case_when(
           (estrato >= 1 & estrato <= 5) & (dominio >= 1 & dominio <= 3) ~ 1,
           (estrato >= 6 & estrato <= 8) & (dominio >= 1 & dominio <= 3) ~ 2,
           (estrato >= 1 & estrato <= 5) & (dominio >= 4 & dominio <= 6) ~ 3,
           (estrato >= 6 & estrato <= 8) & (dominio >= 4 & dominio <= 6) ~ 4,
           (estrato >= 1 & estrato <= 5) & dominio == 7 ~ 5,
           (estrato >= 6 & estrato <= 8) & dominio == 7 ~ 6,
           dominio == 8 ~ 7,
           TRUE ~ NA))%>%
  group_by(domgeo) %>%
  summarize(media_linea = mean(linea, na.rm = TRUE),
            media_mieperho = mean(mieperho, na.rm = TRUE),
            media_percepho = mean(percepho, na.rm = TRUE, wt = peso)) %>%
  mutate(imr = (media_linea * media_mieperho) / media_percepho) %>%
  select(domgeo, imr, media_linea, media_mieperho, media_percepho)

#Uniendo IMR y personas
basePersonas <- basePersonas %>%
  mutate(domgeo = case_when(
    (estrato >= 1 & estrato <= 5) & (dominio >= 1 & dominio <= 3) ~ 1,
    (estrato >= 6 & estrato <= 8) & (dominio >= 1 & dominio <= 3) ~ 2,
    (estrato >= 1 & estrato <= 5) & (dominio >= 4 & dominio <= 6) ~ 3,
    (estrato >= 6 & estrato <= 8) & (dominio >= 4 & dominio <= 6) ~ 4,
    (estrato >= 1 & estrato <= 5) & dominio == 7 ~ 5,
    (estrato >= 6 & estrato <= 8) & dominio == 7 ~ 6,
    dominio == 8 ~ 7,
    TRUE ~ NA))

basePersonas <- basePersonas %>%
  left_join(baseIMR, by = c("domgeo")) %>%
  mutate(r11 = case_when(ocu500 == 1 & p519 == 1 ~ i513t + i518,
                         ocu500 == 1 & p519 == 2 ~ i520,
                         TRUE ~ NA),
         r11r = case_when(
           r11 < 15 ~ 1,
           between(r11, 15, 34.49) ~ 2,
           between(r11, 34.5, 47.49) ~ 3,
           between(r11, 47.5, 48.49) ~ 4,
           between(r11, 48.5, 59.59) ~ 5,
           r11 >= 60 ~ 6,
           TRUE ~ 8),
         r13 = case_when(ocu500 == 2 & p552 == 1 ~ 1,
                         ocu500 == 2 & p552 == 2 ~ 2,
                         ocu500 == 1 & r11r >= 1 & r11r <= 2 & p521 == 1 & p521a == 1 ~ 3,
                         ocu500 == 1 & ingTot < imr & r11r >= 3 & r11r <= 6 ~ 4,
                         ocu500 == 1 & ingTot < imr & r11r >= 1 & r11r <= 2 & p521 == 1 & p521a == 2 ~ 4,
                         ocu500 == 1 & ingTot < imr & r11r >= 1 & r11r <= 2 & p521 == 2 ~ 4,
                         ocu500 == 1 & ingTot >= imr & r11r >= 3 & r11r <= 6 ~ 5,
                         ocu500 == 1 & ingTot >= imr & r11r >= 1 & r11r <= 2 & p521 == 1 & p521a == 2 ~ 5,
                         ocu500 == 1 & ingTot < imr & r11r >= 1 & r11r <= 2 & is.na(p521) ~ 3,
                         ocu500 == 1 & ingTot >= imr & r11r >= 1 & r11r <= 2 & is.na(p521) ~ 5,
                         ocu500 == 1 & ingTot < imr & r11r >= 1 & r11r <= 2 & p521 == 1 & is.na(p521a) ~ 4,
                         ocu500 == 1 & ingTot >= imr & r11r >= 1 & r11r <= 2 & p521 == 1 & is.na(p521a) ~ 3,
                         ocu500 == 3 ~ 7,
                         TRUE ~ NA),
         subempHrs = case_when(r13 == 3 ~ 1,
                               TRUE ~ 0),
         subempIng = case_when(r13 == 4 ~ 1,
                               TRUE ~ 0))
rm(baseIMR)

#Dimensión: Trabajo independiente
basePersonas <- basePersonas %>% 
  mutate(indep = case_when(p507 == 2 ~ 1,
                           p507 == NA ~ NA,
                           TRUE ~ 0),
         indepPerNat = case_when(e1 == 1 ~ 1,
                                 e1 == NA ~ NA,
                                 TRUE ~ 0),
         indepPerJur = case_when(e1 == 2 ~ 1,
                                 e1 == NA ~ NA,
                                 TRUE ~ 0),
         indepNoReg = case_when(e1 == 3 ~ 1,
                                e1 == NA ~ NA,
                                TRUE ~ 0),
         indepAmb = case_when(e2 == 1 ~ 1,
                              e2 == NA ~ NA,
                              TRUE ~ 0),
         indepVeh = case_when(e2 == 3 ~ 1,
                              e2 == NA ~ NA,
                              TRUE ~ 0),
         indepPuestoImpCa = case_when(e2 ==4 ~ 1,
                                      e2 == NA ~ NA,
                                      TRUE ~ 0),
         indepPuestoImpMerc = case_when(e2 == 5 ~ 1,
                                        e2 == NA ~ NA,
                                        TRUE ~ 0),
         indepPuestoFijoCa = case_when(e2 ==6 ~ 1,
                                       e2 == NA ~ NA,
                                       TRUE ~ 0),
         indepPuestoFijoMerc = case_when(e2 == 7 ~ 1,
                                         e2 == NA ~ NA,
                                         TRUE ~ 0),
         indepDomCli = case_when(e2 == 2 ~ 1,
                                 e2 == NA ~ NA,
                                 TRUE ~ 0),
         indepViv = case_when(e2 == 10 ~ 1, 
                              e2 == NA ~ NA,
                              TRUE ~ 0),
         indepVivExc = case_when(e2 == 9 ~ 1,
                                 e2 == NA ~ NA,
                                 TRUE ~ 0),
         indepTaller = case_when(e2 == 8 ~ 1,
                                 e2 == NA ~ NA,
                                 TRUE ~ 0),
         indepOtros = case_when(e2 ==11 ~ 1, 
                                e2 == NA ~ NA,
                                TRUE ~ 0),
         motNecEcon = case_when(e5 == 5 ~ 1,
                                e5 == NA ~ NA,
                                TRUE ~ 0),
         motNoTrab = case_when(e5 == 1 ~ 1,
                               e5 == NA ~ NA,
                               TRUE ~ 0),
         motIng = case_when(e5 == 2 ~ 1,
                            e5 == NA ~ NA,
                            TRUE ~ 0),
         motIndep = case_when(e5 == 3 ~ 1,
                              e5 == NA ~ NA,
                              TRUE ~ 0),
         motTradFam = case_when(e5 == 4 ~ 1,
                                e5 == NA ~ NA,
                                TRUE ~ 0),
         motOtro = case_when(e5 == 6 ~ 1,
                             e5 == NA ~ NA,
                             TRUE ~ 0),
         indProd = case_when(e13a == 1 ~ 1,
                             e13a == NA ~ NA,
                             TRUE ~ 0),
         indCompVent = case_when(e13b == 2 ~ 1,
                                 e13b == NA ~ NA,
                                 TRUE ~ 0),
         indServ = case_when(e13c == 3 ~ 1,
                             e13c == NA ~ NA,
                             TRUE ~ 0))

# Dimensión: inclusión financiera
basePersonas <- basePersonas %>% 
  mutate(cuentaAhorro = case_when(p558e1_1 == 1 ~ 1,
                                  p558e1_1 == NA ~ NA,
                                  TRUE ~ 0),
         cuentaPlazoFijo = case_when(p558e1_2 == 2 ~ 1,
                                     p558e1_2 == NA ~ NA,
                                     TRUE ~ 0),
         cuentaCorr = case_when(p558e1_3 == 3 ~ 1,
                                p558e1_3 == NA ~ NA,
                                TRUE ~ 0),
         cuentaNoTiene = case_when(p558e1_6 == 6 ~ 1,
                                   p558e1_6 == NA ~ NA,
                                   TRUE ~ 0)) %>% 
  mutate(nCuentas = cuentaAhorro + cuentaPlazoFijo + cuentaCorr)

# Dimensión: Migración
basePersonas <-  basePersonas %>% 
  mutate(migrante = case_when(p401g1 == 2 & provubigeo != trunc(p401g2/100) ~ 1,
                              p401g1 == NA ~ NA,
                              TRUE ~ 0),
         migrante5anios = case_when(p401f == 2 & provubigeo != trunc(p401g/100) ~ 1,
                                    p401f == NA ~ NA,
                                    TRUE ~ 0))

# Dimensión: Transporte
basePersonas <- basePersonas %>% 
  mutate(usoMototaxi = case_when(p560t_01 == 1 ~ 1,
                                 p560t_01 == NA ~ NA,
                                 TRUE ~ 0),
         usoMicrobus = case_when(p560t_02 == 1 ~ 1,
                                 p560t_02 == NA ~ NA,
                                 TRUE ~ 0),
         usoOmnibus = case_when(p560t_03 == 1 ~ 1,
                                p560t_03 == NA ~ NA,
                                TRUE ~ 0),
         usoCombi = case_when(p560t_04 == 1 ~ 1,
                              p560t_04 == NA ~ NA,
                              TRUE ~ 0),
         usoColectivo = case_when(p560t_05 == 1 ~ 1,
                                  p560t_05 == NA ~ NA,
                                  TRUE ~ 0),
         usoTaxi = case_when(p560t_06 == 1 ~ 1,
                             p560t_06 == NA ~ NA,
                             TRUE ~ 0)) %>% 
  mutate(nTransporte = usoMototaxi + usoMicrobus + usoOmnibus + usoCombi + usoColectivo + usoTaxi)

basePersonas <- basePersonas %>% 
  mutate(usoDiarioMototaxi = case_when(p560a1_01 == 1 ~ 1,
                                       p560a1_01 == NA ~ NA,
                                       TRUE ~ 0),
         usoDiarioMicrobus = case_when(p560a1_02 == 1 ~ 1,
                                       p560a1_02 == NA ~ NA,
                                       TRUE ~ 0),
         usoDiarioOmnibus = case_when(p560a1_03 == 1 ~ 1,
                                      p560a1_03 == NA ~ NA,
                                      TRUE ~ 0),
         usoDiarioCombi = case_when(p560a1_04 == 1 ~ 1,
                                    p560a1_04 == NA ~ NA,
                                    TRUE ~ 0),
         usoDiarioColectivo = case_when(p560a1_05 == 1 ~ 1,
                                        p560a1_05 == NA ~ NA,
                                        TRUE ~ 0),
         usoDiarioTaxi = case_when(p560a1_06 == 1 ~ 1,
                                   p560a1_06 == NA ~ NA,
                                   TRUE ~ 0)) %>%
  mutate(nTrasporteDiario = usoDiarioMototaxi + usoDiarioMicrobus + usoDiarioOmnibus + usoDiarioCombi + usoDiarioColectivo + usoDiarioTaxi)

#Tiempo de traslados
basePersonas <- basePersonas %>% 
  mutate(estudiaMD = case_when(p308c1 == 1 ~ 1,
                               p308c1 == NA ~ NA,
                               TRUE ~ 0),
         saludMD = case_when(p420a == 1 ~ 1,
                             p420a == NA ~ NA,
                             TRUE ~ 0),
         trabajaMD = case_when(p558d2_1 == 1 ~ 1,
                               p558d2_1 == NA ~ NA,
                               TRUE ~ 0),
         tiempoSalud = p407g1*1440 + p407g2*60 + p407g3)

# Dimensión: Discriminación

basePersonas <- basePersonas %>% 
  mutate(discrRaza = case_when(p22_1_01 == 1 ~ 1,
                               p22_1_01 == NA ~ NA,
                               TRUE ~ 0),
         discrLengua = case_when(p22_1_02 == 1 ~ 1,
                                 p22_1_02 == NA ~ NA,
                                 TRUE ~ 0),
         discrVest = case_when(p22_1_03 == 1 ~ 1,
                               p22_1_03 == NA ~ NA,
                               TRUE ~ 0),
         discrOrigen = case_when(p22_1_04 == 1 ~ 1,
                                 p22_1_04 == NA ~ NA,
                                 TRUE ~ 0),
         discrCostumbres = case_when(p22_1_05 == 1 ~ 1,
                                     p22_1_05 == NA ~ NA,
                                     TRUE ~ 0),
         discrIngresos = case_when(p22_1_06 == 1 ~ 1,
                                   p22_1_06 == NA ~ NA,
                                   TRUE ~ 0),
         discrEduc = case_when(p22_1_07 == 1 ~ 1,
                               p22_1_07 == NA ~ NA,
                               TRUE ~ 0),
         discrEdad = case_when(p22_1_08 == 1 ~ 1,
                               p22_1_08 == NA ~ NA,
                               TRUE ~ 0),
         discrSexo = case_when(p22_1_09 == 1 ~ 1,
                               p22_1_09 == NA ~ NA,
                               TRUE ~ 0),
         discrOrientacion = case_when(p22_1_10 == 1 ~ 1,
                                      p22_1_10 == NA ~ NA,
                                      TRUE ~ 0),
         discrDiscapacidad = case_when(p22_1_11 == 1 ~ 1,
                                       p22_1_11 == NA ~ NA,
                                       TRUE ~ 0),
         discriminacion = case_when(rowSums(select(., matches("discr")), na.rm = TRUE) > 0 ~ 1,
                                    rowSums(select(., matches("discr")), na.rm = TRUE) == 0 ~ 0,
                                    TRUE ~ NA))

# variables de discapacidad
basePersonas <- basePersonas %>% 
  mutate(discapMotora = case_when(p401h1 == 1 ~ 1,
                                  p401h1 == NA ~ NA,
                                  TRUE ~ 0),
         discapVista = case_when(p401h2 == 1 ~ 1,
                                 p401h2 == NA ~ NA,
                                 TRUE ~ 0),
         discapHabla = case_when(p401h3 == 1 ~ 1,
                                 p401h3 == NA ~ NA,
                                 TRUE ~ 0),
         discapOir = case_when(p401h4 == 1 ~ 1,
                               p401h4 == NA ~ NA,
                               TRUE ~ 0),
         discapEntender = case_when(p401h5 == 1 ~ 1,
                                    p401h5 == NA ~ NA,
                                    TRUE ~ 0),
         discapRelacion = case_when(p401h6 == 1 ~ 1,
                                    p401h6 == NA ~ NA,
                                    TRUE ~ 0)) %>% 
  mutate(discapacidad = case_when(rowSums(select(., matches("discap")), na.rm = TRUE) > 0 ~ 1, 
                                  rowSums(select(., matches("discap")), na.rm = TRUE) == 0 ~ 0,
                                  TRUE ~ NA))
         

# variables de confianza en instituciones publicas

basePersonas <- basePersonas %>% 
  mutate(confianzaMP = case_when(p1_04 == 1 ~ 1,
                                 p1_04 == NA ~ NA,
                                 TRUE ~ 0),
         confianzaMD = case_when(p1_05 == 1 ~ 1,
                                 p1_05 == NA ~ NA,
                                 TRUE ~ 0))

# variables de exposición a riesgos e inseguridad
basePersonas <- basePersonas %>% 
  mutate(riesgoEmpleo = case_when(p40_1 == 1 ~ 1,
                                  p40_1 == NA ~ NA,
                                  TRUE ~ 0),
         riesgoQuiebra = case_when(p40_2 == 1 ~ 1,
                                   p40_2 == NA ~ NA,
                                   TRUE ~ 0),
         riesgoEnfermedad = case_when(p40_3 == 1 ~ 1,
                                      p40_3 == NA ~ NA,
                                      TRUE ~ 0),
         riesgoDesastre = case_when(p40_6 == 1 ~ 1,
                                    p40_6 == NA ~ NA,
                                    TRUE ~ 0),
         victimaDelito = case_when(p40_5 == 1 ~ 1,
                                   p40_5 == NA ~ NA,
                                   TRUE ~ 0),
         abandono = case_when(p40_4 == 1 ~ 1,
                              p40_4 == NA ~ NA,
                              TRUE ~ 0)) %>% 
  mutate(numRiesgos = riesgoEmpleo + riesgoQuiebra + riesgoEnfermedad + riesgoDesastre + victimaDelito + abandono)

# variables de pobreza intergeneracional
basePersonas <- basePersonas %>% 
  mutate(educPadre = case_when(p45_1 <= 2 ~ 1,
                               p45_1 == 3 | p45_1 == 4 ~ 2,
                               p45_1 == 5 | p45_1 == 6 | p45_1 == 8 ~ 3,
                               p45_1 == 7 | p45_1 == 9 ~ 4,
                               TRUE ~ NA),
         educMadre = case_when(p45_2 <= 2 ~ 1,
                               p45_2 == 3 | p45_2 == 4 ~ 2,
                               p45_2 == 5 | p45_2 == 6 | p45_2 == 8 ~ 3,
                               p45_2 == 7 | p45_2 == 9 ~ 4,
                               TRUE ~ NA))

educPadreLabel <-  c("Sin nivel", "Primaria Completa", "Secundaria completa", "Estudios Superiores completos")
basePersonas$educPadre <- factor(basePersonas$educPadre, levels = 1:4, labels = educPadreLabel)                             

educMadreLabel <-  c("Sin nivel", "Primaria Completa", "Secundaria completa", "Estudios Superiores completos")
basePersonas$educMadre <- factor(basePersonas$educMadre, levels = 1:4, labels = educMadreLabel)

# variables de relaciones interfamiliares inestables

basePersonas <- basePersonas %>%
  mutate(confPension = case_when(p24_1 == 1 ~ 1,
                                 p24_1 == NA ~ NA,
                                 TRUE ~ 0),
         confTenencia = case_when(p24_2 == 1 ~ 1,
                                  p24_2 == NA ~ NA,
                                  TRUE ~ 0),
         confVisitas = case_when(p24_3 == 1 ~ 1,
                                 p24_3 == NA ~ NA,
                                 TRUE ~ 0),
         confDivision = case_when(p24_6 == 1 ~ 1,
                                  p24_6 == NA ~ NA,
                                  TRUE ~ 0),
         confViolencia = case_when(p24_7 == 1 ~ 1,
                                   p24_7 == NA ~ NA,
                                   TRUE ~ 0),
         confViolacion = case_when(p24_9 == 1 ~ 1,
                                   p24_9 == NA ~ NA,
                                   TRUE ~ 0))

basePersonas <- basePersonas %>%
  mutate(confFamiliares = abandono + confPension + confTenencia + confVisitas + confDivision + confViolencia + confViolacion)

setwd(dirEnaho)
write_dta(data = basePersonas, "base_trabajo/basePersonasFinal.dta")

## 2.3 Combinación de información entre bases-----------------------------------

### Variables de hogar a personas-----------------------------------------------
baseHP <- baseHogares %>%
  select(anio, mes, conglome, vivienda, hogar, ubigeo, dominio, estrato, 
         agua, aguaHoras, aguaPotable, combustibleCocina, desague, mujerJH,
         electricidad, hogarConHijo, hogarJuntos, hogarMonoParent, hogarPension65, 
         hogarQaliWarma, hogarUniFem, hogarUniMayor, hogarVasoLeche, internet, mieperho, 
         nActivos, nActivosPrioritarios, paredLadrillo, pctIngExt, pisoCemento, pisoTierra, 
         pisoTierraCemento, ratioDependencia, techoDebil, telCelu, tipoHogar, tresServicios, 
         vivBajaCalidad, vivCedida, vivInvasion, auto, cocina_kerosene, computadora, 
         equipo_sonido, hacinam, lavadora, licuadora, microondas, pctPerceptores, plancha, 
         refrigerador, tv_color, gashog2d, pobreza, pobrezav, pobre, vulnerableNoPobre, programasSociales)

basePersonas <- basePersonas %>%
  left_join(baseHP, by = c("anio", "mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato"))
rm(baseHP)

setwd(dirEnaho)
write_dta(data = basePersonas, "base_trabajo/basePersonasFinal.dta")

### Variables de base personas a hogar------------------------------------------
basePH <- basePersonas %>%
  mutate(educPadre = as.numeric(educPadre),
         educMadre = as.numeric(educMadre)) %>% 
  group_by(anio, mes, conglome, vivienda, hogar, ubigeo, dominio, estrato) %>%
  summarise(mujer = mean(mujer, na.rn = TRUE),
            edad = mean(edad, na.rn = TRUE),
            anioEduc = mean(anioEduc, na.rn = TRUE),
            nivEduc = mean(nivEduc, na.rn = TRUE),
            conviviente = mean(conviviente, na.rn = TRUE),
            casado = mean(casado, na.rn = TRUE),
            educSec = mean(ifelse(edad >= 17, educSec, NA), na.rm = TRUE),
            educSup = mean(ifelse(edad >= 25, educSup, NA), na.rm = TRUE),
            indigena = mean(indigena, na.rn = TRUE),
            mestizo = mean(mestizo, na.rn = TRUE),
            algunSeg = mean(algunSeg, na.rn = TRUE),
            difSegSis = mean(difSegSis, na.rn = TRUE),
            empInf = mean(empInf, na.rn = TRUE),
            desemp = mean(desemp, na.rn = TRUE),
            empPeq = mean(empPeq, na.rn = TRUE),
            sinContrato = mean(sinContrato, na.rn = TRUE),
            indep = mean(indep, na.rn = TRUE),
            nCuentas = mean(nCuentas, na.rn = TRUE),
            cuentaNoTiene = mean(cuentaNoTiene, na.rn = TRUE),
            migrante = mean(migrante, na.rn = TRUE),
            migrante5anios = mean(migrante5anios, na.rn = TRUE),
            usoMototaxi = mean(usoMototaxi, na.rn = TRUE),
            usoMicrobus = mean(usoMicrobus, na.rn = TRUE),
            usoOmnibus = mean(usoOmnibus, na.rn = TRUE),
            usoCombi = mean(usoCombi, na.rn = TRUE),
            usoColectivo = mean(usoColectivo, na.rn = TRUE),
            usoTaxi = mean(usoTaxi, na.rn = TRUE),
            nTransporte = mean(nTransporte, na.rn = TRUE),
            usoDiarioMototaxi = mean(usoDiarioMototaxi, na.rn = TRUE), 
            usoDiarioMicrobus = mean(usoDiarioMicrobus, na.rn = TRUE), 
            usoDiarioOmnibus = mean(usoDiarioOmnibus, na.rn = TRUE), 
            usoDiarioCombi = mean(usoDiarioCombi, na.rn = TRUE), 
            usoDiarioColectivo = mean(usoDiarioColectivo, na.rn = TRUE), 
            usoDiarioTaxi = mean(usoDiarioTaxi, na.rn = TRUE), 
            nTrasporteDiario = mean(nTrasporteDiario, na.rn = TRUE), 
            tiempoSalud = mean(tiempoSalud, na.rn = TRUE),
            discriminacion = mean(discriminacion, na.rn = TRUE),
            discapacidad = mean(discapacidad, na.rn = TRUE),
            confFamiliares = mean(confFamiliares, na.rm = TRUE),
            educPadre = mean(educPadre, na.rm = TRUE),
            educMadre = mean(educMadre, na.rm = TRUE),
            numRiesgos = mean(numRiesgos, na.rn = TRUE))

baseHogares <- baseHogares %>%
  left_join(basePH, by= c("anio", "mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato"))
rm(basePH)
  
setwd(dirEnaho)
write_dta(data = baseHogares, "base_trabajo/baseHogaresFinal.dta")  
  