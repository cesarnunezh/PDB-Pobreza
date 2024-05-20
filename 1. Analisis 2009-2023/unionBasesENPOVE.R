################################################################################
# Objetivo: Unión de los módulos de la ENPOVE para los años 2018 y 2022
# Proyecto:  GiZ Pobreza Urbana
# 
# Estructura:
# 0. Librerías y direcciones
# 1. Creación de variables de interés a nivel hogar
# 2. Creación de variables de interés a nivel persona
# 3. Variables de base personas a hogar
#
################################################################################

# 0. Librerías y direcciones ----

bdEnpove <- "/etc/data/enpove"
bdTrabajo <- "/etc/data/base_trabajo"
library(dplyr)  
library(haven)

setwd(bdEnpove)

mods2018 <- c("100" ,"200" , "300", "300a", "300b", "400", "500", "600", "600a",
              "600b", "700", "800")
mods2022 <- c("100" ,"200" , "300", "300a", "700", "800")
mods2020 <- c("400", "500", "600")

# Function
convert_labeled_to_numeric <- function(df) {
  for (col in names(df)) {
    if (inherits(df[[col]], "haven_labelled")) {
      df[[col]] <- as.numeric(df[[col]])
    }
  }
  return(df)
}

# Loop through each dataset and year
for (mod in mods2018) {
  # Read data
  data <- read_spss(paste0("enpove", mod, "_2018", ".sav"))
  colnames(data) <- tolower(colnames(data))
  data$anio <- 2018
  assign(paste0("enpove", mod, "_2018"), data)
}

# Loop through each dataset and year
for (mod in mods2022) {
  # Read data
  data <- read_spss(paste0("enpove", mod, "_2022", ".sav"))
  colnames(data) <- tolower(colnames(data))
  data$anio <- 2022
  assign(paste0("enpove", mod, "_2022"), data)
}

# Loop through each dataset and year
for (mod in mods2020) {
  # Read data
  data <- read_spss(paste0("enpove", mod, "_2020", ".sav"))
  colnames(data) <- tolower(colnames(data))
  data$anio <- 2022
  assign(paste0("enpove", mod, "_2022"), data)
}


# 1. Creación de variables de interés a nivel hogar ----------------------------

# 1.1 Base Hogares 2018 --------------------------------------------------------

baseHogares_2018 <- enpove100_2018 %>%
  filter(resultado_final_vivienda == 1) %>%
  mutate(vivBajaCalidad = case_when(p101 == 5 | p101 == 6 | p101 == 7 ~ 1,
                                    p101 == NA ~ NA,
                                    TRUE ~ 0),
         tipoVivienda = p101,
         paredLadrillo = case_when(p102 == 1 ~ 1,
                                   p102 == NA ~ NA,
                                   TRUE ~ 0),
         materialParedd = p102,
         pisoTierra = case_when(p104 == 6 ~ 1,
                                p104 == NA ~ NA,
                                TRUE ~ 0),
         pisocemento = case_when(p104 == 5 ~ 1,
                                 p104 == NA ~ NA,
                                 TRUE ~ 0),
         materialPiso = p104,
         techoDebil = case_when(p103 == 4 | p103 == 5 | p103 == 6 | p103 == 7 | p103 == 8 ~ 1,
                                p103 == NA ~ NA,
                                TRUE ~ 0),
         materialTecho = p103,
         nHabitaciones = p105,
         nHabitacionesDormir = p106,
         condicionVivienda2018 = p108,
         vivTemporal = case_when(p108 == 2 ~ 1,
                                 p108 == NA ~ NA,
                                 TRUE ~ 0),
         vivCedida = case_when(p108 == 3 ~ 1,
                               p108 == NA ~ NA,
                               TRUE ~ 0),
         agua = case_when(p110 == 1 | p110 == 2 ~ 1,
                          p110 == NA ~ NA,
                          TRUE ~ 0),
         desague = case_when(p112 == 1 | p112 == 2 ~ 1,
                             p112 == NA ~ NA,
                             TRUE ~ 0),
         electricidad = case_when(p109 == 1 ~ 1,
                                  p109 == NA ~ NA,
                                  TRUE ~ 0),
         nActivos2018 = rowSums(select(., p113_1:p113_6), na.rm = TRUE),
         tvColor = case_when(p113_1 == 1 ~ 1,
                             p113_1 == NA ~ NA,
                             TRUE ~ 0),
         cocinaGas = case_when(p113_2 == 1 ~ 1,
                               p113_2 == NA ~ NA,
                               TRUE ~ 0),
         licuadora = case_when(p113_3 == 1 ~ 1,
                               p113_3 == NA ~ NA,
                               TRUE ~ 0),
         plancha = case_when(p113_4 == 1 ~ 1,
                             p113_4 == NA ~ NA,
                             TRUE ~ 0),
         computadora = case_when(p113_5 == 1 ~ 1,
                                 p113_5 == NA ~ NA,
                                 TRUE ~ 0),
         telCel = case_when(p113_6 == 1 ~ 1,
                            p113_6 == NA ~ NA,
                            TRUE ~ 0),
         refrigeradora = case_when(p113_10 == 1 ~ 1,
                                   p113_10 == NA ~ NA,
                                   TRUE ~ 0))


colnames(baseHogares_2018) <- sub("nombdepa", "departamento", colnames(baseHogares_2018))
colnames(baseHogares_2018) <- sub("nombprov", "provincia", colnames(baseHogares_2018))
colnames(baseHogares_2018) <- sub("nombdist", "distrito", colnames(baseHogares_2018))
colnames(baseHogares_2018) <- sub("vivienda", "vivienda2018", colnames(baseHogares_2018))
colnames(baseHogares_2018) <- sub("hogar", "hogar2018", colnames(baseHogares_2018))
colnames(baseHogares_2018) <- sub("resultado_final_vivienda", "vresfin", colnames(baseHogares_2018))

baseHogares_2018 <- baseHogares_2018 %>%
  mutate(idhogar = paste(ubigeo, zona, manzana_id, vivienda2018, hogar2018, sep = ""))

baseHogares_2018 <- baseHogares_2018[, !(colnames(baseHogares_2018) %in% c("pano", "pmes", "periodo", "nomccpp", "codccpp", "ciudades_de_estudio", "cono", "p101", "p102", "p104", "p103", "p105", "p106", "p107", "p108", "p110", "p111", "p112", "p109", "p113_1", "p113_2", "p113_3", "p113_4", "p113_5", "p113_6", "p113_7", "p113_8", "p113_9", "p113_10"))]

baseHogares_2018 <- baseHogares_2018 %>% 
  mutate(anio = 2018)

# 1.2 Base Hogares 2022 --------------------------------------------------------

baseHogares_2022 <- enpove100_2022 %>%
  filter(vresfin == 1) %>%
  mutate(vivBajaCalidad = case_when(p101 == 5 | p101 == 6 | p101 == 7 ~ 1,
                                    p101 == NA ~ NA,
                                    TRUE ~ 0),
         tipoVivienda = p101,
         paredLadrillo = case_when(p102 == 1 ~ 1,
                                   p102 == NA ~ NA,
                                   TRUE ~ 0),
         materialPared = p102,
         pisoTierra = case_when(p104 == 6 ~ 1,
                                p104 == NA ~ NA,
                                TRUE ~ 0),
         pisocemento = case_when(p104 == 5 ~ 1,
                                 p104 == NA ~ NA,
                                 TRUE ~ 0),
         materialPiso = p104,
         techoDebil = case_when(p103 == 4 | p103 == 5 | p103 == 6 | p103 == 7 | p103 == 8 ~ 1,
                                p103 == NA ~ NA,
                                TRUE ~ 0),
         materialTecho = p103,
         nHabitaciones = p105,
         nHabitacionesDormir = p106,
         condicionVivienda2018 = case_when(p107 == 1 ~ 1,
                                           p107 == 5 ~ 3,
                                           p107 == 2 | p107 == 3 | p107 == 4 ~ 4,
                                           p107 == 6 | p107 == 7 | p107 == 8 ~ 6,
                                           p107 == NA ~ NA,
                                           TRUE ~ 0),
         condicionVivienda2022 = p107,
         vivTemporal = case_when(p107 == 2 ~ 1,
                                 p107 == NA ~ NA,
                                 TRUE ~ 0),
         vivCedida = case_when(p107 == 3 ~ 1,
                               p107 == NA ~ NA,
                               TRUE ~ 0),
         agua = case_when(p108_1 == 1 | p108_1 == 2 ~ 1,
                          p108_1 == NA ~ NA,
                          TRUE ~ 0),
         desague = case_when(p108_2 == 1 | p108_2 == 2 ~ 1,
                             p108_2 == NA ~ NA,
                             TRUE ~ 0),
         electricidad = case_when(p108_3 == 1 ~ 1,
                                  p108_3 == NA ~ NA,
                                  TRUE ~ 0),
         internet = case_when(p108_4 == 1 ~ 1,
                              p108_4 == NA ~ NA,
                              TRUE ~ 0),
         nActivos2018 = rowSums(select(., p110_1:p110_6), na.rm = TRUE),
         nActivos2022 = rowSums(select(., p110_1:p110_10), na.rm = TRUE),
         tvColor = case_when(p110_1 == 1 ~ 1,
                             p110_1 == NA ~ NA,
                             TRUE ~ 0),
         cocinaGas = case_when(p110_2 == 1 ~ 1,
                               p110_2 == NA ~ NA,
                               TRUE ~ 0),
         licuadora = case_when(p110_3 == 1 ~ 1,
                               p110_3 == NA ~ NA,
                               TRUE ~ 0),
         plancha = case_when(p110_4 == 1 ~ 1,
                             p110_4 == NA ~ NA,
                             TRUE ~ 0),
         computadora = case_when(p110_5 == 1 ~ 1,
                                 p110_5 == NA ~ NA,
                                 TRUE ~ 0),
         telCel = case_when(p110_6 == 1 ~ 1,
                            p110_6 == NA ~ NA,
                            TRUE ~ 0),
         telFijo = case_when(p110_7 == 1 ~ 1,
                             p110_7 == NA ~ NA,
                             TRUE ~ 0),
         radio = case_when(p110_8 == 1 ~ 1,
                           p110_8 == NA ~ NA,
                           TRUE ~ 0),
         refrigerador = case_when(p110_9 == 1 ~ 1,
                                  p110_9 == NA ~ NA,
                                  TRUE ~ 0),
         lavadora = case_when(p110_10 == 1 ~ 1,
                              p110_10 == NA ~ NA,
                              TRUE ~ 0),
         ubigeo <- paste(ccdd, ccpp, ccdi, sep = ""),
  )

baseHogares_2022 <- baseHogares_2022 %>%
  mutate(idhogar = paste(ccdd, ccpp, ccdi, conglomerado, nselv, vivienda, thogar, nhogar, sep = ""))

baseHogares_2022 <- baseHogares_2022 %>% 
  mutate(anio = 2022)

baseHogares_2022 <- baseHogares_2022[, !(colnames(baseHogares_2022) %in% c("ccdd", "ccpp", "ccdi", "ciudad", "p101", "p102", "p104", "p103", "p105", "p106", "p107", "p108_1", "p108_2", "p108_3", "p108_4", "p110_1", "p110_2", "p110_3", "p110_4", "p110_5", "p110_6", "p110_6", "p110_7", "p110_8", "p110_9", "p110_10"))]

# 1.3 Integración BD Hogares ---------------------------------------------------

Hogares2018Names <- colnames(baseHogares_2018)
Hogares2022Names <- colnames(baseHogares_2022)

unique_cols_2022 <- setdiff(Hogares2022Names, Hogares2018Names)

for (col in unique_cols_2022) {
  baseHogares_2018[[col]] <- NA
}

baseHogares_2018 <- baseHogares_2018[Hogares2022Names]

baseHogares <- rbind(baseHogares_2018, baseHogares_2022)

baseHogares <- baseHogares %>%
  select(departamento, provincia, distrito, conglomerado, anio, vivBajaCalidad, tipoVivienda, paredLadrillo, materialPared, pisoTierra, pisocemento, materialPiso, techoDebil, materialTecho, nHabitaciones, nHabitacionesDormir, condicionVivienda2018, condicionVivienda2022, vivTemporal, vivCedida, agua, desague, electricidad, internet, nActivos2018, nActivos2022, tvColor, cocinaGas, licuadora, plancha, computadora, telCel, telFijo, radio, refrigerador, lavadora, idhogar)

# 2. Creación de variables de interés a nivel hogar ----------------------------

# 2.1 Base Personas 2018 -------------------------------------------------------

mod2_2018 <- enpove200_2018 %>%
  select(pano, pmes, periodo, ubigeo, zona, manzana_id, vivienda, hogar, codperso, p203, p204, p205_a, p206, p207)

mod3_2018 <- enpove300_2018 %>%
  select(pano, pmes, periodo, ubigeo, zona, manzana_id, vivienda, hogar, codperso, p301_a, p301_m, p303_a, p303_m, p307_a, p307_m, p307_d, p315_1, matches("^p315_"))

mod4_2018 <- enpove400_2018 %>%
  select(pano, pmes, periodo, ubigeo, zona, manzana_id, vivienda, hogar, codperso, p401_1, p401_2, p401_3, p401_5, p402, matches("^p403_"), p404, matches("^p408_"), p413, p416_1, p416_2, p416_3, p416_4, p416_5)

mod5_2018 <- enpove500_2018 %>%
  select(pano, pmes, periodo, ubigeo, zona, manzana_id, vivienda, hogar, codperso, p501)

mod6_2018 <- enpove600_2018 %>%
  select(pano, pmes, periodo, ubigeo, zona, manzana_id, vivienda, hogar, codperso, ingtot, ingprin, p601, p602, p603, matches("^p604_"), p608 , p610_ptotal, p611, p611a, p612_1, p614_mon, p615_mon, p616_mon, p618, p617, p619, p620, p621, p622, p623)

mod7_2018 <- enpove700_2018 %>%
  select(pano, pmes, periodo, ubigeo, zona, manzana_id, vivienda, hogar, codperso, p701, matches("^p707_"), matches("^p708_"))

mod8_2018 <- enpove800_2018 %>%
  select(pano, pmes, periodo, ubigeo, zona, manzana_id, vivienda, hogar, codperso)


basePersonas_2018 <- mod2_2018 %>% 
  left_join(mod3_2018, by = c("pano", "pmes", "periodo", "ubigeo", "zona", "manzana_id", "vivienda", "hogar", "codperso")) %>% 
  left_join(mod4_2018, by = c("pano", "pmes", "periodo", "ubigeo", "zona", "manzana_id", "vivienda", "hogar", "codperso")) %>% 
  left_join(mod5_2018, by = c("pano", "pmes", "periodo", "ubigeo", "zona", "manzana_id", "vivienda", "hogar", "codperso")) %>% 
  left_join(mod6_2018, by = c("pano", "pmes", "periodo", "ubigeo", "zona", "manzana_id", "vivienda", "hogar", "codperso")) %>%
  left_join(mod7_2018, by = c("pano", "pmes", "periodo", "ubigeo", "zona", "manzana_id", "vivienda", "hogar", "codperso")) %>%
  left_join(mod8_2018, by = c("pano", "pmes", "periodo", "ubigeo", "zona", "manzana_id", "vivienda", "hogar", "codperso"))

rm(mod2_2018, mod3_2018, mod4_2018, mod5_2018, mod6_2018, mod7_2018, mod8_2018)

# Caracteríticas del individuo 

basePersonas_2018 <- basePersonas_2018 %>% 
  mutate(jefeHogar = case_when(p203 == 1 ~ 1,
                               p203 == NA ~ NA,
                               TRUE ~ 0),
         migranteVen = case_when(p207 == 1 ~ 1,
                                 p207 == NA ~ NA,
                                 TRUE ~ 0),
         mujer = case_when(p204 == 2 ~ 1,
                           p204 == NA ~ NA,
                           TRUE ~ 0),
         edad = p205_a,
         grupoEdad = case_when(edad < 25 ~ 1,
                               edad >=25 & edad < 45 ~ 2,
                               edad >=45 & edad < 65 ~ 3,
                               edad == NA ~ NA,
                               TRUE ~ 0),
         estadoCivil = p206,
         nivEduc = case_when(p501 <= 3 ~ 1,
                             p501 == 4 | p501 == 5 ~ 2,
                             p501 == 6 | p501 == 7 | p501 ==9 ~ 3,
                             p501 == 8 | p501 == 10 | p501 == 11 ~ 4,
                             p501 == NA ~ NA,
                             TRUE ~ 0))

# Información de migración

basePersonas_2018 <- basePersonas_2018 %>% 
  mutate(edadMigra = p303_a - p301_a,
         edadMigra = ifelse(p303_m < p301_m | p303_m == p301_m, edadMigra - 1, edadMigra),
         tipoPermiso = case_when(p315_1 == 1 ~ 1,
                                 p315_2 == 1 ~ 2,
                                 p315_3 == 1 ~ 3,
                                 p315_4 == 1 ~ 4,
                                 p315_5 == 1 ~ 5,
                                 p315_6 == 1 ~ 6,
                                 p315_7 == 1 ~ 7,
                                 p315_8 == 1 ~ 8,
                                 p315_9 == 1 ~ 9,
                                 p315_1 == NA | p315_2 == NA | p315_3 == NA | p315_4 == NA | p315_5 == NA | p315_6 == NA | p315_7 == NA | p315_8 == NA | p315_9 == NA ~ NA,
                                 TRUE ~ 0))

basePersonas_2018 <- left_join(basePersonas_2018,
                               enpove300b_2018 %>% 
                                 group_by(pano, ubigeo, zona, manzana_id, vivienda, hogar, codperso) %>%
                                 summarise(nFamVen = n_distinct(n318)),
                               by = c("pano", "ubigeo", "zona", "manzana_id", "vivienda", "hogar", "codperso"))


basePersonas_2018 <- basePersonas_2018 %>%
  mutate(fechaViajeVenz = as.Date(paste(p307_a, p307_m, p307_d, sep = "-")),
         fechaActual = Sys.Date(),
         aniosViajeVenz = as.numeric(difftime(fechaActual, fechaViajeVenz, units = "days") / 365.25),
         migrante5anios = ifelse(aniosViajeVenz >= 4.5 & aniosViajeVenz <= 5.5, 1, 0))


# Condición de empleo

# pre_r3 <- 1 = ocupados | 2 = desocupado

bP2018 <- basePersonas_2018 %>% 
  mutate(across(starts_with("p604_"), ~ . - 2)) %>% 
  mutate(pre_r3 = case_when(p601 == 1 | p602 == 1 | p603 == 1 | rowSums(select(., starts_with("p604_"))) > 0 ~ 1,
                            p601 == 2 & p602 == 2 & p603 == 2 & rowSums(select(., starts_with("p604_"))) == 0 ~ 2, 
                            TRUE ~ NA)) %>% 
  mutate(r3 = case_when(pre_r3 == 1 & !(p608 %in% c(5, 7)) ~ 1,
                        pre_r3 == 1 & (p608 == 5 | p608 == 7) & p610_ptotal>=15 ~ 1,
                        pre_r3 == 1 & (p608 == 5 | p608 == 7) & p610_ptotal<15 & p618 == 1 & p623>=1 & p623<=6 ~ 2,
                        pre_r3 == 1 & (p608 == 5 | p608 == 7) & p610_ptotal<15 & p618 == 2 & p619>=1 & p619<=2 ~ 2,
                        pre_r3 == 1 & (p608 == 5 | p608 == 7) & p610_ptotal<15 & p618 == 2 & p619>=3 & p619<=8 & p620 == 1 &  p621 == 1 & p622 == 10 ~ 2,
                        pre_r3 == 1 & (p608 == 5 | p608 == 7) & p610_ptotal<15 & p618 == 2 & p619>=3 & p619<=8 & p620 == 1 &  p621 == 1 & p622 == 11 & p623>=1 & p623<=6 ~ 2,
                        pre_r3 == 2 & p618 == 1 & p623>=1 & p623<=6 ~ 2,
                        pre_r3 == 2 & p618 == 2 & p619>=1 & p619<=2 ~ 2,
                        pre_r3 == 2 & p618 == 2 & p619>=3 & p619<=8 & p620 == 1 & p621 == 1 & p622 == 10 ~ 2,
                        pre_r3 == 2 & p618 == 2 & p619>=3 & p619<=8 & p620 == 1 & p621 == 1 & p622 == 11 & p623>=1 & p623<=6 ~ 2,
                        pre_r3 == 1 & (p608 == 5 | p608 == 7) & p610_ptotal<15 & p618 == 1 & p623 == 7 ~ 3,
                        pre_r3 == 1 & (p608 == 5 | p608 == 7) & p610_ptotal<15 & p618 == 2 & p619>=3 & p619<=8 & p620 == 1 & p621 == 1 & p622>=1 & p622<=9 ~ 3,
                        pre_r3 == 1 & (p608 == 5 | p608 == 7) & p610_ptotal<15 & p618 == 2 & p619>=3 & p619<=8 & p620 == 1 & p621 == 2 ~ 3,
                        pre_r3 == 1 & (p608 == 5 | p608 == 7) & p610_ptotal<15 & p618 == 2 & p619>=3 & p619<=8 & p620 == 2 ~ 3,
                        pre_r3 == 1 & (p608 == 5 | p608 == 7) & p610_ptotal<15 & p618 == 2 & p619>=3 & p619<=8 & p620 == 1 & p621 == 1 & p622 == 11 & p623 == 7  ~ 3,
                        pre_r3 == 2 & p618 == 1 & p623 == 7 ~ 3,
                        pre_r3 == 2 & p618 == 2 & p619>=3 & p619<=8 & p620 == 1 & p621 == 1 & p622>=1 & p622<=9 ~ 3,
                        pre_r3 == 2 & p618 == 2 & p619>=3 & p619<=8 & p620 == 1 & p621 == 2 ~ 3,
                        pre_r3 == 2 & p618 == 2 & p619>=3 & p619<=8 & p620 == 2 ~ 3,
                        pre_r3 == 2 & p618 == 2 & p619>=3 & p619<=8 & p620 == 1 & p621 == 1 & p622 == 11 & p623 == 7 ~ 3,
                        TRUE ~ NA))

basePersonas_2018 <- bP2018 %>% 
  mutate(r3 = r3)

rm(pre_r3)
install.packages("labeling")
basePersonas_2018$r3 <- factor(basePersonas_2018$r3, labels = c("Ocupado", "Desocupado", "Inactivo"))

# Ingresos laborales y no laborales

basePersonas_2018 <- basePersonas_2018 %>% 
  mutate(subempHrs = case_when(p611a == 1 ~ 1,
                               p611a == NA ~ NA,
                               TRUE ~ 0),
         HrsTrab = p611)

basePersonas_2018 <- basePersonas_2018 %>% 
  mutate(ingLabPrin = ingprin,
         ingLabSec = p616_mon,
         ingTotalLab = ingtot,
         IngMenorRmv = case_when(ingTotalLab < 1025 ~ 1,
                                 ingTotalLab == NA ~ NA,
                                 TRUE ~ 0))

basePersonas_2018 <- left_join(basePersonas_2018,
                               enpove600a_2018 %>% 
                                 group_by(pano, ubigeo, zona, manzana_id, vivienda, hogar, codperso) %>%
                                 summarise(ingNoLab = sum(p629_4, na.rm = TRUE)),
                               by = c("pano", "ubigeo", "zona", "manzana_id", "vivienda", "hogar", "codperso"))

basePersonas_2018 <- basePersonas_2018 %>%
  mutate(ingTotal = coalesce(ingTotalLab, 0) + coalesce(ingNoLab, 0))

# Características de la empresa

basePersonas_2018 <- basePersonas_2018 %>% 
  mutate(empTam = p612_1)

# Seguridad social

basePersonas_2018 <- basePersonas_2018 %>% 
  mutate(discapMotora = case_when(p408_1 == 1 ~ 1,
                                  p408_1 == NA ~ NA,
                                  TRUE ~ 0),
         discapVista = case_when(p408_2 == 1 ~ 1,
                                 p408_2 == NA ~ NA,
                                 TRUE ~ 0),
         discapHabla = case_when(p408_3 == 1 ~ 1,
                                 p408_3 == NA ~ NA,
                                 TRUE ~ 0),
         discapOir = case_when(p408_4 == 1 ~ 1,
                               p408_4 == NA ~ NA,
                               TRUE ~ 0),
         discapEntender = case_when(p408_5 == 1 ~ 1,
                                    p408_5 == NA ~ NA,
                                    TRUE ~ 0),
         discapRelacion = case_when(p408_6 == 1 ~ 1,
                                    p408_6 == NA ~ NA,
                                    TRUE ~ 0),
         discapacidad = case_when(p408_1==1 | p408_2==1 | p408_3==1 | p408_4==1 | p408_5==1 | p408_6==1 ~ 1,
                                  p408_1==NA & p408_2==NA & p408_3==NA & p408_4==NA & p408_5== NA & p408_6==NA ~ 1,
                                  TRUE ~ 0))

basePersonas_2018 <- basePersonas_2018 %>% 
  mutate(segEssalud = case_when(p401_1 == 1 ~ 1,
                                p401_1 == NA ~ NA,
                                TRUE ~ 0),
         segPrivado = case_when(p401_2 == 1 ~ 1,
                                p401_2 == NA ~ NA,
                                TRUE ~ 0),
         segSIS = case_when(p401_3 == 1 ~ 1,
                            p401_3 == NA ~ NA,
                            TRUE ~ 0),
         sinSeguro = case_when(p401_5 == 1 ~ 1,
                               p401_5 == NA ~ NA,
                               TRUE ~ 0),
         enfCronica = case_when(p402 == 1 ~ 1,
                                p402 == NA ~ NA,
                                TRUE ~ 0),
         freqTratEnfCronica = p404,
         accesoAnticonceptivo = case_when(p416_1 == 1 | p416_2 == 1 | p416_3 == 1 ~ 1,
                                          p416_1 == NA | p416_2 == NA | p416_3 == NA ~ NA,
                                          TRUE ~ 0),
         accesoPruebaVIH = case_when(p416_4 == 1 ~ 1,
                                     p416_4 == NA ~ NA,
                                     TRUE ~ 0),
         accesoEducSexual = case_when(p416_5 == 1 ~ 1,
                                      p416_5 == NA ~ NA,
                                      TRUE ~ 0))

# Participación ciudadana

basePersonas_2018 <- basePersonas_2018 %>% 
  mutate(participacion = case_when(p707_1 == 1 | p707_2 == 1 | p707_3 == 1 | p707_4 == 1 | p707_5 == 1 | p707_6 == 1 | p707_7 == 1 | p707_8 == 1 | p707_9 == 1 ~ 1,
                                   p707_1 == NA | p707_2 == NA | p707_3 == NA | p707_4 == NA | p707_5 == NA | p707_6 == NA | p707_7 == NA | p707_8 == NA | p707_9 == NA ~ NA,
                                   TRUE ~ 0),
         participacionVirt = case_when(p708_1 == 1 | p708_2 == 1 | p708_3 == 1 | p708_4 == 1 ~ 1,
                                       p708_1 == NA | p708_2 == NA | p708_3 == NA | p708_4 == NA ~ 1,
                                       TRUE ~ 0))
# Distrito - educación y trabajo

basePersonas_2018 <- basePersonas_2018 %>% 
  mutate(trabajaMD = case_when(p617 == 1 ~ 1,
                               p617 == NA ~ NA,
                               TRUE ~ 0))

# Discriminación

basePersonas_2018 <- basePersonas_2018 %>% 
  mutate(discrOrigen = case_when(p701 == 1 ~ 1,
                                 p701 == NA ~ NA,
                                 TRUE ~ 0),
         discriminacion2018 = discrOrigen)

# Selección de variables relevantes

basePersonas_2018 <- basePersonas_2018 %>%
  select(ubigeo, zona, manzana_id, vivienda, hogar, jefeHogar, migranteVen, mujer, edad, grupoEdad, estadoCivil, nivEduc, edadMigra, tipoPermiso, nFamVen, fechaViajeVenz, fechaActual, aniosViajeVenz, migrante5anios, pre_r3, r3, subempHrs, HrsTrab, ingLabPrin, ingLabSec, ingTotalLab, IngMenorRmv, ingNoLab, ingTotal, empTam, discapMotora, discapVista, discapHabla, discapOir, discapEntender, discapRelacion, discapacidad, segEssalud, segPrivado, segSIS, sinSeguro, enfCronica, freqTratEnfCronica, accesoAnticonceptivo, accesoPruebaVIH, accesoEducSexual, participacion, participacionVirt, trabajaMD, discrOrigen, discriminacion2018)

basePersonas_2018 <- basePersonas_2018 %>%
  mutate(idhogar = paste(ubigeo, zona, manzana_id, vivienda, hogar, sep = "")) %>%
  select(-vivienda)

basePersonas_2018 <- basePersonas_2018 %>% 
  mutate(anio = 2018)      


# 2.2 Base Personas 2022 -------------------------------------------------------

mod2_2022 <- enpove200_2022 %>%
  select(ccdd, ccpp, ccdi, conglomerado, nselv, vivienda, thogar, nhogar, vresfin, p200_n, p203, p208, p204, p205_a, p206)

mod3_2022 <- enpove300_2022 %>%
  select(ccdd, ccpp, ccdi, conglomerado, nselv, vivienda, thogar, nhogar, vresfin, p200_n, p303_anio, p301_a, p303_mes, p301_m, p307, matches("^p315_"))

mod4_2022 <- enpove400_2022 %>%
  select(ccdd, ccpp, ccdi, conglomerado, nselv, vivienda, thogar, nhogar, vresfin, p200_n, p401_1, p401_2, p401_3, p401_5, p402, matches("^p403_"), p404, matches("^p408_"), p416_1, p416_2, p416_3, p416_4, p416_5, p417_1, p417_2, p417_3)

mod5_2022 <- enpove500_2022 %>%
  select(ccdd, ccpp, ccdi, conglomerado, nselv, vivienda, thogar, nhogar, vresfin, p200_n, p501, p501b)

mod6_2022 <- enpove600_2022 %>%
  select(ccdd, ccpp, ccdi, conglomerado, nselv, vivienda, thogar, nhogar, vresfin, p200_n, p601, p602, p603, matches("^p605_"), p612, p615_t, p614, p616a, p617, p618, p622_1, p623_1, p624_1, p625, p627, p628, p629, p630, p631, matches("^p632_"), p638_1_2, p638_2_2, p638_3_2, p638_4_2)

mod7_2022 <- enpove700_2022 %>%
  select(ccdd, ccpp, ccdi, conglomerado, nselv, vivienda, thogar, nhogar, vresfin, p200_n, p701, matches("^p707_"), matches("^p702_"))

mod8_2022 <- enpove800_2022 %>%
  select(ccdd, ccpp, ccdi, conglomerado, nselv, vivienda, thogar, nhogar, vresfin, p200_n)


basePersonas_2022 <- mod2_2022 %>% 
  left_join(mod3_2022, by = c("ccdd", "ccpp", "ccdi", "conglomerado", "nselv", "vivienda", "thogar", "nhogar", "vresfin", "p200_n")) %>% 
  left_join(mod4_2022, by = c("ccdd", "ccpp", "ccdi", "conglomerado", "nselv", "vivienda", "thogar", "nhogar", "vresfin", "p200_n")) %>% 
  left_join(mod5_2022, by = c("ccdd", "ccpp", "ccdi", "conglomerado", "nselv", "vivienda", "thogar", "nhogar", "vresfin", "p200_n")) %>% 
  left_join(mod6_2022, by = c("ccdd", "ccpp", "ccdi", "conglomerado", "nselv", "vivienda", "thogar", "nhogar", "vresfin", "p200_n")) %>%
  left_join(mod7_2022, by = c("ccdd", "ccpp", "ccdi", "conglomerado", "nselv", "vivienda", "thogar", "nhogar", "vresfin", "p200_n")) %>%
  left_join(mod8_2022, by = c("ccdd", "ccpp", "ccdi", "conglomerado", "nselv", "vivienda", "thogar", "nhogar", "vresfin", "p200_n"))

rm(mod2_2022, mod3_2022, mod4_2022, mod5_2022, mod6_2022, mod7_2022, mod8_2022)

# Caracteríticas del individuo 

basePersonas_2022 <- basePersonas_2022 %>% 
  mutate(jefeHogar = case_when(p203 == 1 ~ 1,
                               p203 == NA ~ NA,
                               TRUE ~ 0),
         migranteVen = case_when(p208 == 1 ~ 1,
                                 p208 == NA ~ NA,
                                 TRUE ~ 0),
         mujer = case_when(p204 == 2 ~ 1,
                           p204 == NA ~ NA,
                           TRUE ~ 0),
         edad = p205_a,
         grupoEdad = case_when(edad < 25 ~ 1,
                               edad >=25 & edad < 45 ~ 2,
                               edad >=45 & edad < 65 ~ 3,
                               edad == NA ~ NA,
                               TRUE ~ 0),
         estadoCivil = p206,
         nivEducVen = case_when(p501 <= 3 ~ 1,
                                p501 == 4 | p501 == 5 ~ 2,
                                p501 == 6 | p501 == 7 | p501 ==9 ~ 3,
                                p501 == 8 | p501 == 10 | p501 == 11 ~ 4,
                                p501 == NA ~ NA,
                                TRUE ~ 0),
         nivEducPeru = case_when(p501b <= 3 ~ 1,
                                 p501b == 4 | p501b == 5 ~ 2,
                                 p501b == 6 | p501b == 7 | p501b ==9 ~ 3,
                                 p501b == 8 | p501b == 10 | p501b == 11 ~ 4,
                                 p501b == NA ~ NA,
                                 TRUE ~ 0))

# Condición de empleo

bP2022 <- basePersonas_2022 %>%
  mutate(pre_r3 = case_when(p601 == 1 | p602 == 1 | p603 == 1 | rowSums(select(., starts_with("p605_"))) > 0 ~ 1,
                            p601 == 2 & p603 == 2 & p605_1 == 2 & p605_2 == 2 & p605_3 == 2 & p605_4 == 2 & p605_5 == 2 & p605_6 == 2 & p605_7 == 2 & p605_8 == 2 & p605_9 == 2 & p605_10 == 2 & p605_11 == 2 & p605_12 == 2 ~ 2,
                            TRUE ~ NA),
         p632 = case_when(p632_1 == 1 ~ 1,
                          p632_2 == 1 ~ 2,
                          p632_3 == 1 ~ 3,
                          p632_4 == 1 ~ 4,
                          p632_5 == 1 ~ 5,
                          p632_6 == 1 ~ 6,
                          p632_7 == 1 ~ 7,
                          p632_8 == 1 ~ 8,
                          p632_9 == 1 ~ 9,
                          p632_10 == 1 ~ 10,
                          TRUE ~ NA)) %>%
  mutate(r3 = case_when(pre_r3 == 1 & !(p612 %in% c(5, 7)) ~ 1,
                        pre_r3 == 1 & (p612 == 5 | p612 == 7) & p615_t>=15 ~ 1,
                        pre_r3 == 1 & (p612 == 5 | p612 == 7) & p615_t<15 & p627 == 1 & p632>=1 & p632<=6 ~ 2,
                        pre_r3 == 1 & (p612 == 5 | p612 == 7) & p615_t<15 & p627 == 2 & p628>=1 & p628<=2 ~ 2,
                        pre_r3 == 1 & (p612 == 5 | p612 == 7) & p615_t<15 & p627 == 2 & p628>=3 & p628<=8 & p629 == 1 &  p630 == 1 & p631 == 10 ~ 2,
                        pre_r3 == 1 & (p612 == 5 | p612 == 7) & p615_t<15 & p627 == 2 & p628>=3 & p628<=8 & p629 == 1 &  p630 == 1 & p631 == 11 & p632>=1 & p632<=6 ~ 2,
                        pre_r3 == 2 & p627 == 1 & p632>=1 & p632<=6 ~ 2,
                        pre_r3 == 2 & p627 == 2 & p628>=1 & p628<=2 ~ 2,
                        pre_r3 == 2 & p627 == 2 & p628>=3 & p628<=8 & p629 == 1 & p630 == 1 & p631 == 10 ~ 2,
                        pre_r3 == 2 & p627 == 2 & p628>=3 & p628<=8 & p629 == 1 & p630 == 1 & p631 == 11 & p632>=1 & p632<=6 ~ 2,
                        pre_r3 == 1 & (p612 == 5 | p612 == 7) & p615_t<15 & p627 == 1 & p632 == 7 ~ 3,
                        pre_r3 == 1 & (p612 == 5 | p612 == 7) & p615_t<15 & p627 == 2 & p628>=3 & p628<=8 & p629 == 1 & p630 == 1 & p631>=1 & p631<=9 ~ 3,
                        pre_r3 == 1 & (p612 == 5 | p612 == 7) & p615_t<15 & p627 == 2 & p628>=3 & p628<=8 & p629 == 1 & p630 == 2 ~ 3,
                        pre_r3 == 1 & (p612 == 5 | p612 == 7) & p615_t<15 & p627 == 2 & p628>=3 & p628<=8 & p629 == 2 ~ 3,
                        pre_r3 == 1 & (p612 == 5 | p612 == 7) & p615_t<15 & p627 == 2 & p628>=3 & p628<=8 & p629 == 1 & p630 == 1 & p631 == 11 & p632 == 7  ~ 3,
                        pre_r3 == 2 & p627 == 1 & p632 == 7 ~ 3,
                        pre_r3 == 2 & p627 == 2 & p628>=3 & p628<=8 & p629 == 1 & p630 == 1 & p631>=1 & p631<=9 ~ 3,
                        pre_r3 == 2 & p627 == 2 & p628>=3 & p628<=8 & p629 == 1 & p630 == 2 ~ 3,
                        pre_r3 == 2 & p627 == 2 & p628>=3 & p628<=8 & p629 == 2 ~ 3,
                        pre_r3 == 2 & p627 == 2 & p628>=3 & p628<=8 & p629 == 1 & p630 == 1 & p631 == 11 & p632 == 7 ~ 3,
                        TRUE ~ NA))

basePersonas_2022 <- bP2022 %>% 
  mutate(r3 = r3)

rm(pre_r3)
install.packages("labeling")
basePersonas_2022$r3 <- factor(basePersonas_2022$r3, labels = c("Ocupado", "Desocupado", "Inactivo"))

# Ingresos laborales y no laborales

basePersonas_2022 <- basePersonas_2022 %>% 
  mutate(subempHrs = case_when(p617 == 1 & p618 == 1 ~ 1,
                               p617 == NA | p618 == NA ~ NA,
                               TRUE ~ 0),
         HrsTrab = p616a)

basePersonas_2022 <- basePersonas_2022 %>% 
  mutate(ingLabPrin = coalesce(p622_1, 0) + coalesce(p623_1, 0),
         ingLabSec = p624_1,
         ingTotalLab = coalesce(ingLabPrin, 0) + coalesce(ingLabSec, 0),
         IngMenorRmv = case_when(ingTotalLab < 1025 ~ 1,
                                 ingTotalLab == NA ~ NA,
                                 TRUE ~ 0),
         ingNoLab = coalesce(p638_1_2, 0) + coalesce(p638_2_2, 0) + coalesce(p638_3_2, 0) + coalesce(p638_4_2, 0))


basePersonas_2022 <- basePersonas_2022 %>%
  mutate(ingTotal = coalesce(ingTotalLab, 0) + coalesce(ingNoLab, 0))
         
# Características de la empresa

basePersonas_2022 <- basePersonas_2022 %>% 
  mutate(empTam = p614)

# Seguridad social

basePersonas_2022 <- basePersonas_2022 %>% 
  mutate(discapMotora = case_when(p408_1 == 1 ~ 1,
                                  p408_1 == NA ~ NA,
                                  TRUE ~ 0),
         discapVista = case_when(p408_2 == 1 ~ 1,
                                 p408_2 == NA ~ NA,
                                 TRUE ~ 0),
         discapHabla = case_when(p408_3 == 1 ~ 1,
                                 p408_3 == NA ~ NA,
                                 TRUE ~ 0),
         discapOir = case_when(p408_4 == 1 ~ 1,
                               p408_4 == NA ~ NA,
                               TRUE ~ 0),
         discapEntender = case_when(p408_5 == 1 ~ 1,
                                    p408_5 == NA ~ NA,
                                    TRUE ~ 0),
         discapRelacion = case_when(p408_6 == 1 ~ 1,
                                    p408_6 == NA ~ NA,
                                    TRUE ~ 0),
         discapacidad = case_when(p408_1==1 | p408_2==1 | p408_3==1 | p408_4==1 | p408_5==1 | p408_6==1 ~ 1,
                                  p408_1==NA & p408_2==NA & p408_3==NA & p408_4==NA & p408_5== NA & p408_6==NA ~ NA,
                                  TRUE ~ 0))

basePersonas_2022 <- basePersonas_2022 %>% 
  mutate(segEssalud = case_when(p401_1 == 1 ~ 1,
                                p401_1 == NA ~ NA,
                                TRUE ~ 0),
         segPrivado = case_when(p401_2 == 1 ~ 1,
                                p401_2 == NA ~ NA,
                                TRUE ~ 0),
         segSIS = case_when(p401_3 == 1 ~ 1,
                            p401_3 == NA ~ NA,
                            TRUE ~ 0),
         sinSeguro = case_when(p401_5 == 1 ~ 1,
                               p401_5 == NA ~ NA,
                               TRUE ~ 0),
         enfCronica = p402,
         freqTratEnfCronica = p404,
         accesoAnticonceptivo = case_when(p417_1 == 1 ~ 1,
                                          p417_1 == NA ~ NA,
                                          TRUE ~ 0),
         accesoPruebaVIH = case_when(p417_2 == 1 ~ 1,
                                     p417_2 == NA ~ NA,
                                     TRUE ~ 0),
         accesoEducSexual = case_when(p417_3 == 1 ~ 1,
                                      p417_3 == NA ~ NA,
                                      TRUE ~ 0))

# Participación ciudadana

basePersonas_2022 <- basePersonas_2022 %>% 
  mutate(participacion = case_when(p707_1 == 1 | p707_2 == 1 | p707_3 == 1 | p707_4 == 1 | p707_5 == 1 | p707_6 == 1 | p707_7 == 1 | p707_8 == 1 | p707_9 == 1 ~ 1,
                                   p707_1 == NA | p707_2 == NA | p707_3 == NA | p707_4 == NA | p707_5 == NA | p707_6 == NA | p707_7 == NA | p707_8 == NA | p707_9 == NA ~ NA,
                                   TRUE ~ 0))

basePersonas_2022 <- basePersonas_2022 %>% 
  mutate(trabajaMD = case_when(p625 == 1 ~ 1,
                               p625 == NA ~ NA,
                               TRUE ~ 0))

# Discriminación

basePersonas_2022 <- basePersonas_2022 %>% 
  mutate(discrSexo = case_when(p702_1 == 1 ~ 1,
                               p702_1 == NA ~ NA,
                               TRUE ~ 0),
         discrOrientacion = case_when(p702_2 == 1 ~ 1,
                                      p702_2 == NA ~ NA,
                                      TRUE ~ 0),
         discCondMigr = case_when(p702_3 == 1 ~ 1,
                                  p702_3 == NA ~ NA,
                                  TRUE ~ 0),
         discrSocioEco = case_when(p702_4 == 1 ~ 1,
                                   p702_4 == NA ~ NA,
                                   TRUE ~ 0),
         discrOrigen = case_when(p702_5 == 1 ~ 1,
                                 p702_5 == NA ~ NA,
                                 TRUE ~ 0),
         discrRaza = case_when(p702_6 == 1 ~ 1,
                               p702_6 == NA ~ NA,
                               TRUE ~ 0),
         discriminacion2022 = case_when(p702_1==1 | p702_2==1 | p702_3==1 | p702_4==1 | p702_5==1 | p702_6==1 ~ 1,
                                        p702_1==NA & p702_2==NA & p702_3==NA & p702_4==NA & p702_5== NA & p702_6==NA ~ NA,
                                        TRUE ~ 0))


# Información de migración

enpove300a_2022 <- enpove300a_2022 %>%
  mutate(idpers = paste(ccdd, ccpp, ccdi, conglomerado, nselv, vivienda, thogar, nhogar, vresfin, p200_n, sep = ""))

basePersonas_2022 <- basePersonas_2022 %>%
  mutate(p301_a = as.numeric(p301_a),
         edadMigra = p303_anio - p301_a,
         edadMigra = ifelse(p303_mes < p301_m | p303_mes == p301_m, edadMigra - 1, edadMigra),
         tipoPermiso = p307,
         idpers = paste(ccdd, ccpp, ccdi, conglomerado, nselv, vivienda, thogar, nhogar, vresfin, p200_n, sep = "")) 

enpove300a_2022 <- enpove300a_2022 %>%
  group_by(ccdd, ccpp, ccdi, conglomerado, nselv, vivienda, thogar, nhogar, vresfin, p200_n) %>%
  summarise(respuesta_viv = all(vresfin == 1),
            nFamVen = n_distinct(p313_n)) %>% 
  select(ccdd, ccpp, ccdi, conglomerado, nselv, vivienda, thogar, nhogar, vresfin, p200_n, respuesta_viv, nFamVen)

baseMigra2022 <- basePersonas_2022 %>% 
  left_join(enpove300a_2022, by = c("ccdd", "ccpp", "ccdi", "conglomerado", "nselv", "vivienda", "thogar", "nhogar", "vresfin", "p200_n")) %>%
  filter(respuesta_viv == 1) %>%
  select(-respuesta_viv) 

basePersonas_2022 <- left_join(basePersonas_2022, baseMigra2022, by = "idpers") 

basePersonas_2022 <- basePersonas_2022 %>%
  select(ccdd.x, ccpp.x, ccdi.x, conglomerado.x, nselv.x, vivienda.x, thogar.x, nhogar.x, vresfin.x, p200_n.x, jefeHogar.x, migranteVen.x, mujer.x, edad.x, grupoEdad.x, estadoCivil.x, nivEducVen.x, nivEducPeru.x, pre_r3.x, p632.x, r3.x, subempHrs.x, HrsTrab.x, ingLabPrin.x, ingLabSec.x, ingTotalLab.x, IngMenorRmv.x, ingNoLab.x, ingTotal.x, empTam.x, discapMotora.x, discapVista.x, discapHabla.x, discapOir.x, discapEntender.x, discapRelacion.x, discapacidad.x, segEssalud.x, segPrivado.x, segSIS.x, sinSeguro.x, enfCronica.x, freqTratEnfCronica.x, accesoAnticonceptivo.x, accesoPruebaVIH.x, accesoEducSexual.x, participacion.x, trabajaMD.x, discrSexo.x, discrOrientacion.x, discCondMigr.x, discrSocioEco.x, discrOrigen.x, discrRaza.x, discriminacion2022.x, r3.x, idpers, edadMigra.x, tipoPermiso.x, nFamVen)

basePersonas_2022 <- basePersonas_2022 %>%
  rename(
    ccdd = ccdd.x,
    ccpp = ccpp.x,
    ccdi = ccdi.x,
    conglomerado = conglomerado.x,
    nselv = nselv.x,
    vivienda = vivienda.x,
    thogar = thogar.x,
    nhogar = nhogar.x,
    vresfin = vresfin.x,
    p200_n = p200_n.x,
    jefeHogar = jefeHogar.x,
    migranteVen = migranteVen.x,
    mujer = mujer.x,
    edad = edad.x,
    grupoEdad = grupoEdad.x,
    estadoCivil = estadoCivil.x,
    nivEducVen = nivEducVen.x,
    nivEducPeru = nivEducPeru.x,
    pre_r3 = pre_r3.x,
    p632 = p632.x,
    r3 = r3.x,
    subempHrs = subempHrs.x,
    HrsTrab = HrsTrab.x,
    ingLabPrin = ingLabPrin.x,
    ingLabSec = ingLabSec.x,
    ingTotalLab = ingTotalLab.x,
    IngMenorRmv = IngMenorRmv.x,
    ingNoLab = ingNoLab.x,
    ingTotal = ingTotal.x,
    empTam = empTam.x,
    discapMotora = discapMotora.x,
    discapVista = discapVista.x,
    discapHabla = discapHabla.x,
    discapOir = discapOir.x,
    discapEntender = discapEntender.x,
    discapRelacion = discapRelacion.x,
    discapacidad = discapacidad.x,
    segEssalud = segEssalud.x,
    segPrivado = segPrivado.x,
    segSIS = segSIS.x,
    sinSeguro = sinSeguro.x,
    enfCronica = enfCronica.x,
    freqTratEnfCronica = freqTratEnfCronica.x,
    accesoAnticonceptivo = accesoAnticonceptivo.x,
    accesoPruebaVIH = accesoPruebaVIH.x,
    accesoEducSexual = accesoEducSexual.x,
    participacion = participacion.x,
    trabajaMD = trabajaMD.x,
    discrSexo = discrSexo.x,
    discrOrientacion = discrOrientacion.x,
    discCondMigr = discCondMigr.x,
    discrSocioEco = discrSocioEco.x,
    discrOrigen = discrOrigen.x,
    discrRaza = discrRaza.x,
    discriminacion2022 = discriminacion2022.x,
    r3 = r3.x,
    edadMigra = edadMigra.x,
    tipoPermiso = tipoPermiso.x
  )
  
basePersonas_2022 <- basePersonas_2022 %>%
  mutate(idhogar = paste(ccdd, ccpp, ccdi, conglomerado, nselv, vivienda, thogar, nhogar, sep = "")) %>%
  select(-vivienda)

basePersonas_2022 <- basePersonas_2022 %>% 
  mutate(anio = 2022)     

# 2.3 Integración BD Personas --------------------------------------------------

Personas2018Names <- colnames(basePersonas_2018)
Personas2022Names <- colnames(basePersonas_2022)

unique_cols_Pers_2022 <- setdiff(Personas2022Names, Personas2018Names)

for (col in unique_cols_Pers_2022) {
  basePersonas_2018[[col]] <- NA
}

basePersonas <- bind_rows(basePersonas_2018, basePersonas_2022)


# 3. Variables de base personas a hogar-----------------------------------------

basePH <- basePersonas %>%
  group_by(idhogar) %>%
  summarise(migranteVenH = mean(migranteVen, na.rm = TRUE),
            mujerH = mean(mujer, na.rm = TRUE),
            edadH = mean(edad, na.rm = TRUE),
            grupoEdadH = mean(grupoEdad, na.rm = TRUE),
            estadoCivilH = mean(estadoCivil, na.rm = TRUE),
            nivEducH = mean(nivEduc, na.rm = TRUE),
            nivEducPeruH = mean(nivEducPeru, na.rm = TRUE),
            nivEducVenH = mean(nivEducVen, na.rm = TRUE),
            edadMigraH = mean(edadMigra, na.rm = TRUE),
            tipoPermisoH = mean(tipoPermiso, na.rm = TRUE),
            nFamVenH = mean(nFamVen, na.rm = TRUE),
            aniosViajeVenzH = mean(aniosViajeVenz, na.rm = TRUE),
            migrante5aniosH = mean(migrante5anios, na.rm = TRUE),
            r3H = mean(r3, na.rm = TRUE),
            subempHrsH = mean(subempHrs, na.rm = TRUE),
            HrsTrabH = mean(HrsTrab, na.rm = TRUE),
            ingLabPrinH = mean(ingLabPrin, na.rm = TRUE),
            ingLabSecH = mean(ingLabSec, na.rm = TRUE),
            ingTotalLabH = mean(ingTotalLab, na.rm = TRUE),
            ingMenorRmvH = mean(IngMenorRmv, na.rm = TRUE),
            ingNoLabH = mean(ingNoLab, na.rm = TRUE),
            ingTotalH = mean(ingTotal, na.rm = TRUE),
            empTamH = mean(empTam, na.rm = TRUE),
            discapMotoraH = mean(discapMotora, na.rm = TRUE),
            discapVistaH = mean(discapVista, na.rm = TRUE),
            discapHablaH = mean(discapHabla, na.rm = TRUE),
            discapOirH = mean(discapOir, na.rm = TRUE),
            discapEntenderH = mean(discapEntender, na.rm = TRUE),
            discapRelacionH = mean(discapRelacion, na.rm = TRUE),
            discapacidadH = mean(discapacidad, na.rm = TRUE),
            segEssaludH = mean(segEssalud, na.rm = TRUE),
            segPrivadoH = mean(segPrivado, na.rm = TRUE),
            segSISH = mean(segSIS, na.rm = TRUE),
            sinSeguroH = mean(sinSeguro, na.rm = TRUE),
            enfCronicaH = mean(enfCronica, na.rm = TRUE),
            freqTratEnfCronicaH = mean(freqTratEnfCronica, na.rm = TRUE),
            accesoAnticonceptivoH = mean(accesoAnticonceptivo, na.rm = TRUE),
            accesoPruebaVIHH = mean(accesoPruebaVIH, na.rm = TRUE),
            accesoEducSexualH = mean(accesoEducSexual, na.rm = TRUE),
            participacionH = mean(participacion, na.rm = TRUE),
            participacionVirtH = mean(participacionVirt, na.rm = TRUE),
            trabajaMDH = mean(trabajaMD, na.rm = TRUE),
            discrOrigenH = mean(discrOrigen, na.rm = TRUE),
            discrSexoH = mean(discrSexo, na.rm = TRUE),
            discrOrientacionH = mean(discrOrientacion, na.rm = TRUE),
            discCondMigrH = mean(discCondMigr, na.rm = TRUE),
            discrSocioEcoH = mean(discrSocioEco, na.rm = TRUE),
            discrRazaH = mean(discrRaza, na.rm = TRUE),
            discriminacion2018H = mean(discriminacion2018, na.rm = TRUE),
            discriminacion2022H = mean(discriminacion2022, na.rm = TRUE))
  
baseHogares <- baseHogares %>%
  left_join(basePH, by= c("idhogar"))

rm(basePH)

setwd("/etc/data/base_trabajo")
write_dta(data = baseHogares, "baseHogaresENPOVE.dta")

