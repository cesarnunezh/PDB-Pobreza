################################################################################
# Objetivo: Unión de los módulos de la ENDES para el periodo 2015-2022
# Proyecto:  GiZ Pobreza Urbana
# 
# Estructura:
# 0. Librerías y direcciones
# 1. Append de bases 2015-2022
# 2. Unión de bases a nivel de hogar y personas
# 3. Creación de variables de interés

################################################################################
# 0. Librerías y direcciones ----

bdEndes <- "/etc/data/endes"
#bdEndes <- "C:/Users/User/OneDrive - Universidad del Pacífico/1. Documentos/0. Bases de datos/03. ENDES/1. Data"
bdTrabajo <- "/etc/data/base_trabajo"
#bdTrabajo <- "C:/Users/User/OneDrive - MIGRACIÓN VIDENZA/1. Proyectos/1. Proyectos actuales/GiZ Pobreza Urbana/1. Productos/0. Insumos/1. Bases de datos"
library(dplyr)  
library(haven)

# 1. Append de bases 2015-2022 ----

data15_22 <- c("rec44" ,"rec43" , "rec42", "rec41", "rec21", "re758081", "re516171",
               "re223132", "programas_sociales_x_hogar", "rec82", "rec83", "rec84dv",
               "rec91", "rec94", "rec95", "rec0111", "rech0", "rech1", "rech4", 
               "rech5", "rech6", "rech23")
data19_22 <- c("dit")

# Set working directory
setwd(bdEndes)

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
for (mod in data15_22) {
  for (x in 2015:2022) {
    # Read data
    data <- read_dta(paste0(mod, "_", x, ".dta"))
    colnames(data) <- tolower(colnames(data))
    data$id1 <- x
    data <- convert_labeled_to_numeric(data)
    
    # Store the combined data frame in the list
    if (x == 2015) {
      df <- data
    }
    else {
      dataNames <- colnames(data)
      dfNames <- colnames(df)
      common_cols <- intersect(dataNames, dfNames)
      df <- rbind(df[common_cols], data[common_cols])
      
    }
  }
  assign(paste0(mod), df)
  
}

# Loop through each dataset and year
for (mod in data19_22) {
  for (x in 2019:2022) {
    # Read data
    data <- read_dta(paste0(mod, "_", x, ".dta"))
    colnames(data) <- tolower(colnames(data))
    data$id1 <- x
    data <- convert_labeled_to_numeric(data)
    
    # Store the combined data frame in the list
    if (x == 2019) {
      df <- data
    }
    else {
      dataNames <- colnames(data)
      dfNames <- colnames(df)
      common_cols <- intersect(dataNames, dfNames)
      df <- rbind(df[common_cols], data[common_cols])
      
    }
  }
  assign(paste0(mod), df)
  
}

rm(data, df)

# 2. Unión de bases a nivel de hogar y personas --------------------------------
# Base hogares----
baseHogaresENDES <- rech0 %>% 
  left_join(rech23, by = c("id1", "hhid")) %>% 
  rename(mieperho = hv013,
         nMujeres = hv010,
         nHombres = hv011,
         nNinos = hv014,
         estrato = hv022,
         dominio = hv023,
         region = hv024,
         area = hv025,
         altitud = hv040,
         quintil = hv270,
         riqueza = hv271,
         tvCable = sh61j,
         licuadora = sh61k,
         cocinaGas = sh61l,
         microndas = sh61n,
         lavadora = sh61o,
         computadora = sh61p,
         internet = sh61q,
         cortinas = sh76e) %>% 
  mutate(agua = case_when(hv201 == 11 | hv201 == 12 ~ 1,
                          hv201 == 99 ~ NA,
                          hv201 == NA ~ NA,
                          TRUE ~ 0),
         tiempoAgua = case_when(hv204 == 996 | hv204 == 998 | hv204 == 999 ~ NA,
                                TRUE ~ hv204),
         desague = case_when(hv205 == 11 | hv205 == 12 ~ 1,
                             hv205 == 99 ~ NA,
                             hv205 == NA ~ NA,
                             TRUE ~ 0),
         electricidad = case_when(hv206 == 1  ~ 1,
                                  hv206 == 9 ~ NA,
                                  hv206 == NA ~ NA,
                                  TRUE ~ 0),
         radio = case_when(hv207 == 1  ~ 1,
                           hv207 == 9 ~ NA,
                           hv207 == NA ~ NA,
                           TRUE ~ 0),
         tv = case_when(hv208 == 1  ~ 1,
                        hv208 == 9 ~ NA,
                        hv208 == NA ~ NA,
                        TRUE ~ 0),
         refrigerador = case_when(hv209 == 1  ~ 1,
                                  hv209 == 9 ~ NA,
                                  hv209 == NA ~ NA,
                                  TRUE ~ 0),
         bicicleta = case_when(hv210 == 1  ~ 1,
                               hv210 == 9 ~ NA,
                               hv210 == NA ~ NA,
                               TRUE ~ 0),
         moto = case_when(hv211 == 1  ~ 1,
                          hv211 == 9 ~ NA,
                          hv211 == NA ~ NA,
                          TRUE ~ 0),
         carro = case_when(hv212 == 1  ~ 1,
                           hv212 == 9 ~ NA,
                           hv212 == NA ~ NA,
                           TRUE ~ 0),
         pisoNaturalRustico = case_when(hv213 == 11 | hv213 == 21 | hv213 == 34 ~ 1,
                                       hv213 == NA ~ NA,
                                       TRUE ~ 0),
         paredNaturalRustico = case_when(hv214 < 29 ~ 1,
                                         hv214 == NA ~ NA,
                                         TRUE ~ 0),
         techoNaturalRustico = case_when(hv215 < 29 ~ 1,
                                         hv215 == NA ~ NA,
                                         TRUE ~ 0),
         hacinamiento = case_when(hv216 == 0 ~ 0,
                                  TRUE ~ mieperho / hv216),
         mujerJH = case_when(hv219 == 2 ~ 1,
                             hv219 == NA ~ NA, 
                             TRUE ~ 0),
         edadJH = case_when(hv220 == 98 | hv220 == 99 ~ NA,
                            hv220 == NA ~ NA, 
                            TRUE ~ hv220),
         combustibleCocina = case_when(hv226 >= 6 & hv226 < 95 ~ 1 ,
                                       hv226 == NA ~ NA,
                                       TRUE ~ 0))
baseHogaresENDES <- baseHogaresENDES %>%
  mutate(piso = case_when(hv213 == 10 ~ 3,
                          hv213 == 20 ~ 2,
                          hv213 == 30 ~ 1,
                          TRUE ~ NA),
         pared = case_when(hv214 == 10 ~ 3,
                           hv214 == 20 ~ 2,
                           hv214 == 30 ~ 1,
                           TRUE ~ NA),
         techo = case_when(hv215 == 10 ~ 3,
                           hv215 == 20 ~ 2,
                           hv215 == 30 ~ 1,
                           TRUE ~ NA))

baseHogaresENDES <- baseHogaresENDES %>%
  mutate(nActivosPrioritarios = rowSums(select(., computadora, radio, lavadora, licuadora, microndas, refrigerador, tv)))

# Base personas----
basePersonasENDES <- rech4 %>% 
  rename(hvidx = idxh4) %>% 
  left_join(rech1, by = c("id1", "hhid", "hvidx")) %>% 
  rename(estadoCivil = hv115,
         estudia = hv110) %>% 
  mutate(mujer = case_when(hv104 == 2 ~ 1,
                           hv104 == NA ~ NA,
                           TRUE ~ 0),
         edad = case_when(hv105 == 98 | hv105 == 99 | hv105 == NA ~ NA,
                          TRUE ~ hv105),
         educ = case_when(hv109 == 8 | hv109 == NA ~ NA,
                          TRUE ~ hv109),
         anioEduc = case_when(hv108 == 97 | hv108 == 98 | hv108 == NA ~ NA,
                              TRUE ~ hv108),
         noEstudia = case_when(hv129 == 0 | hv129 == 4 | hv129 == 5 ~ 1,
                               hv129 == NA | hv129 == 9 ~ NA,
                               TRUE ~ 0),
         algunSeguro = case_when(sh11a ==1 | sh11b ==1 | sh11c ==1 | sh11d ==1 | sh11e ~ 1,
                                 TRUE ~ 0),
         pea = case_when(sh13 == 1 | sh13 == 2 | sh13 == 3 | sh13 == 4 | sh13 == 5 ~ 1,
                         TRUE ~ 0)
         )

# Base Mujeres ----

baseMujeresENDES <- rec91 %>%
  left_join(rec0111, by = c("id1", "caseid")) %>%
  left_join(re223132, by = c("id1", "caseid")) %>%
  left_join(rec42, by = c("id1", "caseid")) %>%
  left_join(re516171, by = c("id1", "caseid")) %>%
  left_join(rec84dv, by = c("id1", "caseid")) %>% 
  left_join(re758081, by = c("id1", "caseid"))

# Características de las mujeres
baseMujeresENDES <- baseMujeresENDES%>%
  mutate(nivEducMujer = case_when(v012 > 14 ~ v149,
                                  TRUE ~ NA),
         alfabetismoMujer = v155)

# Variables de violencia
## Violencia física ejercida por el esposo
baseMujeresENDES <- baseMujeresENDES %>%
  mutate(violenciaEsposoEmpujo = case_when(v044 == 1 & v502 > 0 ~ 0,
                                           d105a > 0 & d105a <= 3 ~ 1,
                                           TRUE ~ NA),
         violenciaEsposoAbofeteo = case_when(v044 == 1 & v502 > 0 ~ 0,
                                   d105b > 0 & d105b <= 3 ~ 1,
                                   TRUE ~ NA),
         violenciaEsposoGolpe = case_when(v044 == 1 & v502 > 0 ~ 0,
                                    d105c > 0 & d105c <= 3 ~ 1,
                                    TRUE ~ NA),
         violenciaEsposoPatada = case_when(v044 == 1 & v502 > 0 ~ 0,
                                   d105d > 0 & d105d <= 3 ~ 1,
                                   TRUE ~ NA),
         violenciaEsposoAhorco = case_when(v044 == 1 & v502 > 0 ~ 0,
                                    d105e > 0 & d105e <= 3 ~ 1,
                                    TRUE ~ NA),
         violenciaEsposoArma1 = case_when(v044 == 1 & v502 > 0 ~ 0,
                                      d105f > 0 & d105f <= 3 ~ 1,
                                      TRUE ~ NA),
         violenciaEsposoArma2 = case_when(v044 == 1 & v502 > 0 ~ 0,
                                      d105g > 0 & d105g <= 3 ~ 1,
                                      TRUE ~ NA),
         violenciaEsposoBrazo = case_when(v044 == 1 & v502 > 0 ~ 0,
                                    d105j > 0 & d105j <= 3 ~ 1,
                                    TRUE ~ NA))

## Violencia sexual ejercida por el esposo
baseMujeresENDES <- baseMujeresENDES %>%
  mutate(violenciaEsposoSex = case_when(v044 == 1 & v502 > 0 ~ 0,
                                           d105h > 0 & d105h <= 3 ~ 1,
                                           TRUE ~ NA),
         violenciaEsposoActSex = case_when(v044 == 1 & v502 > 0 ~ 0,
                                             d105i > 0 & d105i <= 3 ~ 1,
                                             TRUE ~ NA))

## Violencia psicológica y/o verbal ejercida por el esposo o compañero
baseMujeresENDES <- baseMujeresENDES %>%
  mutate(violenciaESposoHumil = case_when(v044 == 1 & v502 > 0 ~ 0,
                                    d103a > 0 & d103a <= 3 ~ 1,
                                    TRUE ~ NA),
         violenciaEsposoCelo = case_when(v044 == 1 & v502 > 0 ~ 0,
                                    d101a == 1 ~ 1,
                                    TRUE ~ NA),
         violenciaEsposoAcus = case_when(v044 == 1 & v502 > 0 ~ 0,
                                    d101b == 1 ~ 1,
                                    TRUE ~ NA),
         violenciaEsposoContacto = case_when(v044 == 1 & v502 > 0 ~ 0,
                                      d101c == 1 | d101d == 1 ~ 1,
                                      TRUE ~ NA),
         violenciaEsposoLugar = case_when(v044 == 1 & v502 > 0 ~ 0,
                                    d101e == 1 ~ 1,
                                    TRUE ~ NA),
         violenciaEsposoDinero = case_when(v044 == 1 & v502 > 0 ~ 0,
                                    d101f == 1 ~ 1,
                                    TRUE ~ NA),
         violenciaEsposoAmenaza1 = case_when(v044 == 1 & v502 > 0 ~ 0,
                                   d103b > 0 & d103b <= 3 ~ 1,
                                   TRUE ~ NA),
         violenciaEsposoAmenaza2 = case_when(v044 == 1 & v502 > 0 ~ 0,
                                   d103d > 0 & d103d <= 3 ~ 1,
                                   TRUE ~ NA))

## Cualquier tipo de violencia
baseMujeresENDES <- baseMujeresENDES %>%
  mutate(violenciaEsposo = case_when(v044 == 1 & v502 > 0 ~ 0,
                                    rowSums(select(., starts_with("violenciaEsposo"))) >0  ~ 1,
                                    TRUE ~ NA))

# Uso actual de métodos anticonceptivos
baseMujeresENDES <- baseMujeresENDES %>%
  mutate(usaAnticonceptivo = case_when(v313 == 0 ~ 0,
                                       (v313 == 1 | v313 == 2 | v313 == 3) ~ 1,
                                       TRUE ~ NA),
         tipoAnticonceptivo = case_when(v313 == 0 ~ 0,
                                        (v313 == 1 | v313 == 2) ~ 1,
                                        v313 == 3 ~ 2,
                                        TRUE ~ NA))

#Anemia en mujeres de 15 a 49 años
baseMujeresENDES <- baseMujeresENDES %>%
  mutate(anemiaMujer = case_when(v457 > 3 & v012 > 14 ~ 0,
                                 v457 < 4 & v012 > 14 ~ 1,
                                 TRUE ~ NA),
         nivelAnemiaMujer = case_when(v457 <=3 & v012 > 14 ~ v457,
                                      TRUE ~ NA),
         anemiaSevMujer = case_when(v457 == 1 & v012 > 14 ~ 1,
                                    v457 > 1 & v457 <9 & v012 > 14 ~ 0,
                                    TRUE ~ NA))

baseMujeresENDES <- baseMujeresENDES %>% 
  mutate(tuvoETS = case_when(v763a == 1 ~ 1,
                             v763a == NA ~ NA,
                             TRUE ~ 0))

mujeresHogares <- rech5 %>%
  mutate(caseid = paste(hhid, ha0, sep = " ")) %>% 
  select(id1, caseid, ha69) %>% 
  left_join(baseMujeresENDES, by = c("id1", "caseid")) %>% 
  mutate(pesoVIH = ha69)

# HA69 Peso del VIH (6 decimales)
# V761 En última relación sexual usó condón
# V761B Relación sexual con otro hombre (1) usó condón
# V761C Relación sexual con otro hombre (2) usó condón
# V763A Tuvo alguna ETS en los últimos 12 Meses

#Cobertura de seguro de salud para mujeres de 15 a 49 años
baseMujeresENDES <- baseMujeresENDES %>%
  mutate(seguroEssalud = case_when(v481e == 1 & v012 > 14 ~ 1,
                                 TRUE ~ 0),
         seguroSis = case_when(v481g == 1 & v012 > 14 ~ 1,
                                 TRUE ~ 0),
         seguroFap = case_when(v481f == 1 & v012 > 14 ~ 1,
                                 TRUE ~ 0),
         seguroEps = case_when(v481h == 1 & v012 > 14 ~ 1,
                                 TRUE ~ 0),
         seguroPriv = case_when(v481d ==1 & v012 > 14 ~ 1,
                                TRUE ~ 0),
         algunSeguro = case_when(rowSums(select(baseMujeresENDES, starts_with("v481"))) > 1 & v012 > 14 ~ 1,
                                 TRUE ~ 0))

#Llevamos los indicadores mujeres a nivel hogar
baseMujeres1ENDES <- baseMujeresENDES %>%
  mutate(hhid = substr(caseid, 1, 15)) %>%
  select(id1, hhid, violenciaEsposo, anemiaMujer, anemiaSevMujer, tuvoETS, algunSeguro) %>%
  group_by(id1, hhid, .groups = 'drop') %>%
  summarize(violenciaEsposoH = ifelse(all(is.na(violenciaEsposo)), NA, max(violenciaEsposo, na.rm = TRUE)),
            anemiaMujerH = sum(anemiaMujer, na.rm = TRUE),
            anemiaSevMujerH = sum(anemiaSevMujer, na.rm = TRUE),
            tuvoETSH = sum(tuvoETS, na.rm = TRUE),
            algunSeguroH = sum(algunSeguro, na.rm = TRUE)) %>%
  select(- .groups)

baseHogaresENDES <- baseHogaresENDES %>%
  left_join(baseMujeres1ENDES, by = c("id1", "hhid")) %>%
  mutate(anemiaMujerH = anemiaMujerH / nMujeres,
         anemiaSevMujerH = anemiaSevMujerH / nMujeres,
         tuvoETSH = tuvoETSH / nMujeres,
         algunSeguroH = algunSeguroH / nMujeres)

rm(baseMujeres1ENDES)

# Base Niños----
baseNinosENDES <- rech6 %>% 
  rename(hvidx = hc0) %>% 
  left_join(basePersonasENDES, by = c("id1", "hhid", "hvidx")) %>% 
  mutate(edad = hc1,
         peso = case_when(hc2 == NA ~ NA,
                          TRUE ~ hc2),
         talla = case_when(hc3 == NA ~ NA,
                           TRUE ~ hc3),
         tallaEdad = case_when(hc5 == NA ~ NA,
                               TRUE ~ hc5),
         pesoEdad = case_when(hc8 == NA ~ NA,
                              TRUE ~ hc8),
         pesoTalla = case_when(hc11 == NA ~ NA,
                               TRUE ~ hc11),
         mujer = case_when(hc27 == 2 ~ 1,
                           TRUE ~ hc27),
         anemiaNinos = case_when(hc57 < 4 & edad <= 60 ~ 1,
                                 hc57 == NA ~ NA,
                                 TRUE ~ 0),
         anemiaNivelNinos = case_when(hc57 == 1 ~ 4,
                                      hc57 == 2 ~ 3,
                                      hc57 == 3 ~ 2,
                                      hc57 == 4 ~ 1,
                                      hc57 == NA ~ NA,
                                      TRUE ~ 0),
         anemiaSevNinos = case_when(anemiaNivelNinos == 4 ~ 1,
                                    anemiaNivelNinos < 4 ~ 0,
                                    TRUE ~ NA),
         educMadre = case_when(hc61 == 8 | hc61 == 9 ~ NA,
                               hc61 == NA ~ NA,
                               TRUE ~ hc61),
         desnCrOms = case_when((hc70 < -200 & hv103 == 1) ~ 1,
                               (hc70 >= -200 & hc70 < 601 & hv103 == 1) ~ 0,
                               TRUE ~ NA),
         desnCrSev = case_when((hc70 < -300  & hv103 == 1) ~ 1,
                               (hc70 >= -300 & hc70 < 601 & hv103 == 1) ~ 0,
                               TRUE ~ NA))

baseNinos1ENDES <- baseNinosENDES %>%
  select(id1, hhid, mujer, desnCrOms, desnCrSev, anemiaNinos, anemiaSevNinos) %>%
  group_by(id1, hhid, .groups = 'drop')%>%
  summarize(nNinas = sum(mujer, na.rm = TRUE),
            desnCrOmsH = sum(desnCrOms, na.rm = TRUE),
            desnCrSevH = sum(desnCrSev, na.rm = TRUE),
            anemiaNinosH = sum(anemiaNinos, na.rm = TRUE),
            anemiaSevNinosH = sum(anemiaSevNinos, na.rm = TRUE)) %>%
  select(- .groups)

baseHogaresENDES <- baseHogaresENDES %>%
  left_join(baseNinos1ENDES, by = c("id1", "hhid")) %>%
  mutate(nNinas = nNinas / nNinos,
         desnCrOmsH = desnCrOmsH / nNinos,
         desnCrSevH = desnCrSevH / nNinos,
         anemiaNinosH = anemiaNinosH / nNinos,
         anemiaSevNinosH = anemiaSevNinosH / nNinos)

rm(baseNinos1ENDES)

#Desarrollo infantil - Comunicación efectiva
baseNinosAuxENDES <- dit %>%
  left_join(rec21, by = c("id1", "caseid", "bidx")) %>%
  left_join(rec0111, by = c("id1", "caseid")) %>%
  left_join(rec91, by = c("id1", "caseid")) %>%
  mutate(e3conv = case_when(qi478e3 == 1 ~ 1,
                            qi478e3 == 2 ~ 0,
                            TRUE ~ NA),
         e4conv = case_when(qi478e4 == 1 ~ 1,
                            qi478e4 == 2 ~ 0,
                            TRUE ~ NA),
         e5conv = case_when(qi478e5 == 1 ~ 1,
                            qi478e5 == 2 ~ 0,
                            TRUE ~ NA),
         e345 = case_when(qi478a == 0 & (qi478 >= 9 & qi478 <= 12) ~ e3conv + e4conv + e5conv,
                          TRUE ~ NA),
         r4_9_12m = case_when(e345 < 3 ~ 0,
                              e345 == 3 ~ 1,
                              TRUE ~ NA),
         f3conv = case_when(qi478f3 == 1 ~ 1,
                            qi478f3 == 2 ~ 0,
                            TRUE ~ NA),
         f4conv = case_when(qi478f4 == 1 ~ 1,
                            qi478f4 == 2 ~ 0,
                            TRUE ~ NA),
         f5conv = case_when(qi478f5 == 1 ~ 1,
                            qi478f5 == 2 ~ 0,
                            TRUE ~ NA),
         f345 = case_when(qi478a == 0 & (qi478 >= 13 & qi478 <= 18) ~ f3conv + f4conv + f5conv,
                          TRUE ~ NA),
         r4_13_18m = case_when(f345 < 3 ~ 0,
                               f345 == 3 ~ 1,
                               TRUE ~ NA),
         g1conv = case_when(qi478g1 == 1 ~ 1,
                            qi478g1 == 2 ~ 0,
                            TRUE ~ NA),
         g2aconv = case_when(qi478g2_a == 1 ~ 1,
                             qi478g2_a == 2 ~ 0,
                             TRUE ~ NA),
         g2bconv = case_when(qi478g2_b == 1 ~ 1,
                             qi478g2_b == 2 ~ 0,
                             TRUE ~ NA),
         g2cconv = case_when(qi478g2_c == 1 ~ 1,
                             qi478g2_c == 2 ~ 0,
                             TRUE ~ NA),
         g3conv = case_when(qi478g3 == 1 ~ 1,
                            qi478g3 == 2 ~ 0,
                            TRUE ~ NA),
         g2abc = (g2aconv + g2bconv + g2cconv) / 3,
         g345 = case_when(qi478a == 0 & (qi478 >= 19 & qi478 <= 23) ~ g1conv + g2abc + g3conv,
                          TRUE ~ NA),
         r4_19_23m = case_when(g345 < 3 ~ 0,
                               g345 == 3 ~ 1,
                               TRUE ~ NA),
         h1conv = case_when(qi478h1 == 1 ~ 1,
                            qi478h1 == 2 ~ 0,
                            TRUE ~ NA),
         h2conv = case_when(qi478h2 == 1 ~ 1,
                            qi478h2 == 2 ~ 0,
                            TRUE ~ NA),
         h3conv = case_when(qi478h3 == 1 ~ 1,
                            qi478h3 == 2 ~ 0,
                            TRUE ~ NA),
         h345 = case_when(qi478a == 0 & (qi478 >= 24 & qi478 <= 36) ~ h1conv + h2conv + h3conv,
                          TRUE ~ NA),
         r4_24_36m = case_when(h345 < 3 ~ 0,
                               h345 == 3 ~ 1,
                               TRUE ~ NA),
         desInfCom = rowSums(select(., starts_with("r4_")), na.rm = TRUE))

#Desarrollo infantil - Regulación de emociones
baseNinosAuxENDES <- baseNinosAuxENDES %>%
  mutate(desInfEmo = case_when(qi478a == 0 & (qi478 >= 24 & qi478 <= 36) ~ case_when(qi478h9 == 1 ~ 0,
                                                                                     qi478h9 == 2 ~ 1,
                                                                                     qi478h10 == 1 ~ 0,
                                                                                     qi478h10 == 2 ~ 1,
                                                                                     qi478h10 == 3 ~ 0,
                                                                                     qi478h11 == 1 ~ 0,
                                                                                     qi478h11 == 2 ~ 1,
                                                                                     TRUE ~ NA),
                               qi478a == 0 & (qi478 >= 37 & qi478 <= 54) ~ case_when(qi478i5 == 1 ~ 0,
                                                                                     qi478i5 == 2 ~ 1,
                                                                                     qi478i6 == 1 ~ 0,
                                                                                     qi478i6 == 2 ~ 1,
                                                                                     qi478i6 == 3 ~ 0,
                                                                                     qi478i7 == 1 ~ 0,
                                                                                     qi478i7 == 2 ~ 1,
                                                                                     TRUE ~ NA),
                               qi478a == 0 & (qi478 >= 55 & qi478 <= 71) ~ case_when(qi478j5 == 1 ~ 0,
                                                                                     qi478j5 == 2 ~ 1,
                                                                                     qi478j6 == 1 ~ 0,
                                                                                     qi478j6 == 2 ~ 1,
                                                                                     qi478j6 == 3 ~ 0,
                                                                                     qi478j7 == 1 ~ 0,
                                                                                     qi478j7 == 2 ~ 1,
                                                                                     TRUE ~ NA),
                               TRUE ~ NA))

#Desarrollo infantil - Juegan y dibujan
baseNinosAuxENDES <- baseNinosAuxENDES %>%
  mutate(h5conv = case_when(qi478h5 == 1 ~ 1,
                            qi478h5 == 2 ~ 0,
                            TRUE ~ NA),
         h6conv = case_when(qi478h6 == 1 ~ 1,
                            qi478h6 == 2 ~ 0,
                            TRUE ~ NA),
         h7conv = case_when(qi478h7 == 1 ~ 1,
                            qi478h7 == 2 ~ 0,
                            TRUE ~ NA),
         h567 = case_when(qi478a == 0 & (qi478 >= 24 & qi478 <= 36) ~ h5conv + h6conv + h7conv,
                          TRUE ~ NA),
         desInfJue = case_when(h567 %in% 0:2 ~ 0,
                        h567 == 3 ~ 1,
                        TRUE ~ NA))

baseNinos1ENDES <- baseNinosAuxENDES %>%
  mutate(hhid = substr(caseid, 1, 15)) %>%
  select(id1, hhid, desInfCom, desInfEmo, desInfJue) %>%
  group_by(id1, hhid, .groups = 'drop') %>%
  summarize(desInfComH = sum(desInfCom, na.rm = TRUE),
            desInfEmoH = sum(desInfEmo, na.rm = TRUE),
            desInfJueH = sum(desInfJue, na.rm = TRUE)) %>%
  select(- .groups)

baseHogaresENDES <- baseHogaresENDES %>%
  left_join(baseNinos1ENDES, by = c("id1", "hhid")) %>%
  mutate (desInfComH = desInfComH / nNinos,
          desInfEmoH = desInfEmoH / nNinos,
          desInfJueH = desInfJueH / nNinos)

rm(baseNinos1ENDES, baseNinosAuxENDES)

#Talla y peso al nacer
baseNinosAux1ENDES <- rec0111 %>%
  left_join(rec42, by = c("id1", "caseid")) %>%
  left_join(rec91, by = c("id1", "caseid"))

rec43$midx <- rec43$hidx

baseNinosAux2ENDES <- rec21 %>%
  rename(midx = bidx) %>%
  left_join(rec41, by = c("id1", "caseid", "midx")) %>%
  left_join(rec43, by = c("id1", "caseid", "midx"))

baseNinosAuxENDES <- baseNinosAux1ENDES %>%
  left_join(baseNinosAux2ENDES, by = c("id1", "caseid")) %>%
  mutate(edadM = v008 - b3,
         pesoNac = case_when(m19 < 2500 & v012 > 14 ~ 1,
                             (m19 > 2499 & m19 <= 8000) & v012 > 14 ~ 2,
                             m19 == 9996 & v012 > 14 ~ 3,
                             m19 == 9998 & v012 > 14 ~ 4,
                             TRUE ~ NA),
         tallaNac = case_when(m18 == 5 ~ 1,
                              m18 == 4 ~ 2,
                              m18 == 1 | m18 == 2 ~ 3,
                              m18 == 8 | m18 == 9 ~ 4,
                              TRUE ~ NA),
         pesoNacBajo = case_when(pesoNac == 1 ~ 1,
                                 pesoNac > 1 ~0,
                                 TRUE ~ NA),
         tallaNacBajo = case_when(tallaNac == 1 ~ 1,
                                  tallaNac > 1 ~ 0,
                                  TRUE ~ NA),
         ira0a59 = case_when((b5 == 1 & edadM < 60) ~ 0,
                             (b5 == 1 & edadM < 60 & h31b == 1) ~ 1,
                             TRUE ~ NA),
         eda0a59 = case_when((h11 == 0 | h11 == 8) & b5 == 1 & edadM < 60 & v012 > 14 ~ 0,
                             h11 == 2 & b5 == 1 & edadM < 60 & v012 > 14 ~ 1,
                             TRUE ~ NA))

rm(baseNinosAux1ENDES, baseNinosAux2ENDES)

baseNinos1ENDES <- baseNinosAuxENDES %>%
  mutate(hhid = substr(caseid, 1, 15)) %>%
  select(id1, hhid, pesoNacBajo, tallaNacBajo, ira0a59, eda0a59) %>%
  group_by(id1, hhid, .groups = 'drop') %>%
  summarize(pesoNacBajoH = sum(pesoNacBajo, na.rm = TRUE),
            tallaNacBajoH = sum(tallaNacBajo, na.rm = TRUE),
            ira0a59H = sum(ira0a59, na.rm = TRUE),
            eda0a59H = sum(eda0a59, na.rm = TRUE)) %>%
  select(- .groups)

baseHogaresENDES <- baseHogaresENDES %>%
  left_join(baseNinos1ENDES, by = c("id1", "hhid")) %>%
  mutate(pesoNacBajoH = pesoNacBajoH / nNinos,
         tallaNacBajoH = tallaNacBajoH / nNinos,
         ira0a59H = ira0a59H / nNinos,
         eda0a59H = eda0a59H / nNinos)

rm(baseNinos1ENDES)

setwd("/etc/data/base_trabajo")
write_dta(data = baseHogaresENDES, "baseHogaresENDES.dta")


# REC95 Salud niños <- id: id1 caseid idx95
# DIT Desarrollo Infantil Temprano en niñas y niños menores de 6 años <- id: id1 caseid bidx
# REC43 Salud (niños) <- id: id1 caseid hidx
# REC44 Talla y peso <- id: id1 caseid hwidx