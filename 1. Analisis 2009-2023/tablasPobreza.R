# 0. Librerías y direcciones ----
dirEnaho <- "C:/Users/User/OneDrive - MIGRACIÓN VIDENZA/1. Proyectos/1. Proyectos actuales/23. Artículos PDB/2. PDB - Pobreza Urbana/2. Data/1. Bases/2. ENAHO Anual"
dirOutput <- "C:/Users/User/OneDrive - MIGRACIÓN VIDENZA/1. Proyectos/1. Proyectos actuales/23. Artículos PDB/2. PDB - Pobreza Urbana/2. Data/2. Output"
#dirEnaho <- "/etc/data/"
library(tidyverse)
library(haven)
library(dplyr)
library(writexl)


# 1. Carga de bases de datos ----
setwd(dirEnaho)
baseHogares <- read_dta("baseHogaresFinal.dta")
basePersonas <- read_dta("basePersonasFinal.dta")

basePersonasFiltrada <- basePersonas %>% 
  filter((p204==1 & p205==2) | (p204==2 & p206==1)) %>%
  mutate(pobrezaExtrema = case_when(pobreza == 1 ~ 1,
                                    TRUE ~ 0),
         primaria_c= case_when(p301a == 4  ~ 1,
                               p301a == NA ~ NA,
                                  TRUE ~ 0),
         secundaria_c = case_when(p301a == 6 ~ 1,
                                  p301a == NA ~ NA,
                                  TRUE ~ 0),
         superior_uni_c = case_when(p301a == 10  ~ 1,
                                p301a == NA ~ NA,
                                TRUE ~ 0),
         castellano = case_when(leng == 1 ~ 1,
                                leng== NA ~ NA,
                                TRUE ~ 0),
         lenguaNAt = case_when(leng == 2 ~ 1,
                               leng == NA ~ NA,
                               TRUE ~ 0),
         internet_2023= case_when(anio == 2023 | p1144 == 1 & p1144b1 ==1 | p1144b2 ==1 )
         subempleo = case_when(subempIng ==1 ~ 1,
                                subempIng == NA ~ NA
                                TRUE ~ 0))

baseHogaresFiltrada <- baseHogares %>%
    mutate(pobrezaExtrema = case_when(pobreza == 1 ~ 1,
                                      TRUE ~ 0),
           primaria_cJH = case_when(nivEducJH ==2~ 1,
                                      niveEducJH == NA ~ NA,
                                      TRUE ~ 0),
           secundaria_cJH = case_when(niveEducJH == 3 ~ 1,
                                        niveEduchJH == NA ~ NA,
                                        TRUE ~ 0))


# 2. Tabulaciones de pobreza por año
tabla1 <- basePersonasFiltrada %>% 
  group_by(anio) %>%
  summarize(pobreza = weighted.mean(x = pobre, w = facpob07, na.rm = TRUE),
            pobrezaExtrema = weighted.mean(x = pobrezaExtrema, w = facpob07, na.rm = TRUE))

# Características de la vivienda
varViv <- c("vivBajaCalidad", "vivInvasion", "vivCedida",
            "pisoTierra", "pisoCemento", "techoDebil",
            "paredLadrillo", "combustibleCocina")

tablaPersonas <- function(variable) {
  basePersonasFiltrada %>%
    group_by(anio, pobre) %>%
    summarize_at(vars({{variable}}), ~ weighted.mean(., w = facpob07, na.rm = TRUE)) %>% 
    pivot_wider(values_from = {{variable}}, names_from = pobre) %>% 
    rename(anio = 1,
           nopobre = 2,
           pobre = 3)
  }

tablaHogares <- function(variable) {
  baseHogaresFiltrada %>%
    group_by(anio, pobre) %>%
    summarize_at(vars({{variable}}), ~ weighted.mean(., w = factor07, na.rm = TRUE)) %>% 
    pivot_wider(values_from = {{variable}}, names_from = pobre) %>% 
    rename(anio = 1,
           nopobre = 2,
           pobre = 3)
}

tablasViv <- lapply(varViv, tablaPersonas)

# Servicios básicos en la vivienda
varServicios <- c("agua", "aguaPotable", "desague", 
                  "electricidad", "telCelu", "internet")

tablasServicios <- lapply(varServicios, tablaPersonas)

# Número de activos
varActivos <- c("nActivos", "nActivosPrioritarios")

tablasActivos <- lapply(varActivos, tablaPersonas)

# Necesidades básicas insatisfechas 
varNBI <- c("nbis")

tablasNBI <- lapply(varNBI, tablaHogares) 

# Características del Jefe de Hogar
varJH <- c("mujerJH","jh65mas")
tablasJH <- lapply(varJH, tablaHogares)

# Características del individuo y empleo
varInd <- c("mujer", "empInf", "sinContrato", "indep", "submpleo")

tablasInd <- lapply(varInd, tablaHogares)

# Relaciones Interfamiliares inestables
varRI <- c("confFamiliares")

tablasRI <- lapply(varInd, tablaPersonas)

# Composición del hogar 
#varHog <- c ("tipoHogar")  
#tablasHog <- lapply(varInd, tablaHogares)

#Seguridad Social
varSegSocial <- c("segEssalud", "segPriv", "segEps", "segFfaa", "segSis", "segUniv", "segEsc", "segOtro", "algunSeg", "difSegSis")

tablasSegSocial <- lapply(varSegSocial, tablaPersonas)

#Programas sociales
varProgSociales <- c("programasSociales")

tablasProgSociales <- lapply(varProgSociales, tablaHogares)

setwd(dirOutput)
write_xlsx(c(tablasViv,tablasServicios,tablasActivos,tablasNBI,tablasJH,tablasInd,tablasRI,tablasSegSocial,tablasProgSociales), path = "tabla1.xlsx")

########
setwd(dirOutput)
archivo <- loadWorkbook("tabla1.xlsx")

nombres_hojas <- names(archivo) #nombre hojas actuales
print(nombres_hojas)

#nombres hojas
nuevos_nombres <- c("vivBajaCalidad", "vivInvasion", "vivCedida",
                    "pisoTierra", "pisoCemento", "techoDebil",
                    "paredLadrillo", "combustibleCocina","agua", 
                    "aguaPotable", "desague", "electricidad", 
                    "telCelu", "internet", "nActivos",
                    "nActivosPrioritarios","nbis","mujerJH","jh65mas",
                    "mujer", "empInf", "sinContrato", 
                    "indep", "subempleo","confFamiliares",
                    "segEssalud", "segPriv", "segEps", "segFfaa", 
                    "segSis", "segUniv", "segEsc", "segOtro", 
                    "algunSeg", "difSegSis","programasSociales") 

#guardar nombres en las hojas y exportar
for(i in seq_along(nuevos_nombres)) {
  names(archivo)[i] <- nuevos_nombres[i]
}
saveWorkbook(archivo, "tabla2.xlsx", overwrite = TRUE)