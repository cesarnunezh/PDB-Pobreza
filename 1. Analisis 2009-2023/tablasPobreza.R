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
         primaria_c= case_when(nivEduc == 1  ~ 1,
                                  nivEduc == NA ~ NA,
                                  TRUE ~ 0),
         secundaria_c = case_when(nivEduc == 2 ~ 1,
                                    nivEduc == NA ~ NA,
                                    TRUE ~ 0),
         superior_c = case_when(nivEduc == 3  ~ 1,
                                  nivEduc == NA ~ NA
                                  TRUE ~ 0),
         castellano = case_when(leng ==1 ~ 1,
                                leng== NA ~ NA
                                TRUE ~ 0),
         lenguaNAt = case_when(leng ==2 ~ 1,
                              leng == NA ~ NA
                              TRUE ~ 0))

baseHogaresFiltrada <- baseHogares %>%
    mutate(primaria_incJH = case_when(nivEducJH ==1 ~ 1,
                                      niveEducJH == NA ~ NA,
                                      TRUE ~ 0),
           secundaria_incJH = case_when(niveEducJH == 2 ~ 1,
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
  baseHogares %>%
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
varJH <- c("mujerJH","jh65mas")#dummies
tablasJH <- lapply(varJH, tablaHogares)

# Características del individuo
varInd <- c("mujer", "empInf", "sinContrato", "indep")#dummies

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

nombres_hojas <- names(archivo)#nombre hojas actuales
print(nombres_hojas)

#nombres hojas
nuevos_nombres <- c("vivBajaCalidad", "vivInvasion", "vivCedida",
                    "pisoTierra", "pisoCemento", "techoDebil",
                    "paredLadrillo", "combustibleCocina","agua", "aguaPotable", "desague", 
                    "electricidad", "telCelu", "internet", "nActivos", "nActivosPrioritarios","nbis","mujerJH","jh65mas",
                    "mujer", "empInf", "sinContrato", "indep", "confFamiliares","segEssalud", "segPriv", "segEps", "segFfaa", "segSis", "segUniv", "segEsc", "segOtro", "algunSeg", "difSegSis",
                    "programasSociales") 

# Cambiar nombres de las hojas
for(i in seq_along(nuevos_nombres)) {
  names(archivo)[i] <- nuevos_nombres[i]
}

# Guardar el archivo de Excel con los nuevos nombres de las hojas
saveWorkbook(archivo, "tabla2.xlsx", overwrite = TRUE)