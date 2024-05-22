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
#baseHogares <- read_dta("baseHogaresFinal.dta")
basePersonas <- read_dta("basePersonasFinal.dta")

basePersonasFiltrada <- basePersonas %>% 
  filter((p204==1 & p205==2) | (p204==2 & p206==1)) %>%
  mutate(pobrezaExtrema = case_when(pobreza == 1 ~ 1,
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
#varNBI <- c("nbi1", "nbi2", "nbi3", "nbi4", "nbi5")

#tablasNBI <- lapply(varNBI, calcular_media_ponderada)

# Características del Jefe de Hogar
#varJH <- c("nivEducJH", "lengJH", "mujerJH", "hombreJH", "edadJH", "grupoEdadJH",
#"estadoCivilJH", "empInfJH", "sectorJH", "sectorJHCom", "sectorJHRest", "sectorJHHog", 
#"sinContratoJH", "indepJH", "tamaEmpJH", "anioEducJH", "jh65mas", "jh25menos", "origenJH")

#tablasJH <- lapply(varJH, calcular_media_ponderada)

# Características del individuo
varInd <- c("nivEduc", "leng", "mujer", "hombre", "edad", "grupoEdad",
            "estadoCivil", "empInf", "sector", "sinContrato", "indep",
            "tamaEmp", "anioEduc", "origen")

tablasInd <- lapply(varInd, tablaPersonas)

setwd(dirOutput)
write_xlsx(tablasViv, path = "tabla1.xlsx")
