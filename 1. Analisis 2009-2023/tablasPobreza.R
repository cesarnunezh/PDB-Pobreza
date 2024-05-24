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
         #internet= case_when(anio == 2023 & p1144 == 1 & p1144b1 ==1 | p1144b2 ==1 ~ 1,
                             #internet ==NA ~ NA,
                             #TRUE ~0),
         subempleo = case_when(subempIng ==1 | subempHrs== 1 ~ 1,
                                subempIng == NA ~ NA,
                                TRUE ~ 0),
         CuentaNotiene = case_when(p558e1_6 == 1 ~ 1,
                                   p558e1_6 == NA ~ NA,
                                   TRUE ~ 0))
         #nbis = case_when(nbi1 == 1 | nbi2 == 1 | nbi3 == 1 | nbi4 ==1 |nbi5 ==1 ~ 1,
                          #TRUE ~ 0))

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
varJH <- c("mujerJH","jh65mas")
tablasJH <- lapply(varJH, tablaHogares)

# Características del individuo y empleo
varInd <- c("mujer", "empInf", "sinContrato", "indep")

tablasInd <- lapply(varInd, tablaHogares)

# Características de empleo
#varEmp <- c("subempleo")

#tablasEmp <- lapply(varEmp, tablaPersonas)

# Características Inclusión financiera
varCuenta<- c("CuentaNotiene")

tablasCuenta<- lapply(varCuenta, tablaPersonas)


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
write_xlsx(c(tablasViv,tablasServicios,tablasActivos,tablasNBI,tablasJH,tablasInd,tablasCuenta, tablasRI,tablasSegSocial,tablasProgSociales), path = "tabla1.xlsx")

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
                    "indep","CuentaNotiene","confFamiliares",
                    "segEssalud", "segPriv", "segEps", "segFfaa", 
                    "segSis", "segUniv", "segEsc", "segOtro", 
                    "algunSeg", "difSegSis","programasSociales") 

#guardar nombres en las hojas y exportar
for(i in seq_along(nuevos_nombres)) {
  names(archivo)[i] <- nuevos_nombres[i]
}
saveWorkbook(archivo, "tabla2.xlsx", overwrite = TRUE)
#transferencias, en especies, servicios (cuna más - diurno de acompañamiento familiar)


#graficos 
graph1 <- tablasViv[4][[1]] %>% 
  ggplot() +
  aes(x = anio, y = pobre) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=nopobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según estrato geográfico, 2007-2022", 
       color = "Estrato")

ggsave(filename = "graficos/g_Estrato.png")

