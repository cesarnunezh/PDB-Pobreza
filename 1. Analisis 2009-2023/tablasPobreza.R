# 0. Librerías y direcciones ----
dirEnaho <- "C:/Users/User/OneDrive - MIGRACIÓN VIDENZA/1. Proyectos/1. Proyectos actuales/GiZ Pobreza Urbana/1. Productos/0. Insumos/1. Bases de datos/ENAHO"
dirOutput <- "C:/Users/User/OneDrive - MIGRACIÓN VIDENZA/1. Proyectos/1. Proyectos actuales/23. Artículos PDB/2. PDB - Pobreza Urbana/2. Data/2. Output"
#dirEnaho <- "/etc/data/"
library(tidyverse)
library(haven)
library(dplyr)
library(writexl)


# 1. Carga de bases de datos ----
setwd(dirEnaho)
baseHogares <- read_dta("base_trabajo/baseHogaresFinal.dta")
basePersonas <- read_dta("base_trabajo/basePersonasFinal.dta")

basePersonasFiltrada <- basePersonas %>% 
  filter((p204==1 & p205==2) | (p204==2 & p206==1)) 

# 2. Tabulaciones de pobreza por año
tablaPobreza <- xtabs(data = basePersonasFiltrada, formula = facpob07 ~ anio + pobrezav) %>% 
  as.data.frame() %>% 
  pivot_wider(id_cols = anio, names_from = pobrezav, values_from = Freq) %>% 
  rename(anio = 1,
         pobreExtremo = 2,
         pobreNoExtremo = 3,
         noPobreVulnerable = 4,
         noPobreNoVulnerable = 5) %>% 
  transmute(anio = anio,
            poblacion = pobreExtremo + pobreNoExtremo + noPobreVulnerable + noPobreNoVulnerable,
            pobreza = (pobreExtremo + pobreNoExtremo)/poblacion ,
            pobrezaExtrema = pobreExtremo/poblacion, 
            vulnerabilidad = noPobreVulnerable/poblacion)

setwd(dirOutput)
write_xlsx(table_df, path = "tabla1.xlsx")
