# 0. Librerías y direcciones ----
dirEnaho <- "C:/Users/User/OneDrive - MIGRACIÓN VIDENZA/1. Proyectos/1. Proyectos actuales/GiZ Pobreza Urbana/1. Productos/0. Insumos/1. Bases de datos/ENAHO"
#dirEnaho <- "/etc/data/"
library(tidyverse)
library(haven)
library(dplyr)


# 1. Carga de bases de datos ----
setwd(dirEnaho)
baseHogares <- read_dta("base_trabajo/baseHogaresFinal.dta")
basePersonas <- read_dta("base_trabajo/basePersonasFinal.dta")

xtabs(data = baseHogares , formula = facpob07 ~ anio + pisoTierra)