#*******************************************************************************
# Proyecto: PDB - Pobreza
# Objetivo: Analizar las características de la población pobre y no pobre 
# Autores: CN, JP
#*******************************************************************************

# 0. Funciones -----------------------------------------------------------------
xtile <- function(x, n = NULL, probs = NULL, cutpoints = NULL, w = NULL){
  if (!is.null(n)){
    probs <-  seq(1/n, 1-1/n, length = n -1)
  }
  if (!is.null(probs)){
    cutpoints <-  pctile(x, probs, w = w, na.rm = TRUE)
  }
  # In stata xtile is (-infty, x_p1], (xp1, xpe], like .bincode 
  .bincode(x, c(-Inf, cutpoints , +Inf) , include.lowest = TRUE)
}

pctile <- function(x, probs = c(0.25, 0.5, 0.75), w = NULL, na.rm = FALSE){
  if (is.null(w)){
    quantile(x = x, type = 2, probs = probs, na.rm = na.rm)
  } else{
    if (anyNA(x) | anyNA(w)) {
      if (na.rm) {
        na <- is.na(x) | is.na(w)
        x <- x[!na]
        w <- w[!na]
      }
      else{
        stop("Missing values not allowed when na.rm is FALSE", call. = FALSE)
      } 
    }
    # Ensure x and w in ascending order of x
    order <- order(x)
    cumsum <- cumsum(w[order])
    n <- cumsum[length(cumsum)]
    # follow definition of quantile 2 
    index <- n * probs
    j <- floor(index)
    low <- x[order[pmin(length(x),   .bincode(j, c(-Inf, cumsum)))]]
    high <- x[order[pmin(length(x),   .bincode(j + 1, c(-Inf, cumsum)))]]
    ifelse(j == index, 0.5 * low + 0.5 * high, high)
  }
}

# 1. Librerías y direcciones ---------------------------------------------------
dirEnaho <- "C:/Users/User/OneDrive - MIGRACIÓN VIDENZA/1. Proyectos/1. Proyectos actuales/23. Artículos PDB/2. PDB - Pobreza Urbana/2. Data/1. Bases/2. ENAHO Anual"
dirOutput <- "C:/Users/User/OneDrive - MIGRACIÓN VIDENZA/1. Proyectos/1. Proyectos actuales/23. Artículos PDB/2. PDB - Pobreza Urbana/2. Data/2. Output"
#dirEnaho <- "/etc/data/"
library(tidyverse)
library(haven)
library(dplyr)
library(writexl)
library(openxlsx)

# 2. Carga de bases de datos ---------------------------------------------------
setwd(dirEnaho)
baseHogares <- read_dta("baseHogaresFinal.dta")
basePersonas <- read_dta("basePersonasFinal.dta")

basePersonasFiltrada <- basePersonas %>% 
  filter((p204==1 & p205==2) | (p204==2 & p206==1)) %>%
  filter(area == 1) %>% 
  mutate(pobrezaExtrema = case_when(pobreza == 1 ~ 1,
                                    TRUE ~ 0),
         primaria_c= case_when(p301a >= 4  ~ 1,
                               p301a == NA ~ NA,
                                  TRUE ~ 0),
         secundaria_c = case_when(p301a >= 6 & p301a != 12 ~ 1,
                                  p301a == NA ~ NA,
                                  TRUE ~ 0),
         superior_c = case_when(p301a == 10 | p301a == 8 | p301a == 11 ~ 1,
                                p301a == NA ~ NA,
                                TRUE ~ 0),
         castellano = case_when(leng == 1 ~ 1,
                                leng== NA ~ NA,
                                TRUE ~ 0),
         lenguaNAt = case_when(leng == 2 ~ 1,
                               leng == NA ~ NA,
                               TRUE ~ 0),
         subempleo = case_when(subempIng ==1 | subempHrs== 1 ~ 1,
                                subempIng == NA ~ NA,
                                TRUE ~ 0),
         CuentaNotiene = case_when(p558e1_6 == 1 ~ 1,
                                   p558e1_6 == NA ~ NA,
                                   TRUE ~ 0),
         educPadre_pri = case_when(p45_1 >= 3 ~ 1,
                                   p45_1 == NA ~ NA,
                                   TRUE ~ 0),
         educMadre_pri = case_when(p45_2 >= 3 ~ 1,
                                   p45_2 == NA ~ NA,
                                   TRUE ~ 0),
         educPadre_sec = case_when(p45_1 >= 5 ~ 1,
                                   p45_1 == NA ~ NA,
                                   TRUE ~ 0),
         educMadre_sec = case_when(p45_2 >= 5 ~ 1,
                                   p45_2 == NA ~ NA,
                                   TRUE ~ 0),
         educPadre_sup = case_when(p45_1 >= 9 | p45_1 == 7 ~ 1,
                                   p45_1 == NA ~ NA,
                                   TRUE ~ 0),
         educMadre_sup = case_when(p45_2 >= 9 | p45_2 == 7 ~ 1,
                                   p45_2 == NA ~ NA,
                                   TRUE ~ 0),
         algunSeg = case_when(rowSums(select(., starts_with("seg"))) > 0 ~ 1,
                              TRUE ~ 0))

baseHogaresFiltrada <- baseHogares %>%
  filter(area == 1) %>% 
  mutate(pobrezaExtrema = case_when(pobreza == 1 ~ 1,
                                    TRUE ~ 0),
         nbis = case_when(nbi1 == 1 | nbi2 == 1 | nbi3 == 1 | nbi4 ==1 | nbi5 ==1 ~ 1,
                          nbi1 == NA & nbi2 == NA & nbi3 == NA & nbi4 ==NA & nbi5 ==NA ~ 1,
                          TRUE ~ 0),
         hogar_Juntos = case_when(p710_04 == 1 ~ 1,
                                  p710_04 == NA ~ NA,
                                  TRUE ~ 0),
         hogarCunaMas_d = case_when(p710_01 == 1 ~ 1,
                                    p710_01 == NA ~ 1,
                                    TRUE ~ 0),
         hogarCunaMas_a = case_when(p710_02 == 1 ~ 1,
                                    p710_02 == NA ~ NA,
                                    TRUE ~ 0), 
         hogarProgSocial = case_when(p701_01 == 1 | p701_02 == 1 |p701_03 == 1 | p701_04 == 1 | p710_05 == 1 ~ 1,
                                     p701_01 == NA & p701_02 == NA & p701_03 == NA & p701_04 == NA & p710_05 == NA ~ NA,
                                     TRUE ~ 0), #Vaso de leche, comedor, qaliwarma, pension 65
         hogarProgSocial_joven = case_when(p710_07 == 1 | p710_08 == 1  |p710_09 == 1 | p710_10 == 1~ 1,
                                           p710_07 == NA & p710_08 == NA  & p710_09 == NA & p710_10 == NA ~ NA,
                                           TRUE ~ 0), #JovProd TrabajaPeru ImpulsaPeru Beca18 
         internet= case_when(anio == 2023 & ( p114b1 ==1 | p114b2 ==1) ~ 1,
                             internet == 1 & anio != 2023 ~ 1,
                             internet == NA ~ NA,
                             TRUE ~0))

baseHogaresFiltrada <- baseHogaresFiltrada %>%
  mutate(gashog2dPCap = gashog2d / mieperho,
         gastoPCentil = xtile(gashog2dPCap, 100))

# 3. Tabulaciones de pobreza por año -------------------------------------------
tabla1 <- basePersonasFiltrada %>% 
  group_by(anio) %>%
  summarize(pobreza = weighted.mean(x = pobre, w = facpob07, na.rm = TRUE),
            pobrezaExtrema = weighted.mean(x = pobrezaExtrema, w = facpob07, na.rm = TRUE))

# 4. Caracterización de población pobre y no pobre por año ---------------------

## 4.1. A nivel de personas ----------------------------------------------------

tablaPersonas <- function(variable) {
  basePersonasFiltrada %>%
    group_by(anio, pobre) %>%
    summarize_at(vars({{variable}}), ~ weighted.mean(., w = facpob07, na.rm = TRUE)) %>% 
    pivot_wider(values_from = {{variable}}, names_from = pobre) %>% 
    rename(anio = 1,
           nopobre = 2,
           pobre = 3)
}

varCatP <- c("abandono", "agua", "aguaPotable", "auto", "casado", "cocina_kerosene", 
             "combustibleCocina", "computadora", "confDivision", "confianzaMD", "confianzaMP", 
             "confPension", "confTenencia", "confViolacion", "confViolencia", "confVisitas", 
             "conviviente", "cuentaAhorro", "cuentaCorr", "cuentaNoTiene", "cuentaPlazoFijo", 
             "desague", "desemp", "discapacidad", "discapEntender", "discapHabla", "discapMotora", 
             "discapOir", "discapRelacion", "discapVista", "discrCostumbres", "discrDiscapacidad", 
             "discrEdad", "discrEduc", "discriminacion", "discrIngresos", "discrLengua", 
             "discrOrientacion", "discrOrigen", "discrRaza", "discrSexo", "discrVest", "educMadre", 
             "educPadre", "educSec", "educSup", "electricidad", "empGrande", "empInf", "empMed", 
             "estadoCivil", "estudiaMD", "grupoEdad", "hogarConHijo", 
             "hogarJuntos", "hogarMonoParent", "hogarPension65", "hogarQaliWarma", "hogarUniFem", 
             "hogarUniMayor", "hogarVasoLeche", "hombre", "inactivo", 
             "indepAmb", "indCompVent", "indep", 
             "indepDomCli", "indepNoReg", "indepOtros", "indepPerJur", "indepPerNat", "indepPuestoFijoCa", 
             "indepPuestoImpCa", "indepPuestoImpMerc", "indepTaller", "indepVeh", "indepViv", "indepVivExc", 
             "indigena", "indProd", "indepPuestoFijoMerc", "indServ", "internet", "lavadora", 
             "leng", "licuadora", "mestizo", "migrante", "migrante5anios", "motIndep", "motIng", 
             "motNecEcon", "motNoTrab", "motOtro", "motTradFam", "mujer", "nivEduc", "origen", "paredLadrillo", 
             "pisoCemento", "pisoTierra", "pisoTierraCemento", "riesgoDesastre", 
             "riesgoEmpleo", "riesgoEnfermedad", "riesgoQuiebra", "saludMD", "sector", "sectorCom", "sectorHog", 
             "sectorRest", "segEps", "segEsc", "segEssalud", "segFfaa", "segOtro", "segPriv", "segSis", "segUniv", 
             "sinContrato", "subempHrs", "subempIng", "tamaEmp", "techoDebil", "trabajaMD", 
             "tresServicios", "usoColectivo", "usoCombi", "usoDiarioColectivo", "usoDiarioCombi",
             "usoDiarioMototaxi", "usoDiarioMicrobus", "usoDiarioOmnibus", "usoDiarioTaxi", "usoMicrobus", 
             "usoMototaxi", "usoOmnibus", "usoTaxi", "victimaDelito", "vivBajaCalidad", "vivCedida", "vivInvasion",
             "equipo_sonido", "microondas", "plancha", "refrigerador", "telCelu", "tv_color",
             "aguaHoras", "nActivos", "nActivosPrioritarios", "confFamiliares", "mieperho", "pctPerceptores", 
             "ratioDependencia")

# Variables de hogar y vivienda
tablas <- lapply(varCatP, tablaPersonas)

setwd(dirOutput)
write_xlsx(tablas, path = "tabla1.xlsx")

setwd(dirOutput)
archivo <- loadWorkbook("tabla1.xlsx")

nombres_hojas <- names(archivo) #nombre hojas actuales
print(nombres_hojas)

#nombres hojas
nuevos_nombres <- varCatP

#guardar nombres en las hojas y exportar
for(i in seq_along(nuevos_nombres)) {
  names(archivo)[i] <- nuevos_nombres[i]
}
saveWorkbook(archivo, "tabla1.xlsx", overwrite = TRUE)

# Gráficos 
# Series anuales
for(i in seq_along(tablas)){
  graph <- tablas[[i]] %>% 
    filter(pobre != 0 & nopobre !=0) %>% 
    pivot_longer(cols = c(nopobre, pobre), names_to = "estado", values_to = "valor") %>% 
    select(c(anio, estado, valor)) %>% 
    ggplot() +
    aes(x = anio, y = valor, color = estado) +
    stat_summary(aes(y=valor), fun ="mean", geom="point") +
    stat_summary(aes(y=valor), fun ="mean", geom="line") +
    labs(x = "Año",
         y = varCatP[i]) +
    labs(title = paste("Pobreza urbana según",varCatP[i], ", 2007-2023"), 
         color = "Condición de pobreza")
  
  setwd(dirOutput)
  ggsave(paste("graficosUrb/Pob/fig_",varCatP[i],".png",sep = ""))
}

for(j in 1:length(varCatP)){
  # Filter data for poor households and create a 'periodo' variable
  filtered_data <-basePersonasFiltrada %>% 
    mutate(periodo = case_when(anio >= 2021 ~ "Entre 2021 y 2023",
                               anio < 2020 & anio >= 2017 ~ "Entre 2017 y 2019",
                               TRUE ~ "OMITIR" ),
           pobre = case_when(pobre == 1 ~ "Pobre",
                             pobre != 1 ~ "No pobre")) %>% 
    filter(periodo != "OMITIR") %>%
    group_by(periodo, pobre) %>%
    summarize(weighted_mean = weighted.mean(get(varCatP[j]), w = facpob07, na.rm = TRUE), .groups = 'drop') 
  
  # Create histogram with ggplot2
  p <- filtered_data %>% 
    ggplot() +
    aes( y = weighted_mean, x = pobre, fill = pobre) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ periodo) +
    ggtitle(paste("Porcentaje de ", varCatP[j], " según periodo y condición de pobreza - Urbano")) +
    theme(legend.position = "none") 
  
  # Save the plot as a PNG file
  ggsave(filename = paste0("graficosUrb/Pob/bar_", varCatP[j], ".png"), plot = p)
}


## 4.2. A nivel de hogar -------------------------------------------------------

tablaHogares <- function(variable) {
  baseHogaresFiltrada %>%
    group_by(anio, pobre) %>%
    summarize_at(vars({{variable}}), ~ weighted.mean(., w = factor07, na.rm = TRUE)) %>% 
    pivot_wider(values_from = {{variable}}, names_from = pobre) %>% 
    rename(anio = 1,
           nopobre = 2,
           pobre = 3)
}

varCat <- c("vivBajaCalidad", "vivInvasion", "vivCedida", "pisoTierra", "pisoCemento", "pisoTierraCemento", "techoDebil", "paredLadrillo",
            "combustibleCocina", "agua", "aguaPotable", "aguaSegura", "agua24Horas", "aguaCisterna", "desague", "electricidad", "tresServicios", "telCelu", "internet",
            "nbi1", "nbi2", "nbi3", "nbi4", "nbi5", "hogarVasoLeche", "hogarComedor", "hogarQaliWarma", "hogarCunaMas", "hogarJuntos", "hogarPension65", "hogarJovProd", "hogarTrabajaPeru", "hogarImpulsaPeru", "hogarBeca18",
            "hogarConHijo", "hogarMonoParent", "hogarNuclear", "hogarExtendido", "hogarUniFem", "hogarUniMayor",
            "mujerJH", "mestizoJH", "indigenaJH", "hombreJH", "jovenJH", "adultoJovJH", "adultoJH", "adultoMayorJH", "convivienteJH", "casadoJH",
            "empInfJH", "sectorJHCom", "sectorJHRest", "sinContratoJH", "indepJH", "empPeqJH", "empMedJH", "empGrandeJH",
            "jh65mas", "jh25menos", "migranteJH", "migrante5aniosJH", "educSecJH", "educSupJH")

for(j in 1:length(varCat)){
  # Filter data for poor households and create a 'periodo' variable
  filtered_data <-baseHogaresFiltrada %>% 
    mutate(periodo = case_when(anio >= 2021 ~ "Entre 2021 y 2023",
                               anio < 2020 & anio >= 2017 ~ "Entre 2017 y 2019",
                               TRUE ~ "OMITIR" ),
           pobre = case_when(pobre == 1 ~ "Pobre",
                             pobre != 1 ~ "No pobre")) %>% 
    filter(periodo != "OMITIR") %>%
    group_by(periodo, pobre) %>%
    summarize(weighted_mean = weighted.mean(get(varCat[j]), w = factor07, na.rm = TRUE), .groups = 'drop') 
  
  # Create histogram with ggplot2
  p <- filtered_data %>% 
    ggplot() +
    aes( y = weighted_mean, x = pobre, fill = pobre) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ periodo) +
    ggtitle(paste("Porcentaje de ", varCat[j], " según periodo y condición de pobreza - Urbano")) +
    theme(legend.position = "none") 
  
  # Save the plot as a PNG file
  ggsave(filename = paste0("graficosUrb/Hog/bar_", varCat[j], ".png"), plot = p)
}
