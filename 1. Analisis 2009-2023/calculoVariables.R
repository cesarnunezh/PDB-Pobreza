################################################################################
# Objetivo: Cálculo de indicadores
# Proyecto:  GiZ Pobreza Urbana
# 
# Estructura:
# 0. Librerías
# 1. Unión de bases
# 2. Creación de variables de interés
################################################################################

# 0. Librerías y direcciones ----
dirEnaho <- "C:/Users/User/OneDrive - MIGRACIÓN VIDENZA/1. Proyectos/1. Proyectos actuales/23. Artículos PDB/2. PDB - Pobreza Urbana/2. Data/1. Bases/2. ENAHO Anual"
#dirEnaho <- "/etc/data/"
library(tidyverse)
library(haven)
library(dplyr)
library(mapsPERU)
library(survey)
library(sf)
library(gganimate)
library(writexl)
library(openxlsx)

# 1. Carga de bases de datos ----
setwd(dirEnaho)
baseHogares <- read_dta("baseHogaresFinal.dta")
basePersonas <- read_dta("basePersonasFinal.dta")

# Generamos una base de datos filtrada a nivel de personas 
basePersonasFilter <- basePersonas %>% 
  filter((p204==1 & p205==2) | (p204==2 & p206==1)) %>% 
  mutate(subemp = case_when(subempHrs ==1 | subempIng ==1 ~ 1,
                            ocu500 == 1 & subempHrs == 0 & subempIng ==0 ~ 0,
                            TRUE ~ NA),
         grupoEdad2 = case_when(p208a < 15 ~ 1,
                                p208a >= 15 & p208a < 30 ~ 2,
                                p208a >= 30 & p208a < 45 ~ 3,
                                p208a >= 45 & p208a < 65 ~ 4,
                                p208a >= 65 ~ 5,
                                TRUE ~ NA),
         grupoEdadPNDIS = case_when(p208a < 6 ~ 1,
                                p208a >= 6 & p208a < 18 ~ 2,
                                p208a >= 18 & p208a < 59 ~ 3,
                                p208a >= 60  ~ 4,
                                TRUE ~ NA),
         regionNatural = case_when(dominio <= 3 ~ 1,
                                   dominio >3 & dominio <=6 ~ 2,
                                   dominio == 7 ~ 3,
                                   dominio == 8 ~ 4,
                                   TRUE ~ NA),
         limaMet = case_when(dominio == 8 ~ 1,
                          TRUE ~ 0),
         ciudad = case_when(provubigeo == 1501 ~ "Lima Metropolitana",
                            provubigeo == 701 ~ "Callao",
                            provubigeo == 1401 ~ "Chiclayo",
                            provubigeo == 2501 ~ "Pucallpa",
                            provubigeo == 401 ~ "Arequipa",
                            provubigeo == 1601 ~ "Iquitos",
                            provubigeo == 2301 ~ "Tacna",
                            provubigeo == 1301 ~ "Trujillo",
                            provubigeo == 2001 ~ "Piura",
                            provubigeo == 1101 ~ "Ica",
                            provubigeo == 1201 ~ "Huancayo",
                            TRUE ~ "Otros"),
         hogarMonoParent2 = case_when(hogarMonoParent == 1 ~ "Hogar monoparental",
                                      hogarMonoParent == 0 & hogarConHijo == 1 ~ "Hogar no monoparental con hijo",
                                      TRUE ~ NA),
         discapacidad2 = case_when(rowSums(select(., matches("discap")), na.rm = TRUE) > 0 ~ 1,
                                   rowSums(select(., matches("discap")), na.rm = TRUE) == 0 ~ 0,
                                   TRUE ~ NA),
         personaDep = case_when(p208a < 14 | p208a > 64 ~ 1,
                                discapacidad == 1 ~ 1,
                                p401 == 1 ~ 1,
                                TRUE ~ 0),
         adultoMayor = case_when(p208a > 64 ~ 1,
                                 TRUE ~ 0),
         adultoMayorJH = case_when(p208a > 64 & p203 == 1 ~ 1,
                                   TRUE ~ 0),
         tipoHacinamOMS = case_when(hacinam < 2.5 ~ "Sin hacinamiento",
                                    hacinam >=2.5 & hacinam < 5 ~ "Hacinamiento medio",
                                    hacinam <=5 ~ "Hacinamiento crítico",
                                    TRUE ~ NA),
         tipoHacinamINEI = case_when(hacinam <3 ~ "Sin hacinamiento",
                                     hacinam >=3 ~ "Con hacinamiento",
                                     TRUE ~ NA),
         noEstudia = case_when(p306==2 | p307==2 ~ 1,
                               TRUE ~ 0),
         noCapacita = case_when(p310==2 ~ 1,
                                TRUE ~ 0),
         noTrabaja = case_when(ocu500!=1 & !is.na(ocu500) ~ 1,
                               TRUE ~ 0),
         nini = case_when(noEstudia == 1 & noTrabaja == 1 & noCapacita == 1 ~ "No estudia ni trabaja",
                          noEstudia == 0 | noCapacita == 0 & noTrabaja == 1 ~ "Solo estudia",
                          noEstudia == 1 & noCapacita == 1 & noTrabaja == 0 ~ "Solo trabaja",
                          TRUE ~ NA)) %>% 
  group_by(anio, mes, conglome, vivienda, hogar, ubigeo, dominio, estrato) %>% 
  mutate(hogarDep = sum(personaDep),
         hogarAdultoMayor = sum(adultoMayor)) %>%
  ungroup() %>% 
  mutate(hogarDep = case_when(hogarDep > 0 ~ 1,
                              TRUE ~ 0),
         hogarAdultoMayor = case_when(hogarAdultoMayor > 0 & adultoMayorJH == 1 ~ "Hogar con adulto mayor jefe de hogar",
                                      hogarAdultoMayor > 0 & adultoMayorJH == 0 ~ "Hogar con adulto mayor no jefe de hogar",
                                      TRUE ~ "Hogar sin adulto mayor"),
         nTransPubDiario = case_when(nTrasporteDiario == 0 ~ "0",
                                     nTrasporteDiario == 1 ~ "1",
                                     nTrasporteDiario >=2 ~ "2 o más",
                                     TRUE ~ NA))


df_list <- list()

for (i in 2007:2023) {
  alternative <- basePersonasFilter %>%
    filter(anio==i) %>% 
    filter(area == 1)
  
  # read the data into R using read.dta function
  assign(paste0("bP_", i), alternative)
  
  df_name <- paste0("bP_", i)
  if (exists(df_name)) {
    df_list[[df_name]] <- get(df_name)
  } else {
    cat("Data frame", df_name, "does not exist.\n")
  }
  
  rm(df_name, alternative)
}

rm(list = ls(pattern = "^bP"), basePersonasFilter)

year_list <- as.list(2007:2023)

# 2. Estimaciones de variables ----
## 2.1. Pobreza urbana y vulnerabilidad por estrato y departamento ----

pobreza_dpto <- lapply(df_list, function(df) {
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ dpto, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)

  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ dpto, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "dpto") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

pobreza_estrato <- lapply(df_list, function(df) {
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ estrato, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ estrato, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "estrato") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

pobreza_sexo <- lapply(df_list, function(df) {
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ p207, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ p207, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "p207") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

pobreza_sexJH <- lapply(df_list, function(df) {
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ mujerJH, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ mujerJH, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "mujerJH") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

pobreza_edad <- lapply(df_list, function(df) {
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ grupoEdad, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ grupoEdad, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "grupoEdad") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

pobreza_regionNatural <- lapply(df_list, function(df) {
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ regionNatural, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ regionNatural, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "regionNatural") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

pobreza_limaMet <- lapply(df_list, function(df) {
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ limaMet, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ limaMet, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "limaMet") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

pobreza_ciudad <- lapply(df_list, function(df) {
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ ciudad, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ ciudad, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "ciudad") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

### 2.1.1 Mapas ----
map_peru <- map_REG |> #Cargamos la base de datos sobre los departamentos del Peru
  rename(UBIGEO = COD_REGION ) %>% #renombramos la variable del DF para el merge por UBIGEO 
  mutate(dpto = case_when(UBIGEO == "159900" ~ 26,
                          TRUE ~ as.integer(as.numeric(UBIGEO)/10000)))

data_pobreza <- do.call(rbind, pobreza_dpto)
rownames(data_pobreza) <- NULL

map_shiny <- merge(x = map_peru, y = data_pobreza, by = "dpto", all.x = TRUE)

map_pobreza_animado <- map_shiny  %>%  
  ggplot() +
  aes(geometry = geometry) +
  geom_sf(aes(fill = pobre)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  transition_time(anio) +
  labs(title = "Year: {floor(frame_time)}", fill = "Pobreza urbana (%)") +
  scale_fill_gradient(low = "lightblue", high = "darkblue")

frames_dir <- "frames"
dir.create(frames_dir, showWarnings = FALSE)

# File renderer to save each frame as a PNG
renderer <- file_renderer(dir = frames_dir, prefix = "frame")
                          
animate(map_pobreza_animado, nframes = 17, fps = 1, renderer = renderer)

dirOutput <- "C:/Users/User/OneDrive - MIGRACIÓN VIDENZA/1. Proyectos/1. Proyectos actuales/23. Artículos PDB/2. PDB - Pobreza Urbana/2. Data/2. Output"
setwd(dirOutput)
write_xlsx(data_pobreza, path = "pobrezaUrbana.xlsx")

map_vulnerabilidad_animado <- map_shiny  %>%  
  ggplot() +
  aes(geometry = geometry) +
  geom_sf(aes(fill = vulnerableNoPobre)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  transition_time(anio) +
  labs(title = "Year: {floor(frame_time)}", fill = "Pobreza urbana (%)") +
  scale_fill_gradient(low = "lightblue", high = "darkblue")

animate(map_vulnerabilidad_animado, nframes = 16, fps = 2, renderer = gifski_renderer())

### 2.1.2 Gráficos ----

data_estrato <- do.call(rbind, pobreza_estrato)
rownames(data_estrato) <- NULL

graph1 <- data_estrato  %>% 
  mutate(estrato2 = case_when(estrato == 1 ~ "Estrato 1: De 500 000 a más habitantes",
                              estrato == 2 ~ "Estrato 2: De 100 000 a 499 999 habitantes",
                              estrato == 3 ~ "Estrato 3: De 50 000 a 99 999 habitantes",
                              estrato == 4 ~ "Estrato 4: De 20 000 a 49 999 habitantes",
                              estrato == 5 ~ "Estrato 5: De 2 000 a 19 999 habitantes",
                              TRUE ~ NA)) %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = estrato2) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según estrato geográfico, 2007-2022", 
       color = "Estrato")

ggsave(filename = "graficos/g_Estrato.png")

data_sex <- do.call(rbind, pobreza_sexo)
rownames(data_sex) <- NULL

graph2 <- data_sex  %>% 
  mutate(sexo = case_when(p207 == 1 ~ "Hombre",
                          p207 == 2 ~ "Mujer",
                          TRUE ~ NA)) %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = sexo) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según sexo, 2007-2022", 
       color = "Sexo") 
ggsave(filename = "graficos/g_Sexo.png")

data_sexJH <- do.call(rbind, pobreza_sexJH)
rownames(data_sexJH) <- NULL

graph2 <- data_sexJH  %>% 
  mutate(sexJH = case_when(mujerJH == 1 ~ "Mujer",
                           mujerJH == 0 ~ "Hombre",
                           TRUE ~ NA)) %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = sexJH) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según sexo del jefe del hogar, 2007-2022", 
       color = "Sexo del jefe del hogar") 
ggsave(filename = "graficos/g_SexoJH.png")

data_edad <- do.call(rbind, pobreza_edad)
rownames(data_edad) <- NULL

graph3 <- data_edad  %>% 
  mutate(edad = case_when(grupoEdad == 1 ~ "Hasta 24 años",
                          grupoEdad == 2 ~ "Entre 25 y 44 años",
                          grupoEdad == 3 ~ "Entre 45 y 64 años",
                          grupoEdad == 4 ~ "65 años y más",
                          TRUE ~ NA)) %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = edad) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según grupos de Edad, 2007-2022", 
       color = "Edad") 
ggsave(filename = "graficos/g_Edad.png")

data_regNat <- do.call(rbind, pobreza_regionNatural)
rownames(data_regNat) <- NULL

graph4 <- data_regNat  %>% 
  mutate(regNat = case_when(regionNatural == 1 ~ "Resto costa",
                            regionNatural == 2 ~ "Sierra",
                            regionNatural == 3 ~ "Selva",
                            regionNatural == 4 ~ "Lima Metropolitana",
                            TRUE ~ NA)) %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = regNat) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según región natural, 2007-2022", 
       color = "Región natural") 
ggsave(filename = "graficos/g_regNat.png")

data_limaMet <- do.call(rbind, pobreza_limaMet)
rownames(data_limaMet) <- NULL

graph5 <- data_limaMet  %>% 
  mutate(lima = case_when(limaMet == 1 ~ "Lima Metropolitana",
                          TRUE ~ "Resto urbano")) %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = lima) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según ciudad, 2007-2022", 
       color = "Lima Metropolitana vs Resto urbano") 
ggsave(filename = "graficos/g_limaMet.png")

data_ciudad <- do.call(rbind, pobreza_ciudad)
rownames(data_limaMet) <- NULL

graph6 <- data_ciudad  %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = ciudad) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según ciudad, 2007-2022", 
       color = "Ciudad") 
ggsave(filename = "graficos/g_ciudad.png")

# Pensión vs No Pensión
# Programa Social vs No Programa Social
# Grupos de Edad PNDIS
# Servicios de cuidado NO HAY INFO
# Distancia al trabajo

## R1. Empleo decente ----

#Data

pobreza_informalidad <- lapply(df_list, function(df) {
  
  df <- df %>%
    filter(!is.na(fac500a))
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$fac500a)
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ empInf, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ empInf, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "empInf") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

data_inf <- do.call(rbind, pobreza_informalidad) %>% 
  mutate(empInf2 = case_when(empInf == 1 ~ "Empleo informal",
                             empInf == 0 ~ "Empleo formal",
                             TRUE ~ NA))
rownames(data_inf) <- NULL

pobreza_subempleo <- lapply(df_list, function(df) {
  
  df <- df %>%
    filter(!is.na(fac500a))
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$fac500a)
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ subemp, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ subemp, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "subemp") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

data_subemp <- do.call(rbind, pobreza_subempleo) %>% 
  mutate(subEmp = case_when(subemp == 1 ~ "Subempleados",
                            subemp == 0 ~ "Adecuadamente empleados",
                            TRUE ~ NA))
rownames(data_subemp) <- NULL

# Graficos
graph15 <- data_subemp   %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = subEmp) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según tipo de empleo, 2007-2022", 
       color = "Tipo de empleo") 
ggsave(filename = "graficos/g_subEmp.png")


graph16 <- data_inf  %>% 
  mutate(empInf2 = case_when(empInf == 1 ~ "Empleo informal",
                             empInf == 0 ~ "Empleo formal",
                             TRUE ~ NA)) %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = empInf2) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según tipo de empleo, 2007-2022", 
       color = "Tipo de empleo") 
ggsave(filename = "graficos/g_Inform.png")


# Riesgos
pobreza_numRiesgos <- lapply(df_list, function(df) {
  
  df <- df %>%
    filter(!is.na(fac500a)) %>% 
    mutate(numRiesgos2 = case_when(numRiesgos == 0 ~ "Sin riesgos",
                                   numRiesgos == 1 ~ "Solo un riesgo",
                                   numRiesgos == NA ~ NA,
                                   TRUE ~ "Más de un riesgo"))
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$fac500a)
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ numRiesgos2, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ numRiesgos2, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "numRiesgos2") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

data_riesgoEnf <- do.call(rbind, pobreza_riesgoEnf) 
rownames(data_riesgoEnf) <- NULL

graph17 <- data_riesgoEnf  %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = numRiesgos2) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según ocurrencia de riesgos en el hogar, 2007-2022", 
       color = "Ocurrencia de riesgos") 
ggsave(filename = "graficos/g_numRiesgos.png")

#p401
pobreza_enfCronica <- lapply(df_list, function(df) {
  
  df <- df %>%
    filter(!is.na(fac500a)) %>% 
    mutate(enfCronica = case_when(p401 == 1 ~ "Padece de alguna enfermedad o malestar crónico",
                                  p401 == NA ~ NA,
                                  TRUE ~ "No padece de alguna enfermedad o malestar crónico"))
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$fac500a)
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ enfCronica, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ enfCronica, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "enfCronica") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

data_enfCronica <- do.call(rbind, pobreza_enfCronica) 
rownames(data_enfCronica) <- NULL

graph18 <- data_enfCronica  %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = enfCronica) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según prevalencia de enfermedad crónica, 2007-2022", 
       color = "Prevalencia de enfermedad crónica") 
ggsave(filename = "graficos/g_enfCronica.png")

#tamaEmp
pobreza_tamaEmp <- lapply(df_list, function(df) {
  
  df <- df %>%
    filter(!is.na(fac500a)) 
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$fac500a)
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ tamaEmp, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ tamaEmp, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "tamaEmp") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

data_tamaEmp <- do.call(rbind, pobreza_tamaEmp) 
rownames(data_tamaEmp) <- NULL

graph18 <- data_tamaEmp  %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = tamaEmp) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según tamaño de empresa, 2007-2022", 
       color = "Tamaño de empresa") 
ggsave(filename = "graficos/g_tamaEmp.png")

#pctPerceptores
pobreza_perceptores <- lapply(df_list, function(df) {
  
  df <- df %>% 
    mutate(perceptores = case_when(pctPerceptores <0.25 ~ "Hasta el 24%",
                                   pctPerceptores >= 0.25 & pctPerceptores <0.5 ~ "Entre el 25% y 49%",
                                   pctPerceptores >= 0.5 & pctPerceptores <0.75 ~ "Entre el 50% y 74%",
                                   pctPerceptores == NA ~ NA,
                                   TRUE ~ "De 75% a más"))
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ perceptores, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ perceptores, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "perceptores") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

pobreza_tipoHogar <- lapply(df_list, function(df) {
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ tipoHogar, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ tipoHogar, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "tipoHogar") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

pobreza_Monop <- lapply(df_list, function(df) {
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ hogarMonoParent2 + hombre, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ hogarMonoParent2 + hombre, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = c("hogarMonoParent2", "hombre")) %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

pobreza_adultomayor <- lapply(df_list, function(df) {
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ hogarAdultoMayor, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ hogarAdultoMayor, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "hogarAdultoMayor") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

pobreza_hogarDep <- lapply(df_list, function(df) {
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ hogarDep, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ hogarDep, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "hogarDep") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

data_perceptores <- do.call(rbind, pobreza_perceptores) 
rownames(data_perceptores) <- NULL

graph10 <- data_perceptores %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = perceptores) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según porcentaje de perceptores de ingresos del hogar, 2007-2022", 
       color = "Porcentaje de perceptores de ingresos") 
ggsave(filename = "graficos/g_perceptores.png")

data_tipoHogar <- do.call(rbind, pobreza_tipoHogar)
rownames(data_tipoHogar) <- NULL

graph7 <- data_tipoHogar  %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = tipoHogar) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según tipología de hogar, 2007-2022", 
       color = "Tipo de hogar")
ggsave(filename = "graficos/g_tipoHogar.png")

data_monop <- do.call(rbind, pobreza_Monop) %>% 
  mutate(tipoHogarMono = case_when(hogarMonoParent2 == "Hogar monoparental" & hombre == 0 ~ "Hogar monoparental femenino",
                                   hogarMonoParent2 == "Hogar monoparental" & hombre == 1 ~ "Hogar monoparental masculino",
                                   TRUE ~ NA)) %>% 
  filter(!is.na(tipoHogarMono))

rownames(data_monop) <- NULL

graph8 <- data_monop  %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = tipoHogarMono) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según tipo de hogar con hijos, 2007-2022", 
       color = "Tipo de hogar monoparental") 
ggsave(filename = "graficos/g_monop.png")

data_adultoMayor <- do.call(rbind, pobreza_adultomayor)
rownames(data_adultoMayor) <- NULL

graph9 <- data_adultoMayor %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = hogarAdultoMayor) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según tipo de adulto mayor en el hogar, 2007-2022", 
       color = "Tipo de adulto mayor en el hogar") 
ggsave(filename = "graficos/g_adultoMayor.png")

data_hogarDep <- do.call(rbind, pobreza_hogarDep) %>% 
  mutate(hogarDep = case_when(hogarDep == 1 ~ "Hogar con dependientes",
                              hogarDep == 0 ~ "Hogar sin dependiente"))
rownames(data_hogarDep) <- NULL

graph10 <- data_hogarDep %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = hogarDep) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según tipo de hogar según tenencia de dependientes, 2007-2022", 
       color = "Tipo de hogar según tenencia de dependientes") 
ggsave(filename = "graficos/g_hogarDep.png")



#trabajaMD

pobreza_trabajaMD <- lapply(df_list, function(df) {
  
  df <- df %>%
    filter(!is.na(fac500a))
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$fac500a, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ trabajaMD, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ trabajaMD, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "trabajaMD") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

data_trabajaMD <- do.call(rbind, pobreza_trabajaMD) %>% 
  mutate(trabajaMD2 = case_when(trabajaMD == 1 ~ "Trabaja en el mismo distrito donde vive",
                                trabajaMD == 0 ~ "Trabaja en otro distrito",
                                TRUE ~ NA))
rownames(data_trabajaMD) <- NULL

graph13 <- data_trabajaMD %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = trabajaMD2) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según distancia al trabajo, 2007-2022", 
       color = "Distrito donde trabaja") 
ggsave(filename = "graficos/g_trabajaMD.png")

#nTransPubDiario

pobreza_nTransPub <- lapply(df_list, function(df) {
  
  df <- df %>%
    filter(!is.na(fac500a))
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$fac500a, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ nTransPubDiario, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ nTransPubDiario, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "nTransPubDiario") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

data_nTransPubDiario <- do.call(rbind, pobreza_nTransPub) 
rownames(data_nTransPubDiario) <- NULL

graph13 <- data_nTransPubDiario %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = nTransPubDiario) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según número de transportes públicos utilizados a diario, 2007-2022", 
       color = "Número de transportes públicos utilizados a diario") 
ggsave(filename = "graficos/g_nTransPubDiario.png")

#indigena

pobreza_indigena <- lapply(df_list, function(df) {
  
  df <- df %>%
    filter(!is.na(fac500a))
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$fac500a, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ indigena, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ indigena, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "indigena") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

data_indigena <- do.call(rbind, pobreza_indigena) 
rownames(data_indigena) <- NULL

graph13 <- data_indigena %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = indigena) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según origen indígena, 2007-2022", 
       color = "Origen indígena") 
ggsave(filename = "graficos/g_indigena.png")


## R2. Emprendimiento  ----

pobreza_indep <- lapply(df_list, function(df) {
  
  df <- df %>%
    filter(!is.na(fac500a))
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$fac500a, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ indep, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ indep, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "indep") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

data_indep <- do.call(rbind, pobreza_indep) %>% 
  mutate(indep = case_when(indep == 1 ~ "Trabajador independiente",
                           indep == 0 ~ "Otro tipo"))
rownames(data_indep) <- NULL

graph13 <- data_indep %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = indep) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según trabajo independiente, 2007-2022", 
       color = "Trabajo independiente") 
ggsave(filename = "graficos/g_indep.png")

# indep  & 
    #p107b1 - p107b4
    #nCuentas
    #CuentaAhorro
    #CuentaPlazoFijo
    #CuentaCorr
    #CuentaNoTiene

# e1
# e1a1
# e1b
# e2
# e3
# e4a1 + e4a2 + e4a3 + e4a4 + e4a5
# e5
# e6a 
# e8a | e8b | e8c

## R3. Redes sociales y de apoyo ----

#participacion2
#confFamiliares

## R4. Condiciones de vivienda  ----

#aguaHoras
pobreza_aguaHoras <- lapply(df_list, function(df) {
  
  df <- df %>% 
    mutate(aguaHoras = case_when(aguaHoras == 24 ~ "24 horas",
                                 aguaHoras < 24 & aguaHoras >=12 ~ "Entre 12 y 23 horas",
                                 aguaHoras == NA ~ NA,
                                 TRUE ~ "Menos de 12 horas"))
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ aguaHoras, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ aguaHoras, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "aguaHoras") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

#aguaPotable
pobreza_aguaPotable <- lapply(df_list, function(df) {
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ aguaPotable, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ aguaPotable, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "aguaPotable") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

#vivBajaCalidad
pobreza_vivBajaCalidad <- lapply(df_list, function(df) {
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ vivBajaCalidad, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ vivBajaCalidad, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "vivBajaCalidad") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

#vivInvasion
pobreza_vivInvasion <- lapply(df_list, function(df) {
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ vivInvasion, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ vivInvasion, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "vivInvasion") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

#vivCedida
pobreza_vivCedida <- lapply(df_list, function(df) {
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ vivCedida, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ vivCedida, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "vivCedida") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

#pisoTierraCemento
pobreza_pisoTierraCemento <- lapply(df_list, function(df) {
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ pisoTierraCemento, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ pisoTierraCemento, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "pisoTierraCemento") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

#techoDebil
pobreza_techoDebil <- lapply(df_list, function(df) {
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ techoDebil, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ techoDebil, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "techoDebil") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

pobreza_hacinamOMS <- lapply(df_list, function(df) {
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ tipoHacinamOMS, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ tipoHacinamOMS, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "tipoHacinamOMS") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

pobreza_hacinamINEI <- lapply(df_list, function(df) {
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$facpob07, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ tipoHacinamINEI, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ tipoHacinamINEI, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "tipoHacinamINEI") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

data_hacinamOMS <- do.call(rbind, pobreza_hacinamOMS) 
rownames(data_hacinamOMS) <- NULL

graph11 <- data_hacinamOMS %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = tipoHacinamOMS) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según nivel de hacinamiento OMS, 2007-2022", 
       color = "Nivel de hacinamiento") 
ggsave(filename = "graficos/g_hacinamOMS.png")

data_hacinamINEI <- do.call(rbind, pobreza_hacinamINEI) 
rownames(data_hacinamINEI) <- NULL

graph12 <- data_hacinamINEI %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = tipoHacinamINEI) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según nivel de hacinamiento INEI, 2007-2022", 
       color = "Nivel de hacinamiento") 
ggsave(filename = "graficos/g_hacinamINEI.png")

data_aguaHoras <- do.call(rbind, pobreza_aguaHoras) 
rownames(data_aguaHoras) <- NULL

graph12 <- data_aguaHoras %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = tipoaguaHoras) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según continuidad del servicio de agua, 2007-2022", 
       color = "Horas del servicio") 
ggsave(filename = "graficos/g_aguaHoras.png")

data_vivBajaCalidad <- do.call(rbind, pobreza_vivBajaCalidad) 
rownames(data_vivBajaCalidad) <- NULL

graph12 <- data_vivBajaCalidad %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = tipovivBajaCalidad) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según nivel de hacinamiento INEI, 2007-2022", 
       color = "Nivel de hacinamiento") 
ggsave(filename = "graficos/g_vivBajaCalidad.png")

data_vivInvasion <- do.call(rbind, pobreza_vivInvasion) 
rownames(data_vivInvasion) <- NULL

graph12 <- data_vivInvasion %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = tipovivInvasion) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según invasión de la vivienda, 2007-2022", 
       color = "Invasión de la vivienda") 
ggsave(filename = "graficos/g_vivInvasion.png")

data_vivCedida <- do.call(rbind, pobreza_vivCedida) 
rownames(data_vivCedida) <- NULL

graph12 <- data_vivCedida %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = tipovivCedida) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según cesión de la vivienda, 2007-2022", 
       color = "Vivienda cedida") 
ggsave(filename = "graficos/g_vivCedida.png")

data_pisoTierraCemento <- do.call(rbind, pobreza_pisoTierraCemento) 
rownames(data_pisoTierraCemento) <- NULL

graph12 <- data_pisoTierraCemento %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = tipopisoTierraCemento) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según material del piso de la vivienda, 2007-2022", 
       color = "Material del piso") 
ggsave(filename = "graficos/g_pisoTierraCemento.png")

data_techoDebil <- do.call(rbind, pobreza_techoDebil) 
rownames(data_techoDebil) <- NULL

graph12 <- data_techoDebil %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = tipotechoDebil) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según tipo de techo de la vivienda, 2007-2022", 
       color = "Tipo de techo") 
ggsave(filename = "graficos/g_techoDebil.png")


## R5. Acceso a mercado financiero  ----

#p107b1 - p107b4
#nCuentas
#CuentaAhorro
#CuentaPlazoFijo
#CuentaCorr
#CuentaNoTiene

## R6. Acceso a educación de calidad  ----

#nivEduc

pobreza_nivEduc <- lapply(df_list, function(df) {
  
  df <- df %>%
    filter(!is.na(fac500a))
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$fac500a, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ nivEduc, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ nivEduc, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "nivEduc") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

data_nivEduc <- do.call(rbind, pobreza_nivEduc) 
rownames(data_nivEduc) <- NULL

graph14 <- data_nivEduc %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = nivEduc) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según nivel máximo de educación alcanzada, 2007-2022", 
       color = "Nivel educativo máximo alcanzado")
ggsave(filename = "graficos/g_nivEduc.png")

#estudiaMD

#nini


pobreza_jovNinis <- lapply(df_list, function(df) {
  
  df <- df %>%
    filter(!is.na(fac500a) & grupoEdad2 == 2)
  
  enaho_design <- svydesign(id = df$conglome, data = df, strata = df$estrato, weights = ~df$fac500a, vars = list(region = ~dpto))
  options(survey.lonely.psu="adjust")
  
  pobreza <- svyby(~ pobre, ~ nini, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  vulnerabilidad <- svyby(~ vulnerableNoPobre, ~ nini, design = enaho_design, svymean, na.rm = TRUE) %>% 
    subset(select = -se)
  
  result <- reduce(list(pobreza, vulnerabilidad), merge, by = "nini") %>% 
    mutate(anio = mean(df$anio))
  
  return(result)
})

data_jovNini <- do.call(rbind, pobreza_jovNinis) 
rownames(data_jovNini) <- NULL

graph14 <- data_jovNini %>% 
  ggplot() +
  aes(x = anio, y = pobre, color = nini) +
  stat_summary(aes(y=pobre), fun ="mean", geom="point") +
  stat_summary(aes(y=pobre), fun ="mean", geom="line") +
  labs(x = "Año",
       y = 'Pobreza urbana (%)') +
  labs(title = "Pobreza urbana según situación de estudio y trabajo en jóvenes entre 15 y 30 años, 2007-2022", 
       color = "Situación de estudio y empleo") 
ggsave(filename = "graficos/g_jovNinis.png")

## R7. Acceso a salud de calidad  ----

#saludMD
#distES
#tiempoSalud
#Seguros de salud
#migrante5anios
#migrante
#discrRaza
# p2f$06

rm(list = ls(pattern = "^graph"))
