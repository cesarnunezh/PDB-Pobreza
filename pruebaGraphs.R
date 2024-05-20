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

# 1. Librerías y directorios ---------------------------------------------------
dirEnaho <- "/etc/data/base_trabajo"
library(tidyverse)  
library(haven)
library(readr)

# 2. Generación de gráficos ----------------------------------------------------

## 2.1 Hogares -----------------------------------------------------------------
setwd(dirEnaho)
baseHogares1 <- read_dta("baseHogaresFinal.dta")

setwd('~/GiZ Pobreza-Urbana')
# generamos una base de datos del Estrato 1 (más de 100 mil viviendas) y variables de percentiles
baseHogares1 <- baseHogares1 %>%
  mutate(gashog2dPCap = gashog2d / mieperho,
         gastoPCentil = xtile(gashog2dPCap, 100))

varCat <- c("abandonoH", "confDivisionH", "confPensionH", "confTenenciaH", "confViolacionH", "confViolenciaH", "confVisitasH", "riesgoEnfermedadH", "riesgoQuiebraH", "victimaDelitoH",
            "vivBajaCalidad", "vivInvasion", "vivCedida", "pisoTierra", "pisoCemento", "pisoTierraCemento", "techoDebil", "paredLadrillo",
            "combustibleCocina", "agua", "aguaPotable", "aguaSegura", "agua24Horas", "aguaCisterna", "desague", "electricidad", "tresServicios", "telCelu", "internet",
            "nbi1", "nbi2", "nbi3", "nbi4", "nbi5",
            "hogarVasoLeche", "hogarComedor", "hogarQaliWarma", "hogarCunaMas", "hogarJuntos", "hogarPension65", "hogarJovProd", "hogarTrabajaPeru", "hogarImpulsaPeru", "hogarBeca18",
            "tipoHogar", "hogarConHijo", "hogarMonoParent", "hogarNuclear", "hogarExtendido", "hogarUniFem", "hogarUniMayor",
            "nivEducJH", "lengJH", "mujerJH", "origenJH", "mestizoJH", "indigenaJH", "hombreJH", "grupoEdadJH", "jovenJH", "adultoJovJH", "adultoJH", "adultoMayorJH", "estadoCivilJH", "convivienteJH", "casadoJH",
            "empInfJH", "sectorJH", "sectorJHCom", "sectorJHRest", "sectorJHHog", "sinContratoJH", "indepJH", "tamaEmpJH", "empPeqJH", "empMedJH", "empGrandeJH",
            "jh65mas", "jh25menos", "migranteJH", "migrante5aniosJH", "educSecJH", "educSupJH")

# Histogramas según variables categóricas
for(j in 1:length(varCat)){
  baseHogares1 %>% 
    ggplot() + 
    aes(gashog2dPCap, fill=factor(get(varCat[j]))) +
    geom_histogram(binwidth = 100, alpha= 0.5, position="identity") +
    scale_x_continuous(lim = c(0, 50000), breaks = seq(0, 50000, 100)) +
    ggtitle(paste("Frecuencia de gasto(per cápita) por",varCat[j],
                  "- Estrato 1", sep = " ")) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  ggsave(paste("graficos/fig_",varCat[j],".png",sep = ""))
}  

# Distribución de variables continuas
varNum <- c("numTrabJH", "aguaHoras", "nActivos", "edadJH", "anioEducJH", "nArtefactos", 
            "confFamiliares", "mieperho", "percepho", "pctIngExt", "ratioDependencia", "confFamiliaresH")

for(j in 1:length(varNum)){
  tmp <- baseHogares1 %>% 
    select(gastoPCentil, varNum[j]) %>% 
    drop_na() %>% 
    group_by(gastoPCentil) %>% 
    summarise(var = mean(get(varNum[j])))
  
  ggplot(tmp, aes(x=gastoPCentil, y = var)) +
    geom_point() + 
    geom_smooth(method = "lm") + 
    scale_x_continuous(lim = c(0, 100), breaks = seq(0, 100, 10)) + 
    xlab("Gasto per cápita del hogar (centiles)") + 
    ylab(varNum[j]) + 
    ggtitle(varNum[j])
  
  ggsave(paste("graficos/fig_",varNum[j],".png",sep = ""))
}

## 2.2 Personas ----------------------------------------------------------------
setwd(dirEnaho)
basePersonas1 <- read_dta("basePersonasFinal.dta")

setwd('~/GiZ Pobreza-Urbana')
# generamos una base de datos del Estrato 1 (más de 100 mil viviendas) y variables de percentiles
basePersonas1 <- basePersonas1 %>%
  mutate(gashog2dPCap = gashog2d / mieperho,
         gastoPCentil = xtile(gashog2dPCap, 100))

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
             "sinContrato", "subempHrs", "subempIng", "tamaEmp", "techoDebil", "tipoHogar", "trabajaMD", 
             "tresServicios", "usoColectivo", "usoCombi", "usoDiarioColectivo", "usoDiarioCombi",
             "UsoDiarioMototaxi", "usoDiarioMicrobus", "usoDiarioOmnibus", "usoDiarioTaxi", "usoMicrobus", 
             "usoMototaxi", "usoOmnibus", "usoTaxi", "victimaDelito", "vivBajaCalidad", "vivCedida", "vivInvasion",
             "equipo_sonido", "microondas", "plancha", "refrigerador", "telCelu", "tv_color")

# Histogramas según variables categóricas
for(j in 1:length(varCatP)){
  basePersonas1 %>% 
    ggplot() + 
    aes(gashog2dPCap, fill=factor(get(varCatP[j]))) +
    geom_histogram(binwidth = 100, alpha= 0.5, position="identity") +
    scale_x_continuous(lim = c(0, 50000), breaks = seq(0, 50000, 100)) +
    ggtitle(paste("Frecuencia de gasto(per cápita) por",varCatP[j],
                  "- Estrato 1", sep = " ")) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  ggsave(paste("graficos/fig_P_",varCatP[j],".png",sep = ""))
}  

# Distribución de variables continuas
varNumP <- c("aguaHoras", "anioEduc", "confFamiliares", "edad", "edad", "hacinam", 
             "ingLabPrin", "ingLabSec", "ingNoLab", "ingTot", "mieperho", "nActivos",
             "nActivosPrioritarios", "pctIngExt", "pctPerceptores", "ratioDependencia", "tiempoSalud")

for(j in 1:length(varNumP)){
  tmp <- basePersonas1 %>% 
    select(gastoPCentil, varNumP[j]) %>% 
    drop_na() %>% 
    group_by(gastoPCentil) %>% 
    summarise(var = mean(get(varNumP[j])))
  
  ggplot(tmp, aes(x=gastoPCentil, y = var)) +
    geom_point() + 
    geom_smooth(method = "lm") + 
    scale_x_continuous(lim = c(0, 100), breaks = seq(0, 100, 10)) + 
    xlab("Gasto per cápita del hogar (centiles)") + 
    ylab(varNumP[j]) + 
    ggtitle(varNumP[j])
  
  ggsave(paste("graficos/fig_P_",varNumP[j],".png",sep = ""))
}


# --------------------------------------------------------------------

binw <- diff(range(baseHogares1$gashog2d))/100
ggplot(baseHogares1, aes(x=gashog2d, fill=factor(internet))) +
  geom_histogram(position="fill", binwidth=binw) + 
  scale_x_continuous(lim = c(0, 250000), breaks = seq(0, 250000, 10000)) + 
  xlab("gasto total para cada hogar") + 
  ggtitle("Frecuencia de gasto(total) según el tipo de vivienda")

