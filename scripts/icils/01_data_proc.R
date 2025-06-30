# ---- Base de autoeficacia digital ICILS 2023 ----

# Carga de librerías y pasos previos

pacman::p_load(here, dplyr, haven,sjlabelled, psych, purrr, tidyverse, sjmisc, sjPlot, ggplot2, parameters, table1, car, beeswarm, lme4)
options(scipen = 999)
rm(list = ls())

# Cargar bases de datos 

archivos_bsg <- list.files(
  path = "./data/raw_data/icils_countries", # Directorio actual (cambia si es necesario)
  pattern = "^BSG.*\\.Rdata$",
  full.names = TRUE
)

# Ver qué archivos encontramos
print(archivos_bsg)

# Cargar todos los archivos encontrados
for (archivo in archivos_bsg) {
  load(archivo)
  cat("Cargado:", archivo, "\n")
}

# así hasta cargar todas

# Unir bases de datos

icils23 <- rbind(BSGAUTI3, BSGAZEI3, BSGBFLI3, BSGBIHI3, BSGCHLI3, BSGCYPI3, BSGCZEI3, BSGDEUI3, 
                    BSGDNKI3, BSGDNWI3, BSGESPI3, BSGFINI3, BSGFRAI3, BSGGRCI3, BSGHRVI3, BSGHUNI3,
                    BSGITAI3, BSGKAZI3, BSGKORI3, BSGLUXI3, BSGLVAI3, BSGMLTI3, BSGNLDI3, BSGNORI3, 
                    BSGOMNI3, BSGPRTI3, BSGROUI3, BSGSRBI3, BSGSVKI3, BSGSVNI3, BSGSWEI3, BSGTWNI3,
                    BSGURYI3, BSGUSAI3, BSGXKXI3)

# Seleccionar variables que componen la autoeficacia digital

icils23_proc <- icils23 %>%
  select(CNTRY, IDSCHOOL, IS3G24A, IS3G24B, IS3G24C, IS3G24D, IS3G24E, IS3G24F, IS3G24G, IS3G24H, IS3G24I, IS3G24J,
         IS3G24K, IS3G24L, IS3G24M, PV1CIL)

# Revisar las categorias de respuesta para poder agrupar los NA
sjlabelled::get_labels(icils23_proc)

# Agrupar NA

icils23_proc$IS3G24A <- recode(icils23_proc$IS3G24A, "c(8,9)=NA")
icils23_proc$IS3G24B <- recode(icils23_proc$IS3G24B, "c(8,9)=NA")
icils23_proc$IS3G24C <- recode(icils23_proc$IS3G24C, "c(8,9)=NA")
icils23_proc$IS3G24D <- recode(icils23_proc$IS3G24D, "c(8,9)=NA")
icils23_proc$IS3G24E <- recode(icils23_proc$IS3G24E, "c(8,9)=NA")
icils23_proc$IS3G24F <- recode(icils23_proc$IS3G24F, "c(8,9)=NA")
icils23_proc$IS3G24G <- recode(icils23_proc$IS3G24G, "c(8,9)=NA")
icils23_proc$IS3G24H <- recode(icils23_proc$IS3G24H, "c(8,9)=NA")
icils23_proc$IS3G24I <- recode(icils23_proc$IS3G24I, "c(8,9)=NA")
icils23_proc$IS3G24J <- recode(icils23_proc$IS3G24J, "c(8,9)=NA")
icils23_proc$IS3G24K <- recode(icils23_proc$IS3G24K, "c(8,9)=NA")
icils23_proc$IS3G24L <- recode(icils23_proc$IS3G24L, "c(8,9)=NA")
icils23_proc$IS3G24M <- recode(icils23_proc$IS3G24M, "c(8,9)=NA")

# Recodificación valores

icils23_proc$IS3G24A <- car::recode(icils23_proc$IS3G24A, "c(1)=4; c(2)=3; c(3)=2; c(4)=1")
icils23_proc$IS3G24B <- car::recode(icils23_proc$IS3G24B, "c(1)=4; c(2)=3; c(3)=2; c(4)=1")
icils23_proc$IS3G24C <- car::recode(icils23_proc$IS3G24C, "c(1)=4; c(2)=3; c(3)=2; c(4)=1")
icils23_proc$IS3G24D <- car::recode(icils23_proc$IS3G24D, "c(1)=4; c(2)=3; c(3)=2; c(4)=1")
icils23_proc$IS3G24E <- car::recode(icils23_proc$IS3G24E, "c(1)=4; c(2)=3; c(3)=2; c(4)=1")
icils23_proc$IS3G24F <- car::recode(icils23_proc$IS3G24F, "c(1)=4; c(2)=3; c(3)=2; c(4)=1")
icils23_proc$IS3G24G <- car::recode(icils23_proc$IS3G24G, "c(1)=4; c(2)=3; c(3)=2; c(4)=1")
icils23_proc$IS3G24H <- car::recode(icils23_proc$IS3G24H, "c(1)=4; c(2)=3; c(3)=2; c(4)=1")
icils23_proc$IS3G24I <- car::recode(icils23_proc$IS3G24I, "c(1)=4; c(2)=3; c(3)=2; c(4)=1")
icils23_proc$IS3G24J <- car::recode(icils23_proc$IS3G24J, "c(1)=4; c(2)=3; c(3)=2; c(4)=1")
icils23_proc$IS3G24K <- car::recode(icils23_proc$IS3G24K, "c(1)=4; c(2)=3; c(3)=2; c(4)=1")
icils23_proc$IS3G24L <- car::recode(icils23_proc$IS3G24L, "c(1)=4; c(2)=3; c(3)=2; c(4)=1")
icils23_proc$IS3G24M <- car::recode(icils23_proc$IS3G24M, "c(1)=4; c(2)=3; c(3)=2; c(4)=1")

# Recodificación labels

recodificar <- function(data, vars_prefix = "IS3G24") {
  # Definir las etiquetas comunes
  etiquetas <- c(
    "I do not think I could do this" = 1,
    "I have never done this, but I could work out how to do" = 2,
    "Moderatelly well" = 3,
    "Very well" = 4
  )
  
  # Identificar todas las variables que coinciden con el prefijo
  vars_a_recodificar <- grep(paste0("^", vars_prefix), names(data), value = TRUE)
  
  # Aplicar las etiquetas a cada variable
  for (var in vars_a_recodificar) {
    data[[var]] <- set_labels(data[[var]], labels = etiquetas)
  }
  
  return(data)
}

icils23_proc <- recodificar(icils23_proc)

# Comprobar si se recodificaron correctamente los labels

frq(icils23_proc$IS3G24I)

saveRDS(icils23_proc, "data/proc_data/icils23_proc.rds")

# ---- Base de autoeficacia digital PISA 2022 ----

pisa22 <- readRDS("data/proc_data/pisa22ict.rds")

pisa22_proc <- pisa22 %>%
  select(CNT, CNTRYID, CNTSCHID, sex = ST004D01T, IC183Q01JA, IC183Q02JA, IC183Q03JA, IC183Q04JA,
         IC183Q05JA, IC183Q07JA, IC183Q08JA, IC183Q09JA, IC183Q10JA,
         IC183Q12JA, IC183Q13JA, IC183Q14JA, IC183Q15JA, IC183Q16JA)

pisa22_proc <- pisa22_proc %>% rename("search_info"=IC183Q01JA,
                                      "asses_info"=IC183Q02JA,
                                      "share_info"=IC183Q03JA,
                                      "pair_collab"=IC183Q04JA,
                                      "how_to_share"=IC183Q05JA,
                                      "edit_text"=IC183Q07JA,
                                      "collect_data"=IC183Q08JA,
                                      "create_pres"=IC183Q09JA,
                                      "page_web"=IC183Q10JA,
                                      "change_settings"=IC183Q12JA,
                                      "select_app"=IC183Q13JA,
                                      "create_program"=IC183Q14JA,
                                      "identify_error"=IC183Q15JA,
                                      "logical_solution"=IC183Q16JA)

saveRDS(pisa22_proc, "data/proc_data/pisa22_proc.rds")

