# ---- Base de autoeficacia digital ICILS 2023 ----

# Carga de librerías y pasos previos

pacman::p_load(dplyr, haven,sjlabelled, psych, purrr, tidyr, sjPlot, ggplot2, parameters, table1, car, beeswarm, lme4)
options(scipen = 999)
rm(list = ls())

# Cargar bases de datos 

load("../input/df_countries/BSGAUTI3.Rdata")
load("../input/df_countries/BSGAZEI3.Rdata")
# así hasta cargar todas

# Unir bases de datos

icils_2023 <- rbind(BSGAUTI3, BSGAZEI3, BSGBFLI3, BSGBIHI3, BSGCHLI3, BSGCYPI3, BSGCZEI3, BSGDEUI3, 
                    BSGDNKI3, BSGDNWI3, BSGESPI3, BSGFINI3, BSGFRAI3, BSGGRCI3, BSGHRVI3, BSGHUNI3,
                    BSGITAI3, BSGKAZI3, BSGKORI3, BSGLUXI3, BSGLVAI3, BSGMLTI3, BSGNLDI3, BSGNORI3, 
                    BSGOMNI3, BSGPRTI3, BSGROUI3, BSGSRBI3, BSGSVKI3, BSGSVNI3, BSGSWEI3, BSGTWNI3,
                    BSGURYI3, BSGUSAI3, BSGXKXI3)

# Seleccionar variables que componen la autoeficacia digital

icils_2023_proc <- icils_2023 %>%
  select(CNTRY, IDSCHOOL, IS3G24A, IS3G24B, IS3G24C, IS3G24D, IS3G24E, IS3G24F, IS3G24G, IS3G24H, IS3G24I, IS3G24J,
         IS3G24K, IS3G24L, IS3G24M)

# Revisar las categorias de respuesta para poder agrupar los NA
sjlabelled::get_labels(icils_2023_proc)

# Agrupar NA

icils_2023_proc$IS3G24A <- recode(icils_2023_proc$IS3G24A, "c(8,9)=NA")
icils_2023_proc$IS3G24B <- recode(icils_2023_proc$IS3G24B, "c(8,9)=NA")
icils_2023_proc$IS3G24C <- recode(icils_2023_proc$IS3G24C, "c(8,9)=NA")
icils_2023_proc$IS3G24D <- recode(icils_2023_proc$IS3G24D, "c(8,9)=NA")
icils_2023_proc$IS3G24E <- recode(icils_2023_proc$IS3G24E, "c(8,9)=NA")
icils_2023_proc$IS3G24F <- recode(icils_2023_proc$IS3G24F, "c(8,9)=NA")
icils_2023_proc$IS3G24G <- recode(icils_2023_proc$IS3G24G, "c(8,9)=NA")
icils_2023_proc$IS3G24H <- recode(icils_2023_proc$IS3G24H, "c(8,9)=NA")
icils_2023_proc$IS3G24I <- recode(icils_2023_proc$IS3G24I, "c(8,9)=NA")
icils_2023_proc$IS3G24J <- recode(icils_2023_proc$IS3G24J, "c(8,9)=NA")
icils_2023_proc$IS3G24K <- recode(icils_2023_proc$IS3G24K, "c(8,9)=NA")
icils_2023_proc$IS3G24L <- recode(icils_2023_proc$IS3G24L, "c(8,9)=NA")
icils_2023_proc$IS3G24M <- recode(icils_2023_proc$IS3G24M, "c(8,9)=NA")

saveRDS(icils_2023_proc, "proc/icils_2023_proc.rds")

# ---- Base de autoeficacia digital PISA 2022 ----

pisa_2022 <- read_sav("data/raw_data/pisa_2022.sav")

pisa_2022_proc <- pisa_2022 %>%
  filter(Option_ICTQ == 1) %>%
  select(CNT, CNTRYID, CNTSCHID, IC183Q01JA, IC183Q02JA, IC183Q03JA, IC183Q04JA,
         IC183Q05JA, IC183Q07JA, IC183Q08JA, IC183Q09JA, IC183Q10JA,
         IC183Q12JA, IC183Q13JA, IC183Q14JA, IC183Q15JA, IC183Q16JA)

