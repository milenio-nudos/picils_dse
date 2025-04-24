# ---- Primeras observaciones de autoeficacia digital en ICILS 2023 ----

pacman::p_load(dplyr, haven,sjlabelled, psych, purrr, tidyr, sjPlot, ggplot2, parameters, table1, car, beeswarm, lme4)
options(scipen = 999)
rm(list = ls())

# Cargar base de datos

icils_2023 <- readRDS("../proc/icils_2023_proc.rds")

summary(icils_2023)

# ---- Por hacer ----

# sintaxis de sumatoria de NAs por pais

# ---- Pasos a seguir ----

# modelo general 

# modelos de medicion para cada pais

# sacar modelos configural estimados para cada pais x separado

# visualizar loadings x pais

# invarianza (metrico)