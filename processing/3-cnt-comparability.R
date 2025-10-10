library(pacman)
p_load(dplyr, haven)
rm(list = ls())

pisa <- readRDS("./input/data/proc_data/pisa22_proc.rds")
icils <- readRDS("./input/data/proc_data/icils23_proc.rds")

unique(pisa$CNT)
unique(icils$CNTRY)

icils <- icils %>%
  mutate(CNTRY = case_when(
    as.character(CNTRY) == "BFL" ~ "BEL",
    as.character(CNTRY) == "DNW" ~ "DEU",
    as.character(CNTRY) == "XKX" ~ "KSV",
    TRUE ~ as.character(CNTRY)
  ) %>% as_factor() %>% as.character())

# Verificar los cambios
unique(icils$CNTRY)

paises_comunes <- intersect(unique(pisa$CNT), unique(icils$CNTRY))
icils_filtrado <- icils %>% filter(CNTRY %in% paises_comunes)
pisa_filtrado <- pisa %>% filter(CNT %in% paises_comunes)

saveRDS(icils_filtrado, "./input/data/proc_data/icils_comparable.rds")
saveRDS(pisa_filtrado, "./input/data/proc_data/pisa_comparable.rds")
