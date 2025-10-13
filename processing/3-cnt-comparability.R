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

# Aplicamos labels de PISA a ICILS country iso code

paises_comunes <- intersect(unique(pisa$CNT), unique(icils$CNTRY))
icils_filtrado <- icils %>% filter(CNTRY %in% paises_comunes)
pisa_filtrado <- pisa %>% filter(CNT %in% paises_comunes)

country_labels <- pisa_filtrado %>%
  select(CNT) %>%
  distinct() %>%
  mutate(
    code = as.character(CNT),
    label = as.character(haven::as_factor(CNT))
  ) %>%
  select(code, label)

icils_filtrado <- icils_filtrado %>%
  mutate(
    CNTRY = haven::labelled(
      as.character(CNTRY),
      labels = setNames(country_labels$code, country_labels$label)
    )
  )

print(head(haven::as_factor(icils_filtrado$CNTRY)))


saveRDS(icils_filtrado, "./input/data/proc_data/icils_comparable.rds")
saveRDS(pisa_filtrado, "./input/data/proc_data/pisa_comparable.rds")
