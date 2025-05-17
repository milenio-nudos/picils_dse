# ---- Primeras observaciones de autoeficacia digital en ICILS 2023 ----

pacman::p_load(
  dplyr, 
  haven,
  sjlabelled, 
  psych, 
  purrr, 
  tidyr, 
  sjPlot, 
  ggplot2, 
  parameters, 
  table1, 
  car, 
  beeswarm, 
  lme4,
  scales,
  ggrepel
  )
options(scipen = 999)
rm(list = ls())

# Cargar base de datos

icils_2023 <- readRDS("data/proc_data/icils_2023_proc.rds")

# Visualización de NA por variablwe / país.

# Variables a analizar

item_cols <- c("IS3G24A", "IS3G24B", "IS3G24C", "IS3G24D", "IS3G24E", 
               "IS3G24F", "IS3G24G", "IS3G24H", "IS3G24I", "IS3G24J", 
               "IS3G24K", "IS3G24L", "IS3G24M")

# Convertimos CNTRY a factor

icils_2023$CNTRY <- haven::as_factor(icils_2023$CNTRY)

# Convertimos el data frame en formato long para calcular más eficientemente la proporción de NAs

data_long <- icils_2023 %>%
  select(CNTRY, all_of(item_cols)) %>%
  pivot_longer(cols = all_of(item_cols),
               names_to = "Item",
               values_to = "Valor")

# Cálculo de % NAs por item por país

prop_na_pais_item <- data_long %>%
  group_by(CNTRY, Item) %>%
  summarise(
    Total = n(),
    NAs = sum(is.na(Valor)),
    Prop_NA = NAs / Total,
    .groups = "drop"
  )

# Cálculo de % NAs por item en general

prop_na_general_item <- data_long %>%
  group_by(Item) %>%
  summarise(
    Total_General = n(),
    NAs_General = sum(is.na(Valor)),
    Prop_NA_General = NAs_General / Total_General,
    .groups = "drop"
  )

# Detectamos que países tienen más NAs para incluirle las labels

data_for_labels <- prop_na_pais_item %>%
  group_by(Item) %>%
  slice_max(order_by = Prop_NA, n = 3, with_ties = FALSE) %>% # with_ties=FALSE para asegurar solo 3
  ungroup()

# Convertimos Item a factor en el orden correcto para la visualización

prop_na_pais_item$Item <- factor(prop_na_pais_item$Item, levels = rev(item_cols))
prop_na_general_item$Item <- factor(prop_na_general_item$Item, levels = rev(item_cols))
data_for_labels$Item <- factor(data_for_labels$Item, levels = rev(item_cols))

# Creamos el gráfico

grafico_cleveland_nas <- ggplot(prop_na_pais_item, aes(x = Prop_NA, y = Item)) +
  # Puntos para cada país
  geom_point(aes(color = "País"), alpha = 0.7, size = 2) +
  
  # Punto para el promedio general del item
  geom_point(data = prop_na_general_item, 
             aes(x = Prop_NA_General, y = Item, color = "Promedio General (Item)"), 
             size = 4, shape = 18) + # Usamos una forma diferente (diamante)
  
  # Etiquetas para los 3 países con más NAs por item
  # Pasamos 'data_for_labels' al argumento 'data' de esta capa específica
  geom_text_repel(data = data_for_labels,
                  aes(label = CNTRY), # Usamos directamente la columna CNTRY
                  size = 2.8,    
                  max.overlaps = Inf, 
                  box.padding = 0.4, 
                  point.padding = 0.2,
                  segment.color = 'grey50', 
                  segment.size = 0.3,
                  min.segment.length = 0 
  ) +
  
  # Escalas y etiquetas
  scale_x_continuous(labels = percent_format(accuracy = 1), 
                     name = "% de NAs") +
  scale_y_discrete(name = "Ítem de Autoeficacia Digital") +
  scale_color_manual(name = "Referencia:",
                     values = c("País" = "steelblue", "Promedio General (Item)" = "red"),
                     guide = guide_legend(override.aes = list(shape = c(16, 18), size = c(2,4)))) + 
  
  # Título y tema
  labs(title = "Proporción de NAs por País en Items de Autoeficacia Digital",
       subtitle = "Se etiquetan los 3 países con mayor % de NAs para cada ítem.",
       caption = "Datos: ICILS 2023") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_text(size=9),
    axis.text.x = element_text(size=9)
  )

print(grafico_cleveland_nas)

# ---- Por hacer ----

# sintaxis de sumatoria de NAs por pais

# ---- Pasos a seguir ----

# modelo general 

# modelos de medicion para cada pais

# sacar modelos configural estimados para cada pais x separado

# visualizar loadings x pais

# invarianza (metrico)