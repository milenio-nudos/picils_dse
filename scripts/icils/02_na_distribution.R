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
  ggrepel,
  corrplot,
  ggtext
  )
options(scipen = 999)
rm(list = ls())

# Cargar base de datos

icils_2023 <- readRDS("data/proc_data/icils_2023_proc.rds")

# ---- Visualización de NA por variable / país ----

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


# Redefinimos los niveles del factor con etiquetas en color

colored_labels <- setNames(item_cols, item_cols)  # crea una lista con nombres originales

# Aplicamos color solo a los ítems seleccionados

highlight_items <- c("IS3G24D", "IS3G24K", "IS3G24L")
for (item in highlight_items) {
  colored_labels[item] <- paste0("<span style='color:#D73027;'>", item, "</span>")  # rojo oscuro
}

# Convertimos Item a factor en el orden correcto para la visualización

prop_na_pais_item$Item <- factor(prop_na_pais_item$Item, levels = rev(item_cols), labels = rev(colored_labels))
prop_na_general_item$Item <- factor(prop_na_general_item$Item, levels = rev(item_cols), labels = rev(colored_labels))
data_for_labels$Item <- factor(data_for_labels$Item, levels = rev(item_cols), labels = rev(colored_labels))

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
                     name = "% of Missings") +
  scale_y_discrete(name = "Items") +
  scale_color_manual(name = "Reference:",
                     values = c("País" = "steelblue", "Promedio General (Item)" = "red"),
                     guide = guide_legend(override.aes = list(shape = c(16, 18), size = c(2,4)))) + 
  
  # Título y tema
  labs(title = "Missing values proportion by country around DSE ítems",
       subtitle = "3 most % countries with more missing values are labelled",
       caption = "Data: ICILS 2023
       Corr CIL y mean % NA in above battery: -0.63") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = ggtext::element_markdown(size = 9),
    axis.text.x = element_text(size=9)
  )

print(grafico_cleveland_nas)

# ---- Correlación entre CIL, Selfeff y NAs ----

# Cálculo de promedios CIL, Selfefficacy y prop_NA por país

# Selfeff

selfeff_country_mean <- icils_2023 %>%
  select(CNTRY, all_of(item_cols)) %>%
  pivot_longer(cols = all_of(item_cols),
               names_to = "selfeff_variable",  # Nombre temporal para las columnas de items
               values_to = "selfeff_value") %>% # Nombre temporal para los valores
  group_by(CNTRY) %>%
  summarise(
    selfeff_mean = mean(selfeff_value, na.rm = TRUE), # na.rm = TRUE para ignorar NAs en el cálculo del promedio
    .groups = "drop"  # Elimina la agrupación después de summarise
  )

# CIL

cil_country_mean <- icils_2023 %>%
  group_by(CNTRY) %>%
  summarise(
    CIL_mean = mean(PV1CIL, na.rm = TRUE), # na.rm = TRUE para ignorar NAs
    .groups = "drop"
  )

# Prop_NA

NAs_country_mean <- prop_na_pais_item %>%
  group_by(CNTRY) %>%
  summarise(
    NA_mean = mean(Prop_NA), # na.rm = TRUE para ignorar NAs
    .groups = "drop"
  )

# Unimos dataframes CIL y Selfeff

country_level_summary <- selfeff_country_mean %>%
  left_join(cil_country_mean, by = "CNTRY")

# Unimos dataframe NAs con el anterior

corr_data <- left_join(
  NAs_country_mean,
  country_level_summary,
  by = "CNTRY"
)
# ---- Matriz de correlaciones entre CIL, Selfeff y NAs ----

# Seleccionar y renombrar variables
corr_data <- corr_data %>%
  select(
    NA_mean,
    selfeff_mean,
    CIL_mean
  ) %>%
  rename(
    `Promedio % NAs` = NA_mean,
    `Promedio Autoeficacia` = selfeff_mean,
    `Promedio Puntaje CIL` = CIL_mean
  )

# Calcular la matriz de correlación
matriz_correlacion <- cor(corr_data, use = "pairwise.complete.obs")

# Generar el gráfico
corrplot(matriz_correlacion,
         method = "color",        
         type = "lower",           
         order = "original",       
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = FALSE,
         cl.pos = "r",
         title = "Matriz de Correlaciones entre Indicadores Agregados",
         mar = c(0, 0, 2, 0)
)

# ---- Pasos a seguir ----

# modelo general 

# modelos de medicion para cada pais

# sacar modelos configural estimados para cada pais x separado

# visualizar loadings x pais

# invarianza (metrico)