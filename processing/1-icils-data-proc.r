# ---- ICILS 2023 digital self-efficacy dataset ----

# Load packages and preliminary steps

pacman::p_load(here, dplyr, haven,sjlabelled, psych, purrr, tidyverse, sjmisc, sjPlot, ggplot2, parameters, table1, car, beeswarm, lme4,
               labelled)
options(scipen = 999)
rm(list = ls())

# Load datasets 

archivos_bsg <- list.files(
  path = "./input/data/raw_data/icils_countries", # Current directory (change if needed)
  pattern = "^BSG.*\\.Rdata$",
  full.names = TRUE
)

# List found files
print(archivos_bsg)

# Load all found files
for (archivo in archivos_bsg) {
  load(archivo)
  cat("Cargado:", archivo, "\n")
}

# Repeat until all are loaded

# Bind datasets

icils23 <- rbind(BSGAUTI3, BSGAZEI3, BSGBFLI3, BSGBIHI3, BSGCHLI3, BSGCYPI3, BSGCZEI3, BSGDEUI3, 
                    BSGDNKI3, BSGDNWI3, BSGESPI3, BSGFINI3, BSGFRAI3, BSGGRCI3, BSGHRVI3, BSGHUNI3,
                    BSGITAI3, BSGKAZI3, BSGKORI3, BSGLUXI3, BSGLVAI3, BSGMLTI3, BSGNLDI3, BSGNORI3, 
                    BSGOMNI3, BSGPRTI3, BSGROUI3, BSGSRBI3, BSGSVKI3, BSGSVNI3, BSGSWEI3, BSGTWNI3,
                    BSGURYI3, BSGUSAI3, BSGXKXI3)

# Select variables used in digital self-efficacy

icils23_proc <- icils23 %>%
  select(CNTRY, IDSCHOOL, TOTWGTS, JKZONES, JKREPS, starts_with("SRWGT"), IS3G02, IS3G24A, IS3G24B, IS3G24C, IS3G24D, IS3G24E, IS3G24F, IS3G24G, IS3G24H, IS3G24I, IS3G24J,
         IS3G24K, IS3G24L, IS3G24M, PV1CIL)

# Inspect response categories to group NAs
sjlabelled::get_labels(icils23_proc)

# Group missing codes as NA

icils23_proc$IS3G02 <- recode(icils23_proc$IS3G02, "c(8,9)=NA")
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

# Value recoding

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

# Relabel value labels

recodificar <- function(data, vars_prefix = "IS3G24") {
  # Define common labels
  etiquetas <- c(
    "I do not think I could do this" = 1,
    "I have never done this, but I could work out how to do" = 2,
    "Moderatelly well" = 3,
    "Very well" = 4
  )
  
  # Identify variables matching the prefix
  vars_a_recodificar <- grep(paste0("^", vars_prefix), names(data), value = TRUE)
  
  # Apply labels to each variable
  for (var in vars_a_recodificar) {
    data[[var]] <- set_labels(data[[var]], labels = etiquetas)
  }
  
  return(data)
}

icils23_proc <- recodificar(icils23_proc)

# Get current labels
current_labels <- val_labels(icils23_proc$IS3G02)

# Drop labels for values 8 and 9 (numeric)
new_labels <- current_labels[!current_labels %in% c(8, 9)]

# Reassign cleaned labels
val_labels(icils23_proc$IS3G02) <- new_labels

# Check labels were recoded correctly
frq(icils23_proc$IS3G02)
frq(icils23_proc$IS3G24I)

icils23_proc <- icils23_proc|>
  rename(
    search_info = IS3G24C,
    source_info = IS3G24M,
    evaluate_info = IS3G24J,
    install_app = IS3G24I,
    share_content = IS3G24G,
    write_text = IS3G24B,
    develop_webpage = IS3G24D,
    create_media = IS3G24F,
    insert_image = IS3G24H,
    edit_image = IS3G24A,
    programming = IS3G24K,
    visual_coding = IS3G24L,
    change_settings = IS3G24E,
    sex = IS3G02
  )

icils23_proc$sex <- haven::as_factor(icils23_proc$sex)

icils23_proc <- icils23_proc |>
  group_by(CNTRY) |>
  mutate(SENWT = TOTWGTS / sum(TOTWGTS) * 5000) |>
  ungroup()

saveRDS(icils23_proc, "input/data/proc_data/icils23_proc.rds")
