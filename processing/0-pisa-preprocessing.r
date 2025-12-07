# Because of the GitHub size limit, preprocessing of the PISA database was conducted. The code is bellow.
# This code would be reproducible if you download the database in SPSS format from the OECD webpage 
# (https://www.oecd.org/en/data/datasets/pisa-2022-database.html#data) and put it in the raw data folder.

pacman::p_load(dplyr, haven)
options(scipen = 999)
rm(list = ls())

pisa22 <- read_sav("input/data/raw_data/CY08MSP_STU_QQQ.SAV")

#Filter only countries that have applied the ICT questionnarie and the ICT and sociodemographic variables. 
pisa22 <- pisa22 %>%
  filter(Option_ICTQ == 1) %>%
  select(CNT, CNTRYID, CNTSCHID, CNTSTUID, W_FSTUWT, SENWT, ESCS, SDLEFF, ST001D01T, ST004D01T, starts_with("ST322"), ST337Q08JA, ST338Q08JA, starts_with("IC"), starts_with("ST355"))

saveRDS(pisa22, "input/data/proc_data/pisa22ict.rds")
