library(pacman)
pacman::p_load(dplyr, haven)
rm(list = ls())
pisa22 <- readRDS("input/data/proc_data/pisa22ict.rds")
pisa22_proc <- pisa22 %>%
  select(CNT, CNTRYID, CNTSCHID, sex = ST004D01T, IC183Q01JA, IC183Q02JA, IC183Q03JA, IC183Q04JA,
         IC183Q05JA, IC183Q07JA, IC183Q08JA, IC183Q09JA, IC183Q10JA,
         IC183Q12JA, IC183Q13JA, IC183Q14JA, IC183Q15JA, IC183Q16JA)

#Elimiar respuesta no sabe
pisa22_proc$IC183Q01JA[pisa22_proc$IC183Q01JA==5]<-NA
pisa22_proc$IC183Q02JA[pisa22_proc$IC183Q02JA==5]<-NA
pisa22_proc$IC183Q03JA[pisa22_proc$IC183Q03JA==5]<-NA
pisa22_proc$IC183Q04JA[pisa22_proc$IC183Q04JA==5]<-NA
pisa22_proc$IC183Q05JA[pisa22_proc$IC183Q05JA==5]<-NA
pisa22_proc$IC183Q07JA[pisa22_proc$IC183Q07JA==5]<-NA
pisa22_proc$IC183Q08JA[pisa22_proc$IC183Q08JA==5]<-NA
pisa22_proc$IC183Q09JA[pisa22_proc$IC183Q09JA==5]<-NA
pisa22_proc$IC183Q10JA[pisa22_proc$IC183Q10JA==5]<-NA
pisa22_proc$IC183Q12JA[pisa22_proc$IC183Q12JA==5]<-NA
pisa22_proc$IC183Q13JA[pisa22_proc$IC183Q13JA==5]<-NA
pisa22_proc$IC183Q14JA[pisa22_proc$IC183Q14JA==5]<-NA
pisa22_proc$IC183Q15JA[pisa22_proc$IC183Q15JA==5]<-NA
pisa22_proc$IC183Q16JA[pisa22_proc$IC183Q16JA==5]<-NA

pisa22_proc <- pisa22_proc %>% rename("search_info"=IC183Q01JA,
                                      "evaluate_info"=IC183Q02JA,
                                      "share_content"=IC183Q03JA,
                                      "pair_collab"=IC183Q04JA,
                                      "explain_content"=IC183Q05JA,
                                      "write_text"=IC183Q07JA,
                                      "collect_data"=IC183Q08JA,
                                      "create_media"=IC183Q09JA,
                                      "develop_webpage"=IC183Q10JA,
                                      "change_settings"=IC183Q12JA,
                                      "identify_app"=IC183Q13JA,
                                      "programming"=IC183Q14JA,
                                      "identify_error"=IC183Q15JA,
                                      "logical_solution"=IC183Q16JA)

pisa22_proc$sex <- haven::as_factor(pisa22_proc$sex)

saveRDS(pisa22_proc, "input/data/proc_data/pisa22_proc.rds")

