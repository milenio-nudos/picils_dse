# ---- Base de autoeficacia digital PISA 2022 ----

pisa22 <- readRDS("input/data/proc_data/pisa22ict.rds")

pisa22_proc <- pisa22 %>%
  select(CNT, CNTRYID, CNTSCHID, sex = ST004D01T, IC183Q01JA, IC183Q02JA, IC183Q03JA, IC183Q04JA,
         IC183Q05JA, IC183Q07JA, IC183Q08JA, IC183Q09JA, IC183Q10JA,
         IC183Q12JA, IC183Q13JA, IC183Q14JA, IC183Q15JA, IC183Q16JA)

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

saveRDS(pisa22_proc, "input/data/proc_data/pisa22_proc.rds")

