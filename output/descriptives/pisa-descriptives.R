 # ---- Descriptivos Autoeficacia digital PISA ----

pisa <- readRDS("../input/data/proc_data/pisa22_proc.rds")

pisa <- pisa %>%
  select(IC183Q01JA, IC183Q02JA, IC183Q03JA, IC183Q04JA, IC183Q05JA, 
         IC183Q07JA, IC183Q08JA, IC183Q09JA, IC183Q10JA, IC183Q12JA, 
         IC183Q13JA, IC183Q14JA, IC183Q15JA, IC183Q16JA)

c <- pisa %>% 
  select(IC183Q01JA, IC183Q02JA, IC183Q03JA, IC183Q04JA, IC183Q05JA, 
         IC183Q07JA, IC183Q08JA, IC183Q09JA, IC183Q10JA, IC183Q12JA, 
         IC183Q13JA, IC183Q14JA, IC183Q15JA, IC183Q16JA ) %>% 
  sjPlot::plot_likert(geom.colors = "Reds",
                      title = c("Digital self-efficacy (n = 393.607)"),
                      sort.frq = "pos.desc",
                      geom.size = 0.8,
                      axis.labels = c("search_info", "asses_info", "share_info", "pair_collab",
                                      "explain_howtoshare", "edit_text", "collect_data", "create_pres",
                                      "page_web", "change_settings", "select_app", "create_program", 
                                      "identify_error", "logical_solution"),
                      catcount = 4,
                      values  =  "sum.outside",
                      reverse.colors = F,
                      reverse.scale = T,
                      show.n = FALSE,
                      show.prc.sign = T
  ) +
  ggplot2::theme(legend.position = "bottom")

c

# Bivariados

Mp <- psych::polychoric(pisa[c(1:14)])

Pp <- cor(pisa[c(1:14)], method = "pearson")

diag(Mp$rho) <- NA

diag(Pp) <- NA

rownames(Mp$rho) <- c("search_info (A)",
                      "asses_info (B)",
                      "share_info (C)",
                      "pair_collab (D)",
                      "explain_howtoshare (E)",
                      "edit_text (F)",
                      "collect_data (G)",
                      "create_pres (H)",
                      "page_web (I)", 
                      "change_settings (J)",
                      "select_app (K)",
                      "create_program (L)", 
                      "identify_error (M)",
                      "logical_solution (N)")

#set Column names of the matrix
colnames(Mp$rho) <-c("(A)", "(B)","(C)","(D)","(E)","(F)","(G)",
                    "(H)","(I)", "(J)", "(K)", "(L)", "(M)", "(N)")

rownames(Pp) <- c("search_info (A)",
                  "asses_info (B)",
                  "share_info (C)",
                  "pair_collab (D)",
                  "explain_howtoshare (E)",
                  "edit_text (F)",
                  "collect_data (G)",
                  "create_pres (H)",
                  "page_web (I)", 
                  "change_settings (J)",
                  "select_app (K)",
                  "create_program (L)", 
                  "identify_error (M)",
                  "logical_solution (N)")

#set Column names of the matrix
colnames(Pp) <-c("(A)", "(B)","(C)","(D)","(E)","(F)","(G)",
                "(H)","(I)", "(J)", "(K)", "(L)", "(M)", "(N)")

testpp <- cor.mtest(Mp$rho, conf.level = 0.95)

#Plot the matrix using corrplot
corrplot::corrplot(Mp$rho,
                   method = "color",
                   addCoef.col = "black",
                   type = "upper",
                   tl.col = "black",
                   col = colorRampPalette(c("#E16462", "white", "#0D0887"))(12),
                   bg = "white",
                   na.label = "-")


