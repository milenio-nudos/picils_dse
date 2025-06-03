# ---- Descriptivos autoeficacia digital ICILS ----

library(knitr)
knitr::opts_chunk$set(echo = TRUE, include = TRUE, warning = FALSE, message = FALSE)

table_format <- if(is_html_output()) {
  "html"
} else if(is_latex_output()) {
  "latex"
}
table_format2 <- if(is_html_output()) {
  T
} else if(is_latex_output()) {
  F
}

options(kableExtra.html.bsTable = T)
options(knitr.kable.NA = "")


pacman::p_load(
  dplyr, haven, sjlabelled,  psych,  purrr,  tidyr,  sjPlot,  ggplot2, 
  parameters,  table1,  car,  beeswarm,  lme4, scales, ggrepel, corrplot,
  ggtext, patchwork)

options(scipen = 999)
rm(list = ls())

icils <- readRDS("data/proc_data/icils23_proc.rds")

a <- icils %>% 
  select(IS3G24A, IS3G24B, IS3G24C, IS3G24E, IS3G24F,
         IS3G24G, IS3G24H, IS3G24I, IS3G24J, IS3G24M) %>% 
  sjPlot::plot_likert(geom.colors = "Reds",
                      title = c("General (n = 135.615)"),
                      sort.frq = "pos.desc",
                      geom.size = 0.8,
                      axis.labels = c("edit_photos", "edit_text", "search_info", 
                                      "change_settings", "create_pres", "upload_media", "insert_image", "install_app", "eval_info", "find_source"),
                      catcount = 4,
                      values  =  "sum.outside",
                      reverse.colors = F,
                      reverse.scale = T,
                      show.n = FALSE,
                      show.prc.sign = T
  ) +
  ggplot2::theme(legend.position = "none")

a

b <- icils %>% 
  select(IS3G24D, IS3G24K, IS3G24L) %>% 
  sjPlot::plot_likert(geom.colors = "Reds",
                      title = c("Specialized"),
                      sort.frq = "pos.desc",
                      geom.size = 0.8,
                      axis.labels = c("build_page", "write_code", "devel_program"),
                      catcount = 4,
                      values  =  "sum.outside",
                      reverse.colors = F,
                      reverse.scale = T,
                      show.n = FALSE,
                      show.prc.sign = T
  ) +
  ggplot2::theme(legend.position = "bottom")

b

liker_icils <- a/b
liker_icils


# Bivariados

M <- psych::polychoric(icils[c(3:15)])

P <- cor(icils[c(3:15)], method = "pearson")

diag(M$rho) <- NA

diag(P) <- NA

rownames(M$rho) <- c("edit_photos (A)",
                     "edit_text (B)",
                     "search_info (C)",
                     "build_page (D)",
                     "change_settings (E)",
                     "create_pres (F)",
                     "upload_media (G)",
                     "insert_image (H)",
                     "install_app (I)",
                     "eval_info (J)",
                     "write_code (K)",
                     "devel_program (L)",
                     "find_source (M)")

#set Column names of the matrix
colnames(M$rho) <-c("(A)", "(B)","(C)","(D)","(E)","(F)","(G)",
                    "(H)","(I)", "(J)", "(K)", "(L)", "(M)")

rownames(P) <- c("edit_photos (A)",
                 "edit_text (B)",
                 "search_info (C)",
                 "build_page (D)",
                 "change_settings (E)",
                 "create_pres (F)",
                 "upload_media (G)",
                 "insert_image (H)",
                 "install_app (I)",
                 "eval_info (J)",
                 "write_code (K)",
                 "devel_program (L)",
                 "find_source (M)")

#set Column names of the matrix
colnames(P) <-c("(A)", "(B)","(C)","(D)","(E)","(F)","(G)",
                "(H)","(I)", "(J)", "(K)", "(L)", "(M)")

testp <- cor.mtest(M$rho, conf.level = 0.95)

#Plot the matrix using corrplot
corrplot::corrplot(M$rho,
                   method = "color",
                   addCoef.col = "black",
                   type = "upper",
                   tl.col = "black",
                   col = colorRampPalette(c("#E16462", "white", "#0D0887"))(12),
                   bg = "white",
                   na.label = "-") 

# ---- Descriptivos Autoeficacia digital PISA ----

pisa <- readRDS("data/proc_data/pisa22_proc.rds")

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


