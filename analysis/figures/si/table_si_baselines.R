source("analysis/figures/theme.R")

library(dplyr)
library(stringr)
library(tidyr)
library(knitr)
library(kableExtra)

cat("=== SI Table: Baseline Levels per Petition ===\n")

# ---- Load data ---------------------------------------------------------------

s1 <- readRDS("output/results/study1_clean.rds")
s2 <- readRDS("output/results/study2_clean.rds")

# ---- Short petition labels ---------------------------------------------------

shorten_petition <- function(x) {
  case_when(
    str_detect(x, "Child Poverty")          ~ "Stop Rising Child Poverty",
    str_detect(x, "mother pigs")            ~ "End Suffering for Mother Pigs",
    str_detect(x, "Facial Recognition")     ~ "Oppose Facial Recognition",
    str_detect(x, "nuclear war")            ~ "Stop the Risk of Nuclear War",
    str_detect(x, "parliament")             ~ "Seats in Parliament",
    str_detect(x, "asylum")                 ~ "Lift Ban on Work for Asylum Seekers",
    str_detect(x, "trees")                  ~ "Urge Govts to Protect Trees",
    str_detect(x, "climate crisis")         ~ "Stop Banks Fuelling Climate Crisis",
    str_detect(x, "economic inequality")    ~ "Reducing Economic Inequality",
    str_detect(x, "disabilities")           ~ "Continuing Care for Children",
    TRUE                                    ~ x
  )
}

# ---- Study 1 baselines -------------------------------------------------------
# Attitude DVs: pre-treatment means (all participants, pre-randomisation)
# Behavioral DVs: control-group means

s1_ctrl <- s1 %>% filter(treatment == "control")

s1_pre <- s1 %>%
  group_by(petition_title) %>%
  summarise(
    `Pre. agreement\n(M, SD)`       = sprintf("%.2f (%.2f)",
                                               mean(pretreatment_agreement, na.rm = TRUE),
                                               sd(pretreatment_agreement,   na.rm = TRUE)),
    `Pre. org. opinion\n(M, SD)`    = sprintf("%.2f (%.2f)",
                                               mean(pretreatment_organization_opinion, na.rm = TRUE),
                                               sd(pretreatment_organization_opinion,   na.rm = TRUE)),
    .groups = "drop"
  )

s1_beh <- s1_ctrl %>%
  group_by(petition_title) %>%
  summarise(
    `Sign petition\n(ctrl %)`       = sprintf("%.1f", mean(signed_petition,        na.rm = TRUE) * 100),
    `Bonus donation\n(ctrl M)`      = sprintf("%.2f", mean(donate_proportion,      na.rm = TRUE)),
    `Money effort\n(ctrl M)`        = sprintf("%.2f", mean(extra_task_clicks / 100, na.rm = TRUE)),
    `Choose org\n(ctrl %)`          = sprintf("%.1f", mean(not_switch_organization, na.rm = TRUE) * 100),
    .groups = "drop"
  )

s1_table <- s1_pre %>%
  left_join(s1_beh, by = "petition_title") %>%
  mutate(Petition = shorten_petition(petition_title)) %>%
  arrange(Petition) %>%
  dplyr::select(Petition, everything(), -petition_title)

# ---- Study 2 baselines -------------------------------------------------------

s2_ctrl <- s2 %>% filter(treatment == "control")

s2_pre <- s2 %>%
  group_by(petition_title) %>%
  summarise(
    `Pre. agreement\n(M, SD)`       = sprintf("%.2f (%.2f)",
                                               mean(pretreatment_agreement, na.rm = TRUE),
                                               sd(pretreatment_agreement,   na.rm = TRUE)),
    `Pre. org. opinion\n(M, SD)`    = sprintf("%.2f (%.2f)",
                                               mean(pretreatment_organization_opinion, na.rm = TRUE),
                                               sd(pretreatment_organization_opinion,   na.rm = TRUE)),
    .groups = "drop"
  )

s2_beh <- s2_ctrl %>%
  group_by(petition_title) %>%
  summarise(
    `Sign petition\n(ctrl %)`       = sprintf("%.1f", mean(signed_petition,   na.rm = TRUE) * 100),
    `Donate to org\n(ctrl %)`       = sprintf("%.1f", mean(chose_assigned_org, na.rm = TRUE) * 100),
    .groups = "drop"
  )

s2_table <- s2_pre %>%
  left_join(s2_beh, by = "petition_title") %>%
  mutate(Petition = shorten_petition(petition_title)) %>%
  arrange(Petition) %>%
  dplyr::select(Petition, everything(), -petition_title)

# ---- Table body builder (strips float wrapper for minipage use) --------------

make_table_body <- function(df) {
  nrows <- nrow(df)

  # Escape % before linebreak() since escape = FALSE bypasses kable's escaping
  colnames(df) <- gsub("%", "\\\\%", colnames(df))
  # Convert \n in column names to two-line LaTeX headers via \makecell
  colnames(df) <- linebreak(colnames(df), align = "c")

  tbl <- df %>%
    kable(
      format   = "latex",
      booktabs = TRUE,
      linesep  = "",
      align    = c("l", rep("c", ncol(df) - 1)),
      escape   = FALSE
    ) %>%
    kable_styling(
      latex_options = c("striped"),
      stripe_color  = "gray!15",
      full_width    = FALSE,
      font_size     = 10
    ) %>%
    row_spec(0, bold = TRUE) %>%
    as.character()

  tbl <- gsub("\\\\begin\\{table\\}(\\[.*?\\])?\\s*\n", "", tbl)
  tbl <- gsub("\\\\centering\\s*\n", "", tbl)
  tbl <- gsub("\\\\end\\{table\\}\\s*\n?", "", tbl)
  tbl <- gsub("\\\\centering(\\\\begingroup)", "\\1", tbl)
  tbl
}

s1_body <- make_table_body(s1_table)
s2_body <- make_table_body(s2_table)

# ---- Write .tex fragments ----------------------------------------------------

# Wrap in landscape + resizebox; pdflscape and graphicx are already loaded
# in supplementary_materials.tex so \begin{landscape} works on \input{}.

write_tex <- function(body, out_tex) {
  tex_content <- paste0(
    "\\renewcommand{\\arraystretch}{1.2}\n",
    "\\begin{landscape}\n",
    "\\begin{adjustbox}{max width=\\linewidth, center}\n",
    body, "\n",
    "\\end{adjustbox}\n",
    "\\end{landscape}\n"
  )
  writeLines(tex_content, out_tex)
  cat(sprintf("  Saved %s\n", out_tex))
}

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

write_tex(s1_body, "output/tables/table_si_baselines_study1.tex")
write_tex(s2_body, "output/tables/table_si_baselines_study2.tex")
