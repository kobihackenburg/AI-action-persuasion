source("analysis/figures/theme.R")

library(dplyr)
library(stringr)
library(knitr)
library(kableExtra)

cat("=== SI Table: Sample Sizes per Treatment Group ===\n")

# ---- Load processed data -----------------------------------------------------

s1 <- readRDS("output/results/study1_clean.rds")
s2 <- readRDS("output/results/study2_clean.rds")

# ---- Study 1: N per conversation type ----------------------------------------

s1_order <- c("Control", "Petition-Information", "Organisation-Information", "Random")

s1_counts <- s1 %>%
  mutate(
    condition = case_when(
      str_detect(conversation_type, "^control") ~ "Control",
      conversation_type == "petition"           ~ "Petition-Information",
      conversation_type == "organization"       ~ "Organisation-Information",
      conversation_type == "random"             ~ "Random",
      TRUE                                      ~ conversation_type
    )
  ) %>%
  count(condition, name = "N") %>%
  arrange(match(condition, s1_order)) %>%
  mutate(`%` = sprintf("%.1f", N / sum(N) * 100))

s1_total <- tibble(condition = "Total", N = sum(s1_counts$N), `%` = "100.0")

s1_table <- bind_rows(s1_counts, s1_total) %>%
  rename(Condition = condition)

# ---- Study 2: N per persuasion strategy + control ----------------------------

s2_order <- c(
  "Control",
  "Emotional Activation",
  "Implementation Intentions",
  "Identity Labeling",
  "Commitment Escalation",
  "Anticipated Regret",
  "Mega (All Combined)",
  "Info: Impact Efficacy",
  "Info: Issue"
)

s2_counts <- s2 %>%
  mutate(
    condition = case_when(
      str_detect(conversation_type, "^control")          ~ "Control",
      conversation_type == "emotional-activation"        ~ "Emotional Activation",
      conversation_type == "implementation-intentions"   ~ "Implementation Intentions",
      conversation_type == "identity-labeling"           ~ "Identity Labeling",
      conversation_type == "commitment-escalation"       ~ "Commitment Escalation",
      conversation_type == "anticipated-regret"          ~ "Anticipated Regret",
      conversation_type == "mega"                        ~ "Mega (All Combined)",
      conversation_type == "info-impact-efficacy"        ~ "Info: Impact Efficacy",
      conversation_type == "info-issue"                  ~ "Info: Issue",
      TRUE                                               ~ conversation_type
    )
  ) %>%
  count(condition, name = "N") %>%
  arrange(match(condition, s2_order)) %>%
  mutate(`%` = sprintf("%.1f", N / sum(N) * 100))

s2_total <- tibble(condition = "Total", N = sum(s2_counts$N), `%` = "100.0")

s2_table <- bind_rows(s2_counts, s2_total) %>%
  rename(Condition = condition)

# ---- Save output directory ---------------------------------------------------

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

# ---- Table body builder ------------------------------------------------------

make_table_body <- function(df) {
  nrows <- nrow(df)
  tbl <- df %>%
    kable(
      format   = "latex",
      booktabs = TRUE,
      linesep  = "",
      align    = c("l", "r", "r")
    ) %>%
    kable_styling(
      latex_options = c("striped"),
      stripe_color  = "gray!15",
      full_width    = FALSE,
      font_size     = 10
    ) %>%
    row_spec(nrows, bold = TRUE) %>%
    as.character()

  # Strip the \begin{table}...\end{table} float wrapper so the table can sit
  # inside a minipage (floats are not allowed in minipages).
  tbl <- gsub("\\\\begin\\{table\\}(\\[.*?\\])?\\s*\n", "", tbl)
  tbl <- gsub("\\\\centering\\s*\n", "", tbl)
  tbl <- gsub("\\\\end\\{table\\}\\s*\n?", "", tbl)
  tbl
}

# ---- Build both table bodies -------------------------------------------------

s1_body <- make_table_body(s1_table)
s2_body <- make_table_body(s2_table)

# ---- Write .tex fragments ----------------------------------------------------

tex_content <- paste0(
  "\\noindent\n",
  "\\begin{minipage}[t]{0.42\\textwidth}\n",
  "\\begin{center}\\textbf{A. Study 1}\\end{center}\\smallskip\n",
  "\\resizebox{\\linewidth}{!}{\n", s1_body, "\n}\n",
  "\\end{minipage}\n",
  "\\hfill\n",
  "\\begin{minipage}[t]{0.52\\textwidth}\n",
  "\\begin{center}\\textbf{B. Study 2}\\end{center}\\smallskip\n",
  "\\resizebox{\\linewidth}{!}{\n", s2_body, "\n}\n",
  "\\end{minipage}\n"
)

writeLines(tex_content, "output/tables/table_si_sample_sizes.tex")
cat("  Saved output/tables/table_si_sample_sizes.tex\n")
