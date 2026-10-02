source("analysis/figures/theme.R")

library(dplyr)
library(readr)
library(stringr)
library(broom)
library(knitr)
library(kableExtra)

cat("=== SI Table: Post-Treatment Attrition ===\n")

# ---- Load raw CSVs (non-completers needed for Study 2) -----------------------

s1_raw <- read_csv("data/raw/study1_raw.csv", show_col_types = FALSE)
s2_raw <- read_csv("data/raw/study2_raw.csv", show_col_types = FALSE)

# ---- Study 1 -----------------------------------------------------------------
# Denominator: all rows — study1_raw.csv contains every participant who was
# randomised at session creation (conversation_type is populated for all rows,
# including early dropouts and attention-check failures).
# Numerator:   !completed (did not finish the study).

s1_attrition <- s1_raw %>%
  mutate(
    attrited  = !completed,
    condition = case_when(
      str_detect(conversation_type, "^control") ~ "Control",
      conversation_type == "petition"           ~ "Petition-Info",
      conversation_type == "organization"       ~ "Org-Info",
      conversation_type == "random"             ~ "Random",
      TRUE                                      ~ conversation_type
    )
  )

# ---- Study 2 -----------------------------------------------------------------
# Denominator: final_step >= 3 (post-randomisation, post-pre-treatment survey).
# Numerator:   final_step >= 3 & !completed.
# Attention-check exclusion is NOT applied here — it is a data-quality filter,
# not a dropout indicator.

s2_attrition <- s2_raw %>%
  filter(final_step >= 3) %>%
  mutate(
    attrited  = !completed,
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
  )

# ---- Condition orderings -----------------------------------------------------

s1_order <- c("Control", "Petition-Info", "Org-Info", "Random")

s2_order <- c(
  "Control", "Emotional Activation", "Implementation Intentions",
  "Identity Labeling", "Commitment Escalation", "Anticipated Regret",
  "Mega (All Combined)", "Info: Impact Efficacy", "Info: Issue"
)

# ---- Table builders ----------------------------------------------------------

make_proportion_tbl <- function(df, cond_order) {
  df %>%
    group_by(Condition = condition) %>%
    summarise(
      `Proportion NA` = sprintf("%.3f", mean(attrited)),
      `Total N`       = n(),
      .groups = "drop"
    ) %>%
    arrange(match(Condition, cond_order))
}

make_ftest_tbl <- function(df) {
  fit <- aov(as.integer(attrited) ~ condition, data = df)
  broom::tidy(fit) %>%
    mutate(
      term      = ifelse(term == "condition", "Condition", str_to_title(term)),
      sumsq     = round(sumsq,     2),
      meansq    = round(meansq,    2),
      statistic = round(statistic, 2),
      p.value   = round(p.value,   3)
    )
}

# ---- Shared kable body builder -----------------------------------------------

make_table_body <- function(df, align_cols) {
  tbl <- df %>%
    kable(
      format   = "latex",
      booktabs = TRUE,
      linesep  = "",
      align    = align_cols,
      na       = "NA"
    ) %>%
    kable_styling(
      latex_options = c("striped"),
      stripe_color  = "gray!15",
      full_width    = FALSE,
      font_size     = 10
    ) %>%
    row_spec(0, bold = TRUE) %>%
    as.character()

  # Strip the \begin{table}...\end{table} float wrapper
  tbl <- gsub("\\\\begin\\{table\\}(\\[.*?\\])?\\s*\n", "", tbl)
  tbl <- gsub("\\\\centering\\s*\n", "", tbl)
  tbl <- gsub("\\\\end\\{table\\}\\s*\n?", "", tbl)

  # Strip the leading \centering from \centering\begingroup so that the
  # outer \centering in the \begin{table} float handles positioning cleanly,
  # preventing the table from stretching to fill the full line width.
  tbl <- gsub("\\\\centering(\\\\begingroup)", "\\1", tbl)

  tbl
}

# Note lives OUTSIDE the tabular as a full-width minipage so it always has
# the full text width to wrap into, independent of table width.
write_tex <- function(body, note_text, out_tex) {
  note_block <- paste0(
    "\\\\[3pt]\n",
    "\\begin{minipage}{\\linewidth}\n",
    "\\raggedright\\footnotesize\\textit{Note: ", note_text, "}\n",
    "\\end{minipage}\n"
  )
  tex_content <- paste0(
    "\\renewcommand{\\arraystretch}{1.2}\n",
    "\\begin{adjustbox}{max width=\\linewidth, center}\n",
    body, "\n",
    "\\end{adjustbox}\n",
    note_block
  )
  writeLines(tex_content, out_tex)
  cat(sprintf("  Saved %s\n", out_tex))
}

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

# ---- Study 1 tables ----------------------------------------------------------

s1_note <- "Denominator is all randomised participants (\\texttt{conversation\\_type} assigned at session creation). Post-treatment attrition is defined as failure to complete the study (\\texttt{completed = FALSE}). This includes participants who dropped out at any point after randomisation."

s1_prop_body <- make_table_body(
  make_proportion_tbl(s1_attrition, s1_order),
  align_cols = c("l", "r", "r")
)

s1_ftest_body <- make_table_body(
  make_ftest_tbl(s1_attrition),
  align_cols = c("l", "r", "r", "r", "r", "r")
)

write_tex(s1_prop_body,  s1_note, "output/tables/table_si_attrition_s1_proportion.tex")
write_tex(s1_ftest_body, s1_note, "output/tables/table_si_attrition_s1_ftest.tex")

# ---- Study 2 tables ----------------------------------------------------------

s2_note <- "Denominator is participants with \\texttt{final\\_step} $\\geq$ 3 (post-randomisation, post-pre-treatment survey). Attention-check exclusions are not counted as attrition."

s2_prop_body <- make_table_body(
  make_proportion_tbl(s2_attrition, s2_order),
  align_cols = c("l", "r", "r")
)

s2_ftest_body <- make_table_body(
  make_ftest_tbl(s2_attrition),
  align_cols = c("l", "r", "r", "r", "r", "r")
)

write_tex(s2_prop_body,  s2_note, "output/tables/table_si_attrition_s2_proportion.tex")
write_tex(s2_ftest_body, s2_note, "output/tables/table_si_attrition_s2_ftest.tex")

cat("=== Done ===\n")
