source("analysis/figures/theme.R")

library(dplyr)
library(tidyr)
library(stringr)
library(knitr)
library(kableExtra)

cat("=== SI Table: Design Crossings (4 tables) ===\n")

# ---- Load data ---------------------------------------------------------------

s1 <- readRDS("output/results/study1_clean.rds")
s2 <- readRDS("output/results/study2_clean.rds")

# ---- Shared labels -----------------------------------------------------------

# Study 1
s1_model_labels <- c(
  "anthropic/claude-sonnet-4" = "Claude Sonnet 4",
  "google/gemini-2.5-pro"     = "Gemini 2.5 Pro",
  "openai/chatgpt-4o-latest"  = "ChatGPT-4o",
  "openai/gpt-4.1"            = "GPT-4.1",
  "x-ai/grok-4"               = "Grok 4"
)

s1_prompt_order <- c("Control", "Petition-Info", "Org-Info", "Random")

# Study 2
s2_model_labels <- c(
  "anthropic/claude-opus-4.6"      = "Claude Opus 4.6",
  "google/gemini-3.1-pro-preview"  = "Gemini 3.1 Pro",
  "openai/gpt-4.1"                 = "GPT-4.1",
  "x-ai/grok-4"                    = "Grok 4"
)

s2_strategy_order <- c(
  "Control", "Anticipated Regret", "Commitment Escalation",
  "Emotional Activation", "Identity Labeling", "Implementation Intentions",
  "Info: Impact Efficacy", "Info: Issue", "Mega (All Combined)"
)

# Two-line versions for column headers in wide tables
s2_strategy_linebreak <- c(
  "Control"                  = "Control",
  "Anticipated Regret"       = "Anticipated\nRegret",
  "Commitment Escalation"    = "Commitment\nEscalation",
  "Emotional Activation"     = "Emotional\nActivation",
  "Identity Labeling"        = "Identity\nLabeling",
  "Implementation Intentions"= "Implementation\nIntentions",
  "Info: Impact Efficacy"    = "Info: Impact\nEfficacy",
  "Info: Issue"              = "Info:\nIssue",
  "Mega (All Combined)"      = "Mega\n(All Combined)",
  "Total"                    = "Total"
)

# Shared topic labels (combined across both studies)
topic_labels <- c(
  "Stop Rising Child Poverty – Scrap the Two-Child Limit & Lock in Support" = "Stop Rising Child Poverty",
  "End suffering for mother pigs"                           = "End Suffering for Mother Pigs",
  "Oppose Facial Recognition"                              = "Oppose Facial Recognition",
  "Stop the risk of nuclear war"                           = "Stop the Risk of Nuclear War",
  "Seats in parliament should match how we vote"           = "Seats in Parliament",
  "Lift the ban on work for people seeking asylum"         = "Lift Ban on Work for Asylum Seekers",
  "Urge UK governments to protect trees"                   = "Urge Govts to Protect Trees",
  "Stop banks fuelling the climate crisis"                 = "Stop Banks Fuelling Climate Crisis",
  "Continuing care for children with disabilities"         = "Continuing Care for Children",
  "Reducing economic inequality in the UK"                 = "Reducing Economic Inequality"
)

# ---- Recode Study 1 ----------------------------------------------------------

s1 <- s1 %>%
  mutate(
    prompt_type  = case_when(
      str_detect(conversation_type, "^control") ~ "Control",
      conversation_type == "petition"           ~ "Petition-Info",
      conversation_type == "organization"       ~ "Org-Info",
      conversation_type == "random"             ~ "Random"
    ),
    prompt_type  = factor(prompt_type, levels = s1_prompt_order),
    model_label  = s1_model_labels[model_type],
    topic_label  = topic_labels[petition_title]
  )

# ---- Recode Study 2 ----------------------------------------------------------

s2 <- s2 %>%
  mutate(
    strategy = case_when(
      str_detect(conversation_type, "^control")          ~ "Control",
      conversation_type == "anticipated-regret"          ~ "Anticipated Regret",
      conversation_type == "commitment-escalation"       ~ "Commitment Escalation",
      conversation_type == "emotional-activation"        ~ "Emotional Activation",
      conversation_type == "identity-labeling"           ~ "Identity Labeling",
      conversation_type == "implementation-intentions"   ~ "Implementation Intentions",
      conversation_type == "info-impact-efficacy"        ~ "Info: Impact Efficacy",
      conversation_type == "info-issue"                  ~ "Info: Issue",
      conversation_type == "mega"                        ~ "Mega (All Combined)"
    ),
    strategy    = factor(strategy, levels = s2_strategy_order),
    model_label = s2_model_labels[model_type],
    topic_label = topic_labels[petition_title]
  )

# ---- Cross-tab builder -------------------------------------------------------

make_crosstab <- function(data, row_var, col_var, row_name) {
  counts <- data %>%
    count({{ row_var }}, {{ col_var }}) %>%
    pivot_wider(names_from  = {{ col_var }},
                values_from = n,
                values_fill = 0)

  colnames(counts)[1] <- row_name

  counts <- counts %>%
    mutate(Total = rowSums(across(where(is.integer))))

  totals_row <- counts %>%
    summarise(across(where(is.integer), sum)) %>%
    mutate(!!row_name := "Total")

  bind_rows(counts, totals_row)
}

s1_model   <- make_crosstab(s1, model_label, prompt_type,  "Model")
s1_topic   <- make_crosstab(s1, topic_label, prompt_type,  "Topic")
s2_model   <- make_crosstab(s2, model_label, strategy,     "Model")
s2_topic   <- make_crosstab(s2, topic_label, strategy,     "Topic")

# ---- Table body builder (strips float wrapper) -------------------------------

make_body <- function(df, font_size = 10, linebreak_headers = FALSE) {
  nrows <- nrow(df)

  escape <- TRUE
  if (linebreak_headers) {
    # Rename strategy columns using two-line labels then wrap with \makecell
    colnames(df) <- dplyr::recode(colnames(df), !!!s2_strategy_linebreak)
    colnames(df) <- linebreak(colnames(df), align = "c")
    escape <- FALSE
  }

  tbl <- df %>%
    kable(
      format   = "latex",
      booktabs = TRUE,
      linesep  = "",
      align    = c("l", rep("r", ncol(df) - 1)),
      escape   = escape
    ) %>%
    kable_styling(
      latex_options = c("striped"),
      stripe_color  = "gray!15",
      full_width    = FALSE,
      font_size     = font_size
    ) %>%
    row_spec(0,     bold = TRUE) %>%
    row_spec(nrows, bold = TRUE) %>%
    as.character()

  tbl <- gsub("\\\\begin\\{table\\}(\\[.*?\\])?\\s*\n", "", tbl)
  tbl <- gsub("\\\\centering\\s*\n", "", tbl)
  tbl <- gsub("\\\\end\\{table\\}\\s*\n?", "", tbl)
  tbl <- gsub("\\\\centering(\\\\begingroup)", "\\1", tbl)
  tbl
}

# ---- Write .tex fragments ----------------------------------------------------

write_tex <- function(body, out_tex, landscape = FALSE) {
  body_wrapped <- paste0(
    "\\begin{adjustbox}{max width=\\linewidth, center}\n",
    body, "\n",
    "\\end{adjustbox}\n"
  )

  tex_content <- if (landscape) {
    paste0(
      "\\renewcommand{\\arraystretch}{1.2}\n",
      "\\begin{landscape}\n",
      body_wrapped,
      "\\end{landscape}\n"
    )
  } else {
    paste0("\\renewcommand{\\arraystretch}{1.2}\n", body_wrapped)
  }

  writeLines(tex_content, out_tex)
  cat(sprintf("  Saved %s\n", out_tex))
}

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

write_tex(make_body(s1_model),
          "output/tables/table_si_design_s1_model_x_prompt.tex")

write_tex(make_body(s1_topic),
          "output/tables/table_si_design_s1_topic_x_prompt.tex")

write_tex(make_body(s2_model, font_size = 10, linebreak_headers = TRUE),
          "output/tables/table_si_design_s2_model_x_strategy.tex",
          landscape = TRUE)

write_tex(make_body(s2_topic, font_size = 10, linebreak_headers = TRUE),
          "output/tables/table_si_design_s2_topic_x_strategy.tex",
          landscape = TRUE)
