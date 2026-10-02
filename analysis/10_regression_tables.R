source("analysis/utils.R")

cat("=== 10: Regression Tables for SI ===\n")

fmt <- function(x, digits = 3) formatC(x, digits = digits, format = "f")
fmt_p <- function(p) ifelse(p < 0.001, "$<$0.001", paste0(fmt(p, 3)))
stars <- function(p) ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))

clean_term <- function(term) {
  case_when(
    term == "treatmenttreatment" ~ "Treatment (vs.\\ control)",
    term == "scale(pretreatment_agreement)" ~ "Pre-treatment petition support (std.)",
    term == "scale(pretreatment_organization_opinion)" ~ "Pre-treatment org.\\ support (std.)",
    term == "scale(charity_interest_num)" ~ "Charity interest (std.)",
    term == "randomized_bonus_amount" ~ "Bonus amount (\\pounds)",
    term == "(Intercept)" ~ "Intercept",
    str_starts(term, "strategy") ~ clean_strategy(term),
    TRUE ~ term
  )
}

clean_strategy <- function(s) {
  s %>%
    str_remove("^strategy") %>%
    str_replace_all("-", " ") %>%
    str_replace_all("\\.", " ") %>%
    str_to_title() %>%
    str_replace("Info Issue", "Info (Issue)") %>%
    str_replace("Info Impact Efficacy", "Info (Impact Efficacy)") %>%
    str_replace("Mega", "Mega (All Combined)")
}

write_table <- function(lines, filename) {
  path <- file.path("output", "tables", filename)
  writeLines(lines, path)
  cat(sprintf("  Saved %s\n", path))
}

# SI formatting helpers -------------------------------------------------------

fix_minus <- function(x) {
  ifelse(x == "", x, str_replace(x, "^-", "$-$"))
}

shade_cell <- function(x, shade) {
  if (!shade) return(x)
  paste0("\\cellcolor{gray!15}{", x, "}")
}

shade_cells <- function(cells, shade) {
  vapply(cells, shade_cell, character(1), shade = shade)
}

make_tex_row <- function(label, cells, shade) {
  paste0(shade_cell(label, shade), " & ",
         paste(shade_cells(cells, shade), collapse = " & "), " \\\\")
}

make_tex_se_row <- function(se_cells, shade) {
  paste0(shade_cell("", shade), " & ",
         paste(shade_cells(se_cells, shade), collapse = " & "), " \\\\")
}

makecell_header <- function(name) {
  lookup <- c(
    "Support petition"       = "\\textbf{\\makecell[c]{Support\\\\petition}}",
    "Sign petition"          = "\\textbf{\\makecell[c]{Sign\\\\petition}}",
    "Support organization"   = "\\textbf{\\makecell[c]{Support\\\\organization}}",
    "Bonus donation"         = "\\textbf{\\makecell[c]{Bonus\\\\donation}}",
    "Money raising effort"   = "\\textbf{\\makecell[c]{Money raising\\\\effort}}",
    "Choose organization"    = "\\textbf{\\makecell[c]{Choose\\\\organization}}",
    "Donate to assigned org" = "\\textbf{\\makecell[c]{Donate to\\\\assigned org}}"
  )
  if (name %in% names(lookup)) lookup[[name]] else paste0("\\textbf{", name, "}")
}

# ==============================================================================
# Table S1: Study 1 Main Effects (OLS, all 6 DVs)
# ==============================================================================

cat("Table S1: Study 1 main effects...\n")

s1 <- load_study1()

s1_table_rows <- s1_dvs %>%
  pmap(function(dv, dv_name, dv_type) {
    covariates <- s1_covariates
    if (str_detect(dv, "donat")) covariates <- s1_covariates_donation
    form <- as.formula(str_glue("1 * ({dv}) ~ treatment + {covariates}"))
    model <- lmer(form, data = s1)
    tidy_df <- broom.mixed::tidy(model, conf.int = TRUE, effects = "fixed") %>%
      mutate(p.value = 2 * pnorm(abs(statistic), lower.tail = FALSE))
    list(dv = dv, dv_name = str_remove_all(dv_name, "<[^>]+>"), dv_type = dv_type,
         tidy = tidy_df, n = nrow(model@frame))
  })

n_dvs_s1 <- length(s1_table_rows)
col_names_s1 <- sapply(s1_table_rows, function(x) str_remove(x$dv_name, "\\s*\\(.*\\)"))

header <- c(
  "\\begin{table}[htbp]",
  "    \\centering",
    "    \\caption{Study~1: Average treatment effects of AI conversation (vs.\\ control) on all outcome variables. Linear mixed-effects models with pre-treatment covariates and random intercepts for issue. Standard errors in parentheses.}",
  "    \\label{tab:s1_main}",
  "\\renewcommand{\\arraystretch}{1.2}",
  "\\begin{adjustbox}{max width=\\linewidth, center}",
  "\\begingroup\\fontsize{9}{11}\\selectfont",
  "\\setlength{\\tabcolsep}{4pt}",
  sprintf("\\begin{tabular}{l%s}", paste(rep("c", n_dvs_s1), collapse = "")),
  "\\toprule"
)

col_headers <- paste0(
  "\\textbf{} & ",
  paste(sapply(col_names_s1, makecell_header), collapse = " & "),
  " \\\\"
)

col_types <- paste0(" & ", paste(sapply(s1_table_rows, function(x) {
  paste0("\\textit{", x$dv_type, "}")
}), collapse = " & "), " \\\\")

# Treatment row: extract estimates and SEs, apply fix_minus
treatment_row <- s1_table_rows %>% map(function(x) {
  row <- x$tidy %>% filter(str_starts(term, "treatment"))
  if (nrow(row) == 0) return(list(est = "", se = ""))
  list(est = fix_minus(paste0(fmt(row$estimate[1]), stars(row$p.value[1]))),
       se = paste0("(", fmt(row$std.error[1]), ")"))
})

covariate_terms <- c("scale(pretreatment_agreement)", "scale(pretreatment_organization_opinion)",
                     "scale(charity_interest_num)", "randomized_bonus_amount", "(Intercept)")

cov_rows <- map(covariate_terms, function(term) {
  cells <- s1_table_rows %>% map_chr(function(x) {
    row <- x$tidy %>% filter(term == !!term)
    if (nrow(row) == 0) return("")
    fix_minus(paste0(fmt(row$estimate[1]), stars(row$p.value[1])))
  })
  se_cells <- s1_table_rows %>% map_chr(function(x) {
    row <- x$tidy %>% filter(term == !!term)
    if (nrow(row) == 0) return("")
    paste0("(", fmt(row$std.error[1]), ")")
  })
  list(label = clean_term(term), cells = cells, se_cells = se_cells)
})

# Body: Treatment = block 1 (shaded); cov_rows index i = block i+1
# shade when block index is odd: Treatment(1)=T, petition(2)=F, org(3)=T,
#   charity(4)=F, bonus(5)=T, intercept(6)=F
body <- c(
  col_headers,
  col_types,
  "\\midrule",
  make_tex_row("Treatment (vs.\\ control)", map_chr(treatment_row, "est"), shade = TRUE),
  make_tex_se_row(map_chr(treatment_row, "se"), shade = TRUE),
  "\\addlinespace"
)

for (i in seq_along(cov_rows)) {
  shade <- (i %% 2 == 0)  # block i+1 is shaded when i+1 is odd, i.e. i is even
  cv <- cov_rows[[i]]
  body <- c(body,
            make_tex_row(cv$label, cv$cells, shade = shade),
            make_tex_se_row(cv$se_cells, shade = shade),
            "\\addlinespace")
}

footer_n_cells  <- map_chr(s1_table_rows, ~ format(.x$n, big.mark = ","))

footer <- c(
  "\\midrule",
  make_tex_row("$N$", footer_n_cells, shade = TRUE),
  "\\bottomrule",
  sprintf("\\multicolumn{%d}{l}{\\footnotesize $^{*}p<0.05$; $^{**}p<0.01$; $^{***}p<0.001$. Random intercepts for issue.} \\\\",
          n_dvs_s1 + 1),
  "\\end{tabular}",
  "\\endgroup{}",
  "\\end{adjustbox}",
  "\\end{table}"
)

write_table(c(header, body, footer), "tab_s1_main_effects.tex")

# ==============================================================================
# Table S2: Study 2 Strategy Effects (LMM, all 4 DVs)
# ==============================================================================

cat("Table S2: Study 2 strategy effects...\n")

s2 <- load_study2() %>%
  mutate(strategy = ifelse(treatment == "control", "control", conversation_type) %>%
           fct_relevel("control"))

s2_models <- s2_dvs %>%
  pmap(function(dv, dv_name, dv_type) {
    form <- as.formula(str_glue("1 * ({dv}) ~ strategy + {s2_covariates}"))
    model <- lmer(form, data = s2)
    tidy_df <- broom.mixed::tidy(model, conf.int = TRUE, effects = "fixed") %>%
      mutate(p.value = 2 * pnorm(abs(statistic), lower.tail = FALSE))
    list(dv = dv, dv_name = str_remove_all(dv_name, "<[^>]+>"), dv_type = dv_type,
         tidy = tidy_df, n = nrow(model@frame))
  })

strategy_terms <- s2_models[[1]]$tidy %>%
  filter(str_starts(term, "strategy")) %>%
  pull(term)

n_dvs_s2 <- length(s2_models)
col_names_s2 <- sapply(s2_models, function(x) str_remove(x$dv_name, "\\s*\\(.*\\)"))

header2 <- c(
  "\\begin{table}[htbp]",
  "    \\centering",
  "    \\caption{Study~2: Strategy effects on all outcome variables. Linear mixed-effects models with pre-treatment covariates and random intercepts for issue. Each column reports one outcome variable; each row reports the estimated coefficient for one strategy (vs.\\ control). Standard errors in parentheses.}",
  "    \\label{tab:s2_strategies}",
  "\\renewcommand{\\arraystretch}{1.2}",
  "\\begin{adjustbox}{max width=\\linewidth, center}",
  "\\begingroup\\fontsize{10}{12}\\selectfont",
  "",
  sprintf("\\begin{tabular}{l%s}", paste(rep("c", n_dvs_s2), collapse = "")),
  "\\toprule"
)

col_headers2 <- paste0(
  "\\textbf{} & ",
  paste(sapply(col_names_s2, makecell_header), collapse = " & "),
  " \\\\"
)

col_types2 <- paste0(" & ", paste(sapply(s2_models, function(x) {
  paste0("\\textit{", x$dv_type, "}")
}), collapse = " & "), " \\\\")

body2 <- c(col_headers2, col_types2, "\\midrule")

# Strategy blocks: block index i (1-based); shade when i is odd
for (i in seq_along(strategy_terms)) {
  st    <- strategy_terms[i]
  shade <- (i %% 2 == 1)
  label <- clean_strategy(st)
  cells <- s2_models %>% map_chr(function(x) {
    row <- x$tidy %>% filter(term == st)
    if (nrow(row) == 0) return("")
    fix_minus(paste0(fmt(row$estimate[1]), stars(row$p.value[1])))
  })
  se_cells <- s2_models %>% map_chr(function(x) {
    row <- x$tidy %>% filter(term == st)
    if (nrow(row) == 0) return("")
    paste0("(", fmt(row$std.error[1]), ")")
  })
  body2 <- c(body2,
             make_tex_row(label, cells, shade = shade),
             make_tex_se_row(se_cells, shade = shade),
             "\\addlinespace")
}

# Covariate blocks continue after 8 strategy blocks:
# j=1 -> block 9 (odd = shaded), j=2 -> block 10 (even = unshaded), j=3 -> block 11 (odd = shaded)
cov_terms_s2 <- c("scale(pretreatment_agreement)", "scale(pretreatment_organization_opinion)", "(Intercept)")
for (j in seq_along(cov_terms_s2)) {
  term  <- cov_terms_s2[j]
  shade <- (j %% 2 == 1)
  label <- clean_term(term)
  cells <- s2_models %>% map_chr(function(x) {
    row <- x$tidy %>% filter(term == !!term)
    if (nrow(row) == 0) return("")
    fix_minus(paste0(fmt(row$estimate[1]), stars(row$p.value[1])))
  })
  se_cells <- s2_models %>% map_chr(function(x) {
    row <- x$tidy %>% filter(term == !!term)
    if (nrow(row) == 0) return("")
    paste0("(", fmt(row$std.error[1]), ")")
  })
  body2 <- c(body2,
             make_tex_row(label, cells, shade = shade),
             make_tex_se_row(se_cells, shade = shade),
             "\\addlinespace")
}

# Footer: N = block 12 (even = unshaded)
footer_n2_cells <- map_chr(s2_models, ~ format(.x$n, big.mark = ","))

footer2 <- c(
  "\\midrule",
  make_tex_row("$N$", footer_n2_cells, shade = FALSE),
  "\\bottomrule",
  sprintf("\\multicolumn{%d}{l}{\\footnotesize $^{*}p<0.05$; $^{**}p<0.01$; $^{***}p<0.001$. Random intercepts for issue.} \\\\",
          n_dvs_s2 + 1),
  "\\end{tabular}",
  "\\endgroup{}",
  "",
  "\\end{adjustbox}",
  "\\end{table}"
)

write_table(c(header2, body2, footer2), "tab_s2_strategy_effects.tex")

cat("=== Done ===\n")
