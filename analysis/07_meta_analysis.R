source("analysis/utils.R")
library(metafor)

cat("=== 07: Mini Meta-Analysis ===\n")

# Benchmarking AI persuasion effect sizes against other petition-signing studies

meta_data_path <- "data/meta_analysis_data.csv"

if (file.exists(meta_data_path)) {
  meta_df <- read_csv(meta_data_path, show_col_types = FALSE)
  cat(sprintf("  Loaded %d studies from meta-analysis data\n", nrow(meta_df)))

  # Add our own study estimates
  s1 <- load_study1()
  s1_form <- as.formula(str_glue("1 * signed_petition ~ treatment + {s1_covariates}"))
  s1_est <- fit_lmer_and_tidy(s1_form, s1) %>%
    filter(startsWith(term, "treatment"))

  s2 <- load_study2()
  s2_form <- as.formula(str_glue(
    "1 * signed_petition ~ treatment + {s2_covariates}"
  ))
  s2_est <- tryCatch(
    fit_lmer_and_tidy(s2_form, s2) %>% filter(str_starts(term, "treatment")),
    error = function(e) tibble()
  )

  our_studies <- bind_rows(
    s1_est %>% mutate(study_label = "Study 1 (this paper)", source = "this_paper"),
    s2_est %>% mutate(study_label = "Study 2 (this paper)", source = "this_paper")
  ) %>%
    select(study_label, estimate, conf.low, conf.high, std.error, source)

  results <- list(
    external_studies = meta_df,
    our_estimates = our_studies
  )
} else {
  cat("  Warning: meta-analysis data file not found. Saving empty results.\n")
  results <- list(external_studies = tibble(), our_estimates = tibble())
}

save_results(results, "meta_analysis")

cat("=== Done ===\n")
