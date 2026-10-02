source("analysis/utils.R")
library(metafor)

cat("=== 08: Cross-Study Pooled Estimates ===\n")

s1 <- load_study1()
s2 <- load_study2()

# ---- Pooled overall ATE on petition signing ----------------------------------

s1_form <- as.formula(str_glue("1 * signed_petition ~ treatment + {s1_covariates}"))
s1_est <- fit_lmer_and_tidy(s1_form, s1) %>%
  filter(startsWith(term, "treatment"))

s2_form <- as.formula(str_glue("1 * signed_petition ~ treatment + {s2_covariates}"))
s2_est <- tryCatch(
  fit_lmer_and_tidy(s2_form, s2) %>% filter(str_starts(term, "treatment")),
  error = function(e) tibble()
)

study_estimates <- bind_rows(
  s1_est %>% mutate(study = "study1"),
  s2_est %>% mutate(study = "study2")
)

# Meta-analytic pooling
pooled_ate <- if (nrow(study_estimates) == 2 &&
                  all(!is.na(study_estimates$std.error))) {
  rma_fit <- rma(yi = study_estimates$estimate,
                 sei = study_estimates$std.error,
                 method = "REML")
  tibble(
    pooled_estimate = as.numeric(rma_fit$beta),
    pooled_se = rma_fit$se,
    pooled_ci_low = rma_fit$ci.lb,
    pooled_ci_high = rma_fit$ci.ub,
    pooled_p = rma_fit$pval,
    Q = rma_fit$QE,
    Q_p = rma_fit$QEp,
    I2 = rma_fit$I2
  )
} else {
  tibble(pooled_estimate = NA_real_)
}

# ---- Info-issue replication --------------------------------------------------
# Compare info-issue ATE on attitudes vs. behavior across both studies

# Study 1: info condition vs. control
s1_info <- s1 %>%
  mutate(is_info = case_when(
    treatment == "control" ~ "control",
    prompt_category == "info" ~ "info",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(is_info))

s1_info_effects <- tribble(~dv, ~dv_type,
  "delta_petition", "Attitude",
  "1 * signed_petition", "Behavior"
) %>%
  pmap_dfr(function(dv, dv_type) {
    form <- as.formula(str_glue("{dv} ~ is_info + {s1_covariates}"))
    fit_lmer_and_tidy(form, s1_info) %>%
      filter(str_starts(term, "is_info")) %>%
      mutate(dv = dv, dv_type = dv_type, study = "study1", strategy = "info_issue")
  })

# Study 2: info-issue condition vs. control
s2_info <- s2 %>%
  filter(str_detect(conversation_type, "info-issue") | treatment == "control") %>%
  mutate(is_info = ifelse(treatment == "control", "control", "info"))

s2_info_effects <- tribble(~dv, ~dv_type,
  "delta_petition", "Attitude",
  "1 * signed_petition", "Behavior"
) %>%
  pmap_dfr(function(dv, dv_type) {
    form <- as.formula(str_glue("{dv} ~ is_info + {s2_covariates}"))
    tryCatch({
      fit_lmer_and_tidy(form, s2_info) %>%
        filter(str_starts(term, "is_info")) %>%
        mutate(dv = dv, dv_type = dv_type, study = "study2", strategy = "info_issue")
    }, error = function(e) tibble())
  })

info_replication <- bind_rows(s1_info_effects, s2_info_effects)

# ---- Pooled belief-behavior dissociation -------------------------------------
# Load pre-computed correlations from script 03
bb <- readRDS("output/results/belief_behavior.rds")

results <- list(
  study_estimates = study_estimates,
  pooled_ate = pooled_ate,
  info_replication = info_replication,
  belief_behavior_cors = bb$correlations
)

save_results(results, "pooled_estimates")

cat("=== Done ===\n")
