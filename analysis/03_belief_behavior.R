source("analysis/utils.R")

cat("=== 03: Belief-Behavior Dissociation ===\n")

# ---- Study 1: Condition-level estimates (15 conditions) ----------------------

s1 <- load_study1() %>%
  filter(!is.na(model_type)) %>%
  mutate(cond = ifelse(treatment_type == "control", "control",
                       paste0(treatment_type, " ", model_type)) %>%
           fct_relevel("control"))

estimate_condition_effects <- function(df, dv, covariates, vs_control = TRUE) {
  if (vs_control) {
    form <- as.formula(str_glue("{dv} ~ cond + {covariates}"))
    fit_lmer_and_tidy(form, df) %>% filter(str_starts(term, "cond"))
  } else {
    form <- as.formula(str_glue("{dv} ~ 0 + cond + {covariates}"))
    df %>% filter(cond != "control") %>%
      { fit_lmer_and_tidy(form, .) } %>% filter(str_starts(term, "cond"))
  }
}

covariates <- s1_covariates

s1_belief_petition <- estimate_condition_effects(s1, "delta_petition", covariates)
s1_behavior_petition <- estimate_condition_effects(s1, "1 * signed_petition", covariates)
s1_belief_org <- estimate_condition_effects(s1, "delta_organization", covariates)
s1_behavior_org <- estimate_condition_effects(s1, "org_behavior", covariates)

s1_petition <- left_join(
  s1_belief_petition %>% select(term, belief_estimate = estimate, belief_ci_low = conf.low, belief_ci_high = conf.high),
  s1_behavior_petition %>% select(term, behavior_estimate = estimate, behavior_ci_low = conf.low, behavior_ci_high = conf.high),
  by = "term"
) %>% mutate(study = "study1", outcome_pair = "petition")

s1_org <- left_join(
  s1_belief_org %>% select(term, belief_estimate = estimate, belief_ci_low = conf.low, belief_ci_high = conf.high),
  s1_behavior_org %>% select(term, behavior_estimate = estimate, behavior_ci_low = conf.low, behavior_ci_high = conf.high),
  by = "term"
) %>% mutate(study = "study1", outcome_pair = "organization")

s1_cors <- tibble(
  study = "study1",
  outcome_pair = c("petition", "organization"),
  r = c(
    cor.test(s1_petition$belief_estimate, s1_petition$behavior_estimate)$estimate,
    cor.test(s1_org$belief_estimate, s1_org$behavior_estimate)$estimate
  ),
  p.value = c(
    cor.test(s1_petition$belief_estimate, s1_petition$behavior_estimate)$p.value,
    cor.test(s1_org$belief_estimate, s1_org$behavior_estimate)$p.value
  )
)

# ---- Study 2: Strategy-level estimates (8 strategies) ------------------------

s2 <- load_study2() %>%
  mutate(strategy = ifelse(treatment == "control", "control", conversation_type) %>%
           fct_relevel("control"))

s2_belief <- fit_lmer_and_tidy(
  as.formula(str_glue("delta_petition ~ strategy + {s2_covariates}")), s2
) %>% filter(str_starts(term, "strategy"))

s2_behavior <- fit_lmer_and_tidy(
  as.formula(str_glue("1 * signed_petition ~ strategy + {s2_covariates}")), s2
) %>% filter(str_starts(term, "strategy"))

s2_paired <- left_join(
  s2_belief %>% select(term, belief_estimate = estimate, belief_ci_low = conf.low, belief_ci_high = conf.high),
  s2_behavior %>% select(term, behavior_estimate = estimate, behavior_ci_low = conf.low, behavior_ci_high = conf.high),
  by = "term"
) %>% mutate(study = "study2", outcome_pair = "petition")

s2_cor <- if (nrow(s2_paired) >= 3) {
  ct <- cor.test(s2_paired$belief_estimate, s2_paired$behavior_estimate)
  tibble(study = "study2", outcome_pair = "petition", r = ct$estimate, p.value = ct$p.value)
} else {
  tibble(study = "study2", outcome_pair = "petition", r = NA_real_, p.value = NA_real_)
}

results <- list(
  condition_estimates = bind_rows(s1_petition, s1_org, s2_paired),
  correlations = bind_rows(s1_cors, s2_cor)
)

save_results(results, "belief_behavior")

cat("=== Done ===\n")
