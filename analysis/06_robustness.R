source("analysis/utils.R")

cat("=== 06: Robustness Checks ===\n")

s1 <- load_study1()
s2 <- load_study2()

# ---- Leave-one-issue-out (both studies, all behavioural DVs) -----------------

s1_beh_dvs <- s1_dvs %>% filter(dv_type == "Behavior")
s2_beh_dvs <- s2_dvs %>% filter(dv_type == "Behavior")

s1_issues <- unique(s1$petition_title)
s1_loo <- s1_beh_dvs %>%
  pmap_dfr(function(dv, dv_name, dv_type) {
    covs <- s1_covariates_base
    if (str_detect(dv, "donat")) covs <- paste0(covs, " + randomized_bonus_amount")
    map_dfr(s1_issues, function(drop_issue) {
      d <- s1 %>% filter(petition_title != drop_issue)
      form <- as.formula(str_glue("{dv} ~ treatment + {covs} + (1|petition_title)"))
      tryCatch({
        fit_lmer_and_tidy(form, d) %>%
          filter(startsWith(term, "treatment")) %>%
          mutate(dropped_issue = drop_issue, study = "study1", dv_name = dv_name)
      }, error = function(e) tibble())
    })
  })

s1_fullsample <- s1_beh_dvs %>%
  pmap_dfr(function(dv, dv_name, dv_type) {
    covs <- s1_covariates
    if (str_detect(dv, "donat")) covs <- s1_covariates_donation
    form <- as.formula(str_glue("{dv} ~ treatment + {covs}"))
    tryCatch({
      fit_lmer_and_tidy(form, s1) %>%
        filter(startsWith(term, "treatment")) %>%
        mutate(study = "study1", dv_name = dv_name)
    }, error = function(e) tibble())
  })

s2_issues <- unique(s2$petition_title)

s2_loo <- s2_beh_dvs %>%
  pmap_dfr(function(dv, dv_name, dv_type) {
    map_dfr(s2_issues, function(drop_issue) {
      d <- s2 %>% filter(petition_title != drop_issue)
      form <- as.formula(str_glue(
        "{dv} ~ treatment + scale(pretreatment_agreement) + scale(pretreatment_organization_opinion) + (1|petition_title)"
      ))
      tryCatch({
        fit_lmer_and_tidy(form, d) %>%
          filter(str_starts(term, "treatment")) %>%
          mutate(dropped_issue = drop_issue, study = "study2", dv_name = dv_name)
      }, error = function(e) tibble())
    })
  })

s2_fullsample <- s2_beh_dvs %>%
  pmap_dfr(function(dv, dv_name, dv_type) {
    form <- as.formula(str_glue("{dv} ~ treatment + {s2_covariates}"))
    tryCatch({
      fit_lmer_and_tidy(form, s2) %>%
        filter(str_starts(term, "treatment")) %>%
        mutate(study = "study2", dv_name = dv_name)
    }, error = function(e) tibble())
  })

# ---- Logistic GLMM for binary outcomes (Study 2) ----------------------------

s2_logistic <- tryCatch({
  model <- glmer(
    signed_petition ~ treatment + scale(pretreatment_agreement) +
      scale(pretreatment_organization_opinion) + (1 | petition_title),
    data = s2, family = binomial
  )
  broom.mixed::tidy(model, conf.int = TRUE, effects = "fixed") %>%
    mutate(study = "study2", model_type_check = "logistic_glmm")
}, error = function(e) {
  cat("  Warning: logistic GLMM failed:", e$message, "\n")
  tibble()
})

# ---- AI model as fixed effect (Study 2) --------------------------------------

s2_model_fe <- tryCatch({
  form <- as.formula(
    "1 * signed_petition ~ treatment + model_type + scale(pretreatment_agreement) + scale(pretreatment_organization_opinion) + (1|petition_title)"
  )
  fit_lmer_and_tidy(form, s2) %>%
    mutate(study = "study2", model_type_check = "model_fixed_effect")
}, error = function(e) tibble())

# ---- Demographic covariates robustness (both studies, all DVs) ---------------
# For each DV, estimate the primary model (baseline) and a model with
# age + gender + education + income added. Save both so the treatment
# coefficients can be compared side by side.

demo_covs <- "age + gender + education + income"

# Study 1
s1_demo_clean <- s1 %>% filter(!is.na(age)) %>% droplevels()

s1_demo_robustness <- s1_dvs %>%
  pmap_dfr(function(dv, dv_name, dv_type) {
    covs_base <- s1_covariates
    if (str_detect(dv, "donat")) covs_base <- s1_covariates_donation

    form_base <- as.formula(str_glue("{dv} ~ treatment + {covs_base}"))
    form_demo <- as.formula(str_glue("{dv} ~ treatment + {covs_base} + {demo_covs}"))

    tryCatch({
      base <- fit_lmer_and_tidy(form_base, s1_demo_clean) %>%
        filter(startsWith(term, "treatment")) %>%
        mutate(specification = "Baseline")
      demo <- fit_lmer_and_tidy(form_demo, s1_demo_clean) %>%
        filter(startsWith(term, "treatment")) %>%
        mutate(specification = "With demographics")
      bind_rows(base, demo) %>%
        mutate(dv_name = dv_name, dv_type = dv_type, study = "study1")
    }, error = function(e) tibble())
  })

# Study 2
s2_demo_clean <- s2 %>% filter(!is.na(age)) %>% droplevels()

s2_demo_robustness <- s2_dvs %>%
  pmap_dfr(function(dv, dv_name, dv_type) {
    form_base <- as.formula(str_glue("{dv} ~ treatment + {s2_covariates}"))
    form_demo <- as.formula(str_glue("{dv} ~ treatment + {s2_covariates} + {demo_covs}"))

    tryCatch({
      base <- fit_lmer_and_tidy(form_base, s2_demo_clean) %>%
        filter(str_starts(term, "treatment")) %>%
        mutate(specification = "Baseline")
      demo <- fit_lmer_and_tidy(form_demo, s2_demo_clean) %>%
        filter(str_starts(term, "treatment")) %>%
        mutate(specification = "With demographics")
      bind_rows(base, demo) %>%
        mutate(dv_name = dv_name, dv_type = dv_type, study = "study2")
    }, error = function(e) tibble())
  })

demo_robustness <- bind_rows(s1_demo_robustness, s2_demo_robustness)

results <- list(
  leave_one_out = bind_rows(s1_loo, s2_loo),
  loo_fullsample = bind_rows(s1_fullsample, s2_fullsample),
  logistic_glmm = s2_logistic,
  model_fixed_effect = s2_model_fe,
  demo_robustness = demo_robustness
)

save_results(results, "robustness")

cat("=== Done ===\n")
