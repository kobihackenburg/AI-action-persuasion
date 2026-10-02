source("analysis/utils.R")

cat("=== 05: Heterogeneous Effects by Prior Support ===\n")

# ---- Study 1: CATEs by prior support ----------------------------------------

s1 <- load_study1()

s1_cate_dvs <- tribble(
  ~dv,                       ~dv_name,                ~dv_type,   ~split_var,
  "delta_petition",          "Support petition",      "Attitude", "pretreatment_agreement",
  "delta_organization",      "Support organization",  "Attitude", "pretreatment_organization_opinion",
  "1 * signed_petition",     "Sign petition",         "Behavior", "pretreatment_agreement",
  "donate_proportion",       "Bonus donation",        "Behavior", "pretreatment_organization_opinion",
  "extra_task_clicks/100",   "Money raising effort",  "Behavior", "pretreatment_organization_opinion",
  "not_switch_organization", "Choose organization",   "Behavior", "pretreatment_organization_opinion"
)

s1_cates <- s1_cate_dvs %>%
  pmap_dfr(function(dv, dv_name, dv_type, split_var) {
    covariates <- s1_covariates
    if (str_detect(dv, "donat")) covariates <- s1_covariates_donation
    form <- as.formula(str_glue("{dv} ~ treatment + {covariates}"))

    strong_supporters <- s1 %>% filter(.data[[split_var]] > 5)
    others <- s1 %>% filter(.data[[split_var]] <= 5)

    bind_rows(
      fit_lmer_and_tidy(form, strong_supporters) %>%
        filter(startsWith(term, "treatment")) %>%
        mutate(subgroup = "Strong Supporters"),
      fit_lmer_and_tidy(form, others) %>%
        filter(startsWith(term, "treatment")) %>%
        mutate(subgroup = "Other")
    ) %>%
      mutate(dv = dv, dv_name = dv_name, dv_type = dv_type, study = "study1")
  })

# ---- Study 2: Interaction model (prereg Equation 2) --------------------------

s2 <- load_study2()

s2_interaction_dvs <- tribble(
  ~dv,                       ~dv_name,                 ~dv_type,
  "delta_petition",          "Support petition",       "Attitude",
  "delta_organization",      "Support organization",   "Attitude",
  "1 * signed_petition",     "Sign petition",          "Behavior",
  "1 * chose_assigned_org",  "Donate to assigned org", "Behavior"
)

s2_interactions <- s2_interaction_dvs %>%
  pmap_dfr(function(dv, dv_name, dv_type) {
    form <- as.formula(str_glue(
      "{dv} ~ treatment * scale(pretreatment_agreement) + scale(pretreatment_organization_opinion) + (1|petition_title)"
    ))
    tryCatch({
      fit_lmer_and_tidy(form, s2) %>%
        mutate(dv = dv, dv_name = dv_name, dv_type = dv_type, study = "study2")
    }, error = function(e) tibble())
  })

# Study 2 CATEs (same subgroup split as Study 1 for comparability)
s2_cates <- s2_interaction_dvs %>%
  pmap_dfr(function(dv, dv_name, dv_type) {
    form <- as.formula(str_glue("{dv} ~ treatment + {s2_covariates}"))
    strong <- s2 %>% filter(pretreatment_agreement > 5)
    others <- s2 %>% filter(pretreatment_agreement <= 5)

    bind_rows(
      tryCatch(
        fit_lmer_and_tidy(form, strong) %>%
          filter(str_starts(term, "treatment")) %>%
          mutate(subgroup = "Strong Supporters"),
        error = function(e) tibble()
      ),
      tryCatch(
        fit_lmer_and_tidy(form, others) %>%
          filter(str_starts(term, "treatment")) %>%
          mutate(subgroup = "Other"),
        error = function(e) tibble()
      )
    ) %>%
      mutate(dv = dv, dv_name = dv_name, dv_type = dv_type, study = "study2")
  })

# ---- Ceiling-exclusion robustness check --------------------------------------
# Exclude participants at the scale ceiling (pre-treatment = 7) and re-estimate
# CATEs and interaction models to rule out ceiling effects as an explanation for
# the weaker attitude effects among strong supporters.

s1_noceiling <- load_study1() %>% filter(pretreatment_agreement < 7) %>% droplevels()
s2_noceiling <- load_study2() %>% filter(pretreatment_agreement < 7)

s1_cov_nosupport <- "scale(pretreatment_organization_opinion) + scale(charity_interest_num) + (1|petition_title)"

ceiling_dvs <- tribble(
  ~dv,                    ~dv_name,            ~dv_type,
  "delta_petition",       "Support petition",  "Attitude",
  "1 * signed_petition",  "Sign petition",     "Behavior"
)

s1_ceiling_cates <- ceiling_dvs %>%
  pmap_dfr(function(dv, dv_name, dv_type) {
    strong <- s1_noceiling %>% filter(pretreatment_agreement > 5)
    others <- s1_noceiling %>% filter(pretreatment_agreement <= 5)
    bind_rows(
      fit_lmer_and_tidy(as.formula(str_glue("{dv} ~ treatment + {s1_cov_nosupport}")), strong) %>%
        filter(startsWith(term, "treatment")) %>%
        mutate(subgroup = "Supporters (6 only)"),
      fit_lmer_and_tidy(as.formula(str_glue("{dv} ~ treatment + {s1_covariates}")), others) %>%
        filter(startsWith(term, "treatment")) %>%
        mutate(subgroup = "Other")
    ) %>%
      mutate(dv_name = dv_name, dv_type = dv_type, study = "study1")
  })

s2_ceiling_cates <- ceiling_dvs %>%
  pmap_dfr(function(dv, dv_name, dv_type) {
    s2_cov_nosupport <- "scale(pretreatment_organization_opinion) + (1|petition_title)"
    strong <- s2_noceiling %>% filter(pretreatment_agreement > 5)
    others <- s2_noceiling %>% filter(pretreatment_agreement <= 5)
    bind_rows(
      tryCatch(
        fit_lmer_and_tidy(as.formula(str_glue("{dv} ~ treatment + {s2_cov_nosupport}")), strong) %>%
          filter(str_starts(term, "treatment")) %>%
          mutate(subgroup = "Supporters (6 only)"),
        error = function(e) tibble()
      ),
      tryCatch(
        fit_lmer_and_tidy(as.formula(str_glue("{dv} ~ treatment + {s2_covariates}")), others) %>%
          filter(str_starts(term, "treatment")) %>%
          mutate(subgroup = "Other"),
        error = function(e) tibble()
      )
    ) %>%
      mutate(dv_name = dv_name, dv_type = dv_type, study = "study2")
  })

s2_ceiling_interactions <- ceiling_dvs %>%
  pmap_dfr(function(dv, dv_name, dv_type) {
    form <- as.formula(str_glue(
      "{dv} ~ treatment * scale(pretreatment_agreement) + scale(pretreatment_organization_opinion) + (1|petition_title)"
    ))
    tryCatch({
      fit_lmer_and_tidy(form, s2_noceiling) %>%
        mutate(dv = dv, dv_name = dv_name, dv_type = dv_type, study = "study2")
    }, error = function(e) tibble())
  })

# Pre-treatment distribution
s1_pre_dist <- load_study1() %>%
  count(pretreatment_agreement) %>%
  mutate(pct = n / sum(n))
s2_pre_dist <- load_study2() %>%
  count(pretreatment_agreement) %>%
  mutate(pct = n / sum(n))

results <- list(
  s1_cates = s1_cates,
  s2_cates = s2_cates,
  s2_interactions = s2_interactions,
  s1_ceiling_cates = s1_ceiling_cates,
  s2_ceiling_cates = s2_ceiling_cates,
  s2_ceiling_interactions = s2_ceiling_interactions,
  s1_pre_dist = s1_pre_dist,
  s2_pre_dist = s2_pre_dist
)

save_results(results, "heterogeneous_effects")

cat("=== Done ===\n")
