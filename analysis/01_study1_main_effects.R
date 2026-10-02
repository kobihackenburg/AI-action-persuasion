source("analysis/utils.R")

cat("=== 01: Study 1 Main Effects ===\n")

df <- load_study1()

# ---- Primary analysis: ATEs on all 6 DVs ------------------------------------
# lmer: dv ~ treatment + covariates + (1|petition_title), matching pre-registration

results <- s1_dvs %>%
  pmap_dfr(function(dv, dv_name, dv_type) {
    covariates <- s1_covariates
    if (str_detect(dv, "donat")) {
      covariates <- s1_covariates_donation
    }

    # Overall (all issues pooled)
    form_overall <- as.formula(str_glue("1 * ({dv}) ~ treatment + {covariates}"))
    d_overall <- fit_lmer_and_tidy(form_overall, df) %>%
      filter(startsWith(term, "treatment")) %>%
      mutate(grp = "All", facet = "Overall")

    # By issue (OLS within each issue -- no random intercept needed)
    covariates_by_issue <- s1_covariates_base
    if (str_detect(dv, "donat")) covariates_by_issue <- paste0(s1_covariates_base, " + randomized_bonus_amount")
    form_by_issue <- as.formula(str_glue("1 * ({dv}) ~ treatment + {covariates_by_issue}"))
    d_by_issue <- df %>%
      group_by(petition_title) %>%
      group_modify(function(d, k) {
        fit_and_tidy(form_by_issue, d) %>%
          filter(startsWith(term, "treatment")) %>%
          mutate(facet = "By topic")
      }) %>%
      mutate(grp = petition_title)

    bind_rows(d_overall, d_by_issue) %>%
      mutate(dv = dv, dv_name = dv_name, dv_type = dv_type)
  })

save_results(results, "study1_main_effects")

cat("=== Done ===\n")
