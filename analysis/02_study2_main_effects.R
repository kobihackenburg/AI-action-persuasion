source("analysis/utils.R")

cat("=== 02: Study 2 Main Effects ===\n")

df <- load_study2() %>%
  mutate(strategy = ifelse(treatment == "control", "control", conversation_type) %>%
           fct_relevel("control"))

# ---- Analysis 1: Strategy effects on petition signing (H1) -------------------
# Pre-registered: lmer(dv ~ strategy + scale(pre_petition) + scale(pre_org) + (1|issue))

results_list <- s2_dvs %>%
  pmap(function(dv, dv_name, dv_type) {
    form <- as.formula(str_glue(
      "1 * ({dv}) ~ strategy + {s2_covariates}"
    ))

    tidy_df <- fit_lmer_and_tidy(form, df) %>%
      filter(str_starts(term, "strategy")) %>%
      mutate(
        p.value = 2 * pnorm(abs(statistic), lower.tail = FALSE),
        strategy = str_remove(term, "strategy"),
        dv = dv,
        dv_name = dv_name,
        dv_type = dv_type
      )

    if (dv == "signed_petition") {
      tidy_df <- tidy_df %>%
        mutate(p.adjusted = p.adjust(p.value, method = "BH"))
    } else {
      tidy_df <- tidy_df %>%
        mutate(p.adjusted = NA_real_)
    }

    tidy_df
  })

results <- bind_rows(results_list)

# ---- Analysis 1b: Info-Issue drives attitudes, not behavior (H2) -------------
# Compare info-issue rank on attitudinal vs. behavioral outcomes

info_issue_ranks <- results %>%
  group_by(dv, dv_type) %>%
  mutate(rank = rank(-estimate)) %>%
  filter(str_detect(strategy, "info.issue")) %>%
  select(dv, dv_type, strategy, estimate, conf.low, conf.high, rank)

results_h2 <- list(
  strategy_effects = results,
  info_issue_ranks = info_issue_ranks
)

# ---- Analysis 1c: Info-Impact Efficacy comparison ----------------------------

impact_efficacy_ranks <- results %>%
  group_by(dv, dv_type) %>%
  mutate(rank = rank(-estimate)) %>%
  filter(str_detect(strategy, "info.impact")) %>%
  select(dv, dv_type, strategy, estimate, conf.low, conf.high, rank)

results_h2$impact_efficacy_ranks <- impact_efficacy_ranks

# ---- Overall treatment-vs-control ATEs (for Fig 1) --------------------------

s2_covariates_no_re <- "scale(pretreatment_agreement) + scale(pretreatment_organization_opinion)"

overall_ates <- s2_dvs %>%
  pmap_dfr(function(dv, dv_name, dv_type) {
    form <- as.formula(str_glue("1 * ({dv}) ~ treatment + {s2_covariates}"))
    fit_lmer_and_tidy(form, df) %>%
      filter(str_starts(term, "treatment")) %>%
      mutate(grp = "All", facet = "Overall", dv = dv, dv_name = dv_name, dv_type = dv_type)
  })

by_issue_ates <- s2_dvs %>%
  pmap_dfr(function(dv, dv_name, dv_type) {
    df %>%
      group_by(petition_title) %>%
      group_modify(function(d, k) {
        form <- as.formula(str_glue("1 * ({dv}) ~ treatment + {s2_covariates_no_re}"))
        tryCatch(
          fit_and_tidy(form, d) %>%
            filter(startsWith(term, "treatment")) %>%
            mutate(facet = "By topic"),
          error = function(e) tibble()
        )
      }) %>%
      mutate(grp = petition_title, dv = dv, dv_name = dv_name, dv_type = dv_type)
  })

results_h2$overall_ates <- bind_rows(overall_ates, by_issue_ates)

# ---- Wald test for heterogeneity across strategies (petition signing) --------
# Uses the lmer vcov matrix, which properly accounts for shared-control
# covariance, covariates, and issue random effects.

fit_petition <- lmer(
  as.formula(str_glue("1 * signed_petition ~ strategy + {s2_covariates}")),
  data = df
)

beta <- fixef(fit_petition)
V    <- as.matrix(vcov(fit_petition))
strat_idx  <- grep("^strategy", names(beta))
strat_beta <- beta[strat_idx]
strat_V    <- V[strat_idx, strat_idx]

k <- length(strat_beta)
L <- matrix(0, nrow = k - 1, ncol = k)
for (i in seq_len(k - 1)) { L[i, 1] <- 1; L[i, i + 1] <- -1 }

Lb        <- L %*% strat_beta
wald_stat <- as.numeric(t(Lb) %*% solve(L %*% strat_V %*% t(L)) %*% Lb)
wald_df   <- k - 1
wald_p    <- pchisq(wald_stat, df = wald_df, lower.tail = FALSE)
I2        <- max(0, (wald_stat - wald_df) / wald_stat * 100)

results_h2$q_test <- tibble(
  Q    = wald_stat,
  Q_df = wald_df,
  Q_p  = wald_p,
  I2   = I2
)
cat(sprintf("  Wald test for heterogeneity: chi2(%d) = %.2f, p = %.4f, I2 = %.1f%%\n",
            wald_df, wald_stat, wald_p, I2))

save_results(results_h2, "study2_main_effects")

cat("=== Done ===\n")
