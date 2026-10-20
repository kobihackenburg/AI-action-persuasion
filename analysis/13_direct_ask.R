source("analysis/utils.R")

cat("=== 13: Direct asks vs. factual density (transcript analysis) ===\n")

# Transcript-based test of why behavioural effects are uniform across strategies
# while attitudinal effects are not. Every Study 2 conversation was classified by
# an LLM judge (Claude Sonnet 4.6, temperature 0; see SI Appendix, Section 2.14)
# for (a) whether the assistant made a direct ask to sign the petition (shipped
# as columns direct_ask / n_ask_messages / first_ask_message in study2_raw.csv;
# classifier in classifiers/direct_ask/),
# and (b) fact-checkable claims extracted per assistant message (shipped as
# data/study_2/fact_checks/study2_extract_fact_checks.csv, one row per extracted
# claim; claim text is omitted from the public file since only per-conversation
# claim counts are used here).
#
# Analyses:
#   1. By-strategy ask measures + heterogeneity test (treated only).
#   2. By-strategy factual-claim density + heterogeneity test.
#      646 analysed conversations (629 treated, 17 control) have no rows in the
#      extraction CSV; diagnostics suggest most were skipped rather than
#      genuinely zero-claim, so the primary variant treats them as NA and a
#      sensitivity variant treats them as 0.
#   3. Strategy-level correlations of ask rate and claim density with the
#      covariate-adjusted treatment effects (from study2_main_effects.rds) on
#      petition signing (behaviour) and petition support (attitude), with exact
#      permutation p-values (n = 8 strategies).
#   4. Exploratory individual-level models mirroring 11_info_power.R.

# ---- Merge claim counts into Study 2 data ------------------------------------

s2 <- load_study2()

claim_counts <- read_csv(
  "data/study_2/fact_checks/study2_extract_fact_checks.csv",
  col_select = c(chat_session_id), show_col_types = FALSE
) %>%
  count(chat_session_id, name = "n_claims")

s2 <- s2 %>%
  left_join(claim_counts, by = c("participant_id" = "chat_session_id")) %>%
  mutate(
    n_assistant_msgs = str_count(full_conversation_text, "ASSISTANT:"),
    n_claims_zero    = replace_na(n_claims, 0L),                    # sensitivity: missing = 0 claims
    claims_per_msg   = n_claims / pmax(n_assistant_msgs, 1)
  )

treated <- s2 %>%
  filter(treatment == "treatment") %>%
  mutate(strategy = factor(conversation_type))

cat(sprintf("  Treated: %d | with ask scores: %d | with claim counts: %d (missing %d)\n",
            nrow(treated), sum(!is.na(treated$direct_ask)),
            sum(!is.na(treated$n_claims)), sum(is.na(treated$n_claims))))

# ---- 1 & 2: By-strategy descriptives -----------------------------------------

by_strategy <- treated %>%
  group_by(strategy) %>%
  summarise(
    n              = n(),
    ask_rate       = mean(direct_ask),
    ask_se         = sqrt(ask_rate * (1 - ask_rate) / n),
    n_ask_messages = mean(n_ask_messages),
    first_ask_msg  = mean(first_ask_message, na.rm = TRUE),
    claims_mean    = mean(n_claims, na.rm = TRUE),
    claims_se      = sd(n_claims, na.rm = TRUE) / sqrt(sum(!is.na(n_claims))),
    claims_mean_zero = mean(n_claims_zero),
    claims_per_msg = mean(claims_per_msg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ask_ci_low  = ask_rate - 1.96 * ask_se,
    ask_ci_high = ask_rate + 1.96 * ask_se,
    claims_ci_low  = claims_mean - 1.96 * claims_se,
    claims_ci_high = claims_mean + 1.96 * claims_se
  )

control_ask_rate <- s2 %>% filter(treatment == "control") %>% pull(direct_ask) %>% mean()

cat("\n--- By-strategy transcript measures (treated) ---\n")
by_strategy %>%
  arrange(desc(ask_rate)) %>%
  pmap(function(...) {
    r <- list(...)
    cat(sprintf("  %-26s ask %5.1f%% [%4.1f, %4.1f]  n_asks %.2f  first ask #%.2f  claims %5.1f  claims/msg %.2f\n",
                r$strategy, r$ask_rate * 100, r$ask_ci_low * 100, r$ask_ci_high * 100,
                r$n_ask_messages, r$first_ask_msg, r$claims_mean, r$claims_per_msg))
  }) %>% invisible()
cat(sprintf("  %-26s ask %5.1f%%\n", "(control conversations)", control_ask_rate * 100))

# Heterogeneity across the 8 strategies: joint Wald test that all strategy
# contrasts are zero, from an lmer with the standard covariates (mirrors the
# Wald test in 02_study2_main_effects.R).
wald_heterogeneity <- function(dv, data) {
  m <- lmer(as.formula(str_glue("{dv} ~ strategy + {s2_covariates}")), data = data)
  beta <- fixef(m); V <- as.matrix(vcov(m))
  idx  <- grep("^strategy", names(beta))
  stat <- as.numeric(t(beta[idx]) %*% solve(V[idx, idx]) %*% beta[idx])
  tibble(dv = dv, chisq = stat, df = length(idx),
         p.value = pchisq(stat, df = length(idx), lower.tail = FALSE))
}

heterogeneity <- bind_rows(
  wald_heterogeneity("1 * direct_ask",   treated),
  wald_heterogeneity("n_ask_messages",   treated),
  wald_heterogeneity("n_claims",         treated),
  wald_heterogeneity("n_claims_zero",    treated)
)

cat("\n--- Heterogeneity across strategies (joint Wald) ---\n")
heterogeneity %>%
  pmap(function(...) {
    r <- list(...)
    cat(sprintf("  %-18s chi2(%d) = %8.1f, p = %.3g\n", r$dv, r$df, r$chisq, r$p.value))
  }) %>% invisible()

# ---- 3: Strategy-level correlations with adjusted treatment effects ----------

main_effects <- readRDS("output/results/study2_main_effects.rds")$strategy_effects %>%
  filter(dv %in% c("signed_petition", "delta_petition")) %>%
  dplyr::select(strategy, dv, estimate)

strategy_tbl <- by_strategy %>%
  dplyr::select(strategy, ask_rate, n_ask_messages, claims_mean, claims_mean_zero, claims_per_msg) %>%
  mutate(strategy = as.character(strategy)) %>%
  left_join(
    main_effects %>% pivot_wider(names_from = dv, values_from = estimate) %>%
      rename(behaviour_effect = signed_petition, attitude_effect = delta_petition),
    by = "strategy"
  )

stopifnot(!any(is.na(strategy_tbl$behaviour_effect)),
          !any(is.na(strategy_tbl$attitude_effect)))

# Exact permutation test for a Pearson correlation with n = 8 (8! = 40,320
# permutations; two-sided p = share of permutations with |r| >= |r_obs|).
all_perms <- function(n) {
  if (n == 1) return(matrix(1L, 1, 1))
  sub <- all_perms(n - 1)
  do.call(rbind, lapply(seq_len(n), function(k) {
    rest <- seq_len(n)[-k]
    cbind(k, matrix(rest[sub], nrow(sub), n - 1))
  }))
}

perm_cor_test <- function(x, y) {
  r_obs <- cor(x, y)
  perms <- all_perms(length(y))
  r_all <- apply(perms, 1, function(idx) cor(x, y[idx]))
  tibble(r = r_obs, p.perm = mean(abs(r_all) >= abs(r_obs) - 1e-12))
}

cor_grid <- expand_grid(
  measure = c("ask_rate", "n_ask_messages", "claims_mean", "claims_mean_zero", "claims_per_msg"),
  outcome = c("behaviour_effect", "attitude_effect")
) %>%
  pmap_dfr(function(measure, outcome) {
    perm_cor_test(strategy_tbl[[measure]], strategy_tbl[[outcome]]) %>%
      mutate(measure = measure, outcome = outcome, .before = 1)
  })

cat("\n--- Strategy-level correlations with adjusted treatment effects (n = 8) ---\n")
cor_grid %>%
  pmap(function(...) {
    r <- list(...)
    cat(sprintf("  %-18s x %-17s r = %+.2f  p.perm = %.3f\n", r$measure, r$outcome, r$r, r$p.perm))
  }) %>% invisible()

# ---- 4: Exploratory individual-level models (mirrors 11_info_power.R) --------

d <- treated %>%
  filter(!is.na(delta_petition), !is.na(signed_petition)) %>%
  mutate(
    pa    = as.numeric(scale(pretreatment_agreement)),
    po    = as.numeric(scale(pretreatment_organization_opinion)),
    z_att = as.numeric(scale(delta_petition)),
    z_beh = as.numeric(scale(as.numeric(signed_petition))),
    sign  = as.numeric(signed_petition),
    ask   = as.numeric(direct_ask),
    z_claims = as.numeric(scale(n_claims))
  )

ind_effect <- function(data, outcome, predictor, outcome_type, unit) {
  rhs <- str_glue("{predictor} + strategy + pa + po + (1|petition_title)")
  m <- lmer(as.formula(str_glue("{outcome} ~ {rhs}")), data = data)
  tt <- broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE) %>%
    filter(term == predictor)
  tibble(
    predictor = predictor, outcome_type = outcome_type, unit = unit,
    estimate = tt$estimate, conf.low = tt$conf.low, conf.high = tt$conf.high,
    p.value = 2 * pnorm(abs(tt$statistic), lower.tail = FALSE),
    n = nobs(m)
  )
}

individual_models <- bind_rows(
  ind_effect(d, "z_beh", "ask",      "behaviour", "SD"),
  ind_effect(d, "sign",  "ask",      "behaviour", "pp"),
  ind_effect(d, "z_att", "ask",      "attitude",  "SD"),
  ind_effect(d, "z_beh", "z_claims", "behaviour", "SD"),
  ind_effect(d, "sign",  "z_claims", "behaviour", "pp"),
  ind_effect(d, "z_att", "z_claims", "attitude",  "SD")
)

cat("\n--- Individual-level models (treated, strategy FE + covariates; exploratory) ---\n")
individual_models %>%
  pmap(function(...) {
    r <- list(...)
    mult <- if (r$unit == "pp") 100 else 1
    suf  <- if (r$unit == "pp") "pp" else " SD"
    cat(sprintf("  %-9s -> %-9s (%s): %+.3f%s [%+.3f, %+.3f]  p = %.3g  (n = %d)\n",
                r$predictor, r$outcome_type, r$unit, r$estimate * mult, suf,
                r$conf.low * mult, r$conf.high * mult, r$p.value, r$n))
  }) %>% invisible()

# ---- Save ---------------------------------------------------------------------

results <- list(
  by_strategy       = by_strategy,
  control_ask_rate  = control_ask_rate,
  heterogeneity     = heterogeneity,
  strategy_tbl      = strategy_tbl,
  correlations      = cor_grid,
  individual_models = individual_models,
  n = list(
    treated = nrow(treated),
    claims_missing = sum(is.na(treated$n_claims)),
    control = sum(s2$treatment == "control")
  )
)
save_results(results, "direct_ask")

cat("\n=== Done ===\n")
