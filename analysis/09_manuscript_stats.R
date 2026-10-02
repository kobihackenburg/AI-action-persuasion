source("analysis/utils.R")

cat("=== 09: Manuscript Statistics ===\n\n")

# ---- Descriptive stats -------------------------------------------------------
desc <- readRDS("output/results/descriptive_stats.rds")

cat("--- DESCRIPTIVE STATS ---\n")
cat(sprintf("  Study 1 final N:          %s\n", format(desc$sample$s1_final_n, big.mark = ",")))
cat(sprintf("  Study 2 final N:          %s\n", format(desc$sample$s2_final_n, big.mark = ",")))
cat(sprintf("  Total observations:       %s\n", format(desc$sample$total_observations, big.mark = ",")))
cat(sprintf("  Unique individuals:       %s\n", format(desc$sample$unique_individuals, big.mark = ",")))
cat(sprintf("  Overlap (both studies):   %d\n", desc$sample$overlap_n))
cat(sprintf("  Mean turns (overall):     %.1f\n", desc$conversations$overall_mean_turns))
cat(sprintf("  Mean duration (overall):  %.1f min\n", desc$conversations$overall_mean_duration_min))
cat(sprintf("  Mean turns (S1):          %.1f\n", desc$conversations$s1_mean_turns))
cat(sprintf("  Mean turns (S2):          %.1f\n", desc$conversations$s2_mean_turns))
cat(sprintf("  Mean duration (S1):       %.1f min\n", desc$conversations$s1_mean_duration_min))
cat(sprintf("  Mean duration (S2):       %.1f min\n", desc$conversations$s2_mean_duration_min))

cat("\n--- POST-TREATMENT ATTRITION ---\n")
cat(sprintf("  Study 1: %.1f%% (%d/%d) — %s\n",
            desc$attrition$s1$rate * 100,
            desc$attrition$s1$n_attrited,
            desc$attrition$s1$n_raw,
            desc$attrition$s1$note))
cat(sprintf("  Study 2: %.1f%% (%d/%d reached treatment)\n",
            desc$attrition$s2$rate * 100,
            desc$attrition$s2$n_attrited,
            desc$attrition$s2$n_reached))

# ---- Study 1 main effects ----------------------------------------------------
s1_main <- readRDS("output/results/study1_main_effects.rds")

cat("\n--- STUDY 1 MAIN EFFECTS (Overall ATEs) ---\n")
s1_overall <- s1_main %>% filter(facet == "Overall")
for (i in seq_len(nrow(s1_overall))) {
  row <- s1_overall[i, ]
  cat(sprintf("  %-40s  est = %+.4f  [%.4f, %.4f]  p = %.4f\n",
              row$dv, row$estimate, row$conf.low, row$conf.high, row$p.value))
}

# ---- Study 2 main effects ----------------------------------------------------
s2_main <- readRDS("output/results/study2_main_effects.rds")

cat("\n--- STUDY 2 OVERALL ATEs (treatment vs control) ---\n")
s2_overall <- s2_main$overall_ates %>% filter(facet == "Overall")
for (i in seq_len(nrow(s2_overall))) {
  row <- s2_overall[i, ]
  p_val <- 2 * pnorm(abs(row$statistic), lower.tail = FALSE)
  cat(sprintf("  %-40s  est = %+.4f  [%.4f, %.4f]  z = %.3f  p = %.6f\n",
              row$dv, row$estimate, row$conf.low, row$conf.high, row$statistic, p_val))
}

cat("\n--- STUDY 2 STRATEGY EFFECTS (petition signing) ---\n")
signing <- s2_main$strategy_effects %>%
  filter(dv == "signed_petition") %>%
  arrange(desc(estimate))
for (i in seq_len(nrow(signing))) {
  row <- signing[i, ]
  cat(sprintf("  %-35s  est = %+.4f (%+.1f pp)  [%.4f, %.4f]  p = %.6f  p.adj = %.6f\n",
              row$strategy, row$estimate, row$estimate * 100,
              row$conf.low, row$conf.high, row$p.value, row$p.adjusted))
}

cat("\n--- STUDY 2 WALD TEST (strategy heterogeneity, petition signing) ---\n")
q <- s2_main$q_test
cat(sprintf("  chi2(%d) = %.2f, p = %.4f, I2 = %.1f%%\n", q$Q_df, q$Q, q$Q_p, q$I2))

cat("\n--- STUDY 2 STRATEGY EFFECTS (attitude: delta_petition) ---\n")
att_pet <- s2_main$strategy_effects %>%
  filter(dv == "delta_petition") %>%
  arrange(desc(estimate))
for (i in seq_len(nrow(att_pet))) {
  row <- att_pet[i, ]
  cat(sprintf("  %-35s  est = %+.4f  [%.4f, %.4f]  p = %.6f\n",
              row$strategy, row$estimate, row$conf.low, row$conf.high, row$p.value))
}

cat("\n--- STUDY 2 STRATEGY EFFECTS (attitude: delta_organization) ---\n")
att_org <- s2_main$strategy_effects %>%
  filter(dv == "delta_organization") %>%
  arrange(desc(estimate))
for (i in seq_len(nrow(att_org))) {
  row <- att_org[i, ]
  cat(sprintf("  %-35s  est = %+.4f  [%.4f, %.4f]  p = %.6f\n",
              row$strategy, row$estimate, row$conf.low, row$conf.high, row$p.value))
}

cat("\n--- STUDY 2 STRATEGY EFFECTS (chose_assigned_org) ---\n")
org_choice <- s2_main$strategy_effects %>%
  filter(dv == "chose_assigned_org") %>%
  arrange(desc(estimate))
for (i in seq_len(nrow(org_choice))) {
  row <- org_choice[i, ]
  cat(sprintf("  %-35s  est = %+.4f (%+.1f pp)  [%.4f, %.4f]  p = %.6f\n",
              row$strategy, row$estimate, row$estimate * 100,
              row$conf.low, row$conf.high, row$p.value))
}

# ---- Belief-behavior correlations --------------------------------------------
bb <- readRDS("output/results/belief_behavior.rds")

cat("\n--- BELIEF-BEHAVIOR CORRELATIONS ---\n")
for (i in seq_len(nrow(bb$correlations))) {
  row <- bb$correlations[i, ]
  cat(sprintf("  %s (%s):  r = %.2f, p = %.4f\n",
              row$study, row$outcome_pair, row$r, row$p.value))
}

# ---- Mechanisms --------------------------------------------------------------
mech <- readRDS("output/results/mechanisms.rds")

cat("\n--- STUDY 1 LEARNING CORRELATIONS ---\n")
for (i in seq_len(nrow(mech$learning_correlations))) {
  row <- mech$learning_correlations[i, ]
  cat(sprintf("  %s (%s):  r = %.2f, p = %.4f  (n = %d)\n",
              row$dv, row$dv_type, row$r, row$p.value, row$n))
}

cat("\n--- STUDY 2 LEARNING CORRELATIONS ---\n")
s2_learning <- mech$s2_learning_estimates
s2_dvs_list <- list(
  list(name = "delta_petition", type = "Attitude"),
  list(name = "signed_petition", type = "Behavior"),
  list(name = "delta_organization", type = "Attitude"),
  list(name = "chose_assigned_org", type = "Behavior")
)
for (dv_info in s2_dvs_list) {
  ate_df <- mech$s2_condition_ates[[dv_info$name]]
  if (is.null(ate_df) || nrow(ate_df) < 3) next
  joined <- left_join(
    s2_learning %>% select(term, learning = estimate),
    ate_df %>% select(term, ate = estimate),
    by = "term"
  )
  if (nrow(joined) >= 3) {
    ct <- cor.test(joined$learning, joined$ate)
    cat(sprintf("  %s (%s):  r = %.2f, p = %.4f  (n = %d)\n",
                dv_info$name, dv_info$type, ct$estimate, ct$p.value, nrow(joined)))
  }
}

cat("\n--- STUDY 1 PROMPT-TYPE EFFECTS ---\n")
for (i in seq_len(nrow(mech$prompt_type_effects))) {
  row <- mech$prompt_type_effects[i, ]
  cat(sprintf("  %s | %-25s  est = %+.4f  [%.4f, %.4f]\n",
              row$prompt, row$dv_name, row$estimate, row$conf.low, row$conf.high))
}

cat("\n--- STUDY 2 INFO-ISSUE vs OTHER EFFECTS ---\n")
for (i in seq_len(nrow(mech$s2_info_issue_effects))) {
  row <- mech$s2_info_issue_effects[i, ]
  cat(sprintf("  %s | %-25s  est = %+.4f  [%.4f, %.4f]  p = %.6f\n",
              row$prompt, row$dv_name, row$estimate, row$conf.low, row$conf.high,
              2 * pnorm(abs(row$statistic), lower.tail = FALSE)))
}

cat("\n--- STUDY 2 MANIPULATION CHECKS (targeted-mechanism composite, focal strategy vs. other strategies) ---\n")
mc <- mech$manipulation_checks_composite
if (!is.null(mc) && nrow(mc) > 0) {
  for (i in seq_len(nrow(mc))) {
    row <- mc[i, ]
    cat(sprintf("  %-26s  focal = %.1f  other = %.1f  est = %+.2f  [%.2f, %.2f]  p = %.4g\n",
                row$strategy, row$focal_mean, row$other_mean,
                row$estimate, row$conf.low, row$conf.high, row$p.value))
  }
}

# ---- Info-Issue rank analysis ------------------------------------------------
cat("\n--- STUDY 2 INFO-ISSUE RANKS ---\n")
for (i in seq_len(nrow(s2_main$info_issue_ranks))) {
  row <- s2_main$info_issue_ranks[i, ]
  cat(sprintf("  %s (%s): rank = %d/%d, est = %+.4f\n",
              row$dv, row$dv_type, row$rank, 8, row$estimate))
}

# ---- Monetary magnitude of donation effects ---------------------------------
# Additional money the sponsoring organisations received as a causal result of
# the AI conversation (treatment vs. randomised control), expressed in pounds.
# Counterfactual = control group (cleaner than stated pre-treatment intentions).
cat("\n--- MONETARY MAGNITUDE (additional org. money: treatment vs. control) ---\n")

s1_money <- load_study1() %>%
  mutate(treat = ifelse(treatment == "control", "control", "treated") %>%
           fct_relevel("control"))
s2_money <- load_study2() %>%
  mutate(treat = ifelse(treatment == "control", "control", "treated") %>%
           fct_relevel("control"))

money_ate <- function(d, dv, covars) {
  form <- as.formula(str_glue("{dv} ~ treat + {covars}"))
  fit_lmer_and_tidy(form, d) %>%
    filter(term == "treattreated") %>%
    mutate(p.value = 2 * pnorm(abs(statistic), lower.tail = FALSE))
}

CLICK_VALUE <- 0.03   # GBP per click
ALLOC_VALUE <- 0.25   # GBP allocation in Study 2

n_treat_s1 <- sum(s1_money$treat == "treated")
n_treat_s2 <- sum(s2_money$treat == "treated")

# Study 1: bonus donation (GBP), already in pounds
don <- money_ate(s1_money, "donation_amount", s1_covariates_donation)
# Study 1: click task -> GBP via CLICK_VALUE
clk <- money_ate(s1_money, "extra_task_clicks", s1_covariates)
clk_gbp <- clk %>% mutate(across(c(estimate, conf.low, conf.high), ~ .x * CLICK_VALUE))
# Study 2: allocation -> GBP via ALLOC_VALUE
alloc <- money_ate(s2_money, "1 * chose_assigned_org", s2_covariates)
alloc_gbp <- alloc %>% mutate(across(c(estimate, conf.low, conf.high), ~ .x * ALLOC_VALUE))

report_money <- function(label, est, lo, hi, p, n_treat) {
  cat(sprintf(
    "  %-34s  +GBP%.3f [%.3f, %.3f] p=%.3g | per 1,000 = +GBP%.0f [%.0f, %.0f] | sample total = +GBP%.0f\n",
    label, est, lo, hi, p, est * 1000, lo * 1000, hi * 1000, est * n_treat))
}

report_money("S1 bonus donation (GBP)", don$estimate, don$conf.low, don$conf.high, don$p.value, n_treat_s1)
report_money("S1 click task (GBP)", clk_gbp$estimate, clk_gbp$conf.low, clk_gbp$conf.high, clk_gbp$p.value, n_treat_s1)
cat(sprintf("  %-34s  +GBP%.3f per conversation | per 1,000 = +GBP%.0f | sample total = +GBP%.0f\n",
            "S1 donation + clicks combined",
            don$estimate + clk_gbp$estimate,
            (don$estimate + clk_gbp$estimate) * 1000,
            (don$estimate + clk_gbp$estimate) * n_treat_s1))
report_money("S2 allocation (GBP)", alloc_gbp$estimate, alloc_gbp$conf.low, alloc_gbp$conf.high, alloc_gbp$p.value, n_treat_s2)
cat(sprintf("  control/treated donation means (S1): GBP%.3f / GBP%.3f | N treated S1=%d, S2=%d\n",
            mean(s1_money$donation_amount[s1_money$treat == "control"], na.rm = TRUE),
            mean(s1_money$donation_amount[s1_money$treat == "treated"], na.rm = TRUE),
            n_treat_s1, n_treat_s2))

# ---- Base rates (control vs. treated) ---------------------------------------
# Control-group base rates for the binary behavioural outcomes, to contextualise
# the headline effect sizes (reported alongside the adjusted ATEs in the text).
cat("\n--- BASE RATES (control vs. treated, observed group means) ---\n")
base_rate <- function(d, dv, label, pct = TRUE) {
  ctrl <- mean(d[[dv]][d$treat == "control"], na.rm = TRUE)
  trt  <- mean(d[[dv]][d$treat == "treated"], na.rm = TRUE)
  if (pct) {
    cat(sprintf("  %-34s  control = %.1f%%  treated = %.1f%%  (diff = %+.1fpp)\n",
                label, ctrl * 100, trt * 100, (trt - ctrl) * 100))
  } else {
    cat(sprintf("  %-34s  control = %.3f  treated = %.3f  (diff = %+.3f)\n",
                label, ctrl, trt, trt - ctrl))
  }
}
base_rate(s1_money, "signed_petition", "S1 sign petition")
base_rate(s1_money, "not_switch_organization", "S1 choose org (vs. switch)")
base_rate(s1_money, "donate_proportion", "S1 bonus donation (proportion)", pct = FALSE)
base_rate(s2_money, "signed_petition", "S2 sign petition")
base_rate(s2_money, "chose_assigned_org", "S2 allocate to org")

cat("\n=== Done ===\n")
