source("analysis/utils.R")

cat("=== 11: Information effect on attitudes vs. behaviour (power) ===\n")

# Directly-powered, individual-level test of the attitude-behaviour dissociation.
# Rather than the (low-powered) cross-condition correlation, we estimate the
# effect of information provision (the Information (Issue) prompt vs. all other
# treatment prompts) separately on attitudes and behaviour, among treated
# participants, at the individual level. The power argument:
#   - we DETECT a significant effect of information on behaviour when pooling the
#     two studies (so we are not underpowered to detect an info-behaviour link);
#   - its 95% CI excludes positive values, so we can rule out information being
#     MORE effective for behaviour;
#   - meanwhile information is strongly, significantly POSITIVE on attitudes.
# All "info vs. other" contrasts are vs. OTHER TREATMENT PROMPTS, not vs. control
# (information remains strongly positive vs. control on behaviour).
#
# Model (per study, treated only):
#   outcome ~ info + scale(pre_agreement) + scale(pre_org_opinion) + (1 | issue)
# Pooled: stack both studies, add a study fixed effect; z-scored outcomes are
# standardised within study before stacking. Each participant contributes one row
# per model (no stacking of outcomes / participant random effect needed).

prep <- function(df, id, info_flag, study) {
  df %>%
    mutate(.id = .data[[id]], .info_flag = info_flag, study = study) %>%
    filter(!is.na(delta_petition), !is.na(signed_petition)) %>%
    transmute(
      .id, study, petition_title,
      info_group = factor(if_else(.info_flag, "info", "other"), levels = c("other", "info")),
      pa = as.numeric(scale(pretreatment_agreement)),
      po = as.numeric(scale(pretreatment_organization_opinion)),
      z_att = as.numeric(scale(delta_petition)),
      z_beh = as.numeric(scale(as.numeric(signed_petition))),
      sign  = as.numeric(signed_petition)
    )
}

s1 <- load_study1() %>% filter(treatment == "treatment")
s2 <- load_study2() %>% filter(treatment == "treatment")
d1 <- prep(s1, "participantId",  s1$prompt_category == "info",                "study1")
d2 <- prep(s2, "participant_id", s2$conversation_type_clean == "info_issue", "study2")
pooled <- bind_rows(d1, d2)

n_info_s1 <- sum(d1$info_group == "info"); n_info_s2 <- sum(d2$info_group == "info")

# Fit one model and return the info-vs-other coefficient row.
info_effect <- function(data, outcome, scope, outcome_type, unit, study_fe = FALSE) {
  rhs <- if (study_fe) "info_group + study + pa + po + (1|petition_title)"
         else          "info_group + pa + po + (1|petition_title)"
  m <- lmer(as.formula(paste0(outcome, " ~ ", rhs)), data = data)
  tt <- broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE) %>%
    filter(term == "info_groupinfo")
  tibble(
    scope = scope, outcome_type = outcome_type, unit = unit,
    estimate = tt$estimate, conf.low = tt$conf.low, conf.high = tt$conf.high,
    p.value = 2 * pnorm(abs(tt$statistic), lower.tail = FALSE),
    n = nrow(data)
  )
}

results_tbl <- bind_rows(
  info_effect(d1,     "z_att", "Study 1", "attitude",  "SD"),
  info_effect(d2,     "z_att", "Study 2", "attitude",  "SD"),
  info_effect(pooled, "z_att", "Pooled",  "attitude",  "SD", study_fe = TRUE),
  info_effect(d1,     "z_beh", "Study 1", "behaviour", "SD"),
  info_effect(d2,     "z_beh", "Study 2", "behaviour", "SD"),
  info_effect(pooled, "z_beh", "Pooled",  "behaviour", "SD", study_fe = TRUE),
  info_effect(d1,     "sign",  "Study 1", "behaviour", "pp"),
  info_effect(d2,     "sign",  "Study 2", "behaviour", "pp"),
  info_effect(pooled, "sign",  "Pooled",  "behaviour", "pp", study_fe = TRUE)
)

fmt <- function(row) {
  scale_mult <- if (row$unit == "pp") 100 else 1
  suf <- if (row$unit == "pp") "pp" else " SD"
  sprintf("  %-8s %-9s (%s):  %+.3f%s  [%+.3f, %+.3f]  p = %.4g",
          row$scope, row$outcome_type, row$unit,
          row$estimate * scale_mult, suf,
          row$conf.low * scale_mult, row$conf.high * scale_mult, row$p.value)
}

cat(sprintf("\nN treated: S1 = %d (info = %d), S2 = %d (info = %d), pooled = %d\n",
            nrow(d1), n_info_s1, nrow(d2), n_info_s2, nrow(pooled)))

cat("\n--- Information (Issue) vs. other prompts: effect on ATTITUDE (petition support) ---\n")
for (i in which(results_tbl$outcome_type == "attitude")) cat(fmt(results_tbl[i, ]), "\n")

cat("\n--- Information (Issue) vs. other prompts: effect on BEHAVIOUR (petition signing) ---\n")
for (i in which(results_tbl$outcome_type == "behaviour")) cat(fmt(results_tbl[i, ]), "\n")

results <- list(
  estimates = results_tbl,
  n = list(s1 = nrow(d1), s2 = nrow(d2), pooled = nrow(pooled),
           info_s1 = n_info_s1, info_s2 = n_info_s2)
)
save_results(results, "info_power")

cat("\n=== Done ===\n")
