source("analysis/utils.R")

cat("=== 14: Latent effect correlation (attitude <-> behaviour) ===\n")

# Latent (disattenuated) between-condition correlation r between the true
# treatment effect on the attitude DV and the true effect on the behaviour DV,
# with profile-likelihood 95% CIs:
#
#   Stage 1: systemfit SUR estimates the per-arm, covariate- and issue-adjusted
#            means on both DVs jointly (treated arms only), so the cross-DV
#            sampling covariance (same participants) is captured. Adjusted arm
#            means differ from ATEs vs. control only by a constant shift, which
#            leaves the between-arm correlation unchanged.
#   Stage 2: metafor rma.mv (~ dv | arm, struct = "UN", ML) over the 2*k arm
#            estimates; the between-arm correlation rho is the estimand, with a
#            profile CI (clamped to +/-1 where the profile hits the boundary).
#
# Cells: Study 1 | Study 2 | Combined, for the primary outcome pair (petition
# support <-> petition signing). The studies are independent, so the joint
# profile for a shared r is the sum of the per-study profile log-likelihoods
# (ML throughout so profiles combine additively); the Combined cell reports the
# peak and 1.92-drop CI of that summed profile.
#
# Deps beyond utils.R: systemfit, metafor, Matrix. Runtime: a few seconds.
# Output: output/results/latent_effect_correlation.rds
#         (figure rendered by analysis/figures/si/fig_si_latent_correlation.R)

library(systemfit)
library(metafor)
library(Matrix)

THR <- qchisq(0.95, 1) / 2   # 1.9207: 95% CI = rel-logLik >= -THR

# ---- Data: one row per treated participant, per outcome pair ----------------

load_pair <- function(study, outcome) {
  if (study == "s1") {
    d <- load_study1() %>%
      filter(!is.na(model_type), treatment_type != "control") %>%
      mutate(arm = paste(treatment_type, model_type))
  } else {
    d <- load_study2() %>%
      filter(treatment != "control") %>%
      mutate(arm = as.character(conversation_type_clean))
  }
  beh <- switch(outcome,
    petition = as.numeric(d$signed_petition),
    org      = if (study == "s1") as.numeric(d$org_behavior)      else as.numeric(d$chose_assigned_org),
    donation = if (study == "s1") as.numeric(d$donate_proportion) else as.numeric(d$chose_assigned_org))
  att <- if (outcome == "petition") as.numeric(d$delta_petition) else as.numeric(d$delta_organization)
  tibble(
    arm   = paste0(study, ":", d$arm),
    issue = paste0(study, ":", d$petition_title),
    att   = att, beh = beh,
    z_pre_ag  = as.numeric(scale(d$pretreatment_agreement)),
    z_pre_org = as.numeric(scale(d$pretreatment_organization_opinion)))
}

# ---- Profile helpers ---------------------------------------------------------

clamp1 <- function(prof, lo, hi) {
  if (hi >= max(prof$r) - 1e-6) hi <- 1
  if (lo <= min(prof$r) + 1e-6) lo <- -1
  c(lo, hi)
}
bounds <- function(prof, thr = THR) {
  rel <- prof$rel - max(prof$rel, na.rm = TRUE)
  ok  <- which(rel >= -thr)
  b <- clamp1(prof, min(prof$r[ok]), max(prof$r[ok]))
  c(lo = b[1], hi = b[2])
}
combine_profiles <- function(p1, p2) {
  g <- seq(max(min(p1$r), min(p2$r)), min(max(p1$r), max(p2$r)), by = 0.005)
  i1 <- spline(p1$r, p1$rel, xout = g)$y
  i2 <- spline(p2$r, p2$rel, xout = g)$y
  tibble(r = g, rel = (i1 - max(i1)) + (i2 - max(i2)))
}

# ---- Two-stage: SUR -> rma.mv ------------------------------------------------

sur_arms <- function(d) {
  d <- d %>% mutate(arm = factor(arm), issue = factor(issue))
  sur <- systemfit(list(att = att ~ 0 + arm + issue + z_pre_ag + z_pre_org,
                        beh = beh ~ 0 + arm + issue + z_pre_ag + z_pre_org),
                   method = "SUR", data = d)
  cf <- coef(sur); V <- vcov(sur); k <- grep("arm", names(cf))
  list(cf = cf[k], V = as.matrix(V[k, k]), nm = names(cf)[k])
}
ts_model <- function(parts) {
  cf <- unlist(lapply(parts, `[[`, "cf")); nm <- unlist(lapply(parts, `[[`, "nm"))
  est <- tibble(yi  = as.numeric(cf),
                dv  = factor(ifelse(str_detect(nm, "^att_"), "att", "beh")),
                arm = factor(str_remove(nm, "^(att|beh)_arm")))
  rma.mv(yi = yi, V = as.matrix(bdiag(lapply(parts, `[[`, "V"))),
         mods = ~ 0 + dv, random = ~ dv | arm, struct = "UN", data = est, method = "ML")
}
ts_profile <- function(m) {
  pr <- profile(m, rho = 1, xlim = c(-0.995, 0.995), steps = 90, progbar = FALSE, plot = FALSE)
  tibble(r = pr$rho, rel = pr$ll - max(pr$ll, na.rm = TRUE)) %>%
    filter(is.finite(r), is.finite(rel))
}

# ---- Run all cells -----------------------------------------------------------

run_outcome <- function(outcome) {
  d1 <- load_pair("s1", outcome); d2 <- load_pair("s2", outcome)
  m1 <- ts_model(list(sur_arms(d1))); m2 <- ts_model(list(sur_arms(d2)))
  p1 <- ts_profile(m1); p2 <- ts_profile(m2); pC <- combine_profiles(p1, p2)
  bind_rows(
    tibble(outcome = outcome, cell = "Study 1",  r = m1$rho, !!!bounds(p1)),
    tibble(outcome = outcome, cell = "Study 2",  r = m2$rho, !!!bounds(p2)),
    tibble(outcome = outcome, cell = "Combined", r = pC$r[which.max(pC$rel)], !!!bounds(pC)))
}

results <- bind_rows(run_outcome("petition")) %>%
  mutate(cell = factor(cell, levels = c("Combined", "Study 2", "Study 1")),  # reversed -> Study 1 on top
         outcome = factor(outcome, levels = c("petition"),
                          labels = c("petition (sign)")))

cat("\n=== r and 95% profile CI ===\n")
print(results %>% arrange(outcome, cell) %>%
        mutate(across(where(is.numeric), ~round(., 3))) %>% as.data.frame(), row.names = FALSE)

save_results(results, "latent_effect_correlation")

cat("=== Done ===\n")
