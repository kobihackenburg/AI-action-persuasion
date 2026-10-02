source("analysis/utils.R")
suppressMessages(library(mediation))

cat("=== 12: Causal mediation analysis (self-reported learning) ===\n")

set.seed(20240624)
N_SIMS <- 1000

# ------------------------------------------------------------------------------
# Helper: run one mediation cell (mediate + sensitivity), return tidy rows.
#   - Treatment T (info indicator) is randomized among treated, so treatment
#     ignorability holds by design; mediator ignorability is assessed via medsens.
#   - Mediator model: lm(M ~ T + X); attitude outcome: lm; behaviour: probit glm
#     (probit keeps medsens supported for the binary outcome).
# ------------------------------------------------------------------------------

run_mediation <- function(data, study, outcome, outcome_label, outcome_binary,
                          covars) {
  m_form <- as.formula(paste0("M ~ T + ", covars))
  model.m <- lm(m_form, data = data)

  o_form <- as.formula(paste0(outcome, " ~ T + M + ", covars))
  model.y <- if (outcome_binary) {
    glm(o_form, data = data, family = binomial("probit"))
  } else {
    lm(o_form, data = data)
  }

  med <- mediate(model.m, model.y, treat = "T", mediator = "M", sims = N_SIMS)

  # Sensitivity analysis: rho (error-term correlation between mediator and
  # outcome models) at which the ACME would be zero.
  rho_star <- NA_real_
  r2_star  <- NA_real_
  sens <- tryCatch(
    medsens(med, rho.by = 0.1, effect.type = "indirect"),
    error = function(e) { cat("    medsens failed:", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(sens)) {
    # Package-provided critical values: rho at which ACME = 0, and the
    # corresponding product of mediator/outcome R^2 explained by an unobserved
    # confounder (matches summary.medsens).
    rho_star <- tryCatch(as.numeric(sens$err.cr.d), error = function(e) NA_real_)
    r2_star  <- tryCatch(as.numeric(sens$R2star.d.thresh), error = function(e) NA_real_)
  }

  s1 <- function(x) as.numeric(x)[1]  # force scalar (probit mediate fields can be length 2)
  tibble(
    study = study, outcome = outcome_label, outcome_binary = outcome_binary,
    acme = s1(med$d0), acme_lo = s1(med$d0.ci[1]), acme_hi = s1(med$d0.ci[2]), acme_p = s1(med$d0.p),
    ade = s1(med$z0), ade_lo = s1(med$z0.ci[1]), ade_hi = s1(med$z0.ci[2]), ade_p = s1(med$z0.p),
    total = s1(med$tau.coef), total_lo = s1(med$tau.ci[1]), total_hi = s1(med$tau.ci[2]),
    total_p = s1(med$tau.p),
    prop_med = s1(med$n0), prop_med_lo = s1(med$n0.ci[1]), prop_med_hi = s1(med$n0.ci[2]),
    rho_star = s1(rho_star), r2_star = s1(r2_star),
    n = nrow(data)
  )
}

# ------------------------------------------------------------------------------
# Study 1: T = Information prompt (1) vs. Random prompt (0), among treated.
#          Mediator = self-reported learning (conversation_learned).
# ------------------------------------------------------------------------------

s1 <- load_study1() %>%
  filter(prompt_category %in% c("info", "random"),
         !is.na(conversation_learned)) %>%
  mutate(
    T = as.integer(prompt_category == "info"),
    M = conversation_learned,
    z_agree   = as.numeric(scale(pretreatment_agreement)),
    z_org     = as.numeric(scale(pretreatment_organization_opinion)),
    z_charity = as.numeric(scale(charity_interest_num)),
    petition_title = factor(petition_title)
  )

s1_covars_med <- "z_agree + z_org + z_charity + petition_title"

# ------------------------------------------------------------------------------
# Study 2: T = Information (Issue) strategy (1) vs. other treated strategies (0).
#          Mediator = information-learning mechanism composite.
# ------------------------------------------------------------------------------

s2 <- load_study2() %>%
  filter(treatment == "treatment", !is.na(mechanism_info_learning),
         !is.na(mechanism_info_knowledge)) %>%
  mutate(
    T = as.integer(conversation_type_clean == "info_issue"),
    M = (mechanism_info_learning + mechanism_info_knowledge) / 2,
    z_agree = as.numeric(scale(pretreatment_agreement)),
    z_org   = as.numeric(scale(pretreatment_organization_opinion)),
    petition_title = factor(petition_title)
  )

s2_covars_med <- "z_agree + z_org + petition_title"

# ------------------------------------------------------------------------------
# Run all four cells
# ------------------------------------------------------------------------------

cat("  Study 1: attitude (delta_petition)...\n")
m_s1_att <- run_mediation(s1, "Study 1", "delta_petition", "Petition support (attitude)",
                          FALSE, s1_covars_med)
cat("  Study 1: behaviour (signed_petition)...\n")
m_s1_beh <- run_mediation(s1, "Study 1", "1 * signed_petition", "Petition signing (behaviour)",
                          TRUE, s1_covars_med)

cat("  Study 2: attitude (delta_petition)...\n")
m_s2_att <- run_mediation(s2, "Study 2", "delta_petition", "Petition support (attitude)",
                          FALSE, s2_covars_med)
cat("  Study 2: behaviour (signed_petition)...\n")
m_s2_beh <- run_mediation(s2, "Study 2", "1 * signed_petition", "Petition signing (behaviour)",
                          TRUE, s2_covars_med)

mediation_results <- bind_rows(m_s1_att, m_s1_beh, m_s2_att, m_s2_beh)

# ------------------------------------------------------------------------------
# Print summary
# ------------------------------------------------------------------------------

cat("\n--- CAUSAL MEDIATION (information/learning mediator; ACME via learning) ---\n")
for (i in seq_len(nrow(mediation_results))) {
  r <- mediation_results[i, ]
  cat(sprintf(
    "  %-8s %-30s  ACME = %+.3f [%+.3f, %+.3f] p=%.3g | ADE = %+.3f | Total = %+.3f | PropMed = %.2f | rho*=%s | R2*=%s\n",
    r$study, r$outcome, r$acme, r$acme_lo, r$acme_hi, r$acme_p,
    r$ade, r$total, r$prop_med,
    ifelse(is.na(r$rho_star), "n/a", sprintf("%.2f", r$rho_star)),
    ifelse(is.na(r$r2_star), "n/a", sprintf("%.3f", r$r2_star))
  ))
}

save_results(mediation_results, "mediation")

cat("=== Done ===\n")
