library(tidyverse)
library(lme4)
library(broom)
library(broom.mixed)

# ---- Raw data path resolver --------------------------------------------------
# If unredacted CSVs exist in data/raw/full/, use those; otherwise fall back to
# the redacted files in data/raw/. This lets the pipeline run out of the box
# with public (redacted) data while allowing a drop-in override for full data.

resolve_raw_path <- function(path) {
  full <- file.path("data/raw/full", basename(path))
  if (file.exists(full)) {
    cat(sprintf("  [DATA] Using full (unredacted): %s\n", full))
    return(full)
  }
  cat(sprintf("  [DATA] Using redacted: %s\n", path))
  path
}

# ---- Data loading helpers ----------------------------------------------------

load_study1 <- function() readRDS("output/results/study1_clean.rds")
load_study2 <- function() readRDS("output/results/study2_clean.rds")
load_combined <- function() readRDS("output/results/combined_clean.rds")

# ---- Covariate formulas -----------------------------------------------------

# Study 1: lmer with pre-treatment covariates and random intercepts for issue (matches pre-registration)
s1_covariates_base <- "scale(pretreatment_agreement) + scale(pretreatment_organization_opinion) + scale(charity_interest_num)"
s1_covariates <- paste0(s1_covariates_base, " + (1|petition_title)")
s1_covariates_donation <- paste0(s1_covariates_base, " + randomized_bonus_amount + (1|petition_title)")

# Study 2: matches pre-analysis plan (random intercept for issue)
s2_covariates <- "scale(pretreatment_agreement) + scale(pretreatment_organization_opinion) + (1|petition_title)"

# ---- DV definitions ----------------------------------------------------------

s1_dvs <- tribble(
  ~dv,                       ~dv_name,                                                ~dv_type,
  "delta_petition",          "<b>Support petition</b><br/>(standardized)",             "Attitude",
  "signed_petition",         "<b>Sign petition</b><br/>(binary)",                      "Behavior",
  "delta_organization",      "<b>Support organization</b><br/>(standardized)",         "Attitude",
  "donate_proportion",       "<b>Bonus donation</b><br/>(proportion)",                 "Behavior",
  "extra_task_clicks/100",   "<b>Money raising effort</b><br/>(proportion)",            "Behavior",
  "not_switch_organization", "<b>Choose organization</b><br/>(binary)",                "Behavior"
)

s2_dvs <- tribble(
  ~dv,                  ~dv_name,                                                ~dv_type,
  "signed_petition",    "<b>Sign petition</b><br/>(binary)",                     "Behavior",
  "delta_petition",     "<b>Support petition</b><br/>(standardized)",            "Attitude",
  "delta_organization", "<b>Support organization</b><br/>(standardized)",        "Attitude",
  "chose_assigned_org", "<b>Donate to assigned org</b><br/>(binary)",            "Behavior"
)

# ---- Petition title labels ---------------------------------------------------

petition_labels <- tribble(
  ~petition_title,                                                                    ~petition_short,
  "Stop Rising Child Poverty – Scrap the Two-Child Limit & Lock in Support",          "Stop Rising Child Poverty",
  "End suffering for mother pigs",                                                    "End suffering for mother pigs",
  "Oppose Facial Recognition",                                                        "Oppose Facial Recognition",
  "Stop the risk of nuclear war",                                                     "Stop the risk of nuclear war",
  "Seats in parliament should match how we vote",                                     "Seats in parliament should match how we vote",
  "Lift the ban on work for people seeking asylum",                                   "Lift the ban on work for people seeking asylum",
  "Urge UK governments to protect trees",                                             "Urge UK governments to protect trees",
  "Stop banks fuelling the climate crisis",                                           "Stop banks fuelling the climate crisis"
)

# ---- Model fitting helpers ---------------------------------------------------

fit_and_tidy <- function(formula, data, conf.int = TRUE) {
  model <- lm(formula, data = data)
  broom::tidy(model, conf.int = conf.int)
}

fit_lmer_and_tidy <- function(formula, data, conf.int = TRUE) {
  model <- lme4::lmer(formula, data = data)
  broom.mixed::tidy(model, conf.int = conf.int, effects = "fixed")
}

# ---- Results saving helper ---------------------------------------------------

save_results <- function(obj, name) {
  path <- file.path("output", "results", paste0(name, ".rds"))
  saveRDS(obj, path)
  cat(sprintf("  Saved %s\n", path))
}
