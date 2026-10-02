cat("=== Rendering all figures ===\n\n")

cat("--- Main-text figures ---\n\n")

source("analysis/figures/main/fig_main_effects.R")
source("analysis/figures/main/fig_mechanisms.R")
source("analysis/figures/main/fig_strategy_effects.R")

cat("\n--- SI figures ---\n\n")

source("analysis/figures/si/fig_si_heterogeneous_effects.R")
source("analysis/figures/si/fig_si_leave_one_out.R")
source("analysis/figures/si/fig_si_demographic_robustness.R")
source("analysis/figures/si/fig_si_meta_analysis.R")
source("analysis/figures/si/fig_si_by_issue.R")
source("analysis/figures/si/fig_si_model_effects.R")
source("analysis/figures/si/fig_si_strategy_by_prior.R")
source("analysis/figures/si/fig_si_demographics.R")
source("analysis/figures/si/fig_si_latent_correlation.R")

cat("\n--- SI tables ---\n\n")

source("analysis/figures/si/table_si_baselines.R")
source("analysis/figures/si/table_si_design.R")
source("analysis/figures/si/table_si_sample_sizes.R")
source("analysis/figures/si/table_si_attrition.R")

cat("\n=== All figures and tables rendered ===\n")
