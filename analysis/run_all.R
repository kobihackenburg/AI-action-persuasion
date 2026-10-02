cat("========================================\n")
cat("  Action Persuasion: Full Analysis Run\n")
cat("========================================\n\n")

for (d in c("output/results", "output/figures/main", "output/figures/si",
            "output/tables", "data/processed")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

cat("Step 0: Processing raw data...\n")
source("analysis/00_process_data.R")

cat("\nStep 0b: Descriptive statistics...\n")
source("analysis/00b_descriptive_stats.R")

cat("\nStep 1: Study 1 main effects...\n")
source("analysis/01_study1_main_effects.R")

cat("\nStep 2: Study 2 main effects...\n")
source("analysis/02_study2_main_effects.R")

cat("\nStep 3: Belief-behavior dissociation...\n")
source("analysis/03_belief_behavior.R")

cat("\nStep 4: Mechanisms...\n")
source("analysis/04_mechanisms.R")

cat("\nStep 5: Heterogeneous effects...\n")
source("analysis/05_heterogeneous_effects.R")

cat("\nStep 6: Robustness checks...\n")
source("analysis/06_robustness.R")

cat("\nStep 7: Meta-analysis...\n")
source("analysis/07_meta_analysis.R")

cat("\nStep 8: Pooled estimates...\n")
source("analysis/08_pooled_estimates.R")

cat("\nStep 9: Manuscript statistics...\n")
source("analysis/09_manuscript_stats.R")

cat("\nStep 10: Regression tables...\n")
source("analysis/10_regression_tables.R")

cat("\nStep 11: Information effect on attitudes vs. behaviour (power)...\n")
source("analysis/11_info_power.R")

cat("\nStep 12: Causal mediation analysis...\n")
source("analysis/12_mediation.R")

cat("\nStep 13: Direct asks vs. factual density (transcripts)...\n")
source("analysis/13_direct_ask.R")

cat("\nStep 14: Latent attitude-behaviour effect correlation...\n")
source("analysis/14_latent_effect_correlation.R")

cat("\n--- Rendering figures ---\n\n")
source("analysis/figures/render_all_figures.R")

cat("\n========================================\n")
cat("  All analyses and figures complete.\n")
cat("========================================\n")
