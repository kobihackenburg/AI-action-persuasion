source("analysis/figures/theme.R")
library(dplyr)
library(stringr)

cat("=== SI Fig: Demographic Covariates Robustness ===\n")

results <- readRDS("output/results/robustness.rds")

demo <- results$demo_robustness

if (nrow(demo) > 0) {
  demo <- demo %>%
    mutate(
      study_label = c("study1" = "Study 1", "study2" = "Study 2")[study],
      dv_label = str_remove_all(dv_name, "</?b>")
    )

  g_demo <- ggplot(demo,
    aes(y = dv_label, x = estimate, xmin = conf.low, xmax = conf.high,
        colour = specification, shape = specification)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_linerange(
      alpha = default_linerange_alpha,
      position = position_dodge(width = 0.5)) +
    geom_point(
      size = default_point_size,
      position = position_dodge(width = 0.5)) +
    facet_wrap(~study_label, scales = "free") +
    scale_colour_manual(values = c("Baseline" = "black", "With demographics" = "#B2182B")) +
    scale_shape_manual(values = c("Baseline" = 16, "With demographics" = 17)) +
    theme_paper() +
    theme(axis.text.y = element_markdown(lineheight = 0.9)) +
    labs(
      x = "Treatment effect estimate (with 95% CI)",
      y = NULL,
      colour = "Specification",
      shape = "Specification"
    )

  ggsave("output/figures/si/fig_si_demographic_robustness.pdf",
         g_demo, width = 12, height = 5)
  cat("  Saved output/figures/si/fig_si_demographic_robustness.pdf\n")
} else {
  cat("  No demographic robustness data available.\n")
}
