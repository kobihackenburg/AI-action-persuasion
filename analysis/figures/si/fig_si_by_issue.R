source("analysis/figures/theme.R")

cat("=== SI Fig: By-Issue Breakdowns ===\n")

# ---- Study 1 ----------------------------------------------------------------

s1_results <- readRDS("output/results/study1_main_effects.rds")
by_issue_s1 <- s1_results %>% filter(facet == "By topic")

if (nrow(by_issue_s1) > 0) {
  g1 <- by_issue_s1 %>%
    mutate(dv_type = fct_relevel(dv_type, "Attitude")) %>%
    ggplot(aes(y = dv_name, x = estimate, xmin = conf.low, xmax = conf.high)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_linerange(alpha = default_linerange_alpha) +
    geom_point(size = default_point_size) +
    facet_wrap(~grp, ncol = 2) +
    theme_paper() +
    labs(
      x = "Average treatment effect",
      y = NULL,
      title = "Study 1: Treatment effects by issue"
    )

  ggsave("output/figures/si/fig_si_by_issue.pdf", g1, width = 12, height = 10)
  cat("  Saved output/figures/si/fig_si_by_issue.pdf\n")
}

# ---- Study 2 ----------------------------------------------------------------

s2_results <- readRDS("output/results/study2_main_effects.rds")
by_issue_s2 <- s2_results$overall_ates %>% filter(facet == "By topic")

if (nrow(by_issue_s2) > 0) {
  g2 <- by_issue_s2 %>%
    mutate(dv_type = fct_relevel(dv_type, "Attitude")) %>%
    ggplot(aes(y = dv_name, x = estimate, xmin = conf.low, xmax = conf.high)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_linerange(alpha = default_linerange_alpha) +
    geom_point(size = default_point_size) +
    facet_wrap(~grp, ncol = 2) +
    theme_paper() +
    labs(
      x = "Average treatment effect",
      y = NULL,
      title = "Study 2: Treatment effects by issue"
    )

  ggsave("output/figures/si/fig_si_by_issue_s2.pdf", g2, width = 12, height = 10)
  cat("  Saved output/figures/si/fig_si_by_issue_s2.pdf\n")
}
