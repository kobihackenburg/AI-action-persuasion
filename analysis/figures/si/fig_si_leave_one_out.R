source("analysis/figures/theme.R")
library(dplyr)
library(stringr)

cat("=== SI Fig: Leave-One-Issue-Out ===\n")

results <- readRDS("output/results/robustness.rds")

loo <- results$leave_one_out
fullsample <- results$loo_fullsample

if (nrow(loo) > 0) {

  shorten_issue <- function(x) {
    dplyr::case_when(
      str_detect(x, "Child Poverty")      ~ "Child Poverty",
      str_detect(x, "mother pigs")         ~ "Animal Welfare",
      str_detect(x, "Facial Recognition")  ~ "Facial Recognition",
      str_detect(x, "nuclear war")         ~ "Nuclear Disarmament",
      str_detect(x, "parliament")          ~ "Electoral Reform",
      str_detect(x, "asylum")              ~ "Asylum Seekers",
      str_detect(x, "trees")              ~ "Tree Protection",
      str_detect(x, "banks")              ~ "Climate Finance",
      str_detect(x, "economic inequality") ~ "Economic Inequality",
      str_detect(x, "disabilities")        ~ "Children w/ Disabilities",
      TRUE                                 ~ x
    )
  }

  strip_html <- function(x) str_remove_all(x, "<.*?>")
  clean_dv <- function(x) str_remove(strip_html(x), "\\(.*\\)")

  loo <- loo %>%
    mutate(
      issue_label = shorten_issue(dropped_issue),
      dv_short = trimws(clean_dv(dv_name)),
      study_label = c("study1" = "Study 1", "study2" = "Study 2")[study],
      facet_label = paste0(study_label, ": ", dv_short)
    )

  fullsample <- fullsample %>%
    mutate(
      dv_short = trimws(clean_dv(dv_name)),
      study_label = c("study1" = "Study 1", "study2" = "Study 2")[study],
      facet_label = paste0(study_label, ": ", dv_short)
    )

  facet_order <- loo %>%
    distinct(study, facet_label) %>%
    arrange(study) %>%
    pull(facet_label)
  loo$facet_label <- factor(loo$facet_label, levels = facet_order)
  fullsample$facet_label <- factor(fullsample$facet_label, levels = facet_order)

  g_loo <- ggplot(loo, aes(y = reorder(issue_label, estimate),
                            x = estimate, xmin = conf.low, xmax = conf.high)) +
    geom_vline(data = fullsample, aes(xintercept = estimate),
               linetype = "solid", colour = "grey50", linewidth = 0.4) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70") +
    geom_linerange(alpha = default_linerange_alpha) +
    geom_point(size = default_point_size) +
    facet_wrap(~ facet_label, scales = "free", ncol = 3) +
    theme_paper() +
    theme(
      axis.text.y = element_markdown(),
      strip.text = element_text(size = 9)
    ) +
    labs(
      x = "Treatment effect estimate (with 95% CI)",
      y = "Issue dropped"
    )

  ggsave("output/figures/si/fig_si_leave_one_out.pdf", g_loo,
         width = 15, height = 8)
  cat("  Saved output/figures/si/fig_si_leave_one_out.pdf\n")
} else {
  cat("  No leave-one-out data available.\n")
}
