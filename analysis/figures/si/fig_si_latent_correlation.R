source("analysis/figures/theme.R")

cat("=== SI Fig: Latent attitude-behaviour effect correlation ===\n")

# Forest plot of the latent between-condition correlation r between treatment
# effects on attitudes and on behaviour, with 95% profile-likelihood CIs
# (estimates from analysis/14_latent_effect_correlation.R).

results <- readRDS("output/results/latent_effect_correlation.rds")

p <- ggplot(results, aes(r, cell)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey70") +
  geom_linerange(aes(xmin = lo, xmax = hi), linewidth = .7, colour = "#1b9e77") +
  geom_point(size = 2.8, colour = "#1b9e77") +
  facet_wrap(~outcome, nrow = 1) +
  coord_cartesian(xlim = c(-1, 1)) +
  labs(title = "Latent attitude-behaviour effect correlation r",
       subtitle = "two-stage SUR -> rma.mv; 95% profile-likelihood CIs",
       x = "effect correlation r", y = NULL) +
  theme_minimal(base_size = 13) + theme(panel.grid.major.y = element_blank())

ggsave("output/figures/si/fig_si_latent_correlation.pdf", p, width = 11, height = 4)
cat("  Saved output/figures/si/fig_si_latent_correlation.pdf\n")
