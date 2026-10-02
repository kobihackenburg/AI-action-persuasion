source("analysis/figures/theme.R")
source("analysis/utils.R")
library(dplyr)
library(forcats)
library(stringr)

cat("=== SI: Strategy Effects by Prior Support (Study 2) ===\n")

s2 <- load_study2() %>%
  mutate(strategy = ifelse(treatment == "control", "control", conversation_type) %>%
           fct_relevel("control"))

strategies <- levels(s2$strategy) %>% setdiff("control")

cates <- map_dfr(strategies, function(strat) {
  strong <- s2 %>% filter(strategy %in% c("control", strat), pretreatment_agreement > 5)
  others <- s2 %>% filter(strategy %in% c("control", strat), pretreatment_agreement <= 5)

  fit_sub <- function(d, subgroup_label) {
    if (nrow(d) < 10) return(tibble())
    form <- as.formula(str_glue(
      "1 * signed_petition ~ strategy + scale(pretreatment_agreement) + scale(pretreatment_organization_opinion)"
    ))
    tryCatch({
      fit_and_tidy(form, d) %>%
        filter(str_starts(term, "strategy")) %>%
        mutate(strategy = strat, subgroup = subgroup_label)
    }, error = function(e) tibble())
  }

  bind_rows(
    fit_sub(strong, "Strong Supporters"),
    fit_sub(others, "Other")
  )
})

cates <- cates %>%
  mutate(
    strategy_label = ifelse(strategy %in% names(strategy_labels),
                            strategy_labels[strategy],
                            str_replace_all(strategy, "[_-]", " ") %>% str_to_title()),
    subgroup = factor(subgroup, levels = c("Other", "Strong Supporters"))
  )

strat_order <- cates %>%
  filter(subgroup == "Other") %>%
  arrange(estimate) %>%
  pull(strategy_label)

cates <- cates %>%
  mutate(strategy_label = factor(strategy_label, levels = strat_order))

position <- position_dodgev(0.7)

g <- ggplot(cates, aes(y = strategy_label, x = estimate, xmin = conf.low, xmax = conf.high,
                        shape = subgroup, group = subgroup)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.4) +
  geom_linerange(alpha = default_linerange_alpha, linewidth = 0.3, position = position,
                 color = dv_type_colors["Behavior"]) +
  geom_point(size = default_point_size, position = position,
             color = dv_type_colors["Behavior"]) +
  scale_shape_manual(
    name = "Prior support",
    values = c("Strong Supporters" = 16, "Other" = 2)
  ) +
  scale_x_continuous(breaks = seq(0, 0.3, 0.1)) +
  theme_paper() +
  halfopen_theme +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.15),
    legend.background = element_rect(fill = "white", color = "grey80", linewidth = 0.3),
    legend.key.size = unit(0.8, "lines"),
    legend.title = element_text(face = "bold"),
    legend.margin = margin(3, 3, 3, 3)
  ) +
  labs(x = "Treatment effect on petition signing (vs. control)", y = NULL)

ggsave("output/figures/si/fig_si_strategy_by_prior.pdf", g, width = 16, height = 10, units = "cm")
cat("  Saved output/figures/si/fig_si_strategy_by_prior.pdf\n")
