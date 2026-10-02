source("analysis/figures/theme.R")
library(dplyr)
library(forcats)

cat("=== Main Effects (Both Studies) ===\n")

s1 <- readRDS("output/results/study1_main_effects.rds") %>%
  mutate(study = "Study 1")
s2 <- readRDS("output/results/study2_main_effects.rds")$overall_ates %>%
  mutate(study = "Study 2")

# ---- Harmonize and bind ------------------------------------------------------

all_data <- bind_rows(s1, s2) %>%
  mutate(
    study = factor(study, levels = c("Study 1", "Study 2")),
    dv_type = factor(dv_type, levels = c("Attitude", "Behavior"))
  )

# ---- Overall estimates only --------------------------------------------------

overall_data <- all_data %>%
  filter(facet == "Overall") %>%
  arrange(study, dv_type, desc(estimate))

level_order <- overall_data %>%
  mutate(
    type_rank = as.integer(dv_type),
    dv_label = sprintf("<span style='color:%s'>%s</span>",
                       dv_type_colors[as.character(dv_type)], dv_name)
  ) %>%
  arrange(type_rank, estimate) %>%
  pull(dv_label) %>%
  unique()

overall_data <- overall_data %>%
  mutate(
    dv_name = sprintf("<span style='color:%s'>%s</span>",
                      dv_type_colors[as.character(dv_type)], dv_name),
    dv_name = factor(dv_name, levels = level_order)
  )

# ---- Plot --------------------------------------------------------------------

g <- ggplot(overall_data, aes(y = dv_name, x = estimate, color = dv_type)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.4) +
  geom_linerange(
    aes(xmin = conf.low, xmax = conf.high),
    alpha = default_linerange_alpha, linewidth = 0.3
  ) +
  geom_point(size = 1.5) +
  geom_text(
    aes(label = sprintf("%+.2f", estimate)),
    vjust = -0.7, size = 6.5 / .pt, show.legend = FALSE
  ) +
  facet_grid(
    study ~ .,
    scales = "free_y", space = "free_y",
    switch = "y",
    axes = "all_x", axis.labels = "margins"
  ) +
  scale_color_manual(values = dv_type_colors) +
  theme_paper(base_size = 8) +
  halfopen_theme +
  theme(
    strip.placement = "outside",
    strip.text.y.left = element_text(face = "bold", margin = margin(r = 6)),
    axis.title.x = element_text(face = "bold"),
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.92),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.size = unit(0.6, "lines")
  ) +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(x = "Average treatment effect", y = NULL, color = NULL)

ggsave("output/figures/main/fig_main_effects.pdf", g, width = 8.7, height = 11, units = "cm")
cat("  Saved output/figures/main/fig_main_effects.pdf\n")
