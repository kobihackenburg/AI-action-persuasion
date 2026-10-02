source("analysis/figures/theme.R")
library(dplyr)
library(forcats)
library(stringr)
library(patchwork)

cat("=== SI: Heterogeneous Effects by Prior Support ===\n")

het <- readRDS("output/results/heterogeneous_effects.rds")

petition_dvs <- c("Support petition", "Sign petition")

cates <- bind_rows(het$s1_cates, het$s2_cates) %>%
  mutate(
    dv_type = factor(dv_type, levels = c("Attitude", "Behavior")),
    study = c("study1" = "Study 1", "study2" = "Study 2")[study],
    subgroup = factor(subgroup, levels = c("Other", "Strong Supporters")),
    is_petition = dv_name %in% petition_dvs,
    dv_label = ifelse(
      dv_type == "Behavior",
      sprintf("<span style='color:%s'>%s</span>", dv_type_colors["Behavior"], dv_name),
      dv_name
    )
  )

# DV orders within each column
pet_order <- c("Support petition",
               sprintf("<span style='color:%s'>Sign petition</span>", dv_type_colors["Behavior"]))
org_order <- c("Support organization",
               sprintf("<span style='color:%s'>Bonus donation</span>", dv_type_colors["Behavior"]),
               sprintf("<span style='color:%s'>Money raising effort</span>", dv_type_colors["Behavior"]),
               sprintf("<span style='color:%s'>Choose organization</span>", dv_type_colors["Behavior"]),
               sprintf("<span style='color:%s'>Donate to assigned org</span>", dv_type_colors["Behavior"]))

make_panel <- function(data, dv_order, show_legend = FALSE,
                       show_x_axis = TRUE) {
  data <- data %>%
    mutate(dv_label = factor(dv_label, levels = dv_order))

  position <- position_dodgev(0.8)

  p <- ggplot(data, aes(y = dv_label, x = estimate, xmin = conf.low, xmax = conf.high,
                          color = dv_type, shape = subgroup, group = subgroup)) +
    scale_color_manual(values = dv_type_colors, guide = "none") +
    scale_shape_manual(
      name = "Prior support",
      values = c("Strong Supporters" = 16, "Other" = 2)
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.4) +
    geom_linerange(alpha = default_linerange_alpha, linewidth = 0.3, position = position) +
    geom_point(size = default_point_size, position = position) +
    geom_text(
      aes(label = sprintf("%+.2f", estimate)),
      vjust = -0.8, size = default_text_size, position = position, show.legend = FALSE
    ) +
    scale_y_discrete(drop = TRUE, expand = expansion(add = 0.6)) +
    theme_paper() +
    halfopen_theme +
    labs(x = NULL, y = NULL)

  if (!show_x_axis) {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(
      legend.position = "inside",
      legend.position.inside = c(0.95, 0.5),
      legend.justification = c(1, 0.5),
      legend.background = element_rect(fill = "white", color = "grey80", linewidth = 0.3),
      legend.key.size = unit(0.8, "lines"),
      legend.title = element_text(face = "bold"),
      legend.margin = margin(3, 3, 3, 3)
    )
  }

  p
}

g_s1_pet <- make_panel(cates %>% filter(study == "Study 1", is_petition),
                        pet_order, show_x_axis = FALSE)
g_s1_org <- make_panel(cates %>% filter(study == "Study 1", !is_petition),
                        org_order, show_legend = TRUE, show_x_axis = FALSE)
g_s2_pet <- make_panel(cates %>% filter(study == "Study 2", is_petition),
                        pet_order)
g_s2_org <- make_panel(cates %>% filter(study == "Study 2", !is_petition),
                        org_order)

top_row <- g_s1_pet + g_s1_org
bottom_row <- g_s2_pet + g_s2_org

g <- (top_row / bottom_row) +
  plot_annotation(
    title = NULL,
    theme = theme(plot.margin = margin(5, 5, 5, 5))
  )

# Add row and column labels via patchwork tag
g <- wrap_plots(
  # Row labels as text grobs
  grid::textGrob("Study 1", rot = 90, gp = grid::gpar(fontface = "bold", fontsize = 10)),
  g_s1_pet, g_s1_org,
  grid::textGrob("Study 2", rot = 90, gp = grid::gpar(fontface = "bold", fontsize = 10)),
  g_s2_pet, g_s2_org,
  ncol = 3, widths = c(0.06, 1, 1)
) +
  plot_annotation(
    caption = "ATE (vs. control)"
  ) &
  theme(
    plot.caption = element_text(hjust = 0.55, face = "bold", size = 10, margin = margin(t = 5))
  )

# Column titles
library(grid)
col_title_pet <- wrap_elements(textGrob("Petition outcomes", gp = gpar(fontface = "bold", fontsize = 10)))
col_title_org <- wrap_elements(textGrob("Organization outcomes", gp = gpar(fontface = "bold", fontsize = 10)))

row1_label <- wrap_elements(textGrob("Study 1", rot = 90, gp = gpar(fontface = "bold", fontsize = 10)))
row2_label <- wrap_elements(textGrob("Study 2", rot = 90, gp = gpar(fontface = "bold", fontsize = 10)))

g <- wrap_plots(
  plot_spacer(), col_title_pet, col_title_org,
  row1_label, g_s1_pet, g_s1_org,
  row2_label, g_s2_pet, g_s2_org,
  ncol = 3,
  widths = c(0.05, 1, 1),
  heights = c(0.08, 1, 1)
) +
  plot_annotation(caption = "ATE (vs. control)") &
  theme(plot.caption = element_text(hjust = 0.55, face = "bold", size = 10))

ggsave("output/figures/si/fig_si_heterogeneous_effects.pdf", g, width = 22, height = 14, units = "cm")
cat("  Saved output/figures/si/fig_si_heterogeneous_effects.pdf\n")
