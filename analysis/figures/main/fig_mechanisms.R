source("analysis/figures/theme.R")
library(dplyr)
library(purrr)
library(forcats)

cat("=== Mechanisms (Column-per-Study Layout) ===\n")

fig2_base <- 8

fmt_p <- function(p) ifelse(p < .01, "< .01", sprintf("= %.2f", p))

mechanisms <- readRDS("output/results/mechanisms.rds")
bb <- readRDS("output/results/belief_behavior.rds")

# ==============================================================================
# Row A: Belief-behavior dissociation (both studies)
# ==============================================================================

bb_data <- bb$condition_estimates %>% filter(outcome_pair == "petition")

cor_labels_a <- bb_data %>%
  group_by(study) %>%
  summarise(
    r = cor(belief_estimate, behavior_estimate),
    p = cor.test(belief_estimate, behavior_estimate)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    label = sprintf("italic(r) == '%.2f, ' ~~ italic(p) ~ '%s'", r, fmt_p(p))
  )

g_dissociation <- bb_data %>%
  ggplot(aes(x = belief_estimate, y = behavior_estimate)) +
  geom_smooth(method = "lm", color = "black", linewidth = 0.25,
              linetype = "dashed", fullrange = TRUE, se = TRUE,
              fill = "black", alpha = 0.32) +
  geom_linerange(aes(xmin = belief_ci_low, xmax = belief_ci_high),
                 alpha = default_linerange_alpha, linewidth = 0.3, color = "black") +
  geom_linerange(aes(ymin = behavior_ci_low, ymax = behavior_ci_high),
                 alpha = default_linerange_alpha, linewidth = 0.3, color = "black") +
  geom_point(size = 1.5, color = "black") +
  geom_label(data = cor_labels_a, aes(label = label),
             x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5,
             parse = TRUE, size = 7 / .pt, color = "grey30",
             fill = alpha("white", 0.75), label.size = 0,
             label.padding = unit(2, "pt"),
             inherit.aes = FALSE) +
  facet_wrap(~study, ncol = 1, scales = "free", axes = "all_x",
             axis.labels = "margins",
             labeller = labeller(study = c(
               "study1" = "Study 1 (15 conditions)",
               "study2" = "Study 2 (8 strategies)"
             ))) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 3)) +
  theme_paper(base_size = fig2_base) +
  halfopen_theme +
  theme(
    axis.title.y = element_text(color = dv_type_colors["Behavior"]),
    plot.title = element_text(hjust = -0.08, face = "bold", size = 15,
                              margin = margin(t = 2, b = 4))
  ) +
  labs(
    x = "Support petition",
    y = "Sign petition",
    title = "A"
  )

# ==============================================================================
# Row B: Prompt / strategy-type effects
# ==============================================================================

# Panels B and C show both studies' organisation-directed behavioural outcomes
# under the shared compact label "Organizational engagement"; the measures
# differ (Study 1: composite; Study 2: donate-to-assigned-org choice), which
# the figure caption states explicitly. Study-specific labels are used in the
# by-issue/main-effects figures and the regression tables.
s1_prompt <- mechanisms$prompt_type_effects %>%
  mutate(
    dv_type = fct_relevel(dv_type, "Attitude"),
    dv_name = ifelse(dv_name == "Donation, Task, Org choice",
                     "Organizational engagement", dv_name),
    prompt_label = case_when(
      prompt == "Info" ~ "Information<br>(issue)",
      prompt == "Random" ~ "Other"
    ),
    study_label = "Study 1"
  )

s2_info <- mechanisms$s2_info_issue_effects %>%
  mutate(
    dv_type = fct_relevel(factor(dv_type), "Attitude"),
    dv_name = ifelse(dv_name == "Donate to assigned org",
                     "Organizational engagement", dv_name),
    prompt_label = case_when(
      prompt == "Info: Issue" ~ "Information<br>(issue)",
      prompt == "Other strategies" ~ "Other"
    ),
    study_label = "Study 2"
  )

prompt_data <- bind_rows(s1_prompt, s2_info) %>%
  mutate(
    prompt_label = factor(prompt_label, levels = c("Other", "Information<br>(issue)")),
    study_label = factor(study_label, levels = c("Study 1", "Study 2")),
    dv_type = factor(dv_type, levels = c("Attitude", "Behavior")),
    prompt_annotation = ifelse(
      grepl("Information", prompt_label), "Prompt: Info", "Prompt: Other"
    ),
    topic = case_when(
      dv_name %in% c("Support petition", "Sign petition") ~ "Petition",
      TRUE ~ "Organization"
    ),
    topic = factor(topic, levels = c("Petition", "Organization"))
  )

prompt_data <- prompt_data %>%
  mutate(dv_name = fct_relabel(factor(dv_name), function(x) {
    x <- gsub(" ", "<br>", x)
    ifelse(gsub("<br>", " ", x) %in% beh_dv_names,
           paste0("<span style='color:", dv_type_colors["Behavior"], "'>", x, "</span>"),
           x)
  }))

position_b <- position_dodgev(0.8)

g_prompt <- prompt_data %>%
  ggplot(aes(y = dv_name, x = estimate, xmin = conf.low, xmax = conf.high,
             color = dv_type, shape = prompt_label, group = prompt_label)) +
  scale_color_manual(values = dv_type_colors, guide = "none") +
  scale_shape_manual(
    name = "Prompt type",
    values = c("Information<br>(issue)" = 17, "Other" = 16),
    labels = c("Information<br>(issue)" = "Information\n(Issue)", "Other" = "Other"),
    breaks = c("Information<br>(issue)", "Other")
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70",
             linewidth = 0.4) +
  geom_linerange(alpha = default_linerange_alpha, linewidth = 0.3, position = position_b) +
  geom_point(position = position_b, size = 1.5) +
  geom_text(aes(label = sprintf("%+.2f", estimate)),
            vjust = -0.8, size = 6.5 / .pt, position = position_b, show.legend = FALSE) +
  scale_y_discrete(expand = expansion(add = 0.6)) +
  facet_grid(topic ~ study_label, scales = "free_y", space = "free_y",
             axes = "all", axis.labels = "margins") +
  theme_paper(base_size = fig2_base) +
  halfopen_theme +
  theme(
    legend.position = "inside",
    legend.position.inside = c(1.0, 0.05),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "white", color = "grey80", linewidth = 0.3),
    legend.key.size = unit(0.5, "lines"),
    legend.key.spacing.y = unit(1, "pt"),
    legend.title = element_text(size = fig2_base - 2, face = "bold"),
    legend.text = element_text(size = fig2_base - 2),
    legend.margin = margin(3, 3, 3, 3),
    plot.title = element_text(hjust = -0.08, face = "bold", size = 15,
                              margin = margin(t = 2, b = 4)),
    strip.text.y = element_blank()
  ) +
  labs(
    x = "Average treatment effect",
    y = NULL,
    title = "B"
  )

# ==============================================================================
# Row C: Learning scatters (faceted per study)
# ==============================================================================

dv_spec <- tribble(
  ~ate_key_s1,         ~ate_key_s2,          ~dv_name,                    ~dv_type,
  "delta_petition",    "delta_petition",     "Support petition",          "Attitude",
  "delta_organization","delta_organization", "Support organization",      "Attitude",
  "signed_petition",   "signed_petition",    "Sign petition",             "Behavior",
  "org_behavior",      "chose_assigned_org", "Organizational engagement", "Behavior"
)

build_scatter_data <- function(learning, ates, study_label, spec) {
  pmap_dfr(spec, function(ate_key_s1, ate_key_s2, dv_name, dv_type) {
    ate_key <- if (study_label == "Study 1") ate_key_s1 else ate_key_s2
    joined <- left_join(
      learning %>% dplyr::select(term, x = estimate, x_lo = conf.low, x_hi = conf.high),
      ates[[ate_key]] %>% dplyr::select(term, y = estimate, y_lo = conf.low, y_hi = conf.high),
      by = "term"
    )
    joined %>% mutate(study = study_label, dv_name = dv_name, dv_type = dv_type)
  })
}

scatter_c <- bind_rows(
  build_scatter_data(mechanisms$learning_estimates, mechanisms$condition_ates,
                     "Study 1", dv_spec),
  build_scatter_data(mechanisms$s2_learning_estimates, mechanisms$s2_condition_ates,
                     "Study 2", dv_spec)
) %>%
  mutate(
    study   = factor(study, levels = c("Study 1", "Study 2")),
    dv_name = factor(dv_name, levels = c(
      "Support petition", "Support organization",
      "Sign petition", "Organizational engagement"
    )),
    dv_type = factor(dv_type, levels = c("Attitude", "Behavior"))
  )

cor_labels_c <- scatter_c %>%
  group_by(study, dv_name, dv_type) %>%
  summarise(
    r = cor(x, y, use = "complete.obs"),
    p = cor.test(x, y)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    label = sprintf("italic(r) == '%.2f, ' ~~ italic(p) ~ '%s'", r, fmt_p(p))
  )

study_subtitles <- c(
  "Study 1" = "Study 1 (15 conditions)",
  "Study 2" = "Study 2 (8 strategies)"
)

dv_strip_labeller <- setNames(
  ifelse(levels(scatter_c$dv_name) %in% beh_dv_names,
         paste0("<span style='color:", dv_type_colors["Behavior"], "'>",
                levels(scatter_c$dv_name), "</span>"),
         levels(scatter_c$dv_name)),
  levels(scatter_c$dv_name)
)

fill_vals <- c(
  "Attitude" = unname(dv_type_colors["Attitude"]),
  "Behavior" = unname(dv_type_colors["Behavior"])
)

make_study_scatter <- function(data, cor_data, study_name, show_y_title = TRUE) {
  d  <- data     %>% filter(study == study_name)
  cd <- cor_data %>% filter(study == study_name)

  g <- ggplot(d, aes(x = x, y = y, color = dv_type)) +
    geom_smooth(aes(fill = dv_type), method = "lm", linewidth = 0.25,
                linetype = "dashed", fullrange = TRUE, se = TRUE, alpha = 0.32) +
    geom_linerange(aes(xmin = x_lo, xmax = x_hi), alpha = default_linerange_alpha, linewidth = 0.3) +
    geom_linerange(aes(ymin = y_lo, ymax = y_hi), alpha = default_linerange_alpha, linewidth = 0.3) +
    geom_point(size = 1.5) +
    geom_label(data = cd, aes(label = label, color = dv_type),
               x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5,
               parse = TRUE, size = 6 / .pt, inherit.aes = FALSE,
               fill = alpha("white", 0.75), label.size = 0,
               label.padding = unit(2, "pt"),
               show.legend = FALSE) +
    facet_wrap(~ dv_name, ncol = 2, scales = "free",
               labeller = labeller(dv_name = dv_strip_labeller)) +
    scale_color_manual(values = dv_type_colors, guide = "none") +
    scale_fill_manual(values = fill_vals, guide = "none") +
    scale_y_continuous() +
    theme_paper(base_size = fig2_base) +
    halfopen_theme +
    theme(
      plot.title    = element_text(hjust = -0.08, face = "bold", size = 15,
                                   margin = margin(t = 2, b = 4)),
      plot.subtitle = element_text(face = "bold", size = fig2_base, hjust = 0.5),
      # strip.text.x must also be element_markdown: theme_paper sets it to a
      # plain element_text, which otherwise overrides strip.text for horizontal
      # strips and renders the <span> colour tags as literal text.
      strip.text    = ggtext::element_markdown(size = fig2_base - 1, face = "italic"),
      strip.text.x  = ggtext::element_markdown(size = fig2_base - 1, face = "italic",
                                               hjust = 0.5)
    ) +
    labs(subtitle = study_subtitles[[study_name]], x = NULL)

  if (show_y_title) {
    g <- g + labs(y = "Average treatment effect")
  } else {
    g <- g + theme(axis.title.y = element_blank())
  }

  g
}

g_learning_s1 <- make_study_scatter(scatter_c, cor_labels_c, "Study 1",
                                     show_y_title = TRUE) +
  labs(title = "C")
g_learning_s2 <- make_study_scatter(scatter_c, cor_labels_c, "Study 2",
                                     show_y_title = FALSE)

# ==============================================================================
# Composite: A|B over C
# ==============================================================================

top_row <- (g_dissociation | g_prompt) + plot_layout(widths = c(1, 1))

row_c <- (g_learning_s1 | g_learning_s2)
row_c_with_label <- row_c /
  wrap_elements(grid::textGrob("Self-reported learning",
    gp = grid::gpar(fontsize = fig2_base, fontfamily = "sans"))) +
  plot_layout(heights = c(1, 0.04))

g <- top_row / row_c_with_label + plot_layout(heights = c(1.0, 1.3))

ggsave("output/figures/main/fig_mechanisms.pdf", g, width = 17.8, height = 20, units = "cm")
cat("  Saved output/figures/main/fig_mechanisms.pdf\n")
