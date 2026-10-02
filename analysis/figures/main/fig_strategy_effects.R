source("analysis/figures/theme.R")
library(dplyr)
library(forcats)
library(stringr)
library(ggrepel)

cat("=== Strategy Effects (Study 2) ===\n")

results <- readRDS("output/results/study2_main_effects.rds")
effects <- results$strategy_effects

effects <- effects %>%
  mutate(
    strategy_label = ifelse(
      strategy %in% names(strategy_labels),
      strategy_labels[strategy],
      str_replace_all(strategy, "[_-]", " ") %>% str_to_title()
    )
  )

# ---- Combine behavior + attitude data ----------------------------------------

behavior_data <- effects %>%
  filter(dv == "signed_petition") %>%
  mutate(dv_short = "Sign petition")

attitude_data <- effects %>%
  filter(dv_type == "Attitude") %>%
  mutate(dv_short = ifelse(grepl("petition", dv), "Support petition", "Support organization"))

plot_data <- bind_rows(behavior_data, attitude_data)

# Order strategies by behavioral effectiveness (most effective first for top-to-bottom)
beh_order <- behavior_data %>% arrange(estimate) %>% pull(strategy_label)
row_order <- rev(beh_order)
plot_data <- plot_data %>%
  mutate(
    strategy_label = factor(strategy_label, levels = row_order),
    dv_short = factor(dv_short, levels = c("Support organization", "Sign petition", "Support petition"))
  )

strat_palette <- c(
  "Emotional Activation"      = "#E41A1C",
  "Implementation Intentions" = "#377EB8",
  "Identity Labeling"         = "#4DAF4A",
  "Commitment Escalation"     = "#984EA3",
  "Anticipated Regret"        = "#FF7F00",
  "Mega (All Combined)"       = "#000000",
  "Info (Efficacy)"           = "#F781BF",
  "Info (Issue)"              = "#66C2A5"
)

dv_colors <- c(
  "Sign petition" = unname(dv_type_colors["Behavior"]),
  "Support petition" = "#BBBBBB",
  "Support organization" = "#BBBBBB"
)
dv_shapes <- c(
  "Sign petition" = 16,
  "Support petition" = 17,
  "Support organization" = 15
)

position <- position_dodgev(0.5)

band_data_a <- tibble(
  y = seq_along(row_order),
  ymin = y - 0.5, ymax = y + 0.5
) %>%
  filter(y %% 2 == 0)

g_a <- plot_data %>%
  ggplot(aes(y = strategy_label, x = estimate, xmin = conf.low, xmax = conf.high,
             color = dv_short, shape = dv_short, group = dv_short)) +
  geom_rect(data = band_data_a, inherit.aes = FALSE,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
            fill = "grey95") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.4) +
  geom_linerange(aes(alpha = dv_short), linewidth = 0.3, position = position) +
  geom_point(size = 1.5, position = position) +
  geom_label(
    data = . %>% filter(dv_short == "Sign petition") %>%
      mutate(row_idx = as.integer(strategy_label),
             bg = ifelse(row_idx %% 2 == 0, "grey95", "white")),
    aes(label = sprintf("%+.2f", estimate), fill = bg),
    hjust = 0.5, vjust = -0.2, size = 6.5 / .pt,
    position = position, show.legend = FALSE,
    label.size = 0, label.padding = unit(1.5, "pt"), alpha = 0.8
  ) +
  scale_fill_identity() +
  scale_color_manual(values = dv_colors, name = NULL,
                     breaks = c("Sign petition", "Support petition", "Support organization")) +
  scale_shape_manual(values = dv_shapes, name = NULL,
                     breaks = c("Sign petition", "Support petition", "Support organization")) +
  scale_alpha_manual(values = c("Sign petition" = default_linerange_alpha,
                                "Support petition" = 0.25,
                                "Support organization" = 0.25),
                     guide = "none") +
  scale_y_discrete(labels = function(x) sapply(x, function(lab) paste(strwrap(lab, width = 14), collapse = "<br>"))) +
  theme_paper(base_size = 8) +
  halfopen_theme +
  theme(
    axis.title.x = element_text(face = "bold", margin = margin(t = 2)),
    plot.title = element_text(hjust = -0.08, face = "bold", size = 15,
                              margin = margin(t = 2, b = 4)),
    legend.position = "top",
    legend.justification = "center",
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.size = unit(0.6, "lines"),
    legend.margin = margin(0, 0, -4, 0)
  ) +
  guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1)) +
  labs(
    x = "Average treatment effect",
    y = NULL,
    title = "A"
  )

# ==============================================================================
# Panel B: Heatmap of mechanism activation by strategy
# ==============================================================================

s2 <- readRDS("output/results/study2_clean.rds") %>%
  filter(treatment == "treatment")

battery_items <- list(
  Emotion          = c("mechanism_emotion_strong", "mechanism_emotion_empathy"),
  "Implementation Intentions" = c("mechanism_implement_visualise", "mechanism_implement_plan"),
  Identity         = c("mechanism_identity_action", "mechanism_identity_followthrough"),
  Commitment       = c("mechanism_commitment_increasing", "mechanism_commitment_consistency"),
  Regret           = c("mechanism_regret_signing", "mechanism_regret_disappointment"),
  "Info (Efficacy)" = c("mechanism_efficacy_impact", "mechanism_efficacy_difference"),
  "Info (Issue)"    = c("mechanism_info_learning", "mechanism_info_knowledge")
)

library(tidyr)
profile_data <- purrr::imap_dfr(battery_items, function(items, bat_label) {
  s2 %>%
    mutate(battery_score = rowMeans(across(all_of(items)), na.rm = TRUE)) %>%
    group_by(conversation_type) %>%
    summarise(
      mean_score = mean(battery_score, na.rm = TRUE),
      se = sd(battery_score, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
    mutate(
      conf.low = mean_score - 1.96 * se,
      conf.high = mean_score + 1.96 * se,
      battery_short = bat_label,
      strategy_clean = conversation_type
    ) %>%
    rename(estimate = mean_score) %>%
    dplyr::select(strategy_clean, battery_short, estimate, conf.low, conf.high)
})

profile_data <- profile_data %>%
  mutate(
    strategy_label = ifelse(
      strategy_clean %in% names(strategy_labels),
      strategy_labels[strategy_clean],
      str_replace_all(strategy_clean, "_", " ") %>% str_to_title()
    ),
    strategy_label = factor(strategy_label, levels = beh_order)
  )

# Order batteries by mean score across strategies (ascending)
battery_order <- profile_data %>%
  group_by(battery_short) %>%
  summarise(mean_est = mean(estimate), .groups = "drop") %>%
  arrange(mean_est) %>%
  pull(battery_short)

# Compute per-strategy average across all batteries
avg_data <- profile_data %>%
  group_by(strategy_clean, strategy_label) %>%
  summarise(
    estimate = mean(estimate),
    conf.low = mean(conf.low),
    conf.high = mean(conf.high),
    .groups = "drop"
  ) %>%
  mutate(battery_short = "Average")

profile_data <- bind_rows(profile_data, avg_data) %>%
  mutate(battery_short = factor(battery_short, levels = c(battery_order, "Average")))

# Map each strategy to its corresponding mechanism battery for the diagonal
strat_to_battery <- c(
  "Emotional Activation"      = "Emotion",
  "Implementation Intentions" = "Implementation Intentions",
  "Identity Labeling"         = "Identity",
  "Commitment Escalation"     = "Commitment",
  "Anticipated Regret"        = "Regret",
  "Info (Efficacy)"           = "Info (Efficacy)",
  "Info (Issue)"              = "Info (Issue)",
  "Mega (All Combined)"       = "Average"
)

# Reverse mapping: battery short name -> strategy name for x-axis labels
battery_to_strat <- setNames(names(strat_to_battery), strat_to_battery)

# Column order: reverse so diagonal goes top-left to bottom-right
col_order <- rev(strat_to_battery[as.character(row_order)])

heat_data <- profile_data %>%
  mutate(
    strategy_label = factor(strategy_label, levels = row_order),
    battery_short = factor(battery_short, levels = col_order)
  )

heat_data <- heat_data %>%
  group_by(battery_short) %>%
  mutate(z_score = (estimate - mean(estimate)) / sd(estimate)) %>%
  ungroup()

# Mark diagonal cells
heat_data <- heat_data %>%
  mutate(is_diagonal = strat_to_battery[as.character(strategy_label)] == as.character(battery_short))

# Short display labels for heatmap x-axis
x_labels <- c(
  "Info (Issue)"              = "Info (Issue)",
  "Emotion"                   = "Emotion",
  "Commitment"                = "Commitment",
  "Identity"                  = "Identity",
  "Regret"                    = "Regret",
  "Info (Efficacy)"           = "Info (Efficacy)",
  "Implementation Intentions" = "Implementation",
  "Average"                   = "**Average**"
)

heat_data <- heat_data %>%
  mutate(is_avg = battery_short == "Average")

# Nudge Average column slightly right to create a visual gap
heat_data <- heat_data %>%
  mutate(x_num = as.numeric(battery_short),
         x_num = ifelse(is_avg, x_num + 0.15, x_num))

g_b <- ggplot(heat_data, aes(x = x_num, y = strategy_label, fill = z_score)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_tile(data = heat_data %>% filter(is_diagonal),
            color = "black", linewidth = 0.25, fill = NA) +
  geom_text(data = heat_data %>% filter(!is_avg),
            aes(label = sprintf("%.0f", estimate)), size = 6.5 / .pt, color = "black") +
  geom_text(data = heat_data %>% filter(is_avg),
            aes(label = sprintf("%.0f", estimate)), size = 6.5 / .pt,
            color = "black", fontface = "bold") +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
    name = "Relative\nactivation"
  ) +
  scale_x_continuous(
    breaks = c(seq_along(col_order)[-length(col_order)],
               length(col_order) + 0.15),
    labels = x_labels,
    expand = expansion(mult = 0.06)
  ) +
  theme_void(base_size = 8) +
  theme(
    plot.title = element_text(hjust = -0.08, face = "bold", size = 15,
                              margin = margin(t = 2, b = 4)),
    axis.text.x = ggtext::element_markdown(size = 6, angle = 30, hjust = 0.5,
                                            margin = margin(t = 1)),
    axis.title.x = element_text(face = "bold", size = 8, margin = margin(t = 2)),
    axis.text.y = element_blank(),
    legend.position = "none"
  ) +
  labs(x = "Mechanism activation (0-100)", y = NULL, title = "B")

# ==============================================================================
# Combine panels
# ==============================================================================

g_heat <- g_b

g_a <- g_a + theme(plot.margin = margin(0, 0, 0, 0))
g_heat <- g_heat + theme(plot.margin = margin(0, 0, 0, 0))

g <- g_a | g_heat

ggsave("output/figures/main/fig_strategy_effects.pdf", g, width = 17.8, height = 10, units = "cm")
cat("  Saved output/figures/main/fig_strategy_effects.pdf\n")
