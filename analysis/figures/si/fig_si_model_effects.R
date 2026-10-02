source("analysis/figures/theme.R")
source("analysis/utils.R")

cat("=== SI Fig: Model x Strategy Effects on Petition Signing ===\n")

# ==============================================================================
# Panel A: Study 1 — condition x model
# ==============================================================================

s1 <- load_study1() %>%
  mutate(treatment_type = fct_relevel(treatment_type, "control"))

s1_model_labels <- c(
  "anthropic/claude-sonnet-4"   = "Claude Sonnet 4",
  "google/gemini-2.5-pro"       = "Gemini 2.5 Pro",
  "openai/chatgpt-4o-latest"    = "ChatGPT-4o",
  "openai/gpt-4.1"              = "GPT-4.1",
  "x-ai/grok-4"                 = "Grok 4"
)

s1_model_colors <- c(
  "Claude Sonnet 4" = "#6A3D9A",
  "Gemini 2.5 Pro"  = "#1F78B4",
  "ChatGPT-4o"      = "#FF7F00",
  "GPT-4.1"         = "#33A02C",
  "Grok 4"          = "#E31A1C"
)

s1_condition_labels <- c(
  "petition"     = "Info (Issue)",
  "organization" = "Info (Organization)",
  "random"       = "Random"
)

s1_effects <- imap_dfr(s1_model_labels, function(lab, raw_name) {
  sub <- s1 %>% filter(model_type == raw_name)
  form <- as.formula(paste0(
    "1 * signed_petition ~ treatment_type + ",
    "scale(pretreatment_agreement) + scale(pretreatment_organization_opinion) + ",
    "scale(charity_interest_num) + (1|petition_title)"
  ))
  fit_lmer_and_tidy(form, sub) %>%
    filter(str_starts(term, "treatment_type")) %>%
    mutate(
      condition = str_remove(term, "treatment_type"),
      model_label = lab
    )
})

s1_effects <- s1_effects %>%
  mutate(
    condition_label = s1_condition_labels[condition],
    model_label = factor(model_label, levels = c("Claude Sonnet 4", "Gemini 2.5 Pro", "ChatGPT-4o", "GPT-4.1", "Grok 4"))
  )

s1_row_order <- c("Info (Issue)", "Info (Organization)", "Random")
s1_effects <- s1_effects %>%
  mutate(condition_label = factor(condition_label, levels = rev(s1_row_order)))

s1_band <- tibble(y = seq_along(s1_row_order), ymin = y - 0.5, ymax = y + 0.5) %>%
  filter(y %% 2 == 0)

s1_pos <- position_dodgev(0.6)

g_a <- ggplot(s1_effects, aes(y = condition_label, x = estimate,
                               xmin = conf.low, xmax = conf.high,
                               color = model_label, shape = model_label,
                               group = model_label)) +
  geom_rect(data = s1_band, inherit.aes = FALSE,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
            fill = "grey95") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.4) +
  geom_linerange(linewidth = 0.3, alpha = 0.4, position = s1_pos) +
  geom_point(size = 1.8, position = s1_pos) +
  scale_color_manual(values = s1_model_colors, name = "Model") +
  scale_shape_manual(values = c(16, 17, 15, 18, 8), name = "Model") +
  theme_paper(base_size = 8) +
  halfopen_theme +
  theme(
    plot.title = element_text(hjust = -0.08, face = "bold", size = 15, margin = margin(t = 2, b = 4)),
    axis.title.x = element_text(face = "bold", margin = margin(t = 2)),
    legend.position = "top",
    legend.justification = "center",
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.size = unit(0.5, "lines"),
    legend.text = element_text(size = 6),
    legend.margin = margin(0, 0, -2, 0)
  ) +
  guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1)) +
  labs(x = "Treatment effect on petition signing (vs. control)", y = NULL,
       title = "A    Study 1")

# ==============================================================================
# Panel B: Study 2 — strategy x model
# ==============================================================================

s2 <- load_study2() %>%
  mutate(strategy = ifelse(treatment == "control", "control", conversation_type) %>%
           fct_relevel("control"))

s2_model_labels <- c(
  "anthropic/claude-opus-4.6"      = "Claude Opus 4.6",
  "google/gemini-3.1-pro-preview"  = "Gemini 3.1 Pro",
  "openai/gpt-4.1"                 = "GPT-4.1",
  "x-ai/grok-4"                    = "Grok 4"
)

s2_model_colors <- c(
  "Claude Opus 4.6"  = "#6A3D9A",
  "Gemini 3.1 Pro"   = "#1F78B4",
  "GPT-4.1"          = "#33A02C",
  "Grok 4"           = "#E31A1C"
)

s2_effects <- imap_dfr(s2_model_labels, function(lab, raw_name) {
  sub <- s2 %>% filter(model_type == raw_name)
  form <- as.formula(paste0(
    "1 * signed_petition ~ strategy + ",
    "scale(pretreatment_agreement) + scale(pretreatment_organization_opinion) + (1|petition_title)"
  ))
  fit_lmer_and_tidy(form, sub) %>%
    filter(str_starts(term, "strategy")) %>%
    mutate(
      p.value = 2 * pnorm(abs(statistic), lower.tail = FALSE),
      strategy = str_remove(term, "strategy"),
      model_label = lab
    )
})

s2_effects <- s2_effects %>%
  mutate(
    strategy_label = ifelse(
      strategy %in% names(strategy_labels),
      strategy_labels[strategy],
      str_replace_all(strategy, "[_-]", " ") %>% str_to_title()
    )
  )

main_results <- readRDS("output/results/study2_main_effects.rds")
beh_order <- main_results$strategy_effects %>%
  filter(dv == "signed_petition") %>%
  arrange(estimate) %>%
  mutate(strategy_label = ifelse(
    strategy %in% names(strategy_labels),
    strategy_labels[strategy],
    str_replace_all(strategy, "[_-]", " ") %>% str_to_title()
  )) %>%
  pull(strategy_label)
s2_row_order <- rev(beh_order)

s2_effects <- s2_effects %>%
  mutate(
    strategy_label = factor(strategy_label, levels = s2_row_order),
    model_label = factor(model_label, levels = c("Claude Opus 4.6", "Gemini 3.1 Pro", "GPT-4.1", "Grok 4"))
  )

s2_band <- tibble(y = seq_along(s2_row_order), ymin = y - 0.5, ymax = y + 0.5) %>%
  filter(y %% 2 == 0)

s2_pos <- position_dodgev(0.6)

g_b <- ggplot(s2_effects, aes(y = strategy_label, x = estimate,
                               xmin = conf.low, xmax = conf.high,
                               color = model_label, shape = model_label,
                               group = model_label)) +
  geom_rect(data = s2_band, inherit.aes = FALSE,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
            fill = "grey95") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.4) +
  geom_linerange(linewidth = 0.3, alpha = 0.4, position = s2_pos) +
  geom_point(size = 1.8, position = s2_pos) +
  scale_color_manual(values = s2_model_colors, name = "Model") +
  scale_shape_manual(values = c(16, 17, 15, 18), name = "Model") +
  scale_y_discrete(labels = function(x) sapply(x, function(lab) paste(strwrap(lab, width = 14), collapse = "<br>"))) +
  theme_paper(base_size = 8) +
  halfopen_theme +
  theme(
    plot.title = element_text(hjust = -0.08, face = "bold", size = 15, margin = margin(t = 2, b = 4)),
    axis.title.x = element_text(face = "bold", margin = margin(t = 2)),
    legend.position = "top",
    legend.justification = "center",
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.size = unit(0.5, "lines"),
    legend.text = element_text(size = 6),
    legend.margin = margin(0, 0, -2, 0)
  ) +
  guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1)) +
  labs(x = "Treatment effect on petition signing (vs. control)", y = NULL,
       title = "B    Study 2")

# ==============================================================================
# Combine
# ==============================================================================

g <- g_a / g_b + plot_layout(heights = c(3, 8))

ggsave("output/figures/si/fig_si_model_effects.pdf", g, width = 14, height = 14, units = "cm")
cat("  Saved output/figures/si/fig_si_model_effects.pdf\n")
