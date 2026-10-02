source("analysis/figures/theme.R")

library(dplyr)
library(forcats)

cat("=== SI Fig: Demographic Distributions ===\n")

# ---- Load processed data ----------------

s1 <- readRDS("output/results/study1_clean.rds")
s2 <- readRDS("output/results/study2_clean.rds")

# ---- Shared aesthetics -------------------------------------------------------

demo_fill <- "grey40"

demo_theme <- theme_paper() +
  halfopen_theme +
  theme(panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3))

ideology_order <- c("Left", "Centre-left", "Centre", "Centre-right", "Right", "Other/Skip",
                    "Other/Prefer not to say")

# ---- Plot builders -----------------------------------------------------------

make_age_plot <- function(df) {
  df %>%
    filter(!is.na(age), age >= 18, age <= 100) %>%
    ggplot(aes(x = age)) +
    geom_histogram(binwidth = 5, fill = demo_fill, color = "white", linewidth = 0.2) +
    demo_theme +
    labs(x = NULL, y = "count", title = "Age")
}

make_gender_plot <- function(df) {
  df %>%
    filter(!is.na(gender)) %>%
    mutate(gender = fct_infreq(gender)) %>%
    ggplot(aes(x = gender)) +
    geom_bar(fill = demo_fill) +
    demo_theme +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    labs(x = NULL, y = "count", title = "Gender")
}

make_ideology_plot <- function(df) {
  valid_levels <- ideology_order[ideology_order %in% unique(df$ideology)]
  df %>%
    filter(!is.na(ideology)) %>%
    mutate(ideology = factor(ideology, levels = valid_levels)) %>%
    ggplot(aes(x = ideology)) +
    geom_bar(fill = demo_fill) +
    demo_theme +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    labs(x = NULL, y = "count", title = "Ideology")
}

# ---- Assemble panels ---------------------------------------------------------

study1_row <- wrap_elements(
  full = (make_age_plot(s1) | make_gender_plot(s1) | make_ideology_plot(s1)) +
    plot_annotation(
      title = "Study 1",
      theme = theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5))
    )
)

study2_row <- wrap_elements(
  full = (make_age_plot(s2) | make_gender_plot(s2) | make_ideology_plot(s2)) +
    plot_annotation(
      title = "Study 2",
      theme = theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5))
    )
)

g <- study1_row / study2_row

# ---- Save --------------------------------------------------------------------

ggsave("output/figures/si/fig_si_demographics.pdf", g, width = 10, height = 8)
cat("  Saved output/figures/si/fig_si_demographics.pdf\n")
