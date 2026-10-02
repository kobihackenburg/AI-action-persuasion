library(ggplot2)
library(ggstance)
library(scales)
library(ggtext)
library(patchwork)

# ---- Base theme --------------------------------------------------------------

theme_paper <- function(base_size = 11) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = base_size),
      strip.text.x = element_text(hjust = 0.5),
      axis.text.y = element_markdown(),
      plot.title = element_text(face = "bold", size = base_size + 1),
      legend.position = "right"
    )
}

# ---- Shared aesthetics -------------------------------------------------------

dv_type_colors <- c("Attitude" = "#000000", "Behavior" = "#B2182B")
beh_dv_names   <- c("Sign petition", "Organizational engagement")

halfopen_theme <- theme(
  panel.grid.major.x = element_blank(),
  panel.border       = element_blank(),
  axis.line.x        = element_line(color = "grey40", linewidth = 0.3),
  axis.line.y        = element_line(color = "grey40", linewidth = 0.3),
  axis.ticks         = element_line(color = "grey40", linewidth = 0.3)
)

# ---- Color palettes ----------------------------------------------------------

issue_colors <- c("black", hue_pal()(8))

strategy_colors <- c(
  "control"               = "#999999",
  "emotional_activation"  = "#E41A1C",
  "implementation_intentions" = "#377EB8",
  "identity_labeling"     = "#4DAF4A",
  "commitment_escalation" = "#984EA3",
  "anticipated_regret"    = "#FF7F00",
  "mega"                  = "#A65628",
  "info_impact_efficacy"  = "#F781BF",
  "info_issue"            = "#66C2A5"
)

study_colors <- c("study1" = "#2166AC", "study2" = "#B2182B")

# ---- Label definitions -------------------------------------------------------

strategy_labels <- c(
  "emotional_activation"      = "Emotional Activation",
  "emotional-activation"      = "Emotional Activation",
  "implementation_intentions"  = "Implementation Intentions",
  "implementation-intentions"  = "Implementation Intentions",
  "identity_labeling"         = "Identity Labeling",
  "identity-labeling"         = "Identity Labeling",
  "commitment_escalation"     = "Commitment Escalation",
  "commitment-escalation"     = "Commitment Escalation",
  "anticipated_regret"        = "Anticipated Regret",
  "anticipated-regret"        = "Anticipated Regret",
  "mega"                      = "Mega (All Combined)",
  "info_impact_efficacy"      = "Info (Efficacy)",
  "info-impact-efficacy"      = "Info (Efficacy)",
  "info_issue"                = "Info (Issue)",
  "info-issue"                = "Info (Issue)",
  "control"                   = "Control"
)

dv_labels <- c(
  "signed_petition"         = "Sign petition",
  "delta_petition"          = "Support petition (attitude)",
  "delta_organization"      = "Support organization (attitude)",
  "donate_proportion"       = "Bonus donation",
  "extra_task_clicks/100"   = "Money raising effort",
  "not_switch_organization" = "Choose organization"
)

# ---- Common geom defaults ----------------------------------------------------

default_point_size <- 2.5
default_linerange_alpha <- 0.5
default_text_size <- 3
default_dodge_width <- 0.85
