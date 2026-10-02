source("analysis/figures/theme.R")
library(dplyr)
library(ggtext)
library(patchwork)

cat("=== SI Fig: Meta-Analysis Forest Plot ===\n")

# ---- Load petition-signing ATEs for focal study (Hackenburg et al. 2026) ----
.s1_rds <- readRDS("output/results/study1_main_effects.rds")
.s2_rds <- readRDS("output/results/study2_main_effects.rds")$overall_ates

.s1_pet <- .s1_rds[.s1_rds$dv == "signed_petition" & .s1_rds$facet == "Overall", ]
.s2_pet <- .s2_rds[.s2_rds$dv == "signed_petition" & .s2_rds$facet == "Overall" &
                     .s2_rds$grp == "All", ]

focal_effect_s1 <- .s1_pet$estimate  * 100
focal_ci_lo_s1  <- .s1_pet$conf.low  * 100
focal_ci_hi_s1  <- .s1_pet$conf.high * 100

focal_effect_s2 <- .s2_pet$estimate  * 100
focal_ci_lo_s2  <- .s2_pet$conf.low  * 100
focal_ci_hi_s2  <- .s2_pet$conf.high * 100

# ---- Load data ---------------------------------------------------------------

data_path   <- "data/meta_analysis_data.csv"
output_path <- "output/figures/si/fig_si_meta_analysis.pdf"

raw <- read.csv(data_path, stringsAsFactors = FALSE, check.names = FALSE)

# ---- Rename columns to working names -----------------------------------------

meta <- raw[, nchar(trimws(names(raw))) > 0] %>%   # drop empty-named columns
  rename(
    panel             = survey_vs_field,
    study_label       = study_reference,
    est_desc          = estimate_description,
    arm               = arm_label,
    total_sample_size = total_sample_size,
    operationalises   = `operationalises_real_petition_signing (as opposed to expressing an intent to sign)`,
    n_control         = `control_group_total (participants in control condition)`,
    prop_control_raw  = `proportion_control (% prcpts who signed in the control condition)`,
    n_treatment       = `treatment_group_total (participants in treatment condition)`,
    prop_treatment_raw = `proportion_treatment (% prcpts who signed in the treatment condition)`,
    effect_raw        = effect_size_percentage_points,
    se_raw            = standard_error
  ) %>%
  dplyr::select(panel, study_label, est_desc, arm, total_sample_size, operationalises,
         n_control, prop_control_raw,
         n_treatment, prop_treatment_raw,
         effect_raw, se_raw)

# Helper: extract the leading integer from strings like "1,038 person-petitions"
parse_n <- function(x) {
  x <- gsub(",", "", as.character(x))          # remove thousands separator
  x <- gsub("^([0-9]+).*$", "\\1", x)         # keep only leading digits
  suppressWarnings(as.numeric(x))
}

# ---- Keep only rows that belong to a panel and have usable data --------------

meta <- meta %>%
  filter(
    !is.na(panel),
    tolower(trimws(panel)) %in% c("field", "survey")
  ) %>%
  mutate(
    panel              = tolower(trimws(panel)),
    n_control          = parse_n(n_control),
    n_treatment        = parse_n(n_treatment),
    prop_control_raw   = suppressWarnings(as.numeric(prop_control_raw)),
    prop_treatment_raw = suppressWarnings(as.numeric(prop_treatment_raw)),
    effect_raw         = suppressWarnings(as.numeric(effect_raw))
  )

# ---- Asterisk for willingness_to_sign studies --------------------------------
# Studies where the outcome is operationalised as willingness to sign (rather
# than actual petition signing) get a * appended to the study name label.

meta <- meta %>%
  mutate(
    study_label = ifelse(
      trimws(operationalises) == "willingness_to_sign",
      paste0(study_label, "*"),
      study_label
    )
  )

# ---- Normalize proportions to 0–1 scale -------------------------------------
# Some rows store proportions as percentages (e.g. 4.4 meaning 4.4%).
# Treat any value > 1 as already on a 0–100 scale and divide by 100.

meta <- meta %>%
  mutate(
    p_control   = case_when(
      !is.na(prop_control_raw)  & prop_control_raw  > 1 ~ prop_control_raw  / 100,
      TRUE                                               ~ prop_control_raw
    ),
    p_treatment = case_when(
      !is.na(prop_treatment_raw) & prop_treatment_raw > 1 ~ prop_treatment_raw / 100,
      TRUE                                                 ~ prop_treatment_raw
    )
  )

# ---- Compute effect size -----------------------------------------------------
# Prefer the spreadsheet value; fall back to |p_treatment - p_control| * 100.

meta <- meta %>%
  mutate(
    effect_size = case_when(
      !is.na(effect_raw)                        ~ abs(as.numeric(effect_raw)),
      !is.na(p_treatment) & !is.na(p_control)  ~ abs(p_treatment - p_control) * 100,
      TRUE                                      ~ NA_real_
    )
  ) %>%
  filter(!is.na(effect_size))

# ---- Compute binomial SE and 95% CI -----------------------------------------
# CI is only computed when numeric n values and valid proportions are available
# for both groups.  Rows missing this data get ci_lo = ci_hi = NA (no error bar).

meta <- meta %>%
  mutate(
    has_ci_data = !is.na(n_control) & !is.na(n_treatment) &
                  !is.na(p_control) & !is.na(p_treatment) &
                  n_control > 0    & n_treatment > 0,
    sd_control   = sqrt(p_control   * (1 - p_control))   * 100,
    sd_treatment = sqrt(p_treatment * (1 - p_treatment)) * 100,
    se = ifelse(
      has_ci_data,
      sqrt(sd_treatment^2 / n_treatment + sd_control^2 / n_control),
      NA_real_
    ),
    ci_lo = ifelse(has_ci_data, effect_size - 1.96 * se, NA_real_),
    ci_hi = ifelse(has_ci_data, effect_size + 1.96 * se, NA_real_)
  )

# Override CI for rows where se_raw contains "lo, hi" bounds directly
# (e.g. Sands & de Kadt stores "0.8, 21.6" meaning ci_lo=0.8, ci_hi=21.6).
has_ci_range <- !is.na(meta$se_raw) & grepl(",", meta$se_raw)
if (any(has_ci_range)) {
  parts <- strsplit(meta$se_raw[has_ci_range], ",")
  meta$ci_lo[has_ci_range] <- sapply(parts, function(p) suppressWarnings(as.numeric(trimws(p[1]))))
  meta$ci_hi[has_ci_range] <- sapply(parts, function(p) suppressWarnings(as.numeric(trimws(p[2]))))
}

# Mark existing rows as non-focal
meta$is_focal <- FALSE

# ---- Inject focal study rows (Hackenburg et al. 2026) -----------------------
# CIs come directly from the study output RDS files (already on 0–100 pp scale).
focal_rows <- data.frame(
  panel              = "survey",
  study_label        = c("Study 1", "Study 2"),
  est_desc           = NA_character_,
  arm                = c("Study 1", "Study 2"),
  total_sample_size  = c("8,000", "9,950"),
  operationalises    = "actual_petition_signing",
  n_control          = NA_real_,
  prop_control_raw   = NA_real_,
  n_treatment        = NA_real_,
  prop_treatment_raw = NA_real_,
  effect_raw         = NA_real_,
  se_raw             = NA_character_,
  p_control          = NA_real_,
  p_treatment        = NA_real_,
  has_ci_data        = FALSE,
  sd_control         = NA_real_,
  sd_treatment       = NA_real_,
  se                 = NA_real_,
  effect_size        = c(focal_effect_s1, focal_effect_s2),
  ci_lo              = c(focal_ci_lo_s1,  focal_ci_lo_s2),
  ci_hi              = c(focal_ci_hi_s1,  focal_ci_hi_s2),
  is_focal           = TRUE,
  stringsAsFactors   = FALSE
)

meta$total_sample_size <- as.character(meta$total_sample_size)
meta <- bind_rows(meta, focal_rows)

# ---- Muradova & Beauvais arm labels -----------------------------------------
# Study 1 = UK Sample, Study 2 = US Sample (injected as est_desc).

meta <- meta %>%
  mutate(
    est_desc = case_when(
      grepl("Muradova", study_label, ignore.case = TRUE) & trimws(arm) == "Study 1" ~ "UK Sample",
      grepl("Muradova", study_label, ignore.case = TRUE) & trimws(arm) == "Study 2" ~ "US Sample",
      TRUE ~ est_desc
    )
  )

# ---- Build display labels ----------------------------------------------------
# Line 1: study name (black)
# Line 2 (optional): estimate_description in grey parentheses
# Line 3 (optional): n = X,XXX in grey, when n data is available

meta <- meta %>%
  mutate(
    n_direct   = ifelse(!is.na(n_control) & !is.na(n_treatment),
                        n_control + n_treatment, NA_real_),
    n_fallback = parse_n(total_sample_size),
    n_total_fmt = ifelse(
      !is.na(n_direct),
      paste0("n = ", format(n_direct,   big.mark = ",")),
      ifelse(
        !is.na(n_fallback),
        paste0("n = ", format(n_fallback, big.mark = ",")),
        NA_character_
      )
    ),
    display_label = paste0(
      study_label,
      ifelse(
        !is.na(est_desc) & nchar(trimws(est_desc)) > 0,
        paste0("<br><span style='color:grey60;font-size:6pt'>(",
               est_desc, ")</span>"),
        ""
      ),
      ifelse(
        !is.na(n_total_fmt),
        paste0("<br><span style='color:grey60;font-size:6pt'>",
               n_total_fmt, "</span>"),
        ""
      )
    )
  )

# ---- Assemble combined plot data --------------------------------------------
# Within each panel: focal (current study) rows sort to the top, then
# remaining studies sort alphabetically.

plot_data <- meta %>%
  mutate(
    panel_label = case_when(
      panel == "field"  ~ "Field studies",
      panel == "survey" ~ "Survey studies"
    ),
    panel_label  = factor(panel_label, levels = c("Survey studies", "Field studies")),
    study_type   = ifelse(is_focal, "Current study", "Other studies")
  ) %>%
  arrange(panel_label, !is_focal, study_label) %>%
  mutate(
    row_id = row_number(),
    y_var  = factor(row_id, levels = rev(row_id))
  )

n_field  <- sum(plot_data$panel_label == "Field studies")
n_survey <- sum(plot_data$panel_label == "Survey studies")

cat("  Field rows:  ", n_field,  "\n")
cat("  Survey rows: ", n_survey, "\n")

y_lbl <- setNames(plot_data$display_label, as.character(plot_data$y_var))

# ---- Shared x-axis limits ---------------------------------------------------

# Anchor breaks at 0 and step outward by 2 in both directions so both sides
# land on even numbers and 0 is always labelled. Axis limits are derived from
# the outermost breaks so there is no gap between the last tick and the edge.
x_lo     <- floor(min(c(plot_data$ci_lo, 0), na.rm = TRUE))
x_hi     <- ceiling(max(c(plot_data$ci_hi, plot_data$effect_size), na.rm = TRUE))
# Breaks land on even numbers anchored at 0; axis limits extend one unit
# beyond the outermost break so bars are never clipped at the edge.
x_breaks <- sort(unique(c(seq(0, x_lo, by = -2), seq(0, x_hi, by = 2))))
x_min    <- min(x_breaks)
x_max    <- max(x_breaks) + 1

beh_red  <- unname(dv_type_colors["Behavior"])
study_colors <- c("Current study" = "black", "Other studies" = beh_red)

# ---- Build forest plot ------------------------------------------------------

g <- ggplot(plot_data, aes(x = effect_size, y = y_var, color = study_type)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_segment(
    data      = plot_data %>% filter(!is.na(ci_lo)),
    aes(x = ci_lo, xend = ci_hi, yend = y_var, color = study_type),
    linewidth = 0.3,
    alpha     = default_linerange_alpha
  ) +
  geom_point(
    aes(fill = study_type),
    shape = 21,
    size  = default_point_size
  ) +
  geom_text(
    aes(label = sprintf("%.1f", effect_size)),
    vjust       = -0.9,
    hjust       = 0.5,
    size        = 8 / 2.845,
    show.legend = FALSE
  ) +
  scale_color_manual(
    name   = NULL,
    values = study_colors,
    labels = c("Current study" = "Current findings",
               "Other studies" = "Previous findings"),
    guide  = guide_legend(override.aes = list(
      shape     = 21,
      fill      = study_colors,
      size      = 2,
      linetype  = 0,
      alpha     = 1
    ))
  ) +
  scale_fill_manual(values = study_colors, guide = "none") +
  scale_y_discrete(labels = y_lbl) +
  scale_x_continuous(breaks = x_breaks) +
  coord_cartesian(xlim = c(x_min, x_max)) +
  facet_grid(
    panel_label ~ .,
    scales    = "free_y",
    space     = "free_y",
    switch    = "y",
    axes      = "all_x"
  ) +
  labs(x = "Absolute effect size (percentage points)", y = NULL) +
  theme_paper(base_size = 8) +
  halfopen_theme +
  theme(
    strip.placement        = "outside",
    strip.text.y.left      = element_text(face = "bold", margin = margin(r = 6)),
    axis.text.y            = element_markdown(lineheight = 1.4),
    axis.title.x           = element_text(face = "bold"),
    panel.spacing.y        = unit(0.3, "lines"),
    legend.position        = "inside",
    legend.position.inside = c(0.99, 0.99),
    legend.justification   = c(1, 1),
    legend.background      = element_rect(fill = "white", color = "grey80",
                                          linewidth = 0.3),
    legend.key.size        = unit(0.5, "lines"),
    legend.text            = element_text(size = 7),
    legend.margin          = margin(3, 6, 3, 4)
  )

# ---- Save --------------------------------------------------------------------

ggsave(output_path, g, width = 17, height = (n_field + n_survey) * 1.3 + 2,
       units = "cm")
cat("  Saved:", output_path, "\n")
