source("analysis/utils.R")

cat("=== 04: Mechanisms ===\n")

# ---- Self-reported learning correlations (both studies) ----------------------

s1 <- load_study1() %>%
  filter(!is.na(model_type)) %>%
  mutate(cond = ifelse(treatment_type == "control", "control",
                       paste0(treatment_type, " ", model_type)) %>%
           fct_relevel("control"))

covariates <- s1_covariates

# Condition-level mean learning and ATEs (Study 1)
estimate_cond <- function(dv, vs_control = TRUE) {
  if (vs_control) {
    form <- as.formula(str_glue("{dv} ~ cond + {covariates}"))
    fit_lmer_and_tidy(form, s1) %>% filter(str_starts(term, "cond"))
  } else {
    form <- as.formula(str_glue("{dv} ~ 0 + cond + {covariates}"))
    s1 %>% filter(cond != "control") %>%
      { fit_lmer_and_tidy(form, .) } %>% filter(str_starts(term, "cond"))
  }
}

s1_learning <- estimate_cond("conversation_learned", vs_control = FALSE)

s1_learning_cors <- tribble(~dv, ~dv_type,
  "delta_petition", "Attitude",
  "1 * signed_petition", "Behavior",
  "delta_organization", "Attitude",
  "org_behavior", "Behavior"
) %>%
  pmap_dfr(function(dv, dv_type) {
    ate <- estimate_cond(dv, vs_control = FALSE)
    joined <- left_join(
      s1_learning %>% select(term, learning = estimate),
      ate %>% select(term, ate = estimate),
      by = "term"
    )
    ct <- cor.test(joined$learning, joined$ate)
    tibble(study = "study1", dv = dv, dv_type = dv_type,
           r = ct$estimate, p.value = ct$p.value, n = nrow(joined))
  })

# ---- Prompt-type effects (Study 1) ------------------------------------------
# Compare control vs. info vs. random on beliefs and behaviors

s1_prompt_type <- tribble(~dv, ~dv_name, ~dv_type,
  "delta_petition", "Support petition", "Attitude",
  "1 * signed_petition", "Sign petition", "Behavior",
  "delta_organization", "Support organization", "Attitude",
  "org_behavior", "Donation, Task, Org choice", "Behavior"
) %>%
  pmap_dfr(function(dv, dv_name, dv_type) {
    form <- as.formula(str_glue("{dv} ~ prompt_category + {covariates}"))
    fit_lmer_and_tidy(form, s1) %>%
      filter(startsWith(term, "prompt_category")) %>%
      mutate(
        prompt = str_remove(term, "prompt_category") %>% str_to_title(),
        dv = dv, dv_name = dv_name, dv_type = dv_type, study = "study1"
      )
  })

# ---- Manipulation checks (Study 2) ------------------------------------------
# Each strategy vs. others on corresponding mechanism items

s2 <- load_study2()

mechanism_mapping <- tribble(
  ~strategy_pattern,          ~mechanism_items,
  "emotional.activation",     c("mechanism_emotion_strong", "mechanism_emotion_empathy"),
  "implementation.intentions", c("mechanism_implement_visualise", "mechanism_implement_plan"),
  "identity.labeling",        c("mechanism_identity_action", "mechanism_identity_followthrough"),
  "commitment.escalation",    c("mechanism_commitment_increasing", "mechanism_commitment_consistency"),
  "anticipated.regret",       c("mechanism_regret_signing", "mechanism_regret_disappointment"),
  "info.impact.efficacy",     c("mechanism_efficacy_difference", "mechanism_efficacy_impact"),
  "info.issue",               c("mechanism_info_learning", "mechanism_info_knowledge")
)

s2_treatment <- s2 %>% filter(treatment == "treatment")

manipulation_checks <- mechanism_mapping %>%
  pmap_dfr(function(strategy_pattern, mechanism_items) {
    map_dfr(mechanism_items, function(item) {
      if (!item %in% names(s2_treatment)) return(tibble())
      d <- s2_treatment %>%
        mutate(is_focal = as.integer(str_detect(conversation_type, strategy_pattern)))
      form <- as.formula(str_glue("{item} ~ is_focal + {s2_covariates}"))
      tryCatch({
        fit_lmer_and_tidy(form, d) %>%
          filter(term == "is_focal") %>%
          mutate(strategy = strategy_pattern, mechanism_item = item)
      }, error = function(e) tibble())
    })
  })

# Composite (battery-level) manipulation checks: each strategy's targeted
# mechanism battery (mean of its two items) vs. all other treated participants.
manipulation_checks_composite <- mechanism_mapping %>%
  pmap_dfr(function(strategy_pattern, mechanism_items) {
    items <- mechanism_items[mechanism_items %in% names(s2_treatment)]
    if (length(items) == 0) return(tibble())
    d <- s2_treatment %>%
      mutate(.composite = rowMeans(across(all_of(items)), na.rm = TRUE),
             is_focal = as.integer(str_detect(conversation_type, strategy_pattern)))
    focal_mean <- mean(d$.composite[d$is_focal == 1], na.rm = TRUE)
    other_mean <- mean(d$.composite[d$is_focal == 0], na.rm = TRUE)
    form <- as.formula(str_glue("`.composite` ~ is_focal + {s2_covariates}"))
    tryCatch({
      fit_lmer_and_tidy(form, d) %>%
        filter(term == "is_focal") %>%
        mutate(strategy = strategy_pattern,
               focal_mean = focal_mean, other_mean = other_mean,
               p.value = 2 * pnorm(abs(statistic), lower.tail = FALSE))
    }, error = function(e) tibble())
  })

# ---- Study 2: Strategy-level learning and ATEs --------------------------------

s2 <- s2 %>%
  mutate(mechanism_info_composite = (mechanism_info_learning + mechanism_info_knowledge) / 2)

s2_with_strategy <- s2 %>%
  mutate(strategy = ifelse(treatment == "control", "control", conversation_type_clean) %>%
           fct_relevel("control"))

estimate_s2_cond <- function(dv, vs_control = TRUE) {
  if (vs_control) {
    form <- as.formula(str_glue("{dv} ~ strategy + {s2_covariates}"))
    fit_lmer_and_tidy(form, s2_with_strategy) %>% filter(str_starts(term, "strategy"))
  } else {
    s2_treat_only <- s2_with_strategy %>% filter(strategy != "control")
    form <- as.formula(str_glue("{dv} ~ 0 + strategy + {s2_covariates}"))
    fit_lmer_and_tidy(form, s2_treat_only) %>% filter(str_starts(term, "strategy"))
  }
}

s2_learning <- estimate_s2_cond("mechanism_info_composite", vs_control = FALSE)

s2_condition_ates <- list(
  delta_petition = estimate_s2_cond("delta_petition"),
  signed_petition = estimate_s2_cond("1 * signed_petition"),
  delta_organization = estimate_s2_cond("delta_organization"),
  chose_assigned_org = estimate_s2_cond("1 * chose_assigned_org")
)

# ---- Study 2: Info-issue vs. other strategies --------------------------------

s2_with_prompt <- s2_with_strategy %>%
  mutate(prompt_group = case_when(
    treatment == "control" ~ "Control",
    conversation_type_clean == "info_issue" ~ "Info: Issue",
    TRUE ~ "Other strategies"
  ) %>% fct_relevel("Control"))

s2_info_issue <- tribble(~dv, ~dv_name, ~dv_type,
  "delta_petition", "Support petition", "Attitude",
  "1 * signed_petition", "Sign petition", "Behavior",
  "delta_organization", "Support organization", "Attitude",
  "1 * chose_assigned_org", "Donate to assigned org", "Behavior"
) %>%
  pmap_dfr(function(dv, dv_name, dv_type) {
    form <- as.formula(str_glue("{dv} ~ prompt_group + {s2_covariates}"))
    fit_lmer_and_tidy(form, s2_with_prompt) %>%
      filter(startsWith(term, "prompt_group")) %>%
      mutate(
        prompt = str_remove(term, "prompt_group"),
        dv = dv, dv_name = dv_name, dv_type = dv_type, study = "study2"
      )
  })

# ---- Cross-strategy mechanism effects ----------------------------------------
# For each mechanism battery, compute EVERY strategy's effect on that battery.
# This lets us ask: across all strategies, does activating mechanism X predict
# behavioral effectiveness?

mechanism_batteries <- tribble(
  ~battery_name,               ~items,
  "Emotional activation",      c("mechanism_emotion_strong", "mechanism_emotion_empathy"),
  "Implementation intentions", c("mechanism_implement_visualise", "mechanism_implement_plan"),
  "Identity labeling",         c("mechanism_identity_action", "mechanism_identity_followthrough"),
  "Commitment escalation",     c("mechanism_commitment_increasing", "mechanism_commitment_consistency"),
  "Anticipated regret",        c("mechanism_regret_signing", "mechanism_regret_disappointment"),
  "Impact efficacy",           c("mechanism_efficacy_difference", "mechanism_efficacy_impact"),
  "Information learning",      c("mechanism_info_learning", "mechanism_info_knowledge")
)

s2_cross_mechanism <- mechanism_batteries %>%
  pmap_dfr(function(battery_name, items) {
    valid_items <- items[items %in% names(s2_with_strategy)]
    if (length(valid_items) == 0) return(tibble())

    s2_with_strategy$`.composite` <- rowMeans(
      s2_with_strategy[, valid_items, drop = FALSE], na.rm = TRUE
    )
    form <- as.formula(str_glue("`.composite` ~ strategy + {s2_covariates}"))
    tryCatch({
      fit_lmer_and_tidy(form, s2_with_strategy) %>%
        filter(str_starts(term, "strategy")) %>%
        mutate(battery = battery_name)
    }, error = function(e) tibble())
  })

cat("  Cross-strategy mechanism effects:", nrow(s2_cross_mechanism), "rows\n")

# ---- Save all results --------------------------------------------------------

results <- list(
  learning_correlations = s1_learning_cors,
  learning_estimates = s1_learning,
  condition_ates = list(
    delta_petition = estimate_cond("delta_petition"),
    signed_petition = estimate_cond("1 * signed_petition"),
    delta_organization = estimate_cond("delta_organization"),
    org_behavior = estimate_cond("org_behavior")
  ),
  prompt_type_effects = s1_prompt_type,
  manipulation_checks = manipulation_checks,
  manipulation_checks_composite = manipulation_checks_composite,
  s2_learning_estimates = s2_learning,
  s2_condition_ates = s2_condition_ates,
  s2_info_issue_effects = s2_info_issue,
  s2_cross_mechanism = s2_cross_mechanism
)

save_results(results, "mechanisms")

cat("=== Done ===\n")
