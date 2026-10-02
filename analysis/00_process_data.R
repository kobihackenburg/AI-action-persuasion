library(tidyverse)
library(jsonlite)
source("analysis/utils.R")

cat("=== 00_process_data.R: Processing raw data ===\n")

# ---- Shared helpers ---------------------------------------------------------

s1_org_patterns <- list(
  list(pattern = "End suffering for mother pigs",                org = "Compassion in World Farming"),
  list(pattern = "Lift the ban on work for people seeking asylum", org = "Refugee Action"),
  list(pattern = "Oppose Facial Recognition",                    org = "Liberty"),
  list(pattern = "Seats in parliament should match how we vote", org = "Electoral Reform Society"),
  list(pattern = "Stop Rising Child Poverty",                    org = "Save the Children"),
  list(pattern = "Stop banks fuelling the climate crisis",       org = "ActionAid"),
  list(pattern = "Stop the risk of nuclear war",                 org = "Campaign for Nuclear Disarmament"),
  list(pattern = "Urge UK governments to protect trees",         org = "Woodland Trust")
)

map_s1_org <- function(title) {
  for (entry in s1_org_patterns) {
    if (grepl(entry$pattern, title, fixed = TRUE)) return(entry$org)
  }
  NA_character_
}

# Salience designations from Study 2 prereg (Section 3.2). The 4 low-salience
# issues were carried over from Study 1; 2 S1-only issues have no designation.
salience_patterns <- list(
  list(pattern = "Stop Rising Child Poverty",                    salience = "High"),
  list(pattern = "Lift the ban on work for people seeking asylum", salience = "High"),
  list(pattern = "End suffering for mother pigs",                salience = "Low"),
  list(pattern = "Oppose Facial Recognition",                    salience = "Low"),
  list(pattern = "Seats in parliament should match how we vote", salience = "Low"),
  list(pattern = "Urge UK governments to protect trees",         salience = "Low")
)

map_salience <- function(title) {
  for (entry in salience_patterns) {
    if (grepl(entry$pattern, title, fixed = TRUE)) return(entry$salience)
  }
  NULL
}

null_mechanisms <- function() {
  list(
    emotion_strong = NULL, emotion_empathy = NULL,
    implement_visualise = NULL, implement_plan = NULL,
    identity_action = NULL, identity_followthrough = NULL,
    commitment_increasing = NULL, commitment_consistency = NULL,
    regret_signing = NULL, regret_disappointment = NULL,
    efficacy_difference = NULL, efficacy_impact = NULL,
    info_learning = NULL, info_knowledge = NULL
  )
}

parse_s2_conversation <- function(text) {
  if (is.na(text) || text == "") return(NULL)
  turns <- strsplit(text, " \\| ")[[1]]
  parsed <- lapply(turns, function(turn) {
    m <- stringr::str_match(turn, "^(\\d+)\\.\\s*(ASSISTANT|USER):\\s*(.*)")
    if (is.na(m[1])) return(NULL)
    list(turn = as.integer(m[2]), role = tolower(m[3]), text = trimws(m[4]))
  })
  Filter(Negate(is.null), parsed)
}

safe_bool <- function(x) if (is.null(x) || is.na(x)) FALSE else as.logical(x)
safe_age  <- function(x) if (is.null(x) || is.na(x) || x > 120 || x < 13) NULL else as.integer(x)

# ---- Study 1 ----------------------------------------------------------------

cat("Loading Study 1 raw data...\n")
s1_raw_all <- read_csv(resolve_raw_path("data/raw/study1_raw.csv"), show_col_types = FALSE)
cat(sprintf("  Study 1 (all): %d rows, %d columns\n", nrow(s1_raw_all), ncol(s1_raw_all)))

s1_raw <- s1_raw_all %>%
  filter(completed == TRUE) %>%
  filter(attention_check %in% c("Disagree", "Strongly disagree"))
cat(sprintf("  Study 1 (analysis sample): %d rows\n", nrow(s1_raw)))

s1 <- s1_raw %>%
  mutate(
    signed_petition = replace_na(signed_petition, FALSE),
    wants_to_sign_petition = replace_na(wants_to_sign_petition, FALSE),
    extra_task_clicks = replace_na(task_clicks, 0),
    charity_interest_num = case_match(
      charity_interest,
      "Strongly disagree" ~ 1L, "Disagree" ~ 2L,
      "Agree" ~ 3L, "Strongly agree" ~ 4L
    ),
    donate_proportion = donation_amount / randomized_bonus_amount,
    delta_petition = (posttreatment_agreement - pretreatment_agreement) / sd(pretreatment_agreement),
    delta_organization = (posttreatment_organization_opinion - pretreatment_organization_opinion) / sd(pretreatment_organization_opinion),
    not_switch_organization = final_organization != "GiveWell",
    org_behavior = (donate_proportion + extra_task_clicks / 100 + not_switch_organization) / 3,
    treatment = ifelse(str_detect(conversation_type, "control"), "control", "treatment"),
    treatment_type = ifelse(str_detect(conversation_type, "control"), "control", conversation_type),
    prompt_category = case_when(
      str_detect(conversation_type, "control") ~ "control",
      conversation_type == "random" ~ "random",
      TRUE ~ "info"
    ),
    study = "study1"
  )

cat("  Building Study 1 JSON...\n")
s1_json <- list(
  study = "study1",
  n_participants = nrow(s1_raw),
  participants = s1_raw %>% pmap(function(...) {
    row <- list(...)

    conv_parsed <- tryCatch({
      p <- fromJSON(row$full_conversation)
      lapply(seq_len(nrow(p)), function(i) {
        list(turn = i, role = p$role[i], text = p$content[i])
      })
    }, error = function(e) NULL)

    list(
      study              = "study1",
      participant_id     = row$participantId,
      model_type         = row$model_type,
      conversation_type  = row$conversation_type,
      petition_title     = row$petition_title,
      petition_organization = map_s1_org(row$petition_title),
      petition_salience  = map_salience(row$petition_title),
      randomized_bonus_amount = row$randomized_bonus_amount,
      demographics = list(
        age                = safe_age(row$age),
        gender             = row$gender,
        education          = row$education,
        income             = row$income,
        ideology           = row$ideology,
        political_knowledge = row$political_knowledge
      ),
      ai_attitudes = list(
        usage_frequent     = row$ai_usage_frequent,
        trust_reliable_pre = row$ai_trust_reliable,
        trust_reliable_post = NULL
      ),
      quality = list(
        attention_check      = row$attention_check,
        conversation_timed_out = NULL,
        conversation_error     = NULL
      ),
      pre_treatment = list(
        agreement            = row$pretreatment_agreement,
        organization_opinion = row$pretreatment_organization_opinion,
        charity_interest     = row$charity_interest
      ),
      post_treatment = list(
        agreement            = row$posttreatment_agreement,
        organization_opinion = row$posttreatment_organization_opinion
      ),
      outcomes = list(
        signed_petition        = safe_bool(row$signed_petition),
        wants_to_sign_petition = safe_bool(row$wants_to_sign_petition),
        donation_amount        = row$donation_amount,
        extra_task_clicks      = if (!is.na(row$task_clicks)) as.integer(row$task_clicks) else NULL,
        org_choice             = NULL,
        final_organization     = row$final_organization
      ),
      mechanisms = null_mechanisms(),
      ratings = list(
        learned            = row$conversation_learned,
        enjoyable          = row$conversation_enjoyable,
        understood         = row$conversation_understood,
        biased             = row$conversation_biased,
        increased_petition = row$conversation_increased_petition,
        increased_donation = row$conversation_increased_donation
      ),
      conversation = if (!is.null(conv_parsed)) {
        list(length = length(conv_parsed), turns = conv_parsed)
      } else {
        NULL
      }
    )
  })
)

write_json(s1_json, "data/processed/study1.json", pretty = TRUE, auto_unbox = TRUE, null = "null")
cat("  Saved data/processed/study1.json\n")

# ---- Study 2 ----------------------------------------------------------------

cat("Loading Study 2 raw data...\n")
s2_raw <- read_csv(resolve_raw_path("data/raw/study2_raw.csv"), show_col_types = FALSE)

s2 <- s2_raw %>%
  filter(completed == TRUE) %>%
  filter(attention_check %in% c("Disagree", "Strongly disagree")) %>%
  mutate(
    delta_petition = (posttreatment_agreement - pretreatment_agreement) / sd(pretreatment_agreement, na.rm = TRUE),
    delta_organization = (posttreatment_organization_opinion - pretreatment_organization_opinion) / sd(pretreatment_organization_opinion, na.rm = TRUE),
    signed_petition = replace_na(as.logical(signed_petition), FALSE),
    wants_to_sign_petition = replace_na(as.logical(wants_to_sign_petition), FALSE),
    not_switch_organization = org_choice == "organization",
    chose_assigned_org = (org_choice == "organization"),
    treatment = ifelse(str_detect(conversation_type, "control"), "control", "treatment"),
    conversation_type_clean = str_replace_all(conversation_type, "-", "_"),
    study = "study2"
  )

cat(sprintf("  Study 2: %d participants (completed), %d columns\n", nrow(s2), ncol(s2)))

cat("  Building Study 2 JSON...\n")
s2_json <- list(
  study = "study2",
  n_participants = nrow(s2),
  participants = s2 %>% pmap(function(...) {
    row <- list(...)

    conv <- if (!is.na(row$full_conversation_text) && row$full_conversation_text != "") {
      turns <- parse_s2_conversation(row$full_conversation_text)
      list(length = length(turns), turns = turns)
    } else {
      NULL
    }

    list(
      study              = "study2",
      participant_id     = row$participant_id,
      model_type         = row$model_type,
      conversation_type  = row$conversation_type,
      petition_title     = row$petition_title,
      petition_organization = row$petition_organization,
      petition_salience  = row$petition_salience,
      randomized_bonus_amount = NULL,
      demographics = list(
        age                = safe_age(row$age),
        gender             = row$gender,
        education          = row$education,
        income             = row$income,
        ideology           = row$ideology,
        political_knowledge = row$political_knowledge
      ),
      ai_attitudes = list(
        usage_frequent      = row$ai_usage_frequent,
        trust_reliable_pre  = row$ai_trust_reliable,
        trust_reliable_post = row$ai_trust_post
      ),
      quality = list(
        attention_check      = row$attention_check,
        conversation_timed_out = row$conversation_timed_out,
        conversation_error     = row$conversation_error
      ),
      pre_treatment = list(
        agreement            = row$pretreatment_agreement,
        organization_opinion = row$pretreatment_organization_opinion,
        charity_interest     = NULL
      ),
      post_treatment = list(
        agreement            = row$posttreatment_agreement,
        organization_opinion = row$posttreatment_organization_opinion
      ),
      outcomes = list(
        signed_petition        = safe_bool(row$signed_petition),
        wants_to_sign_petition = safe_bool(row$wants_to_sign_petition),
        donation_amount        = NULL,
        extra_task_clicks      = NULL,
        org_choice             = row$org_choice,
        final_organization     = row$final_organization
      ),
      mechanisms = list(
        emotion_strong         = row$mechanism_emotion_strong,
        emotion_empathy        = row$mechanism_emotion_empathy,
        implement_visualise    = row$mechanism_implement_visualise,
        implement_plan         = row$mechanism_implement_plan,
        identity_action        = row$mechanism_identity_action,
        identity_followthrough = row$mechanism_identity_followthrough,
        commitment_increasing  = row$mechanism_commitment_increasing,
        commitment_consistency = row$mechanism_commitment_consistency,
        regret_signing         = row$mechanism_regret_signing,
        regret_disappointment  = row$mechanism_regret_disappointment,
        efficacy_difference    = row$mechanism_efficacy_difference,
        efficacy_impact        = row$mechanism_efficacy_impact,
        info_learning          = row$mechanism_info_learning,
        info_knowledge         = row$mechanism_info_knowledge
      ),
      ratings = list(
        learned            = NULL,
        enjoyable          = row$conversation_enjoyable,
        understood         = row$conversation_understood,
        biased             = row$conversation_biased,
        increased_petition = row$conversation_increased_petition,
        increased_donation = NULL
      ),
      conversation = conv
    )
  })
)

write_json(s2_json, "data/processed/study2.json", pretty = TRUE, auto_unbox = TRUE, null = "null")
cat("  Saved data/processed/study2.json\n")

# ---- Combined outcomes CSV ---------------------------------------------------

cat("Building combined outcomes CSV...\n")

s1_combined <- s1 %>%
  transmute(
    study, model_type, conversation_type, petition_title,
    pretreatment_agreement, posttreatment_agreement,
    pretreatment_organization_opinion, posttreatment_organization_opinion,
    signed_petition, wants_to_sign_petition, final_organization,
    delta_petition, delta_organization, treatment, treatment_type
  )

s2_combined <- s2 %>%
  transmute(
    study, model_type, conversation_type, petition_title,
    pretreatment_agreement, posttreatment_agreement,
    pretreatment_organization_opinion, posttreatment_organization_opinion,
    signed_petition, wants_to_sign_petition, final_organization,
    delta_petition, delta_organization, treatment,
    treatment_type = conversation_type_clean
  )

combined <- bind_rows(s1_combined, s2_combined)
write_csv(combined, "data/processed/combined_outcomes.csv")
cat(sprintf("  Saved data/processed/combined_outcomes.csv (%d rows)\n", nrow(combined)))

# ---- Save analysis-ready R data frames ---------------------------------------

saveRDS(s1, "output/results/study1_clean.rds")
saveRDS(s2, "output/results/study2_clean.rds")
saveRDS(combined, "output/results/combined_clean.rds")
cat("  Saved analysis-ready .rds files\n")

cat("=== Done ===\n")
