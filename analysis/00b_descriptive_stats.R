source("analysis/utils.R")

cat("=== 00b: Descriptive Statistics ===\n")

s1 <- load_study1()
s2 <- load_study2()

# ---- 1. Sample sizes ---------------------------------------------------------

s1_n <- nrow(s1)
s2_n <- nrow(s2)
total_n <- s1_n + s2_n

s1_ids <- unique(s1$participantId)
s2_ids <- unique(s2$participant_id)
overlap_ids <- intersect(s1_ids, s2_ids)

unique_n <- length(union(s1_ids, s2_ids))
overlap_n <- length(overlap_ids)

cat(sprintf("  Study 1 final N:       %s\n", format(s1_n, big.mark = ",")))
cat(sprintf("  Study 2 final N:       %s\n", format(s2_n, big.mark = ",")))
cat(sprintf("  Total observations:    %s\n", format(total_n, big.mark = ",")))
cat(sprintf("  Unique individuals:    %s\n", format(unique_n, big.mark = ",")))
cat(sprintf("  Overlap (both studies): %d\n", overlap_n))

# ---- 2. Conversation characteristics (treatment only) ------------------------
# Step 3 is the conversation step in both studies (verified by duration).
# time_on_step_3 is recorded in seconds.

s1_treat <- s1 %>% filter(treatment == "treatment")
s2_treat <- s2 %>% filter(treatment == "treatment")

# conversation_length counts total messages; a "turn" is one user + AI pair
s1_mean_turns    <- mean(s1_treat$conversation_length, na.rm = TRUE) / 2
s2_mean_turns    <- mean(s2_treat$conversation_length, na.rm = TRUE) / 2
overall_turns    <- mean(c(s1_treat$conversation_length, s2_treat$conversation_length), na.rm = TRUE) / 2

s1_mean_dur_min  <- mean(s1_treat$time_on_step_3, na.rm = TRUE) / 60
s2_mean_dur_min  <- mean(s2_treat$time_on_step_3, na.rm = TRUE) / 60
overall_dur_min  <- mean(c(s1_treat$time_on_step_3, s2_treat$time_on_step_3), na.rm = TRUE) / 60

cat(sprintf("\n  Conversation turns  — S1: %.1f, S2: %.1f, Overall: %.1f\n",
            s1_mean_turns, s2_mean_turns, overall_turns))
cat(sprintf("  Conversation mins   — S1: %.1f, S2: %.1f, Overall: %.1f\n",
            s1_mean_dur_min, s2_mean_dur_min, overall_dur_min))

# ---- 3. Post-treatment attrition rates ---------------------------------------
# Raw CSVs are loaded here because we need non-completers (Study 2) and the
# full unfiltered sample (Study 1). The study2_clean.rds excludes dropouts.

s1_raw_attr <- read_csv(resolve_raw_path("data/raw/study1_raw.csv"), show_col_types = FALSE)
s2_raw_attr <- read_csv(resolve_raw_path("data/raw/study2_raw.csv"), show_col_types = FALSE)

# Study 1: all randomised participants (conversation_type set at session
# creation); attrited = !completed.
s1_n_raw      <- nrow(s1_raw_attr)
s1_n_attrited <- sum(!s1_raw_attr$completed)
s1_attr_rate  <- s1_n_attrited / s1_n_raw

# Study 2: denominator = final_step >= 3 (post-randomisation,
# post-pre-treatment survey). Attention-check exclusion is NOT applied here.
s2_reached    <- s2_raw_attr %>% filter(final_step >= 3)
s2_n_reached  <- nrow(s2_reached)
s2_n_attrited <- sum(!s2_reached$completed)
s2_attr_rate  <- s2_n_attrited / s2_n_reached

cat(sprintf("\n  Post-treatment attrition:\n"))
    cat(sprintf("    Study 1: %.1f%% (%d/%d) — all randomised, !completed\n",
                s1_attr_rate * 100, s1_n_attrited, s1_n_raw))
cat(sprintf("    Study 2: %.1f%% (%d/%d reached final_step >= 3)\n",
            s2_attr_rate * 100, s2_n_attrited, s2_n_reached))

# ---- Save --------------------------------------------------------------------

desc_stats <- list(
  sample = list(
    s1_final_n       = s1_n,
    s2_final_n       = s2_n,
    total_observations = total_n,
    unique_individuals = unique_n,
    overlap_n        = overlap_n
  ),
  conversations = list(
    s1_mean_turns      = s1_mean_turns,
    s2_mean_turns      = s2_mean_turns,
    overall_mean_turns = overall_turns,
    s1_mean_duration_min  = s1_mean_dur_min,
    s2_mean_duration_min  = s2_mean_dur_min,
    overall_mean_duration_min = overall_dur_min
  ),
  attrition = list(
    s1 = list(
      note      = "All randomised participants; attrited = !completed",
      n_raw     = s1_n_raw,
      n_attrited = s1_n_attrited,
      rate      = s1_attr_rate
    ),
    s2 = list(
      n_reached  = s2_n_reached,
      n_attrited = s2_n_attrited,
      rate       = s2_attr_rate
    )
  )
)

save_results(desc_stats, "descriptive_stats")
cat("=== Done ===\n")
