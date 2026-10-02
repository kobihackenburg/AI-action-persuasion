# Data Codebook

## Overview

| Dataset | File | N | Description |
|---------|------|---|-------------|
| Study 1 raw | `raw/study1_raw.csv` | 8,412 | Firebase export (62 columns): demographics, AI attitudes, conversations, outcomes |
| Study 2 raw | `raw/study2_raw.csv` | 10,978 | Full Study 2 data (80 columns, incl. direct-ask classification) |
| Study 1 processed | `processed/study1.json` | 8,000 | Unified JSON schema with parsed conversations (completed + attention-check filter) |
| Study 2 processed | `processed/study2.json` | 9,950 | Unified JSON schema with parsed conversations (completed + attention-check filter) |
| Combined outcomes | `processed/combined_outcomes.csv` | 17,950 | Harmonized outcome data from both studies |
| Study 2 fact-check claims | `study_2/fact_checks/study2_extract_fact_checks.csv` | 177,753 | One row per fact-checkable claim extracted from Study 2 conversations (anonymized `chat_session_id` only) |

**Data redaction:** Participant IDs have been anonymized (sequential identifiers `P00001`, `P00002`, ...) and conversation transcript text has been replaced with `[REDACTED]`. All other variables (demographics, outcomes, mechanisms, ratings, etc.) are unchanged. To run analyses with unredacted data, place the original CSVs in `raw/full/` -- the pipeline detects and uses them automatically.

---

## Unified JSON Schema (`processed/study1.json`, `processed/study2.json`)

Both processed files share an identical per-participant structure. Fields that don't apply to a given study are set to `null`. This design allows direct schema merging across studies.

### Top-level metadata

| Field | Type | Values | Study-specific? |
|-------|------|--------|-----------------|
| `study` | string | `"study1"`, `"study2"` | -- |
| `participant_id` | string | Anonymized ID (`P00001`, ...) | -- |
| `model_type` | string | see table below | -- |
| `conversation_type` | string | see table below | -- |
| `petition_title` | string | 8 petition titles per study | -- |
| `petition_organization` | string | Organization sponsoring the petition | -- |
| `petition_salience` | string \| null | `"High"`, `"Low"` | S2: all petitions labeled. S1: 6 overlapping issues labeled retrospectively from S2 prereg; 2 S1-only issues ("nuclear war", "banks/climate") = null |
| `randomized_bonus_amount` | number \| null | 0.5, 1, 2, 4 | S1 only (S2 = null) |

### `demographics`

| Field | Type | Notes |
|-------|------|-------|
| `age` | integer \| null | 18-120; 3 S1 outliers set to null |
| `gender` | string | e.g. "Male", "Female", "Non-binary" |
| `education` | string | e.g. "Undergraduate", "Postgraduate" |
| `income` | string | Income bracket |
| `ideology` | string | Political ideology |
| `political_knowledge` | string | Political knowledge level/score |

### `ai_attitudes`

| Field | Type | Notes |
|-------|------|-------|
| `usage_frequent` | string | 4-point agreement (Strongly disagree to Strongly agree) |
| `trust_reliable_pre` | string | Pre-conversation AI trust (4-point agreement) |
| `trust_reliable_post` | string \| null | Post-conversation AI trust; S2 only (S1 = null) |

### `quality`

| Field | Type | Notes |
|-------|------|-------|
| `attention_check` | string | 4-point agreement scale |
| `conversation_timed_out` | boolean \| null | S2 only (S1 = null) |
| `conversation_error` | boolean \| null | S2 only (S1 = null) |

### `pre_treatment`

| Field | Type | Range | Notes |
|-------|------|-------|-------|
| `agreement` | integer | 1-7 | Petition support (Likert) |
| `organization_opinion` | integer | 1-7 | Organization opinion (Likert) |
| `charity_interest` | string \| null | 4-point agreement text | S1 only (S2 = null) |

### `post_treatment`

| Field | Type | Range |
|-------|------|-------|
| `agreement` | integer | 1-7 |
| `organization_opinion` | integer | 1-7 |

### `outcomes`

| Field | Type | Notes |
|-------|------|-------|
| `signed_petition` | boolean | **Never null.** NA in raw data converted to FALSE |
| `wants_to_sign_petition` | boolean | **Never null.** NA in raw data converted to FALSE |
| `donation_amount` | number \| null | GBP donated from bonus; S1 only (S2 = null) |
| `extra_task_clicks` | integer \| null | 10-110 range (minimum 10 is mandatory baseline); S1 only (S2 = null) |
| `org_choice` | string \| null | `"givewell"`, `"organization"`, or `"none"`; S2 only (S1 = null) |
| `final_organization` | string | Name of chosen org or "GiveWell" |

**Note on behavioral outcomes:** Study 1 measured donation behavior (randomized bonus + amount donated + effort task clicks + org switch). Study 2 measured a simpler binary charity choice (25p to assigned org vs. GiveWell). These are coded as separate fields (`donation_amount`/`extra_task_clicks` vs. `org_choice`) because they are not comparable across studies.

### `mechanisms`

All 14 mechanism items are 0-100 slider scales. **All null for Study 1** (not measured). Measured for all Study 2 participants.

| Field | Domain | Item wording |
|-------|--------|-------------|
| `emotion_strong` | Emotional Activation | "The conversation made me feel strong emotions" |
| `emotion_empathy` | Emotional Activation | "I felt empathy for those affected" |
| `implement_visualise` | Implementation Intentions | "I can visualise myself signing the petition" |
| `implement_plan` | Implementation Intentions | "I have a clear plan for signing" |
| `identity_action` | Identity Labeling | "Signing aligns with who I am" |
| `identity_followthrough` | Identity Labeling | "Someone like me would follow through" |
| `commitment_increasing` | Commitment Escalation | "I felt increasing commitment during the conversation" |
| `commitment_consistency` | Commitment Escalation | "I want to be consistent with what I said" |
| `regret_signing` | Anticipated Regret | "I would regret not signing" |
| `regret_disappointment` | Anticipated Regret | "I would be disappointed in myself if I didn't act" |
| `efficacy_difference` | Info -- Impact Efficacy | "Signing the petition can make a real difference" |
| `efficacy_impact` | Info -- Impact Efficacy | "My signature would have an impact" |
| `info_learning` | Info -- Issue | "I learned new information about the issue" |
| `info_knowledge` | Info -- Issue | "The conversation increased my knowledge" |

**Note:** Five legacy mechanism columns from Study 2 raw data (`legacy_mechanism_emotion_compassion`, etc.) are 100% NA (vestigial from a piloting wording change) and are **excluded** from the processed JSON.

### `ratings`

All 0-100 continuous scales.

| Field | Description | Notes |
|-------|-------------|-------|
| `learned` | "I learned a lot from the conversation" | S1 only (S2 = null) |
| `enjoyable` | "The conversation was enjoyable" | Both |
| `understood` | "I felt understood by the AI" | Both |
| `biased` | "The AI was biased" | Both |
| `increased_petition` | "The conversation increased my support for the petition" | Both |
| `increased_donation` | "The conversation increased my willingness to donate" | S1 only (S2 = null) |

### `conversation`

| Field | Type | Description |
|-------|------|-------------|
| `length` | integer | Total number of individual messages (user + assistant) |
| `turns[]` | array | Ordered list of message objects |
| `turns[].turn` | integer | 1-indexed message number |
| `turns[].role` | string | `"assistant"` or `"user"` |
| `turns[].text` | string | Message content (`[REDACTED]` in public release) |

Study 1 raw conversations are stored as JSON arrays (`[{"role": ..., "content": ...}]`); Study 2 raw conversations are pipe-delimited text (`1. ASSISTANT: ... | 2. USER: ...`). Both are parsed into the unified `{turn, role, text}` format during processing. In the public release, all message text is replaced with `[REDACTED]`; the turn structure (count, roles, ordering) is preserved.

---

## Study 1 Raw Variables (`study1_raw.csv`)

The raw file is the full Firebase export with 62 columns, including:

- `participantId` -- Anonymized participant ID (originally Prolific ID)
- `age`, `gender`, `education`, `income`, `ideology`, `political_knowledge` -- demographics
- `ai_usage_frequent`, `ai_trust_reliable` -- AI attitude items (4-point agreement)
- `ai_impact_positive` -- all NA (not collected in S1)
- `attention_check` -- 4-point agreement
- `turnstile_verification_failed` -- bot verification flag (TRUE/NA)
- `model_type`, `conversation_type`, `petition_title`
- `randomized_bonus_amount` -- 0.5, 1, 2, or 4 GBP
- `pretreatment_agreement`, `pretreatment_organization_opinion` -- 1-7 Likert
- `posttreatment_agreement`, `posttreatment_organization_opinion` -- 1-7 Likert
- `signed_petition`, `wants_to_sign_petition` -- logical (NA = did not sign)
- `donation_amount` -- GBP
- `task_clicks` -- 10-110 (minimum 10 is mandatory; this is `extra_task_clicks` in processed JSON)
- `final_organization` -- org name or "GiveWell"
- `charity_interest` -- 4-point agreement text
- `conversation_learned`, `conversation_enjoyable`, `conversation_understood`, `conversation_biased` -- 0-100
- `conversation_increased_petition`, `conversation_increased_donation` -- 0-100
- `full_conversation` -- JSON array (`content` fields redacted in public release)
- `conversation_length` -- total message count
- `generated_system_prompt` -- system prompt (for "random" condition)


## Study 2 Raw Variables (`study2_raw.csv`)

See full variable listing in the Study 2 preanalysis plan. Key columns beyond the unified schema above:

- `session_id`, `start_time`, `last_updated`, `completed`, `completion_time`
- `final_step`, `final_step_name` -- survey progress tracking
- `full_conversation_text` -- pipe-delimited transcript (text redacted in public release)
- `conversation_timed_out`, `conversation_error`, `conversation_error_details`, `conversation_error_model`
- `opened_petition_popup_early`, `popup_1`, `popup_2` -- petition popup interactions
- `time_on_step_0` through `time_on_step_10` -- timing data

### Direct-ask classification columns (Study 2 raw)

Each Study 2 conversation was classified by an LLM judge (Claude Sonnet 4.6, temperature 0; see SI Appendix, Section 2.14) for whether the AI assistant made a direct, explicit request that the participant sign the petition. Used by `analysis/13_direct_ask.R`.

| Column | Type | Description |
|--------|------|-------------|
| `direct_ask` | logical | TRUE if the assistant made a direct ask anywhere in the conversation |
| `n_ask_messages` | integer | Number of assistant messages containing a direct ask |
| `first_ask_message` | integer \| NA | 1-indexed assistant-message number of the first direct ask |
| `ask_quote` | string | Verbatim assistant text supporting the classification (`[REDACTED]` in public release; not used by any analysis script) |

---

## Study 2 Fact-Check Claims (`study_2/fact_checks/study2_extract_fact_checks.csv`)

Fact-checkable claims were extracted from every assistant message in every Study 2 conversation by an LLM classifier (Claude Sonnet 4.6, temperature 0; see SI Appendix, Section 2.14). The public file contains one row per extracted claim with a single column:

| Column | Type | Description |
|--------|------|-------------|
| `chat_session_id` | string | Anonymized participant/conversation ID (`P00001`, ...), matching `participant_id` in the Study 2 data |

The analysis (`analysis/13_direct_ask.R`) uses only per-conversation claim counts (rows per `chat_session_id`), so the claim text, source message, and other classifier fields are omitted from the public release. Conversations with no extracted claims do not appear in the file (the file covers 9,400 unique conversations); the missing-conversations sensitivity check in `13_direct_ask.R` accounts for analysed conversations that are absent.

---

## Derived Variables (computed in `00_process_data.R`)

| Variable | Formula | Studies |
|----------|---------|---------|
| `delta_petition` | (post - pre agreement) / SD(pre agreement) | Both |
| `delta_organization` | (post - pre org opinion) / SD(pre org opinion) | Both |
| `donate_proportion` | donation_amount / randomized_bonus_amount | Study 1 only |
| `not_switch_organization` | final_organization != "GiveWell" (S1) or org_choice == "organization" (S2) | Both |
| `org_behavior` | mean(donate_proportion, extra_task_clicks/100, not_switch_organization) | Study 1 only |
| `treatment` | "control" if control-*, else "treatment" | Both |
| `treatment_type` | conversation_type (cleaned) | Both |
| `prompt_category` | "control", "random", or "info" | Study 1 only |

---

## Model Types by Study

| Study 1 | Study 2 |
|---------|---------|
| `anthropic/claude-sonnet-4` | `anthropic/claude-opus-4.6` |
| `openai/chatgpt-4o-latest` | `openai/gpt-4.1` |
| `openai/gpt-4.1` | `google/gemini-3.1-pro-preview` |
| `google/gemini-2.5-pro` | `x-ai/grok-4` |
| `x-ai/grok-4` | |

## Conversation Types by Study

| Study 1 | Study 2 |
|---------|---------|
| `control-antibiotics` | `control-antibiotics` |
| `control-fluoridation` | `control-fluoridation` |
| `control-recycling` | `control-recycling` |
| `control-sweetners` | `control-sweeteners` |
| `petition` | `emotional-activation` |
| `organization` | `implementation-intentions` |
| `random` | `identity-labeling` |
| | `commitment-escalation` |
| | `anticipated-regret` |
| | `info-impact-efficacy` |
| | `info-issue` |
| | `mega` |

## Overlapping Petition Issues (6 of 8)

Both studies: child poverty (High), pig suffering (Low), facial recognition (Low), asylum/work ban (High), protect trees (Low), proportional representation (Low).

Study 1 only: nuclear war (no salience label), banks/climate crisis (no salience label).
Study 2 only: continuing care (High), economic inequality (High).

Salience designations come from the Study 2 pre-analysis plan (Section 3.2), based on Gallup's Most Important Problem measure and UK polling data. The 4 low-salience issues were carried over from Study 1; their salience labels are applied retrospectively to Study 1 participants assigned to those issues.
