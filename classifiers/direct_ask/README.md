# Direct-ask classifier (Study 2)

This is the classifier used to code whether the AI assistant made a **direct ask** to sign the petition in each Study 2 conversation (SI Appendix, Section 2.14). Its outputs are the `direct_ask`, `n_ask_messages`, `first_ask_message` and `ask_quote` columns in `data/raw/study2_raw.csv`, which are analysed in `analysis/13_direct_ask.R`.

The prompts in `config.py`, the transcript rendering in `classify.py`, and the response parsing and normalisation in `api.py` are identical to those used for the published analysis.

## What is coded

A direct ask is an assistant message that explicitly invites, requests, urges, or recommends that the participant **sign the petition**. Signing the petition is the only target action.

- **Counts:** explicit requests or invitations to sign ("Will you sign the petition?"), explicit recommendations to sign ("I'd recommend adding your name"), and urging to sign conditional on agreement ("If you agree, the best next step is to sign").
- **Does not count:** describing the petition, organisation, or issue; asking about the participant's opinions or reasoning; inviting any other action (donating, sharing, following the organisation, contacting a representative); anything in the participant's messages.
- Each assistant message is counted at most once, however many asks it contains.

The full wording is `SYSTEM_PROMPT` and `CLASSIFY_PROMPT` in [`config.py`](config.py).

## Procedure

- **Model:** Claude Sonnet 4.6 (`claude-sonnet-4-6`), Anthropic Messages API, temperature 0, max 1,024 output tokens.
- **One call per conversation.** The transcript is rendered with assistant messages labelled `[Assistant #k]` (k counts assistant messages only) and participant messages labelled `[User]`, then inserted into `CLASSIFY_PROMPT`. The assistant turn is prefilled with `{` to force a JSON response.
- **Parsing:** the first JSON object in the response is taken.
- **Consistency rules** (`normalize_result` in [`api.py`](api.py)):
  - If `direct_ask` is false, `n_ask_messages` is set to 0 and `first_ask_message` and `ask_quote` to null.
  - If `direct_ask` is true, `n_ask_messages` is at least 1.
  - If `direct_ask` is missing, it is inferred from `n_ask_messages`.
- Conversations with no assistant messages are coded as no ask without an API call. Rows with no conversation text are left blank.

All 9,950 analysed conversations were classified without error. Direct asks appeared in 85.3% of treated conversations (N = 7,994) and 0.1% of control conversations (N = 1,956), which discussed unrelated topics.

## Output columns

| Column | Meaning |
|--------|---------|
| `direct_ask` | `True` if any assistant message is a direct ask |
| `n_ask_messages` | Number of assistant messages containing a direct ask |
| `first_ask_message` | 1-based index, over assistant messages only, of the first message with a direct ask; blank if none |
| `ask_quote` | Shortest verbatim quote of the first direct ask; blank if none (`[REDACTED]` in the public data) |

## Running

The classifier needs the unredacted transcripts, which are not public (see `data/codebook.md`). It reads `data/raw/full/study2_raw.csv` by default. Run on the public, redacted CSV, it would classify `[REDACTED]` placeholders.

```bash
pip install -r classifiers/direct_ask/requirements.txt
export ANTHROPIC_API_KEY=...
python3 classifiers/direct_ask/classify.py --limit 20   # quick test
python3 classifiers/direct_ask/classify.py              # full run
```

Results are cached in `cache/` so that interrupted runs resume where they left off, and are written to `output/study2_direct_ask.csv` with one row per input row.
