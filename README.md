# Artificial Intelligence Can Persuade People to Take Political Actions

```bibtex
@misc{hackenburg2026actionpersuasion,
      title={Artificial intelligence can persuade people to take political actions}, 
      author={Kobi Hackenburg and Luke Hewitt and Caroline Wagner and Christopher Summerfield and Ben M. Tappin},
      year={2026},
      eprint={2604.09200},
      archivePrefix={arXiv},
      primaryClass={cs.CY},
      url={https://arxiv.org/abs/2604.09200}, 
}
```


## Repository Structure

```
analysis/              R scripts (00-14) + figure/table generators
  figures/             main/ (3 figures) and si/ (9 figures, 4 tables)
  run_all.R            Master script: data processing -> analysis -> figures
  utils.R              Shared helpers and data path resolver
data/
  raw/                 Raw CSVs (study1_raw.csv, study2_raw.csv)
  processed/           Processed JSON + combined outcomes CSV
  study_2/fact_checks/ Extracted fact-checkable claims (anonymized IDs, one row per claim)
  codebook.md          Variable definitions
output/
  results/             .rds model summaries
  figures/             Generated figure PDFs (main/ and si/)
  tables/              Generated .tex tables
SI_Appendix.pdf        Supplementary Information
```

Scripts `11`–`14` contain the analyses added in revision: an individual-level test of information provision on attitudes vs. behaviour (`11_info_power.R`; SI Section 2.11), causal mediation via self-reported learning (`12_mediation.R`; SI Section 2.13), transcript analysis of direct asks and factual density (`13_direct_ask.R`; SI Section 2.14), and the latent correlation between condition-level attitude and behaviour effects (`14_latent_effect_correlation.R`).

## How to Run

```r
source("analysis/run_all.R")  # Full pipeline: processing -> analysis -> figures
```

Required packages: `tidyverse`, `lme4`, `broom`, `broom.mixed`, `jsonlite`, `metafor`, `scales`, `patchwork`, `ggtext`, `ggstance`, `ggrepel`, `mediation`, `systemfit`, `Matrix`

## Data

Participant IDs are anonymized and conversation transcripts are redacted. All other variables (demographics, outcomes, mechanisms, etc.) are unchanged. See `data/codebook.md` for full variable definitions.

The transcript-classification outputs used by `13_direct_ask.R` (direct-ask columns in `study2_raw.csv` and the fact-check claim file in `data/study_2/fact_checks/`) are provided as data. The classification pipeline itself (LLM judge: Claude Sonnet 4.6, temperature 0; see SI Appendix, Section 2.14) is not included, as it requires unredacted transcripts and API credentials to run.
