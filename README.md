# LIPS

This repository contains all the code developed for the Master's thesis [ExploringPerplexity_RUG.pdf](./ExploringPerplexity_RUG.pdf), also available at the [Thesis Repository of the University of Groningen](https://arts.studenttheses.ub.rug.nl/38278/).

## Scope

All scripts and notebooks in this repository were written in support of the analyses, models, and results presented in the thesis, with some exceptions:

- `panss_correlation_items.R` is **not** part of the thesis work. This script is being developed for a separate, forthcoming paper that explores the item-level PANSS correlation analysis in greater depth than the thesis does.

- `panss_outputs/PANSS_item_*.png` are outputs from `panss_correlation_items.R` and thus are also **not** part of the thesis work.

If you are looking to reproduce or review the thesis results specifically, you can disregard these files.

## Repository Contents

| File / Folder | Description |
|---|---|
| `ExploringPerplexity_RUG.pdf` | Full thesis document. |
| `preprocessing` | Python scripts used to preprocess texts before calculating perplexity.  |
| `perplexity_scripts` | Python scripts used to calculate perplexity scores across different models and context sizes. |
| `requirements.R` | Requirements to run the R scripts. |
| `results_analysis.R` | Analysis of perplexity scores before their use in the study. |
| `exploration_outputs` | Outputs of `results_analysis.R.R`. |
| `data_analysis.R` | Analysis of participants' demographic and clinical data. |
| `demographics_outputs` | Outputs of `data_analysis.R`. |
| `mixedeffects.R` | Linear mixed effects models to test whether perplexity depends on diagnostic group and context size. |
| `models_outputs` | Outputs of `mixedeffects.R`. |
| `panss_correlation.R` | Analysis of correlations between PANSS scores and perplexity scores. |
| `panss_correlation_items.R` | **Not part of the thesis.** Item-level PANSS correlation analysis for a separate, upcoming paper. |
| `panss_outputs` | Outputs of `panss_correlation.R` and `panss_correlation_items.R`. |

