# Main Study

## Purpose

This folder contains the main quantitative dataset and R scripts used to analyze the full sample. The files are organized into descriptive statistics, preliminary screening, Hypothesis 1, and Hypothesis 2 output folders.

## Folder contents

### Direct files

| File | Type | Size |
|---|---:|---:|
| `alldata.csv` | csv | 4.0 KB |
| `analysis_results_h1.r` | r | 15.5 KB |
| `analysis_results_h2.r` | r | 24.9 KB |
| `descriptive_statistics.r` | r | 14.4 KB |
| `preliminary.r` | r | 17.3 KB |


### Direct subfolders

| Subfolder | Purpose |
|---|---|
| `descriptive_statistics` | This folder contains the descriptive statistics outputs for the main study, including prepared datasets, summary tables, and plots for holistic scores, error frequencies, and re.... |
| `preliminary` | This folder contains preliminary quantitative outputs, including data screening results, missing-value checks, outlier summaries, ANCOVA assumption diagnostics, Shapiro-Wilk nor.... |
| `results_h1` | This folder contains the inferential outputs for Hypothesis 1, which tested the effect of the Integrated Correction Model on post-test holistic writing performance while control.... |
| `results_h2` | This folder contains the inferential outputs for Hypothesis 2, which tested between-group differences in grammar, vocabulary, and mechanics error reduction, including main-categ.... |


## Recommended use

Run the R scripts from this folder after checking that the input dataset path is correct. Output folders store the generated tables and plots used in the dissertation.

## Notes

- CSV files are generally suitable for GitHub. Keep generated PNG plots and output CSV files together so the analysis remains reproducible.
