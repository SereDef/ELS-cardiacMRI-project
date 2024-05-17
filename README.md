# ELS-cardiacMRI-project

Pipeline for the analysis of "Prenatal and postnatal stress and heart morphology"

## Pipeline

0.  Data_prep: outcome and covariate data extraction and recoding. Random forest multiple imputation and computation of BSA-adjusted z-scores
1.  Descriptives: merging with ELS exposure imputed dataset. Filtering and data descriptives.
2.  Main_analysis: hierarchical ressessions and causal mediation analyses.

### Dependencies

R version 4.2.2

| Package  | Version |
|----------|---------|
| foreign  | 0.8-84  |
| dplyr    | 1.1.2   |
| mice     | 3.16.0  |
| miceadds | 3.16-18 |
| gamlss   | 5.4-12  |
| rms      | 6.7-0   |
