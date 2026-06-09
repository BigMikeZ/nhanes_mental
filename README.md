# Social Determinants of Depression: A Multi-Model Analysis Using NHANES 2013–2018
**Author:** Mike Zhang — [GitHub](https://github.com/BigMikeZ)

[View Full Report Here](https://bigmikez.github.io/nhanes_mental/)

## Project Overview
This project examines social determinants of depression among US adults using three complementary analytical approaches applied to nationally representative survey data from the National Health and Nutrition Examination Survey (NHANES) 2013–2018.

## Data
All the data involved in this analysis came from the National Health and Nutrition Examination Survey (NHANES) conducted by the U.S. Centers for Disease Control and Prevention every two years. Specifically, cycles 2013-2018 were selected. Data are publicly available at [CDC NHANES](https://www.cdc.gov/nchs/nhanes/).

## Methods
- **Survey-weighted logistic regression** — inferential model with odds ratios
- **LASSO** — variable selection via penalized regression
- **Random forest** — non-linear prediction with permutation importance

## Key Findings
- Food insecurity was the strongest and most consistent predictor of depression 
  across all three models
- Current smoking, income, and sex were consistently important predictors
- Physical activity showed no meaningful association after adjustment
- All three models achieved similar discriminative performance (AUC 0.714–0.726)

## Tech Stack
- **Language:** R
- **Data import, merge, & transformation:** `tidyverse`, `haven`, `nhanesA`, `glue`
- **Construct weighted survey data**: `survey`, `srvyr`
- **Model construction**: `tidymodels`
- **LASSO**: `glmnet`
- **Random forest**: `ranger`, `vip`
- **Model comparison**: `pROC`
- **Model parameter extraction**: `broom`
- **Summary table generation and save**: `gtsummary`, `gt`, `naniar`
- **Data visualization**: `ggplot2`, `ggsci`

## Reproducing the Analysis
1. Clone the repository
2. Install required packages (see scripts for full list)
3. Run scripts in order: `01_download_data.R` → `05_visualizations.R`
4. Render `report/nhanes_survival_report.qmd` to generate the final report

## Repository Structure 
```
nhanes_mental/
├── data/
│   └── processed/       # Processed .rds files output by scripts
├── final_report.qmd     # Quarto Markdown file with scripts to render final report
├── scripts/             # Run in order (01 → 05) before rendering report
├── output/              # Figures and model objects
│   ├── tables/          # Summary tables of data characteristics and the main Coxph model
│   ├── models/          # Saved Coxph models as RDS files
│   └── figures/         # Forest plot of Coxph model
└── docs/                # Final report rendered as HTML
```
