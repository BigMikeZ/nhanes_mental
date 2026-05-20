library(srvyr)
library(tidyverse)
library(gtsummary)

nhanes_clean <- read_rds("data/processed/nhanes_clean.rds")

nhanes_svy <- nhanes_clean |> 
  as_survey_design(
    ids     = SDMVPSU,
    strata  = SDMVSTRA,
    weights = WTMEC2YR,
    nest    = TRUE
  )

summary_table <- tbl_svysummary(
  nhanes_svy,
  by      = depressed,
  include = c(RIDAGEYR, RIAGENDR, RIDRETH3, INDFMPIR, HUQ090, ALQ130,
              sleep_hr, phq9_score, dmdeduc2_collapsed, fsdad_recoded, smoking_status,
              activity),
  statistic = list(
    all_continuous()  ~ "{mean} ({sd})",
    all_categorical() ~ "{p}%"
  ),
  digits = list(all_continuous() ~ 1, all_categorical() ~ 1),
  label   = list(
    RIDAGEYR            ~  "Age (years)",
    RIAGENDR            ~  "Sex",
    RIDRETH3            ~  "Ethnicity",
    INDFMPIR            ~  "Family income-to-poverty ratio",
    HUQ090              ~  "Seen a mental health professional past year",
    ALQ130              ~  "Alcohol consumption (dirnks/day)",
    sleep_hr            ~  "Sleep duration (hours)",
    phq9_score          ~  "Patient Health Questionnaire (PHQ-9) score",
    dmdeduc2_collapsed  ~  "Education",
    fsdad_recoded       ~  "Food security",
    smoking_status      ~  "Smoking status",
    activity            ~  "Physical activity"
  )
 ) |> 
  add_p() |> 
  add_overall()

summary_table
summary_table |> 
  as_gt() |> 
  gt::gtsave("output/tables/summary_tb.html")
