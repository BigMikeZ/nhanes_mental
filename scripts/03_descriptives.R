library(srvyr)
library(tidyverse)
library(gtsummary)
library(naniar)

nhanes_clean <- read_rds("data/processed/nhanes_clean.rds")
nhanes_clean_full <- readRDS("data/processed/nhanes_clean_full.rds")

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
  include = c(RIDAGEYR, RIAGENDR, RIDRETH3, INDFMPIR, ALQ130,
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

nhanes_clean_full |> 
  select(depressed, RIAGENDR, RIDRETH3, dmdeduc2_collapsed,
         INDFMPIR, fsdad_recoded, activity, smoking_status, 
         ALQ130, sleep_hr) |> 
  miss_var_summary()

missing_tbl <- nhanes_clean_full |> 
  mutate(
    any_missing = factor(
      (is.na(ALQ130)) | (is.na(depressed)) | (is.na(INDFMPIR)) | (is.na(dmdeduc2_collapsed)) | 
      (is.na(fsdad_recoded)),
      levels = c(TRUE, FALSE),
      labels = c("Any missing", "Complete")
    )
  ) |> 
  tbl_summary(
    by = any_missing,
    include = c(ALQ130, depressed, INDFMPIR, dmdeduc2_collapsed, fsdad_recoded),
    missing = "no",
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{p}%"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ 1),
    label  = list(
      INDFMPIR            ~  "Family income-to-poverty ratio",
      ALQ130              ~  "Alcohol consumption (dirnks/day)",
      depressed           ~  "Depressed",
      dmdeduc2_collapsed  ~  "Education",
      fsdad_recoded       ~  "Food security"
    )
  ) |> 
  add_p()
missing_tbl
missing_tbl |>
  as_gt() |> 
  gt::gtsave("output/tables/missing_tbl.html")

nhanes_clean_full |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(everything()) |>
  filter(value > 0) |>
  arrange(desc(value))

