library(tidyverse)
library(tidymodels)
library(broom)
library(srvyr)
library(ggsci)
library(vip)
library(pROC)

# Generate forest plot for logistic regression 
model_logistic <- read_rds("output/models/model_logistic.rds")

tidy(model_logistic, exponentiate = TRUE, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(term = fct_recode(term,
                           "Food security: Very low"          = "fsdad_recodedVery low",
                           "Food security: Low"               = "fsdad_recodedLow",
                           "Food security: Marginal"          = "fsdad_recodedMarginal",
                           "Smoking: Current"                 = "smoking_statusCurrent",
                           "Smoking: Former"                  = "smoking_statusFormer",
                           "Sex: Male"                        = "RIAGENDRMale",
                           "Race: Mexican American"           = "RIDRETH3Mexican American",
                           "Race: Non-Hispanic Asian"         = "RIDRETH3Non-Hispanic Asian",
                           "Race: Non-Hispanic Black"         = "RIDRETH3Non-Hispanic Black",
                           "Race: Other Hispanic"             = "RIDRETH3Other Hispanic",
                           "Race: Other/Multi-Racial"         = "RIDRETH3Other Race - Including Multi-Racial",
                           "Education: < High school"         = "dmdeduc2_collapsedLess than high school",
                           "Education: High school/GED"       = "dmdeduc2_collapsedHigh school/GED",
                           "Education: Some college"          = "dmdeduc2_collapsedSome college",
                           "Physical activity: None"          = "activityNone",
                           "Physical activity: Moderate"      = "activityModerate",
                           "Income-to-poverty ratio (log)"    = "log(INDFMPIR + 0.01)",
                           "Avg drinks/day"                   = "ALQ130",
                           "Sleep duration (hours)"           = "sleep_hr",
                           "Age (years)"                      = "RIDAGEYR"
  )) |> 
  ggplot(aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), orientation = "y", width = 0.2) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(
    x = "Odds Ratio", 
    y = NULL,
    title = "Logistic Regression: Odds Ratios for Depression",
    caption = "Odds ratios and confidence intervals for depression from the survey-weighted 
    logistic regression model. NHANES 2013-2018"
  ) 

ggsave("output/figures/forest_plot.png")

# Create LASSO coefficient plot
last_fit_result <- read_rds("output/models/lasso_final_fit.rds")

last_fit_result |> 
  extract_fit_parsnip() |> 
  tidy() |> 
  filter(term != "(Intercept)", estimate != 0) |> 
  mutate(
    term = fct_recode(term,
                      "Income-to-poverty ratio (log)"  = "INDFMPIR",
                      "Avg drinks/day"                 = "ALQ130",
                      "Sleep duration (hours)"         = "sleep_hr",
                      "Sex: Male"                      = "RIAGENDR_Male",
                      "Race: Mexican American"         = "RIDRETH3_Mexican.American",
                      "Race: Non-Hispanic Asian"       = "RIDRETH3_Non.Hispanic.Asian",
                      "Race: Other/Multi-Racial"       = "RIDRETH3_Other.Race...Including.Multi.Racial",
                      "Education: < High school"       = "dmdeduc2_collapsed_Less.than.high.school",
                      "Food security: Marginal"        = "fsdad_recoded_Marginal",
                      "Food security: Low"             = "fsdad_recoded_Low",
                      "Food security: Very low"        = "fsdad_recoded_Very.low",
                      "Smoking: Former"                = "smoking_status_Former",
                      "Smoking: Current"               = "smoking_status_Current"
    ),
    direction = if_else(estimate > 0, "Higher risk", "Lower risk")
  ) |> 
  ggplot(aes(x = estimate, y = fct_reorder(term, estimate), fill = direction)) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("Higher risk" = "#d73027", "Lower risk" = "#4575b4")) +
  theme_minimal() +
  labs(x = "Coefficient (normalized)", y = NULL, fill = NULL,
       title = "LASSO: Selected Predictors of Depression") +
  theme(legend.position = "bottom") +
  labs(caption = "Coefficients are on the normalized scale. Variables shrunk to zero by LASSO are excluded. NHANES 2013-2018")

ggsave("output/figures/lasso_coefficient.png")

# Create variable importance plot for random forest
rf_last_fit <- readRDS("output/models/rf_last_fit.rds")

rf_last_fit |> 
  extract_fit_parsnip() |> 
  vi() |> 
  filter(Importance >= 0) |> 
  mutate(
    Variable = fct_recode(Variable,
                          "Income-to-poverty ratio"        = "INDFMPIR",
                          "Food security: Very low"        = "fsdad_recoded_Very.low",
                          "Smoking: Current"               = "smoking_status_Current",
                          "Sex: Male"                      = "RIAGENDR_Male",
                          "Food security: Low"             = "fsdad_recoded_Low",
                          "Education: < High school"       = "dmdeduc2_collapsed_Less.than.high.school",
                          "Sleep duration (hours)"         = "sleep_hr",
                          "Age (years)"                    = "RIDAGEYR",
                          "Avg drinks/day"                 = "ALQ130",
                          "Food security: Marginal"        = "fsdad_recoded_Marginal",
                          "Race: Non-Hispanic Asian"       = "RIDRETH3_Non.Hispanic.Asian",
                          "Race: Mexican American"         = "RIDRETH3_Mexican.American",
                          "Smoking: Former"                = "smoking_status_Former",
                          "Education: High school/GED"     = "dmdeduc2_collapsed_High.school.GED",
                          "Education: Some college"        = "dmdeduc2_collapsed_Some.college",
                          "Race: Non-Hispanic Black"       = "RIDRETH3_Non.Hispanic.Black",
                          "Race: Other Hispanic"           = "RIDRETH3_Other.Hispanic",
                          "Physical activity: None"        = "activity_None",
                          "Race: Other/Multi-Racial"       = "RIDRETH3_Other.Race...Including.Multi.Racial",
    )
  ) |> 
  ggplot(aes(x = Importance, y = fct_reorder(Variable, Importance))) +
  geom_col(fill = "#4575b4") +
  theme_minimal() +
  labs(
    x = "Permutation Importance",
    y = NULL, 
    title = "Random Forest: Variable Importance", 
    caption = "Importance measured by mean decrease in AUC after permutation. NHANES 2013-18"
  )

ggsave("output/figures/rf_importance.png")

# Descriptive bar by subgroup
nhanes_clean <- readRDS("data/processed/nhanes_clean.rds")

nhanes_svy <- nhanes_clean |> 
  as_survey_design(
    ids     = SDMVPSU,
    strata  = SDMVSTRA,
    weights = WTMEC2YR,
    nest    = TRUE
  )

prev_data <- bind_rows(
  nhanes_svy |>
    group_by(group = fsdad_recoded, depressed) |>
    summarise(pct = survey_mean() * 100) |>
    filter(depressed == "Yes") |>
    mutate(category = "Food Security"),
  nhanes_svy |>
    group_by(group = smoking_status, depressed) |>
    summarise(pct = survey_mean() * 100) |>
    filter(depressed == "Yes") |>
    mutate(category = "Smoking Status"),
  nhanes_svy |>
    group_by(group = RIAGENDR, depressed) |>
    summarise(pct = survey_mean() * 100) |>
    filter(depressed == "Yes") |>
    mutate(category = "Sex")
) |>
  mutate(group = as.character(group))

prev_data |> 
  mutate(group = factor(group, levels = c(
    "Very low", "Low", "Marginal", "Full",
    "Female", "Male",
    "Current", "Former", "Never"
  ))) |> 
  ggplot(aes(x = pct, y = fct_reorder2(group, pct, category), fill = category)) +
  geom_col() +
  geom_errorbar(
    aes(xmin = pct - 1.96 * pct_se, xmax = pct + 1.96 * pct_se), 
    width = 0.2,
    linewidth = 0.5,
    orientation = "y"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    x = "Weighted prevalence of depression (%)", 
    y = NULL,
    fill = "Subgroup",
    title = "Depression Prevalence by Key Subgroups",
    caption = "Error bars represent 95% confidence intervals. NHANES 2013-18."
  )
ggsave("output/figures/weighted_prevalence.png")

# Construct model comparison plot
set.seed(123)
nhanes_split <- initial_split(nhanes_clean, prop = 0.8, strata = depressed)
nhanes_train <- training(nhanes_split)
nhanes_test  <- testing(nhanes_split)

test_probs <- predict(model_logistic, 
                      newdata = nhanes_test, 
                      type = "response")

roc(nhanes_test$depressed, test_probs) |> 
  auc()

model_comp <- tribble(
  ~Model                        ,    ~AUC,
  "Weighted logistic regression",   0.714,
  "LASSO regression"            ,   0.723,
  "Random forest"               ,   0.727
)

model_comp |> 
  ggplot(aes(x = AUC, y = Model)) +
  geom_col(fill = "#4575b4", width = 0.5) +
  geom_text(aes(label = round(AUC, 3)), hjust = -0.2) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  scale_x_continuous(limits = c(0, 0.80)) +
  theme_minimal() +
  labs(
    x = "Test-set AUC",
    y = NULL,
    title = "Model Comparison: Discriminative Performance",
    caption = "Dashed line represents random chance (AUC = 0.5). NHANES 2013-18."
  )
ggsave("output/figures/model_comparison.png")  
