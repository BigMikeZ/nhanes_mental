library(tidyverse)
library(tidymodels)
library(broom)
library(ggsci)

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


