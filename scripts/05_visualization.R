library(tidyverse)
library(broom)
library(ggsci)

# Generate forest plot for logistic regression model
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


