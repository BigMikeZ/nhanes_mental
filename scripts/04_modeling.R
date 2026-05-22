library(tidyverse)
library(tidymodels)
library(survey)
library(gtsummary)

nhanes_clean <- read_rds("data/processed/nhanes_clean.rds")

nhanes_svy_model <- svydesign(
  ids    = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weight = ~WTMEC2YR,
  nest   = TRUE,
  data   = nhanes_clean
) 

#Build logistic regression model
model_logistic <- svyglm(
  depressed ~ RIDAGEYR + RIAGENDR + RIDRETH3 + dmdeduc2_collapsed +
    INDFMPIR + fsdad_recoded + activity + smoking_status +
    ALQ130 + sleep_hr,
  design = nhanes_svy_model,
  family = quasibinomial()
)
summary(model_logistic)  

tbl_regression(model_logistic, exponentiate = TRUE) |> 
  as_gt() |> 
  gt::gtsave("output/tables/logistic_summary.html")

# Check linearity assumption (logit outcome on predictors)
model_glm <- glm(
  depressed ~ RIDAGEYR + INDFMPIR + ALQ130 + sleep_hr +
    RIAGENDR + RIDRETH3 + dmdeduc2_collapsed +
    fsdad_recoded + activity + smoking_status,
  data   = nhanes_clean,
  family = binomial()
)

probabilities <- predict(model_glm, type = "response")

nhanes_check <- nhanes_clean |> 
  select(RIDAGEYR, sleep_hr, INDFMPIR, ALQ130)

predictors <- colnames(nhanes_check)

check_data <- nhanes_check |> 
  mutate(logit = log(probabilities/(1 - probabilities))) |> 
  pivot_longer(-logit, names_to = "predictors", values_to = "predictor.value")

ggplot(check_data, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

# Check influential values (cook's distance)
plot(model_glm, which = 4, id.n = 3)
