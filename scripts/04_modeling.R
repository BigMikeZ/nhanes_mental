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

# Build LASSO
set.seed(123)
nhanes_split <- initial_split(nhanes_clean, prop = 0.8, strata = depressed)
nhanes_train <- training(nhanes_split)
nhanes_test  <- testing(nhanes_split)

set.seed(123)
nhanes_folds <- vfold_cv(nhanes_train, v = 10, strata = depressed)

lasso_recipe <- recipe(depressed ~ ., data = nhanes_train) |> 
  update_role(SEQN, cycle, WTMEC2YR, SDMVPSU, SDMVSTRA, phq9_score,
              new_role = "ID") |> 
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())

lasso_recipe |> prep() |> bake(new_data = NULL) |> glimpse()

lasso_spec <- logistic_reg(
  penalty = tune(),
  mixture = 1
) |>
  set_engine("glmnet") |>
  set_mode("classification")

lasso_workflow <- workflow() |>
  add_recipe(lasso_recipe) |>
  add_model(lasso_spec)

lambda_grid <- grid_regular(
  penalty(range = c(-4, 0), trans = log10_trans()),
  levels = 50
)

set.seed(123)
lasso_tune <- tune_grid(
  lasso_workflow,
  resamples = nhanes_folds,
  grid      = lambda_grid,
  metrics   = metric_set(roc_auc)
)

autoplot(lasso_tune)
show_best(lasso_tune, metric = "roc_auc")

best_lambda <- select_by_one_std_err(lasso_tune, 
                                     metric = "roc_auc",
                                     desc(penalty))
best_lambda

final_workflow <- lasso_workflow |>
  finalize_workflow(best_lambda)

final_fit <- final_workflow |>
  fit(data = nhanes_train)

final_fit |>
  extract_fit_parsnip() |>
  tidy() |>
  filter(estimate != 0) |>
  arrange(desc(abs(estimate)))

last_fit_result <- final_workflow |>
  last_fit(nhanes_split, metrics = metric_set(roc_auc))

collect_metrics(last_fit_result)
last_fit_result |>
  collect_predictions() |>
  roc_curve(truth = depressed, .pred_Yes, event_level = "second") |>
  autoplot()
