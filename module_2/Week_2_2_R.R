

#### Coding Week_2 Class_2 ####

# Let's fit our first machine learning model! :) 



rm(list = ls())

# Load R packages ---------------------------------------------------------

library(dplyr)
library(readxl)
library(tidyr)
library(table1)
library(mice)
library(haven)
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# Set working directory
options(scipen=999)



# Upload the data ---------------------------------------------------------

heart_failure <- read_csv("module_2/heart_failure.csv")
heart_failure

heart_failure <- heart_failure %>% 
  mutate(death = factor(death))

# Inspect variables -------------------------------------------------------

barplot(table(heart_failure$death))
barplot(table(heart_failure$sex))
hist(heart_failure$age)


# Split into training and testing -----------------------------------------

# choose a different split proportion?
set.seed(123)
hf_split <- initial_split(heart_failure, prop = 0.8)
hf_train <- training(hf_split)
hf_test <- testing(hf_split)

# Create cross validation folds
hf_folds <- vfold_cv(hf_train, v = 10)


# Build a recipe ----------------------------------------------------------

hf_recipe <- recipe(death ~ ., data = hf_train) %>% 
  step_dummy(sex) %>% 
  step_normalize(age, serum_creatinine:time)

wf <- workflow() %>% 
  add_recipe(hf_recipe)

# Specify the model -------------------------------------------------------

tune_spec_lasso <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")


# Tune the model ----------------------------------------------------------

# Fit lots of values
lasso_grid <- tune_grid(
  add_model(wf, tune_spec_lasso),
  resamples = hf_folds,
  grid = grid_regular(penalty(), levels = 30)
)

# Choose the best value
highest_roc_auc_lasso <- lasso_grid %>%
  select_best(metric = "roc_auc")

# Fit the final model -----------------------------------------------------

final_lasso <- finalize_workflow(
  add_model(wf, tune_spec_lasso),
  highest_roc_auc_lasso
)


# Model evaluation --------------------------------------------------------

last_fit(final_lasso, hf_split) %>%
  collect_metrics()

# which variables were most important?
final_lasso %>%
  fit(hf_train) %>%
  extract_fit_parsnip() %>%
  vip::vi(lambda = highest_roc_auc_lasso$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) 

# Graph for variable importance 
final_lasso %>%
  fit(hf_train) %>%
  extract_fit_parsnip() %>%
  vip::vi(lambda = highest_roc_auc_lasso$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  ggplot(mapping = aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col()
