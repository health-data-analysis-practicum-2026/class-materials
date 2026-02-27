

#### Coding the NYC HANES data ####

rm(list = ls())

library(dplyr)
library(readxl)
library(tidyr)
library(table1)
library(mice)
library(haven)
library(tidyverse)

options(scipen=999)


# Upload the data 
dat <- read_sas("./module_2/d.sas7bdat")
dim(dat)


### DATA WRANGLING ##


# Rename the variables 
dat_1 <- 
  dat %>% 
  select(id = KEY,
         age = SPAGE,
         race = DMQ_14_1,
         gender = GENDER,
         born = US_BORN,
         diet = DBQ_1,
         income = INC20K,
         diabetes = DIQ_1,
         bmi = BMI,
         cholesterol = BPQ_16,
         drinkfreq = ALQ_1,
         drinkunit = ALQ_1_UNIT,
         smoking = SMOKER3CAT,
         hypertension = BPQ_2)


# Check the data types 
glimpse(dat_1)
summary (dat_1)

colSums(is.na(dat_1))

# Create a nice missing values table
missing_table <- data.frame(
  Variable = names(dat_1),
  Missing_Count = sapply(dat_1, function(x) sum(is.na(x))),
  Missing_Percent = sapply(dat_1, function(x) round(mean(is.na(x)) * 100, 2))
)

# Reset row names
rownames(missing_table) <- NULL

print(missing_table)


## Checking the variables ## 

# Recode the categorical variables
dat_1 <- dat_1 %>% 
  mutate(race = factor(race, 
                       levels = c(100, 110, 120, 140, 180, 250), 
                       labels = c('White', 'Black/African American', 
                                  'Indian /Alaska Native', 
                                  'Pacific Islander', 
                                  'Asian', 'Other Race')),
         gender = factor(gender, 
                         levels = c(1, 2), 
                         labels = c('Male', 'Female')),
         born = factor(born, 
                       levels = c(1, 2),
                       labels = c("US Born", "Non-US Born")),
         diet = factor(diet, 
                       levels = c(5:1), 
                       labels = c('Poor', 'Fair', 'Good', 
                                  'Very good','Excellent')),
         income = factor(income, 
                         levels = c(1:6), 
                         labels = c('Less than $20,000','$20,000 - $39,999',
                                    '$40,000 - $59,999','$60,000 - $79,999',
                                    '$80,000 - $99,999','$100,000 or more')),
         diabetes = factor(diabetes, 
                           levels = c(2, 1, 3), 
                           labels = c('No','Yes','Prediabetes')),
         cholesterol = factor(cholesterol, 
                              levels = c(2, 1), 
                              labels = c('Low value','High value')),
         smoking = factor(smoking, 
                          levels=c(3:1), 
                          labels = c('Never smoker','Former smoker','Current smoker')),
         hypertension = factor(hypertension, 
                               levels = c(2, 1), 
                               labels = c('No','Yes'))
  )

summary(dat_1)
glimpse(dat_1)

## Checking the Drink variable--the missing values ##

# Using the Variable Codebook, we see that drinkfreq (ALQ_1 in the original survey data) and 
# drinkunit (ALQ_1_UNIT in the original survey data) together 
# reflect the respondent’s answer to the question of how often they drink any 
# type of alcoholic beverage. Specifically, each respondent’s drinking frequency 
# may be defined as drinkfreq drinks per drinkunit. For example:
#   
# If drinkfreq = 2 and drinkunit = 1, the respondent reported drinking 2 times per week
# If drinkfreq = 3 and drinkunit = 2, the respondent reported drinking 3 times per month
# If drinkfreq = 30 and drinkunit = 3, the respondent reported drinking 30 times per year
# Note that a value of drinkfreq = 0 means the respondent never drinks.
# 

dat_1 %>% 
  count(drinkfreq) %>% 
  print(n = Inf)

dat_1 %>% 
  count(drinkunit) %>% 
  print(n = Inf)


# Recoding the Drink variable
dat_1 <- dat_1 %>% 
  mutate(drink_denom = case_when(drinkfreq == 0 | drinkunit == 1 ~ 1,
                                 drinkunit == 2 ~ 52 / 12,
                                 drinkunit == 3 ~ 52),
         drink = drinkfreq / drink_denom)

summary(dat_1$drink)

hist(dat_1$drink)

# Redefine the drink variable 
dat_1 <- dat_1 %>% 
  mutate(drinkcat = factor(drink >= 1, 
                           labels = c('<1 / wk', '1+ / week')))

dat_1 %>% count (drinkcat)

dat_1 =dat_1 %>% select(!c(drinkfreq, drinkunit))

colSums(is.na(dat_1))

dat_1 = na.omit(dat_1)




### EDA ###

dat_1 %>% 
  ggplot(aes(x = hypertension, y = age)) +
  geom_boxplot() + 
  ggtitle('Distribution of age by hypertension status')


library(ggpubr)

p2 <- dat_1 %>% 
  ggplot(aes(x = hypertension, y=gender)) + 
  geom_boxplot() + ggtitle('distribution of gender')

p3 <- dat_1 %>% 
  ggplot(aes(x = hypertension, fill = gender)) + 
  geom_bar() + 
  ggtitle('distribution of gender')

p4 <- dat_1 %>% 
  ggplot(aes(x = hypertension, fill = gender)) + 
  geom_bar(position = "fill") + 
  ggtitle('distribution of gender') + 
  ylab('proportion')

ggarrange(p2, p3, p4, ncol=3, nrow=1)


# Create a table
library(gtsummary)

# Create Table 1 with p-values
dat_1 %>%
  select(hypertension, age, race, gender, born, diet, income,
         diabetes, bmi, cholesterol, smoking, drinkcat) %>%
  tbl_summary(
    by = hypertension,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    missing = "no"
  ) %>%
  add_p() %>%
  add_overall() %>%
  bold_labels()





# Load R packages ---------------------------------------------------------

library(tidyverse)
library(tidymodels)
tidymodels_prefer()


# Load data ---------------------------------------------------------------
# Done above


# Inspect variables -------------------------------------------------------

barplot(table(dat_1$hypertension))
barplot(table(dat_1$gender))
hist(dat_1$age)


# Split into training and testing -----------------------------------------

set.seed(123)

library (tidymodels)
library(dplyr)

# choose a different split proportion?
set.seed(123)
hf_split <- initial_split(dat_1, prop = 0.8)
hf_train <- training(hf_split)
hf_test <- testing(hf_split)

# Create cross validation folds
hf_folds <- vfold_cv(hf_train, v = 10)



# Build a recipe ----------------------------------------------------------

hf_recipe <- recipe(hypertension ~ age + race + 
                      gender + 
                      born + diet + income + diabetes + bmi +
                      cholesterol +smoking + 
                      drinkcat, data = hf_train) %>% 
  step_dummy(race, gender, born, diet, income,diabetes, cholesterol,smoking,
             drinkcat )  %>%
  step_normalize(age,bmi)


wf <- workflow() %>% 
  add_recipe(hf_recipe)


# Specify the model -------------------------------------------------------

tune_spec_lasso <- logistic_reg(penalty = tune(), mixture = 1) |>
  set_engine("glmnet")


# Tune the model ----------------------------------------------------------

# Fit lots of values
lasso_grid <- tune_grid(
  add_model(wf, tune_spec_lasso),
  resamples = hf_folds,
  grid = grid_regular(penalty(), levels = 50)
)

# Choose the best value
highest_roc_auc_lasso <- lasso_grid |>
  select_best(metric = "roc_auc")  


# Fit the final model -----------------------------------------------------

final_lasso <- finalize_workflow(
  add_model(wf, tune_spec_lasso),
  highest_roc_auc_lasso
)


# Model evaluation --------------------------------------------------------

last_fit(final_lasso, hf_split) |>
  collect_metrics()

# which variables were most important?
final_lasso |>
  fit(hf_train) |>
  extract_fit_parsnip() |>
  vip::vi(lambda = highest_roc_auc_lasso$penalty) |>
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) |>
  ggplot(mapping = aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col()




### RF ### 
set.seed(20231018)

# Specify model -----------------------------------------------------------

tune_spec_rf <- rand_forest(
  mtry = tune(),
  trees = 100,
  min_n = tune()
) |>
  set_mode("classification") |>
  set_engine("ranger")

# Tune hyperparameters ----------------------------------------------------

rf_grid <- tune_grid(
  add_model(wf, tune_spec_rf),
  resamples = hf_folds,
  grid = grid_regular(
    mtry(range = c(5, 8)),
    min_n(), #default c(2, 40)
    levels = 5)
)

# Fit model ---------------------------------------------------------------

highest_roc_auc_rf <- rf_grid |>
  select_best(metric = "roc_auc")

final_rf <- finalize_workflow(
  add_model(wf, tune_spec_rf),
  highest_roc_auc_rf
)

# Evaluate ----------------------------------------------------------------

last_fit(final_rf, hf_split) |>
  collect_metrics()

# create a confusion matrix
last_fit(final_rf, hf_split) |> 
  collect_predictions() |> 
  conf_mat(hypertension, .pred_class) |> 
  autoplot()


### SVM ###
#Specify model -----------------------------------------------------------
  
tune_spec_svm <- svm_rbf(cost = tune()) |> 
  set_engine("kernlab") |> 
  set_mode("classification")


# Tune hyperparameters ----------------------------------------------------

# Fit lots of values
svm_grid <- tune_grid(
  add_model(wf, tune_spec_svm),
  resamples = hf_folds,
  grid = grid_regular(cost(), levels = 20)
)

# Fit model ---------------------------------------------------------------

highest_roc_auc_svm <- svm_grid |>
  select_best(metric = "roc_auc")

final_svm <- finalize_workflow(
  add_model(wf, tune_spec_svm),
  highest_roc_auc_svm
)


# Evaluate ----------------------------------------------------------------

last_fit(final_svm, hf_split,
         metrics = metric_set(roc_auc, accuracy, f_meas)) |>
  collect_metrics()

# create a confusion matrix
last_fit(final_svm, hf_split) |> 
  collect_predictions() |> 
  conf_mat(death, .pred_class) |> 
  autoplot()
