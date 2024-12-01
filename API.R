## API.R file 

library(idyverse)
library(tidymodels)
library(plumber)

# Reading in data
diab_data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")

diab_data <- diab_data |>
  mutate(diab_bin = factor(Diabetes_binary, levels = c(0, 1),
                           labels = c("no diabetes", "diabetes")),
         smoker = factor(Smoker, levels = c(0, 1), 
                         labels = c("no", "yes")),
         genHlth = factor(GenHlth, levels  = c(1, 2, 3, 4, 5), 
                          labels = c("excellent", "very good", "good", "fair", "poor")),
         sex = factor(Sex, levels = c(0, 1), 
                      labels = c("female", "Male")),
         age = factor(Age, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 
                      labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")),
         income = factor(Income, levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                         labels = c("<10", "10-15", "15-20", "20-25", "25-35", "35-50", "50-75", "75+"))) |>
  select(diab_bin, smoker, genHlth, sex, age, income) 
diab_data

# Fitting best model to the entire dataset
best_model <- rf_wfl |>
  finalize_workflow(rf_best) |>
  fit(diab_data)

# Confusion matrix for the our best model on the entire dataset
conf_matrix <- conf_mat(diab_data |>
           mutate(estimate = best_model |> predict(diab_data)  |> pull()),
         diab_bin,
         estimate)
conf_matrix