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
                      labels = c("female", "male")),
         age = factor(Age, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 
                      labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")),
         income = factor(Income, levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                         labels = c("<10", "10-15", "15-20", "20-25", "25-35", "35-50", "50-75", "75+"))) |>
  select(diab_bin, smoker, genHlth, sex, age, income) 
diab_data

# Defining the recipe
rec <- recipe(diab_bin ~ ., data = diab_data) |>
  step_dummy(smoker, genHlth, sex, age, income)
rec

# Defining the model
rf_mod <- rand_forest(mtry = 6) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

# Creating the workflow
rf_wfl <- workflow() |>
  add_recipe(rec) |>
  add_model(rf_mod)

# Fitting best model to the entire dataset
best_model <- rf_wfl |>
  fit(diab_data)


## Creating the different endpoints

# 1 - pred endpoint

#* @param smoker (yes, no)
#* @param genHlth (excellent, very good, good, fair, poor)
#* @param sex (female, male)
#* @param age (x18.24, x25.29, x30.34, x35.39, x40.44, x45.49, x50.54, x55.59, x60.64, x65.69, x70.74, x75.79, x80.0)
#* @param income (x10, x10.15, x15.20, x20.25, x25.35, x35.50, x50.75, x75.0)
#* @get /class
function(smoker, genHlth, sex, age, income) {
  defaults <- diab_data|>
    summarise(
      smoker = names(which.max(table(smoker))),
      genHlth = names(which.max(table(genHlth))),
      sex = names(which.max(table(sex))),
      age = names(which.max(table(age))),
      income = names(which.max(table(income)))) |>
    as.list()
  
  inputs <- list(
    smoker = ifelse(is.null(smoker), defaults$smoker, smoker),
    genHlth = ifelse(is.null(genHlth), defaults$genHlth, genHlth),
    sex = ifelse(is.null(sex), defaults$sex, sex),
    age = ifelse(is.null(age), defaults$age, age),
    income = ifelse(is.null(income), defaults$income, income))
  
  input_tib <- as_tibble(inputs)
  pred <- predict(best_model, new_data = input_data)
  
  list(
    input = inputs,
    prediction = pred$pred_class)
}
# query with http://localhost:PORT/pred [defaults]
# query with http://localhost:PORT/pred?smoker=yes&sex=Female
# query with http://localhost:PORT/pred?smoker=no&genHlth=fair&sex=Male&age=x25.35&income=x75.0


# 2 - info endpoint

#* @get /info
function() {
  c("Jenna Christensen",
    "https://jennachristensenn.github.io/finalProject/")
}
# query with http://localhost:PORT/info


# 3 - confusion endpoint

#* @serializer png
#* @get /confusion
function() {
  conf_matrix <- conf_mat(diab_data |>
                            mutate(estimate = best_model |> predict(diab_data)  |> pull()),
                          diab_bin,
                          estimate)
  
  conf_df <- as.data.frame(conf_matrix$table)
  conf_df <- conf_df |>
    mutate(Label = case_when(
      Prediction == "no diabetes" & Truth == "no diabetes" ~ "True Negative",
      Prediction == "diabetes" & Truth == "diabetes" ~ "True Positive",
      Prediction == "diabetes" & Truth == "no diabetes" ~ "False Positive",
      Prediction == "no diabetes" & Truth == "diabetes" ~ "False Negative"
    ))
  conf_df <- conf_df |>
    mutate(Annotation = paste0(Label, "\n", Freq))
  
  plot <- ggplot(conf_df, aes(x = Prediction, y = Truth, label = Annotation)) +
    geom_tile(fill = "white", color = "black") +
    geom_text(size = 5) + # add false positive and so forth 
    labs(title = "Confusion Matrix", x = "Predicted", y = "True") 
  print(plot)
}
# query with http://localhost:PORT/confusion
