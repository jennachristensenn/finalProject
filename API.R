## API.R file 

library(tidyverse)
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

# Defining the recipe -- look into prep and bake to see the extent of the x. variables 
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

#* @param smoker 
#* @param genHlth 
#* @param sex 
#* @param age 
#* @param income
#* @get /class
function(smoker = "", genHlth = "", sex = "", age = "", income = "") {
  
  def_smoker <- names(sort(table(diab_data$smoker), decreasing = TRUE))[1]
  def_genHlth <- names(sort(table(diab_data$genHlth), decreasing = TRUE))[1]
  def_sex <- names(sort(table(diab_data$sex), decreasing = TRUE))[1]
  def_age <- names(sort(table(diab_data$age), decreasing = TRUE))[1]
  def_income <- names(sort(table(diab_data$income), decreasing = TRUE))[1]
  
  if (smoker == "") smoker <- def_smoker
  if (genHlth == "") genHlth <- def_genHlth
  if (sex == "") sex <- def_sex
  if (age == "") age <- def_age
  if (income == "") income <- def_income
  
  new_data <- tibble(
    smoker = smoker,
    genHlth = genHlth,
    sex = sex,
    age = age,
    income = income)
  
  prediction <- predict(best_model, new_data)
  return(prediction$.pred_class)
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
