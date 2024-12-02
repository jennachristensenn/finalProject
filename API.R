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


## Creating the different endpoints

# 1 - pred endpoint

#* @param num1 first parameter
#* @param num2 second parameter
#* @param num3 third
#* @param num4 fourth
#* @param num5 fifth maybe more to come
#* @get /class
function(predictors = c("")) {
  #needs work!!!
}
# query with ... 
# query with ... 
# query with ... 


# 2 - info endpoint

#* @get /info
function() {
  c("Jenna Christensen",
    "https://jennachristensenn.github.io/finalProject/")
}
# query with ... 


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
# query with ...

# Adjusting this again 