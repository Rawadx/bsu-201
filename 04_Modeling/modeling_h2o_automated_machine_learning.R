# H2O MODELING ----

# 1. Setup -----



#Load Libraries 

library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(stringr)
library(forcats)
library(cowplot)
library(fs)
library(glue)


#Load Data

path_train <- "00_Data/telco_train.xlsx"
path_test <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
test_raw_tbl <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = , col_names = FALSE ) 

# Processing Pipeline
source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# ML Pre-processing

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_num2factor(JobLevel, levels = c('1', '2', '3', '4', '5')) %>%
  step_num2factor(StockOptionLevel, levels = c('0', '1', '2', '3'), transform = function(x) {x + 1}) %>% 
  prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data =  train_readable_tbl)
test_tbl <- bake(recipe_obj, new_data = test_readable_tbl)

# 2. Modeling -----

h2o.init()

split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)

train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
#test_h2o <- as.h2o(test_tbl)   #H2O best practice is to use only train and validate sets. 

y <- "Attrition"
x <- setdiff(names(train_h2o), y) 

automl_models_h2o <- h2o.automl(
  
  x = x, 
  y = y, 
  training_frame = train_h2o, 
  validation_frame = valid_h2o,
  #leaderboard_frame = test_h2o #leaderboard_frame is not necessary
  max_runtime_secs = 30, 
  nfolds = 5, # cross validation 
)


typeof(automl_models_h2o)
slotNames(automl_models_h2o)
automl_models_h2o@leaderboard  # S4 object are accessed using @
automl_models_h2o@leader

h2o.getModel("GBM_grid__1_AutoML_20201005_000040_model_2")
h2o.getModel("StackedEnsemble_BestOfFamily_AutoML_20201005_000040")


automl_models_h2o@leaderboard %>% 
  
  as_tibble() %>% 
  slice(1) %>% 
  pull(model_id) %>% 
  
  h2o.getModel()

#function to get model id 
extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1, verbose = TRUE) { 
  
  model_name <- h2o_leaderboard %>% 
  as_tibble() %>% 
    slice(n) %>% 
    pull(model_id) 
  
  if(verbose) message(model_name)
  
  return(model_name)
  
}

# Saving Models 

automl_models_h2o@leaderboard %>% 
  extract_h2o_model_name_by_position(2) %>% 
  h2o.getModel()

h2o.getModel("StackedEnsemble_AllModels_AutoML_20201005_000040") %>% 
  h2o.saveModel(path = "04_Modeling/h2o_models")


h2o.getModel("GBM_grid__1_AutoML_20201005_000040_model_2") %>% 
  h2o.saveModel(path = "04_Modeling/h2o_models")


h2o.getModel("DeepLearning_grid__1_AutoML_20201005_000040_model_1") %>% 
  h2o.saveModel(path = "04_Modeling/h2o_models")

#alternatively we could have done the same this way: 
automl_models_h2o@leaderboard %>%
  extract_h2o_model_name_by_position(4) %>%
  h2o.getModel() %>%
  h2o.saveModel(path = "04_Modeling/h2o_models/")


# Loading Models

deep_learning_h2o <- h2o.loadModel("04_modeling/h2o_models/DeepLearning_grid__1_AutoML_20201005_000040_model_1")

# Making Predictions 

stacked_ensemble_h2o <- h2o.loadModel("04_modeling/h2o_models/StackedEnsemble_AllModels_AutoML_20201005_000040")

stacked_ensemble_h2o

predictions <- h2o.predict(stacked_ensemble_h2o, newdata= as.h2o(test_tbl))

typeof(predictions)

predictions_tbl <- predictions %>% as_tibble()



