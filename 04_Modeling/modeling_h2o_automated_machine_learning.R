# H2O Modeling ----

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

