# DATA PREPARATION ----
# Machine Readable ----

# Libraries 

library(recipes)
library(tidyverse)
library(readxl)
library(tidyquant)
library(forcats)


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

# Plot Faceted Histogram Function ----

data <- train_raw_tbl

plot_hist_facet <- function(data, bins = 10, ncol = 5, 
                            fct_reorder = FALSE, fct_reverse = FALSE, 
                            fill = palette_light()[[3]], 
                            color = "white", scale = "free") { 
  
  data_factored <-data %>% 
    mutate_if(is.character, as.factor) %>% 
    mutate_if(is.factor, as.numeric) %>% 
    gather(key = key, value = value, factor_key = TRUE) 
  
 if(fct_reorder) { 
   data_factored <- data_factored %>% 
     mutate(key =  as.character(key) %>% as.factor() ) #this arranges factors automatically
 }
  
  if(fct_reverse) { 
    data_factored <- data_factored %>% 
      mutate(key = fct_rev(key))
    }

  g <- data_factored %>% 
    ggplot(aes(x = value, group = key)) + 
    geom_histogram(bins = bins, fill = fill, color = color)+
    facet_wrap(~ key, ncol = ncol, scale = scale ) + 
    theme_tq()
  
  return(g)
}


train_raw_tbl %>% 
  select(Attrition, everything()) %>% 
  plot_hist_facet(bins = 10, ncol = 5)

# Data Preprocessing with Recipes ----

# Plan: Correlation Analysis 

# 1. Zero Variance Features ----

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
  step_zv(all_predictors())


# 2. Transformations ----

skewed_feature_names <- train_readable_tbl %>% 
  select_if(is.numeric) %>% 
  map_df(skewness) %>% 
  gather(factor_key = T) %>% 
  arrange(desc(value)) %>% 
  filter(value >= 0.8) %>% 
  pull(key) %>% 
  as.character()

train_readable_tbl %>% 
  select(skewed_feature_names) %>% 
  plot_hist_facet()


skewed_feature_names <- train_readable_tbl %>% 
  select_if(is.numeric) %>% 
  map_df(skewness) %>% 
  gather(factor_key = T) %>% 
  arrange(desc(value)) %>% 
  filter(value >= 0.8) %>% 
  filter(!key %in% c("JobLevel", "StockOptionLevel")) %>% 
  pull(key) %>% 
  as.character()

factor_names <- c("JobLevel", "StockOptionLevel")

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_YeoJohnson(skewed_feature_names) %>% 
  step_mutate_at(factor_names, fn = as.factor)

recipe_obj %>% 
  prep() %>% 
  bake(train_readable_tbl) %>% 
  select(skewed_feature_names) %>% 
  plot_hist_facet()

# 3. Center / Scale ----

train_readable_tbl %>% 
  select_if(is.numeric) %>% 
  plot_hist_facet()

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_YeoJohnson(skewed_feature_names) %>% 
  step_mutate_at(factor_names, fn = as.factor) %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric())

recipe_obj$steps[[4]] # Before Prep
prepared_recipe <- recipe_obj %>% prep() 
prepared_recipe$steps[[4]] # After Prep

prepared_recipe %>% 
  bake(new_data = train_readable_tbl) %>%
  select_if(is.numeric) %>% 
  plot_hist_facet()

# 4. Dummy Variables ----

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_YeoJohnson(skewed_feature_names) %>% 
  step_mutate_at(factor_names, fn = as.factor) %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric())

recipe_obj %>% 
  prep() %>% 
  bake(new_data = train_readable_tbl) %>% 
  select(contains("JobRole")) %>% 
  plot_hist_facet()

dummied_recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_YeoJohnson(skewed_feature_names) %>% 
  step_mutate_at(factor_names, fn = as.factor) %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric()) %>% 
  step_dummy(all_nominal()) # all_nomical selects all categorical data


dummied_recipe_obj %>% 
  prep() %>% 
  bake(new_data = train_readable_tbl) %>% 
  select(contains("JobRole")) %>% 
  plot_hist_facet(ncol = 3)

dummied_recipe_obj %>% 
  prep() %>% 
  bake(new_data = train_readable_tbl) %>% 
  select(contains("JobRole")) %>% View()

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_YeoJohnson(skewed_feature_names) %>% 
  step_mutate_at(factor_names, fn = as.factor) %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric()) %>% 
  step_dummy(all_nominal()) # all_nomical selects all categorical data


# 5. Interaction Variable / Engineered Features - not needed here
# 6. Multivariate Transformation - not needed here

# Final Recipe ----

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_YeoJohnson(skewed_feature_names) %>% 
  step_mutate_at(factor_names, fn = as.factor) %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric()) %>% 
  step_dummy(all_nominal()) %>% 
  prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)

train_tbl %>% glimpse() %>% View()

test_tbl <- bake(recipe_obj, new_data = test_readable_tbl)

test_tbl %>% glimpse() %>% View()

# tidy 

tidy(recipe_obj, number = 4) %>% View()

# Correlation Analysis ----

data <- train_tbl
feature_exp <- quo(Attrition_Yes)  # quo is used outside of function

get_cor <- function(data, target, use = "pairwise.complete.obs",
                    fct_reorder = FALSE, fct_rev = FALSE) { 
  
          feature_exp <- enquo(target) #enquo is used inside functions
          feature_name <- quo_name(feature_exp)
          
          data_cor <- data %>% 
            mutate_if(is.character, as.factor) %>% 
            mutate_if(is.factor, as.numeric) %>% 
            cor(use = use) %>% 
            as.tibble %>% 
            mutate(feature = names(.)) %>%  
            select(feature, !!feature_exp) %>% 
            filter(!(feature == feature_name)) %>% 
            mutate_if(is.character, as_factor)
          
          if(fct_reorder) { 
            
            data_cor <- data_cor %>% 
              mutate(feature = fct_reorder(feature, !!feature_exp)) %>% 
              arrange(feature)
            
          }
          
          if(fct_rev) { 
            
            data_cor <- data_cor %>% 
              mutate(feature = fct_rev(feature)) %>% 
              arrange(feature)
            
          }
          
          return(data_cor)
          
}

train_tbl %>% 
get_cor(Attrition_Yes, fct_reorder = T, fct_rev = T) %>% View()


data <- train_tbl
feature_exp <- quo(Attrition_Yes)

plot_cor <- function(data, target, fct_reorder = FALSE, fct_rev = FALSE, 
                     included_lbl = TRUE, lbl_precision = 2, lbl_position = "outward", 
                     size = 2, line_size = 1, vert_size = 1, 
                     color_pos = palette_light()[[1]], 
                     color_neg = palette_light()[[2]]) { 
  
  
        feature_exp <- enquo(target) 
        feature_name <- quo_name(feature_exp)
        
        data_cor <- data %>% 
          get_cor(!! feature_exp, fct_reorder = fct_reorder , fct_rev = fct_rev) %>% 
          mutate(feature_name_text = round(!! feature_exp, lbl_precision)) %>% 
          mutate(Correlation = case_when(
            (!! feature_exp) >= 0 ~ "Positive", 
            TRUE  ~                 "Negative") %>% as.factor()) 
        
        g <- data_cor %>% 
          ggplot(aes_string(x = feature_name, y = "feature", group = "feature")) + 
          geom_point(aes(color = Correlation), size = size) + 
          geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = line_size) + 
          geom_vline(xintercept = 0, color = palette_light()[[1]], size = vert_size) + 
          expand_limits(x = c(-1,1)) + 
          theme_tq() +
          scale_color_manual(values = c(color_neg, color_pos))
          
        if(included_lbl) g <- g + geom_label(aes(label = feature_name_text), hjust = lbl_position)
  
        return(g)
        
}

train_tbl %>% 
  select(Attrition_Yes,  contains("Job")) %>% 
  plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

# Correlation Evaluation ----


#   1. Descriptive features: age, gender, marital status 
train_tbl %>%
  select(Attrition_Yes, Age, contains("Gender"),contains("MaritalStatus"), DistanceFromHome) %>%
  plot_cor(Attrition_Yes)

#   2. Employment features: department, job role, job level
train_tbl %>%
  select(Attrition_Yes, contains("employee"), contains("department"), contains("Job")) %>%
  plot_cor(Attrition_Yes) 

#   3. Compensation features: HourlyRate, MonthlyIncome, StockOptionLevel 
train_tbl %>%
  select(Attrition_Yes, contains("income"), contains("rate"), contains("salary"), contains("stock")) %>%
  plot_cor(Attrition_Yes)

#   4. Survey Results: Satisfaction level, WorkLifeBalance 
train_tbl %>%
  select(Attrition_Yes, contains("satisfaction"), contains("life")) %>%
  plot_cor(Attrition_Yes)

#   5. Performance Data: Job Involvment, Performance Rating
train_tbl %>%
  select(Attrition_Yes, contains("performance"), contains("involvement")) %>%
  plot_cor(Attrition_Yes)

#   6. Work-Life Features 
train_tbl %>%
  select(Attrition_Yes, contains("overtime"), contains("travel")) %>%
  plot_cor(Attrition_Yes) 

#   7. Training and Education 
train_tbl %>%
  select(Attrition_Yes, contains("training"), contains("education")) %>%
  plot_cor(Attrition_Yes)

#   8. Time-Based Features: Years at company, years in current role
train_tbl %>%
  select(Attrition_Yes, contains("years")) %>%
  plot_cor(Attrition_Yes)
