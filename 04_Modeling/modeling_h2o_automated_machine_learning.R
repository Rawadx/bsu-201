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


# Visualizing the Leaderboard ----


h2o.init()

data_transformed <- automl_models_h2o@leaderboard %>% 
  as_tibble() %>% 
  mutate(model_type = str_split(model_id, "_", simplify = T)[,1]) %>% 
  slice(1:10) %>% 
  rownames_to_column() %>% 
  mutate(
         model_id = as.factor(model_id) %>% reorder(auc), 
         model_type = as.factor(model_type)
         ) %>% 
  gather(key = key, value = value, -c(model_id, model_type, rowname), factor_key = T) %>% 
  mutate(model_id = paste0(rowname, ".", model_id) %>% as.factor() %>% fct_rev())

data_transformed %>% 
  ggplot(aes(value, model_id, color = model_type)) +
  geom_point(size = 3) + 
  geom_label(aes(label = round(value, 2), hjust = "inward")) + 
  facet_wrap(~ key, scales = "free_x") + 
  theme_tq() + 
  scale_color_tq() + 
  labs(
    
    title = "H2O Leaderboard Metrics", 
    subtitle =  paste0("Ordered by:auc"), 
    y = "Model_Position, Model_ID", 
    x = ""
    
  )

h2o_leaderboard <- automl_models_h2o@leaderboard

plot_h2o_leaderboard <- function(h2o_leaderboard, order_by = c("auc", "logloss"), 
                                 n_max = 20, size = 4, include_lbl = TRUE) { 
      
        
        # Setup 
        order_by <- tolower(order_by[[1]])
        
        leaderboard_tbl <- h2o_leaderboard %>% 
          as_tibble() %>% 
          mutate(model_type = str_split(model_id, "_", simplify = T)[,1]) %>% 
          rownames_to_column(var = "rowname") %>% 
          mutate(model_id = paste0(rowname, ".", as.character(model_id)) %>% as.factor()) %>% 
          select(1:4, 9)  # To eliminate probably newly added values 
        
        if(order_by == "auc") { 
          
        data_transformed_tbl <- leaderboard_tbl %>% 
          slice(1:n_max) %>% 
          mutate(
            model_id = as.factor(model_id) %>% reorder(auc), 
            model_type = as.factor(model_type)
          ) %>% 
          gather(key = key, value = value, 
                 -c(model_id, model_type, rowname), factor_key = T) 
        }
        
        else if (order_by == "logloss") { 
          
          data_transformed_tbl <- leaderboard_tbl %>% 
            slice(1:n_max) %>% 
            mutate(
              model_id = as.factor(model_id) %>% reorder(logloss) %>% fct_rev(), 
              model_type = as.factor(model_type)
            ) %>% 
            gather(key = key, value = value, 
                   -c(model_id, model_type, rowname), factor_key = T) 
          
        }
        
        else { 
          
          stop(paste0("order_by = '", order_by, "is not a permitted option.'"))
          
        }
        
        
        g <- data_transformed_tbl %>% 
          ggplot(aes(value, model_id, color = model_type)) + 
          geom_point(size = size) + 
          facet_wrap(~ key, scales = "free_x") + 
          theme_tq() + 
          scale_color_tq() + 
          labs(
            title = "H2O Leaderboard Metrics", 
            subtitle =  paste0("Ordered by: ", order_by),  
            y = "Model_Position, Model_ID", 
            x = ""
            
          )
        
        if(include_lbl) { g <- g + geom_label(aes(label = round(value, 2), hjust = "inward")) }
          
        return(g)
  
}

automl_models_h2o@leaderboard %>% 
  plot_h2o_leaderboard(order_by = "logloss", include_lbl = T)

# 4. Assessing Performance -----

stacked_ensemble_h2o <- h2o.loadModel("04_modeling/h2o_models/StackedEnsemble_AllModels_AutoML_20201005_000040")

deep_learning_h2o <- h2o.loadModel("04_modeling/h2o_models/DeepLearning_grid__1_AutoML_20201005_000040_model_1")

gbm_h2o <- h2o.loadModel("04_modeling/h2o_models/GBM_grid__1_AutoML_20201005_000040_model_2")

performance_h2o <- h2o.performance(stacked_ensemble_h2o, newdata = as.h2o(test_tbl))

typeof(performance_h2o)

performance_h2o %>% slotNames()

performance_h2o@metrics

# Classifier Summary Metrics 

h2o.auc(performance_h2o)  # caution: The train, val, and xval arguments only work for models (not performance objects)

#h2o.auc(stacked_ensemble_h2o, train = T, valid = T, xval = T)

h2o.giniCoef(performance_h2o)

h2o.logloss(performance_h2o)

h2o.confusionMatrix(stacked_ensemble_h2o)  # top row are predictions (Yes, No) and left column is actual

# Precision vs. Recall Plot

performance_tbl <- performance_h2o %>% 
h2o.metric() %>% 
  as_tibble()

performance_tbl

performance_tbl %>% 
  filter(f1 == max(f1))


performance_tbl %>% 
  ggplot(aes(x = threshold)) +
  geom_line(aes(y = precision), color = "blue", size = 1) + 
  geom_line(aes( y = recall), color = "red", size = 1) + 
  geom_vline(xintercept = h2o.find_threshold_by_max_metric(performance_h2o, "f1")) + 
  theme_tq() + 
  labs( 
    title = "Precision vs. Recall", y = "value"
    )

# ROC Plot

path <- "04_modeling/h2o_models/DeepLearning_grid__1_AutoML_20201005_000040_model_1"

load_model_performance_metrics <- function(path, test_tbl) { 
  
  model_h2o <- h2o.loadModel(path)
  perf_h2o <- h2o.performance(model_h2o, as.h2o(test_tbl))
  
  perf_h2o %>% 
    h2o.metric() %>% 
    as_tibble() %>% 
    mutate(auc = h2o.auc(perf_h2o)) %>% 
    select(fpr, tpr, auc)
  
}


model_metrics_tbl <- fs::dir_info(path = "04_modeling/h2o_models/") %>% 
  select(path) %>% 
  mutate(metrics = map(path, load_model_performance_metrics, test_tbl)) %>% 
  unnest(cols = c(metrics))

model_metrics_tbl %>% 
  mutate(
    path = str_split(path, pattern = "/", simplify = T)[, 3] %>% as_factor(), 
         auc = auc %>% round(3) %>%  as.character() %>% as_factor()
    ) %>% 
  ggplot(aes(fpr, tpr, color = path, linetype = auc)) + 
  geom_line(size = 1) + 
  theme_tq() + 
  scale_color_tq() + 
  theme(legend.direction = "vertical") + 
  labs(
    title = "ROC Plot", 
    subtitle = "Performance of 4 Top Models"
  )


# Precision vs Recall Comparison 


load_model_performance_metrics <- function(path, test_tbl) { 
  
  model_h2o <- h2o.loadModel(path)
  perf_h2o <- h2o.performance(model_h2o, as.h2o(test_tbl))
  
  perf_h2o %>% 
    h2o.metric() %>% 
    as_tibble() %>% 
    mutate(auc = h2o.auc(perf_h2o)) %>% 
    select(fpr, tpr, auc, precision, recall)
  
}


model_metrics_tbl <- fs::dir_info(path = "04_modeling/h2o_models/") %>% 
  select(path) %>% 
  mutate(metrics = map(path, load_model_performance_metrics, test_tbl)) %>% 
  unnest(cols = c(metrics))

model_metrics_tbl %>% 
  mutate(
    path = str_split(path, pattern = "/", simplify = T)[, 3] %>% as_factor(), 
    auc = auc %>% round(3) %>%  as.character() %>% as_factor()
  ) %>% 
  ggplot(aes(recall, precision, color = path, linetype = auc)) + 
  geom_line(size = 1) + 
  theme_tq() + 
  scale_color_tq() + 
  theme(legend.direction = "vertical") + 
  labs(
    title = "Precision vs. Recall  Plot", 
    subtitle = "Performance of 4 Top Models"
  )


# Gain and Lift 

ranked_predictions_tbl <- predictions_tbl %>% 
  bind_cols(test_tbl) %>% 
  select(predict:Yes, Attrition) %>% 
  arrange(desc(Yes))

calculated_gain_lift_tbl <- ranked_predictions_tbl %>% 
  mutate(ntile = ntile(Yes, n = 10)) %>% 
  group_by(ntile) %>% 
  summarise(
    cases = n(), 
    responses = sum(Attrition == "Yes")
    ) %>% 
  arrange(desc(ntile)) %>% 
  mutate(group = row_number()) %>% 
  select(group, cases, responses) %>% 
  mutate(
    cumulative_responses = cumsum(responses), 
    pct_responses = responses / sum(responses), 
    gain = cumsum(pct_responses), 
    cumulative_pct_cases = cumsum(cases) / sum(cases), 
    lift = gain / cumulative_pct_cases, 
    gain_baseline = cumulative_pct_cases, 
    lift_basesline = gain_baseline / cumulative_pct_cases
    
  )

calculated_gain_lift_tbl

gain_lift_tbl <- performance_h2o %>% 
  h2o.gainsLift() %>% 
  as_tibble()

# capture rate in h2o results is actually the gain

gain_transformed_tbl <-  gain_lift_tbl %>% 
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift ) %>% 
  select(-contains("lift")) %>% 
  mutate(baseline = cumulative_data_fraction) %>% 
  rename(gain = cumulative_capture_rate) %>% 
  gather(key = key, value = value, gain, baseline)  #purpose of gather is to be able to use color and facet_wrap in ggplot


gain_transformed_tbl %>% 
  ggplot(aes(x = cumulative_data_fraction, y = value, color = key))  + 
  geom_line(size = 1.5) + 
  theme_tq() + 
  scale_color_tq() + 
  labs(
    
    title = "Gain Chart", 
    x = "Cumulative Data Fraction", 
    y = "Gain"
    
  )


lift_transformed_tbl <-  gain_lift_tbl %>% 
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift ) %>% 
  select(-contains("capture")) %>% 
  mutate(baseline = 1) %>%  # lift baseline is always 1 gain = cumulative percentage cases
  rename(lift = cumulative_lift) %>% 
  gather(key = key, value = value, lift, baseline) 

lift_transformed_tbl %>% 
  ggplot(aes(x = cumulative_data_fraction, y = value, color = key))  + 
  geom_line(size = 1.5) + 
  theme_tq() + 
  scale_color_tq() + 
  labs(
    
    title = "Lift Chart", 
    x = "Cumulative Data Fraction", 
    y = "Lift"
    
  )

# Performance Visualization ----

h2o_leaderboard <- automl_models_h2o@leaderboard
newdata <- test_tbl 
order_by <- "auc"
max_models <- 4
size <- 1


plot_h2o_performance <- function(h2o_leaderboard, newdata, order_by = c("auc", "logloss"), 
                                 max_models = 3, size = 1.5) {
         
  # Inputs
  
  leaderboard_tbl <- h2o_leaderboard %>%  
    as_tibble() %>% 
    slice(1:max_models)
  
  newdata_tbl <- newdata %>% 
    as_tibble()
  
  order_by <- tolower(order_by[[1]])
  order_by_exp <- rlang::sym(order_by) # converts string into variable (symbol)
  
  h2o.no_progress()  #turn off progress bars from showing
  
  # 1. Model Metrics 
  
  get_model_performance_metrics <- function(model_id, test_tbl) { 
    
    model_h2o <- h2o.getModel(model_id)
    perf_h2o <- h2o.performance(model_h2o, as.h2o(test_tbl))
    
    perf_h2o %>% 
      h2o.metric() %>% 
      as_tibble() %>% 
      mutate(auc = h2o.auc(perf_h2o)) %>% 
      select(threshold, fpr, tpr,  precision, recall)
    
  }
  
  model_metrics_tbl <- leaderboard_tbl %>% 
    mutate(metrics = map(model_id, get_model_performance_metrics, newdata_tbl)) %>% 
    unnest(cols = c(metrics)) %>% 
    mutate(
      model_id = as_factor(model_id) %>% 
        fct_reorder(!! order_by_exp, .desc = ifelse(order_by == "auc", TRUE, FALSE)), 
    auc = auc %>% 
      round(3) %>% 
      as.character() %>% 
      as_factor() %>% 
      fct_reorder(as.numeric(model_id)), 
    logloss = logloss  %>% 
      round(4) %>% 
      as.character() %>% 
      as_factor() %>% 
      fct_reorder(as.numeric(model_id))
       
    )
  
  # 1A. ROC Plot
  
                            
  p1 <- model_metrics_tbl %>% 
    ggplot(aes_string("fpr", "tpr", color = "model_id", linetype = order_by)) +
    geom_line(size = size) + 
    theme_tq() + 
    scale_color_tq() + 
    labs( 
      title = "ROC", 
      x = "TPR", 
      y = "FPR"
      
      ) + 
    theme(legend.direction = "vertical")
  
  # 1B. Precision vs. Recall
    
  p2 <- model_metrics_tbl %>% 
    ggplot(aes_string("recall", "precision", color = "model_id", linetype = order_by)) +
    geom_line(size = size) + 
    theme_tq() + 
    scale_color_tq() + 
    labs( 
      title = "Precision Vs. Recall", 
      x = "Recall", 
      y = "Precision"
      
    ) + 
    theme(legend.position = "none")       
  
  # 2. Gain / Lift 
  
  get_gain_lift <- function(model_id, test_tbl) { 
    
    model_h2o <- h2o.getModel(model_id)
    perf_h2o <- h2o.performance(model_h2o, as.h2o(test_tbl))
    
    perf_h2o %>% 
      h2o.gainsLift() %>% 
      as_tibble() %>% 
      mutate(auc = h2o.auc(perf_h2o)) %>% 
      select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift)
    
  }
  
  
  gain_lift_tbl <- leaderboard_tbl %>% 
    mutate(metrics = map(model_id, get_gain_lift, newdata_tbl)) %>% 
    unnest(cols = c(metrics)) %>% 
    mutate(
      model_id = as_factor(model_id) %>% 
        fct_reorder(!! order_by_exp, .desc = ifelse(order_by == "auc", TRUE, FALSE)), 
      auc = auc %>% 
        round(3) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id)), 
      logloss = logloss  %>% 
        round(4) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id))
      
    ) %>% 
    rename(
      gain = cumulative_capture_rate, 
      lift = cumulative_lift
    )
  
  # 2A. Gain Plot 
  
  p3 <- gain_lift_tbl %>% 
    ggplot(aes_string("cumulative_data_fraction", "gain", color = "model_id", linetype = order_by)) +
    geom_line(size = size) + 
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, 
                 color = "black", size = size) + 
    theme_tq() + 
    scale_color_tq() + 
    expand_limits(x = c(0,1), y = c(0,1)) + 
    labs( 
      title = "Gain",
      x = "Cumulative Data Fraction",
      y = "Gain"
    ) + 
    theme(legend.position = "none")       
  
    
  # 2B. Lift Plot
  
  p4 <- gain_lift_tbl %>% 
    ggplot(aes_string("cumulative_data_fraction", "lift", color = "model_id", linetype = order_by)) +
    geom_line(size = size) + 
    geom_segment(x = 0, y = 1, xend = 1, yend = 1, 
                 color = "black", size = size) + 
    theme_tq() + 
    scale_color_tq() + 
    expand_limits(x = c(0,1), y = c(0,1)) + 
    labs( 
      title = "Lift", 
      x = "Cumulative Data Fraction", 
      y = "Lift"
    ) + 
    theme(legend.position = "none")     
  
  
  # Combine using cowplot
  
  p_legend <- get_legend(p1)
  p1 <- p1 + theme(legend.position = "none")
  
  p <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 2) 
  
  p_title <- ggdraw() + 
    draw_label("H2O Model Metrics", size = 18, fontface = "bold", 
               color = palette_light()[[1]])
  p_subtitle <- ggdraw() + 
    draw_label(glue("Order by {toupper(order_by)}"), size = 10, 
               color = palette_light()[[1]])
  
  ret <- plot_grid(p_title, p_subtitle, p, p_legend, 
                   ncol = 1, rel_heights = c(0.05, 0.05, 1, 0.05 * max_models))
  
  h2o.show_progress()
  
  return(ret)
  
  } 


automl_models_h2o@leaderboard %>% 
  plot_h2o_performance(newdata = test_tbl, order_by = "auc", max_models = 4)
