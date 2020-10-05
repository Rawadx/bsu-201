
# Bonus: Grid Search and CV -----
h2o.init()

deep_learning_h2o <- h2o.loadModel("04_modeling/h2o_models/DeepLearning_grid__1_AutoML_20201005_000040_model_1")

deep_learning_h2o

test_tbl

h2o.performance(deep_learning_h2o, newdata = as.h2o(test_tbl))

deep_learning_grid_01 <- h2o.grid(
  
  algorithm = "deeplearning", 
  grid_id = "deep_learning_grid_01", 
  
  # H2o Deep learning
  x = x, 
  y = y, 
  training_frame = train_h2o, 
  validation_frame = valid_h2o, 
  nfolds = 5,
  
  hyper_params = list(
    
    hidden = list(c(10, 10, 10), c(50,20,10), c(20,20,20)), 
    epochs = c(10,50,100)
    
  )
  
) 

deep_learning_grid_01

h2o.getGrid(grid_id = "deep_learning_grid_01", sort_by = "auc", decreasing = TRUE)

DeepLearning_grid__1 <- h2o.getModel("deep_learning_grid_01_model_7")

DeepLearning_grid__1 %>% h2o.auc(train = T, valid = T, xval = T)

DeepLearning_grid__1 %>% 
  h2o.performance(newdata = as.h2o(test_tbl))
