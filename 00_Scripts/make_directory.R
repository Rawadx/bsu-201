library(fs)


make_project_directories <- function() { 
  
  dir_names <- c(
    "00_Data", 
    "00_Scripts",
    "01_Business_Understanding", 
    "02_Data_Understanding",
    "03_Data_Preparation", 
    "04_Modeling",
    "05_Evaluating",
    "06_Deployment")
  
  dir_create(dir_names)
  
  }