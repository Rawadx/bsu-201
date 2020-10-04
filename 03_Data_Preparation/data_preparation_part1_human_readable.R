# DATA PREPARATION ----
# Human Readable ----

# Libraries 

library(tidyverse)
library(readxl)
library(tidyquant)
library(forcats)
library(stringr)


#Load Data

path_train <- "00_Data/telco_train.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = , col_names = FALSE ) 

# Tidying the Data ----

glimpse(train_raw_tbl)
View(definitions_raw_tbl)

definitions_tbl <- definitions_raw_tbl %>% 
  fill(...1, .direction = "down") %>% 
  filter(!is.na(...2)) %>% 
  separate(...2, into= c("key", "value"), sep = " '", remove = TRUE) %>% 
  rename(column_name = ...1) %>% 
  mutate(key = as.numeric(key)) %>% 
  mutate(value = value %>% str_replace(pattern = "'", replacement = "")) 

definitions_tbl %>% View()

definitions_list <- definitions_tbl %>% 
  split(.$column_name) %>% 
  map(~ select(., -column_name)) %>% 
  map(~ mutate(., value = as_factor(value)))

definitions_list[[1]]  %>% View()

for(i in seq_along(definitions_list)) { 
  
  list_name <- names(definitions_list)[i] #pulling defintion names 
  colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, "_value")) #assigning new definition names
  
}

definitions_list[[1]]  %>% View()

data_merged_tbl <- list(HR_DATA = train_raw_tbl) %>% 
  append(definitions_list, after = 1) %>% 
  reduce(left_join) %>% 
  select(-one_of(names(definitions_list))) %>%   #remove unnecessary definitions columns
  set_names(str_replace_all(names(.), pattern = "_value", replacement = ""))  %>% 
  select(sort(names(.))) 

data_merged_tbl %>% glimpse() %>% Value()

data_merged_tbl %>% 
  select_if(is.character) %>% View()

data_merged_tbl %>% 
  distinct(BusinessTravel) %>% View()

data_processed_tbl <- data_merged_tbl %>% 
  mutate_if(is.character, as_factor) %>% 
 # select_if(is.factor) %>% 
 # map(levels) %>% View()
  mutate(
    
    BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", "Travel_Rarely", "Travel_Frequently"), 
    MaritalStatus = MaritalStatus %>% fct_relevel("Single", "Married", "Divorced")
      
  )

data_processed_tbl %>% 
  select_if(is.factor) %>% 
  map(levels) %>% 
  View()

# Processing Pipeline ----

process_hr_data_readable <- function(data, definitions_tbl) { 
  
  definitions_list <- definitions_raw_tbl %>% 
    fill(...1, .direction = "down") %>% 
    filter(!is.na(...2)) %>% 
    separate(...2, into= c("key", "value"), sep = " '", remove = TRUE) %>% 
    rename(column_name = ...1) %>% 
    mutate(key = as.numeric(key)) %>% 
    mutate(value = value %>% str_replace(pattern = "'", replacement = ""))  %>% 
    split(.$column_name) %>% 
    map(~ select(., -column_name)) %>% 
    map(~ mutate(., value = as_factor(value))) %>% View()
  
  
  for(i in seq_along(definitions_list)) { 
    list_name <- names(definitions_list)[i] #pulling defintion names 
    colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, "_value")) #assigning new definition names
  }

  data_merged_tbl <- list(HR_DATA = data) %>% # replaced train_raw_tbl with data
    append(definitions_list, after = 1) %>% 
    reduce(left_join) %>% 
    select(-one_of(names(definitions_list))) %>%   #remove unnecessary definitions columns
    set_names(str_replace_all(names(.), pattern = "_value", replacement = ""))  %>% 
    select(sort(names(.)))  %>% 
    mutate_if(is.character, as_factor) %>% 
    # select_if(is.factor) %>% 
    # map(levels) %>% View()
    mutate(
      BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", "Travel_Rarely", "Travel_Frequently"), 
      MaritalStatus = MaritalStatus %>% fct_relevel("Single", "Married", "Divorced")
    )
  
  return(data_merged_tbl)
  
  }