# BUSINESS UNDERSTANDING ----

#libraries 

library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)

#load Data
path_train <- "00_Data//telco_train.xlsx"
train_raw_tbl <- readxl::read_xlsx(path_train, sheet = 1)


#Data Subset

dep_job_role_tbl <- train_raw_tbl %>% 
  select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

dep_job_role_tbl

# 1. Business Science Problem Framework ----
# 1A. View Business As Machine ----

# BSU's: departments and Job Role
# Define Objectives: Retain High Performers
# Assess Outcomes: TBD

dep_job_role_tbl %>% 
  group_by(Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pct = n/sum(n)) %>% 
  View()


# 1B. Understand the Drivers -----

# Investigate the objectives: 16% Attrition 

# Synthesize Outcomes: High Counts and High Percentages    

# Hypothesize Drivers: Job Role and Department
# Department ----

dep_job_role_tbl %>% 
  
  group_by(Department, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  
  group_by(Department) %>% 
  mutate(pct = n/sum(n)) %>% 
  View()

# Job Role ----

dep_job_role_tbl %>% 
  
  group_by(Department, JobRole, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  
  group_by(Department, JobRole) %>% 
  mutate(pct = n/sum(n)) %>% 
  ungroup() %>% 
  
  filter(Attrition %in% "Yes") %>% 
  View()

# 1c. Measure the Drivers ----

# Collect Information on Employee Attrition: Ongoing 

# Develop KPIs: Industry KPI: 8.8%

dep_job_role_tbl %>% 
  
  group_by(Department, JobRole, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  
  group_by(Department, JobRole) %>% 
  mutate(pct = n/sum(n)) %>% 
  ungroup() %>% 
  
  filter(Attrition %in% "Yes") %>% 
  arrange(desc(pct)) %>% 
  mutate(
    above_industry_average = case_when(
      pct > 0.088 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>% 
  View()

# 1D. Uncover Problems and Opportunities ----

calculate_attrition_cost <- function(
  
  #Employee
  n = 1, 
  salary = 80000, 
  
  #Direct Costs
  seperation_cost = 500, 
  vacancy_cost = 10000,
  acquisition_cost = 4900,
  placement_cost = 3500,
  
  #Productivity Costs
  net_revenue_per_employee = 250000,
  workdays_per_year = 240, 
  workdays_position_open = 40, 
  workdays_onboarding = 60, 
  onboarding_efficiency = 0.5
  
){
  
  # Direct Costs
  direct_cost <- sum(seperation_cost, vacancy_cost, acquisition_cost, placement_cost)
  
  # Productivity Costs
  productivity_cost <- net_revenue_per_employee/ workdays_per_year * 
                           (workdays_position_open + workdays_onboarding * onboarding_efficiency)
  
  # Savings of Salary and Benefits (Cost Reduction)
  salary_benefit_reduction <- salary/workdays_per_year * workdays_position_open
  
  # Estimated Turnover per Employee
  cost_per_employee = direct_cost + productivity_cost - salary_benefit_reduction
  
  #Total Cost of Employee Turnover
  total_cost <- n * cost_per_employee
  
  return(total_cost)
  
  
}

# Calculating Cost By Job Role ----


dep_job_role_tbl %>% 
  
  group_by(Department, JobRole, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  
  group_by(Department, JobRole) %>% 
  mutate(pct = n/sum(n)) %>% 
  ungroup() %>% 
  
  filter(Attrition %in% "Yes") %>% 
  arrange(desc(pct)) %>% 
  mutate(
    above_industry_average = case_when(
      pct > 0.088 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>% 
  
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)
  ) %>% 
  
  View()

# Workflow of Attrition ----

dep_job_role_tbl %>% 
  
  count(Department, JobRole, Attrition) %>% # replaces below code 
  
#
#  group_by(Department, JobRole, Attrition) %>% 
#  summarise(n = n()) %>% 
#  ungroup() %>% 
#  

  count_to_pct(Department, JobRole) %>% View()# replaces below code 
  
#  
# group_by(Department, JobRole) %>% 
# mutate(pct = n/sum(n)) %>% 
# ungroup() %>% 
#  
  filter(Attrition %in% "Yes") %>% 
  arrange(desc(pct)) %>% 
  mutate(
    above_industry_average = case_when(
      pct > 0.088 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>% 
  
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)
  ) %>% 
  
  View()

# function count_to_pct starts
  
count_to_pct <- function(data, ..., col = n) { 
  
  grouping_vars_expr <- quos(...)
  col_expr <- enquo(col)
  
  ret <- data %>% 
    group_by(!!!grouping_vars_expr) %>% 
    mutate(pct = !!col_expr/sum(!!col_expr)) %>% 
    ungroup() 
  
  
  # alternative to enquo using {{}} operator
  # group_by(!!! grouping_vals_expr) %>% 
  #   mutate(pct = ({{col}}) / sum({{col}})) %>%
  #   ungroup()
    
  return(ret)
  
}
# function count_pct ends

dep_job_role_tbl %>% 
  
  count(Department, JobRole, Attrition) %>% 
  
  count_to_pct(Department, JobRole) %>% 


filter(Attrition %in% "Yes") %>% 
  arrange(desc(pct)) %>% 
  mutate(
    above_industry_average = case_when(
      pct > 0.088 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>% 
  
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)
  ) %>% 
  
  View()

# function assess_attrition starts

assess_attrition <- function(data, attrition_col, attrition_value, baseline_pct) { 
  
  attrition_col_exp <- enquo(attrition_col)
  
  data %>% filter((!!attrition_col_exp) %in% attrition_value) %>% 
    arrange(desc(baseline_pct)) %>% 
    mutate(
      above_industry_average = case_when(
        pct > baseline_pct ~ "Yes",
        TRUE ~ "No"
      )
    ) 
  
  }
# function assess_attrition ends

dep_job_role_tbl %>% 
  
  count(Department, JobRole, Attrition) %>% 
  
  count_to_pct(Department, JobRole) %>% 
  
  assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = 0.088 ) %>% #replaces below code
  
  # filter(Attrition %in% "Yes") %>% 
  # arrange(desc(pct)) %>% 
  # mutate(
  #   above_industry_average = case_when(
  #     pct > 0.088 ~ "Yes",
  #     TRUE ~ "No"
  #   )
  # ) %>% 
  
 
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)
  ) %>%
  
  View()

# Visualization of the Attrition Cost ----

dep_job_role_tbl %>% 
  
  count(Department, JobRole, Attrition) %>% 
  
  count_to_pct(Department, JobRole) %>% 
  
  assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = 0.088 ) %>% #replaces below code
  
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)
  ) %>% 
  
  # Data Manipulation
  
  mutate(name = str_c(Department, JobRole, sep = ":") %>% as.factor()) %>% 
  mutate(name = fct_reorder(name, cost_of_attrition)) %>% 
  mutate(cost_text = str_c("$", format(cost_of_attrition / 1e6, digits = 2),
                           "M", sep = "")) %>% 
  # Plotting 
  
  ggplot(aes(x = cost_of_attrition, y = name)) + 
  geom_segment(aes(xend = 0, yend = name ), color = palette_light()[[1]]) +
  geom_point(aes(size = cost_of_attrition), color = palette_light()[[1]]) +
  scale_x_continuous(labels = scales::dollar) +
  geom_label(aes(label = cost_text, size = cost_of_attrition), 
             hjust = "inward", color = palette_light()[[1]]) +
  theme_tq() + 
  scale_size(range = c(3, 5)) + 
  labs(title = "Estimated Cost of Attrition: By Dept and Job Role", 
       x = "Cost of Attrition", y = "") + 
  theme(legend.position = "none")


  # plotting_attrition Function 

  plot_attrition <- function(data, ..., .value, 
                             fct_reorder = TRUE, 
                             fct_rev = FALSE, 
                             include_lbl = TRUE, 
                             color = palette_light()[[1]], 
                             units = c("0" , "K", "M")) { 
    
    
    # inputs
    
    group_vars_expr  <- quos(...)
    
    if(length(group_vars_expr) == 0)
      group_vars_expr <- quos(rlang::sym(colnames(data)[[1]])) #transforms string into symbol
    
    value_expr <- enquo(.value)  #handles quosure
    value_name <- quo_name(value_expr)  #handles text of the quosure (return character value that is needed for aes_string() function)
    
    unit_val <- switch (units[[1]],
      "M" = 1e6, 
      "K" = 1e3, 
      "0" = 1)
    if(units[[1]] == "0") units <- ""
    
    
    # Data Manipulation
    
    usd <- scales::dollar_format(prefix = "$", largest_with_cents = 1e3)
    
    
    data_manipulated <- data %>% 
      
    mutate(name = str_c(!!! group_vars_expr, sep = ": ") %>% as_factor()) %>% 
    mutate(value_text = str_c(usd((!! value_expr) / unit_val),
                              units[[1]], sep = "")) 
    
    if(fct_reorder) {

      data_manipulated <- data_manipulated %>%
        mutate(name = forcats::fct_reorder(name, (!! value_expr))) %>%
        arrange(name)
    }


    if(fct_rev) {

      data_manipulated <- data_manipulated %>%
        mutate(name = fct_rev(name)) %>%
        arrange(name)
    }

      
  
    #Visualization
    
    g <- data_manipulated %>%   
      ggplot(aes_string(x = value_name, y = "name")) + 
      geom_segment(aes(xend = 0, yend = name ), color = color) +
      geom_point(aes_string(size = value_name), color = color) +
      scale_x_continuous(labels = scales::dollar) +
      theme_tq() + 
      scale_size(range = c(3, 5)) + 
      theme(legend.position = "none")
    
    if(include_lbl) { 
      
      g <- g + 
        geom_label(aes_string(label = "value_text", size = value_name), 
                   hjust = "inward", color = color)
      
    }
    
    return(g)
    
  }
  
  
  dep_job_role_tbl %>%
    
    count(Department, JobRole, Attrition) %>% 
    
    count_to_pct(Department,JobRole) %>% 
    
    assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = 0.088 ) %>% #replaces below code
   
    mutate(
      cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)
    ) %>% 
    plot_attrition(Department, JobRole, .value = cost_of_attrition, 
                   units = "M") + 
    
    labs( 
      
      title = "Estimatd Cost of Attrition by Department and Job Role", 
      x = "Cost of Attrition", 
      subtitle = "Looks like Sales Executives and Laboratory Technician are biggest drivers of cost"
      )
    
                             