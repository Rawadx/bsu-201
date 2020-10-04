# HOMEWORK 1 ----

# Libraries ----
library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)

# Source Scripts ----
source("00_Scripts/assess_attrition.R")

# Data ----
path_train     <- "00_Data/telco_train.xlsx"
train_raw_tbl  <- read_excel(path_train, sheet = 1)

dept_jobrole_tbl <- train_raw_tbl %>%
    select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition) %>% View()

kpi_industry_turnover_pct <- 0.088

# Productivity Cost by Role ----

productivity_cost_by_role_tbl <- read_excel("00_Data/productivity_cost_by_role.xlsx")
productivity_cost_by_role_tbl %>% View()


# Q1: Which Job Role has the highest total cost of attrition? ----

complete_data_tbl <-  inner_join(train_raw_tbl, productivity_cost_by_role_tbl, by = c("Department", "JobRole"))  
complete_data_tbl %>% View()

inner_join(train_raw_tbl, productivity_cost_by_role_tbl, by = c("Department", "JobRole")) %>%
  count(Department, JobRole, Attrition, Salary_Average, EmployeeCount, Revenue_Average) %>% 
  filter(Attrition %in% "Yes") %>% 
  mutate(cost_of_attrition = calculate_attrition_cost(n = n, salary = Salary_Average, net_revenue_per_employee = Revenue_Average)) %>% 
  arrange(desc(cost_of_attrition)) %>% View()
  

# Q2: What is the total cost of attrition for the Research & Development: Research Scientist job role? ----

inner_join(train_raw_tbl, productivity_cost_by_role_tbl, by = c("Department", "JobRole")) %>%
  count(Department, JobRole, Attrition, Salary_Average, EmployeeCount, Revenue_Average) %>% 
  filter(Attrition %in% "Yes") %>% 
  mutate(cost_of_attrition = calculate_attrition_cost(n = n, salary = Salary_Average, net_revenue_per_employee = Revenue_Average)) %>% 
  arrange(desc(cost_of_attrition)) %>% 
  filter(Department %in% "Research & Development" & JobRole %in% "Research Scientist") %>% 
  View()


# Q3: What percentage do the top four Job Roles account for in terms of the total cost of attrition? ----
inner_join(train_raw_tbl, productivity_cost_by_role_tbl, by = c("Department", "JobRole")) %>%
  count(Department, JobRole, Attrition, Salary_Average, EmployeeCount, Revenue_Average) %>% 
  filter(Attrition %in% "Yes") %>% 
  mutate(cost_of_attrition = calculate_attrition_cost(n = n, salary = Salary_Average, net_revenue_per_employee = Revenue_Average)) %>%
  mutate(pct = cost_of_attrition / sum (cost_of_attrition)) %>% 
  arrange(desc(cost_of_attrition)) %>% 
  head(n = 4) %>% 
  summarise(sum_top_4 = sum(pct)) %>% 
  View()

# Q4. Which Department has the highest total cost of attrition? ----

inner_join(train_raw_tbl, productivity_cost_by_role_tbl, by = c("Department", "JobRole")) %>%
  count(Department, Attrition, Salary_Average, Revenue_Average) %>% 
  filter(Attrition %in% "Yes") %>% 
  mutate(cost_of_attrition = calculate_attrition_cost(n = n, salary = Salary_Average, net_revenue_per_employee = Revenue_Average)) %>%
  mutate(pct = cost_of_attrition / sum (cost_of_attrition)) %>% 
  group_by(Department) %>%  
  summarize(Total_cost = sum(cost_of_attrition)) %>% 
  arrange(desc(Total_cost)) %>% View()


# Q5: What percentage does the top Department account for in terms of the total cost of attrition? ----


inner_join(train_raw_tbl, productivity_cost_by_role_tbl, by = c("Department", "JobRole")) %>%
  count(Department, Attrition, Salary_Average, Revenue_Average) %>% 
  filter(Attrition %in% "Yes") %>% 
  mutate(cost_of_attrition = calculate_attrition_cost(n = n, salary = Salary_Average, net_revenue_per_employee = Revenue_Average)) %>%
  mutate(pct = cost_of_attrition / sum (cost_of_attrition)) %>% 
  group_by(Department) %>%  
  summarize(Total_cost = sum(cost_of_attrition)) %>% 
  mutate(pct_total = Total_cost / sum (Total_cost)) %>% 
  arrange(desc(Total_cost)) %>% View()
