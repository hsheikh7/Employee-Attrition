
#Load Libraries 
library(tidyverse)
library(tidyquant)
library(readxl)
library(skimr)
library(GGally)
library(magrittr)

# Load data
employee_attrition_tbl <- read_csv("data/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.txt")
definitions_raw_tbl <- read_excel('data/data_definitions.xlsx', sheet = 1, col_names = FALSE)

definitions_raw_tbl

glimpse(employee_attrition_tbl)
 
# Exploratory Data Analysis (EDA) 
 
# Step 1: Data Summarization 
 
skim(employee_attrition_tbl)
 
 
# Character Data Type
employee_attrition_tbl %>%
 select_if(is.character) %>%
 glimpse()
 
employee_attrition_tbl %>%
 select_if(is.character) %>%
 map(unique)

employee_attrition_tbl %>%
 select_if(is.character) %>%
 map(~ table(.) %>% prop.table())
 
# Numeric Data
employee_attrition_tbl %>%
 select_if(is.numeric) %>%
 map(~ unique(.) %>% length())
 
 
employee_attrition_tbl %>%
 select_if(is.numeric) %>%
 map_df(~ unique(.) %>% length()) %>%
 gather() %>%
 arrange(value) %>%
 filter(value <= 10)
 
 
# Step 2: Data Visualization 
 
employee_attrition_tbl %>%
   select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
   ggpairs() 
 
employee_attrition_tbl %>%
   select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
   ggpairs(aes(color = Attrition), lower = "blank", legend = 1,
           diag  = list(continuous = wrap("densityDiag", alpha = 0.5))) +
   theme(legend.position = "bottom")
 
 
data <- employee_attrition_tbl %>%
 select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome)
 
 plot_ggpairs <- function(data, color = NULL, density_alpha = 0.5) {
   
   color_expr <- enquo(color)
   
   if (rlang::quo_is_null(color_expr)) {
     
     g <- data %>%
       ggpairs(lower = "blank") 
     
   } else {
     
     color_name <- quo_name(color_expr)
     
     g <- data %>%
       ggpairs(mapping = aes_string(color = color_name), 
               lower = "blank", legend = 1,
               diag = list(continuous = wrap("densityDiag", 
                                             alpha = density_alpha))) +
       theme(legend.position = "bottom")
   }
   
   return(g)
   
 }
 
employee_attrition_tbl %>%
 select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
 plot_ggpairs(color = Attrition)

 
 # Explore Features 
 
# Compensation Features
# 1. Question: What can you deduce about the interaction between Monthly Income and Attrition?
 #C: Those that are leaving have a lower Monthly Income

employee_attrition_tbl %>%
  select(Attrition, contains("income"), contains("rate"), contains("salary"), contains("stock")) %>%
  plot_ggpairs(Attrition)

# 2. Question: What can you deduce about the interaction between Percent Salary Hike and Attrition?
# C: Those that are leaving have lower Percent Salary Hike

# Compensation Features
# 3. Question: What can you deduce about the interaction between Stock Option Level and Attrition?
# C: It's difficult to deduce anything based on the visualization
# As you see the chart, there are some fluctuations which need more examination. 

# Survey Results
# 4. Question: What can you deduce about the interaction between Environment Satisfaction and Attrition?
 # C: It's difficult to deduce anything based on the visualization

employee_attrition_tbl %>% 
 select(Attrition, contains("satisfaction"), contains("life") ) %>% 
 plot_ggpairs(color = Attrition)
 
# 5. Question: What can you deduce about the interaction between Work Life Balance and Attrition
# D: It's difficult to deduce anything based on the visualization

# Performance Data
employee_attrition_tbl %>% 
  select(Attrition, contains("performance"), contains("involvement")) %>% 
  plot_ggpairs(color = Attrition)
# 6. Question: What Can you deduce about the interaction between Job Involvement and Attrition?
# B: Those that are leaving have a lower density of 1's and 2's

 
# Work-Life Features
employee_attrition_tbl %>%
  select(Attrition, contains("overtime"), contains("travel")) %>%
  plot_ggpairs(Attrition)
# 7. Question: What can you deduce about the interaction between Over Time and Attrition?
# B: The proportion of those staying that are working Over Time are high compared to those that are not staying

# Training and Education
employee_attrition_tbl %>%
  select(Attrition, contains("training"), contains("education")) %>%
  plot_ggpairs(Attrition)
# 8. Question: What can you deduce about the interaction between Training Times Last Year and Attrition
# C: It's difficult to deduce anything based on the visualization

# Time-Based Features
employee_attrition_tbl %>%
 select(Attrition, contains("years")) %>%
 plot_ggpairs(Attrition)
 
# 9.Question: What can you deduce about the interaction between Years At Company and Attrition
# B: People that leave tend to have less working years at the company
 
# 10.Question: What can you deduce about the interaction between Years Since Last Promotion and Attrition?
# B: Those that are leaving have fewer years since last promotion than those that are staying 
 


