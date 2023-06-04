
#Load Libraries 
library(tidyverse)
library(tidyquant)
library(readxl)
library(skimr)
library(GGally)
#Read Data 

 
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
 
# 1. Compensation Features
# Question: What can you deduce about the interaction between Monthly Income and Attrition?
# Those that are leaving the company have a higher Monthly Income
# That those are staying have a lower Monthly Income
# Those that are leaving have a lower Monthly Income
# It's difficult to deduce anything based on the visualization
 employee_attrition_tbl %>%
  select(Attrition, contains("income"), contains("rate"), contains("salary"), contains("stock")) %>%
  plot_ggpairs(Attrition)


# 2. Compensation Features
# Question: What can you deduce about the interaction between Percent Salary Hike and Attrition?
# Those that are leaving the company have a higher Percent Salary Hike
# Those that are staying have a lower Percent Salary Hike
# Those that are leaving have lower Percent Salary Hike
# It's difficult to deduce anything based on the visualization

# 3. Compensation Features
# Question: What can you deduce about the interaction between Stock Option Level and Attrition?
# Those that are leaving the company have a higher stock option level
# Those that are staying have a higher stock option level
# It's difficult to deduce anything based on the visualization
 employee_attrition_tbl %>%
  select(Attrition, contains("income"), contains("rate"), contains("salary"), contains("stock")) %>%
  plot_ggpairs(Attrition)
 
#4. Survey Results
# Question: What can you deduce about the interaction between Environment Satisfaction and Attrition?
# A higher proportion of those leaving have a low environment satisfaction level
# A higher proportion of those leaving have a high environment satisfaction level
# It's difficult to deduce anything based on the visualization



# 5. Survey Results
# Question: What can you deduce about the interaction between Work Life Balance and Attrition
# Those that are leaving have higher density of 2's and 3's
# Those that are staying have a higher density of 2's and 3's
# Those that are staying have a lower density of 2's and 3's
# It's difficult to deduce anything based on the visualization

 employee_attrition_tbl %>%
  select(Attrition, contains("satisfaction"), contains("life")) %>%
  plot_ggpairs(Attrition)


# 6. Performance Data
# Question: What Can you deduce about the interaction between Job Involvement and Attrition?
# Those that are leaving have a lower density of 3's and 4's
# Those that are leaving have a lower density of 1's and 2's
# Those that are staying have a lower density of 2's and 3's
# It's difficult to deduce anything based on the visualization

 employee_attrition_tbl %>%
  select(Attrition, contains("performance"), contains("involvement")) %>%
  plot_ggpairs(Attrition)

# 7. Work-Life Features
# Question: What can you deduce about the interaction between Over Time and Attrition?
#The proportion of those leaving that are working Over Time are high compared to those that are not leaving
# The proportion of those staying that are working Over Time are high compared to those that are not staying

 employee_attrition_tbl %>%
  select(Attrition, contains("overtime"), contains("travel")) %>%
  plot_ggpairs(Attrition)

# 8. Training and Education
# Question: What can you deduce about the interaction between Training Times Last Year and Attrition
# People that leave tend to have more annual trainings
# People that leave tend to have less annual trainings
# It's difficult to deduce anything based on the visualization

 employee_attrition_tbl %>%
  select(Attrition, contains("training"), contains("education")) %>%
  plot_ggpairs(Attrition)

# 9. Time-Based Features
# Question: What can you deduce about the interaction between Years At Company and Attrition
# People that leave tend to have more working years at the company
# People that leave tend to have less working years at the company
# It's difficult to deduce anything based on the visualization

#   8. Time-Based Features: Years at company, years in current role
 employee_attrition_tbl %>%
  select(Attrition, contains("years")) %>%
  plot_ggpairs(Attrition)

# 10. Time-Based Features
# Question: What can you deduce about the interaction between Years Since Last Promotion and Attrition?
#   Those that are leaving have more years since last promotion than those that are staying
# Those that are leaving have fewer years since last promotion than those that are staying
# It's difficult to deduce anything based on the visualization
 
 
 
 