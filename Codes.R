
#Load Libraries 
library(tidyverse)
library(tidyquant)
library(readxl)
library(skimr)
library(GGally)

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

employee_attrition_tbl %>%
  select(Attrition, contains("income"), contains("rate"), contains("salary"), contains("stock")) %>%
  plot_ggpairs(Attrition)

employee_attrition_tbl %>% select(DailyRate, HourlyRate, MonthlyIncome, MonthlyRate, PercentSalaryHike, StockOptionLevel)
 
# 2. Compensation Features
# Question: What can you deduce about the interaction between Percent Salary Hike and Attrition?

# 3. Compensation Features
# Question: What can you deduce about the interaction between Stock Option Level and Attrition?

 employee_attrition_tbl %>%
  select(Attrition, contains("income"), contains("rate"), contains("salary"), contains("stock")) %>%
  plot_ggpairs(Attrition)
 
#4. Survey Results
# Question: What can you deduce about the interaction between Environment Satisfaction and Attrition?

# 5. Survey Results
# Question: What can you deduce about the interaction between Work Life Balance and Attrition

 employee_attrition_tbl %>%
  select(Attrition, contains("satisfaction"), contains("life")) %>%
  plot_ggpairs(Attrition)


# 6. Performance Data
# Question: What Can you deduce about the interaction between Job Involvement and Attrition?

 employee_attrition_tbl %>%
  select(Attrition, contains("performance"), contains("involvement")) %>%
  plot_ggpairs(Attrition)

# 7. Work-Life Features
# Question: What can you deduce about the interaction between Over Time and Attrition?

 employee_attrition_tbl %>%
  select(Attrition, contains("overtime"), contains("travel")) %>%
  plot_ggpairs(Attrition)

# 8. Training and Education
# Question: What can you deduce about the interaction between Training Times Last Year and Attrition

 employee_attrition_tbl %>%
  select(Attrition, contains("training"), contains("education")) %>%
  plot_ggpairs(Attrition)

# 9. Time-Based Features
# Question: What can you deduce about the interaction between Years At Company and Attrition

# 8. Time-Based Features: Years at company, years in current role
 employee_attrition_tbl %>%
  select(Attrition, contains("years")) %>%
  plot_ggpairs(Attrition)

# 10. Time-Based Features
# Question: What can you deduce about the interaction between Years Since Last Promotion and Attrition?

 