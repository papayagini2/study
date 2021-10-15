rm(list=ls())

library(tidyverse)
library(magrittr)
library(caret)
library(skimr)
library(funModeling)

setwd("D:/#000.자격증공부/#003. ADP실기/각종주제별코드/R과외/study/Pima_Indian/")

read.csv("diabetes.csv") -> diabetes

diabetes %>%  glimpse()

diabetes %>% mutate(Outcome = factor(Outcome, 
                                     levels = c(0,1),
                                     labels = c("No", "Yes"))) -> diabetes

# EDA: 목적 변수 
diabetes %>%  names()
# [1] "Pregnancies"              "Glucose"                  "BloodPressure"            "SkinThickness"           
# [5] "Insulin"                  "BMI"                      "DiabetesPedigreeFunction" "Age"                     
# [9] "Outcome" 

library(ggplot2)
diabetes$Outcome %>%  table() %>%  prop.table() %>%  round(., 3)
diabetes %>% ggplot() + geom_bar() + 
  aes(x=Outcome, fill = Outcome)

# imbalanced data 
diabetes %>%  plot_num()
funModeling::cross_plot(data = diabetes, input = diabetes[, 1:8],
                        target =  diabetes[,9] ) 
                            
                        
