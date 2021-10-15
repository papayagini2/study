rm(list = ls())

library(tidyverse)
library(magrittr)

setwd("D:/#000.자격증공부/#003. ADP실기/각종주제별코드/R과외/study/작업형1")

left_join( read.csv("X_train.csv") ->  X_train,
           read.csv("y_train.csv") ->  y_train ) %>%
  mutate(index = "train", .before = 1) -> train

read.csv("X_test.csv")  %>%  mutate(index = "test", .before = 1) ->  X_test

bind_rows(train, X_test) -> full

full %>% glimpse()

# EDA 데이터 탐색 

full %>% filter(index == "train") -> train 
full %>% filter(index == "test")  -> test 

full %>%  names()

full$'총구매액'      -> full$total 
full$'최대구매액'    -> full$max
full$'환불금액'   -> full$refund
full$'주구매상품' -> full$product
full$'주구매지점' -> full$store
full$'내점일수'   -> full$days
full$'내점당구매건수' -> full$counts
full$'주말방문비율'   -> full$week
full$'구매주기'       -> full$cycle 

full %>%  names()
full %>%  glimpse()

full %>%  dplyr::select(index, cust_id, gender, 
                        total, max, refund, product, store, 
                        days, counts, week, cycle) -> full 

full$index     %<>%  as.factor()
full$product   %<>%  as.factor()
full$store     %<>%  as.factor()
full$gender    %<>%  as.factor()

full %>%  filter(index == "train") -> train 
full %>%  filter(index == "test")  -> test 

# 목적변수 탐색 
full %>%  names()
library(ggplot2)
full %>%  ggplot() + 
  geom_bar(aes(x=gender, fill = gender))  
train %>%  ggplot() + 
  geom_bar(aes(x=gender, fill = gender))  

# NA 분포.. 
full  %>%  is.na() %>%  colSums()
train %>%  is.na() %>%  colSums()
test  %>%  is.na() %>%  colSums()

# 목적변수와 X인자간의 상관성 분석 



