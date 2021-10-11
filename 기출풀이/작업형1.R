rm(list=ls())
sessionInfo()
library(tidyverse); library(magrittr); 

# mtcars 의 qsec 최대/최소 정규화 후 0.5 이상의 행 갯수 구하시오.  
data("mtcars")

mtcars %>%  names()
# [1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear" "carb"

mtcars$qsec %>% min() -> min
mtcars$qsec %>% max() -> max 

mtcars %>%  mutate(qsec_norm = (qsec-min)/(max-min), .before = 1) %>% 
       filter(qsec_norm > 0.5) %>%  nrow()
