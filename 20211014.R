rm(list = ls())

library(tidyverse)
library(magrittr)


data("mtcars")
mtcars %>%  glimpse()


mtcars %>%  names()
# [1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear" "carb"

mtcars %>% select(mpg, cyl) %>%   plot()

Titanic %>%  glimpse()
Titanic %>%  head()

Titanic %>%  as.data.frame() -> df1
table(df1$Sex, df1$Survived)

###

InsectSprays %>%  names()
InsectSprays %>%  glimpse()


InsectSprays %>%  group_by(spray) %>%  summarise(sum(count))
aggregate(count ~ spray, data = InsectSprays, sum)

iris %>%  select(1:4)


set.seed(1)
rnorm(10000,0,1) %>%  plot()

install.packages("doBy")
library(doBy)

set.seed(1) 
rnorm(10, 0, 1) %>%  as.data.frame() -> dd
names(dd) <- c("col")
dd  %>%  arrange(desc(col))

iris %>% names()
# [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"   
iris$Species %>%  table()

iris %>%  filter(Species == "setosa") %>% which()

airquality %>%  is.na() %>%  which()

which(!is.na(airquality))


InsectSprays %>%  group_by(spray) %>%  summarise(n = mean(count))
InsectSprays %>%  aggregate()
?aggregate()

chickwts %>%  
aggregate(weight ~ feed, data = chickwts, mean)
