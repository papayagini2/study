# 목적: ADP 스터디 발표 8월 1일(일), 
# 발표 주제: ADP 출제기관 빅분기 문제 유형 학습과 caret, recipes 학습 
# 빅분기, ADP -> 모두 진흥원 
# 빅분기 예시문제와 동일하게 나왔음. 

sessionInfo()
library(tidyverse,    verbose = FALSE, warn.conflicts = FALSE)
library(magrittr,     verbose = FALSE, warn.conflicts = FALSE)
library(recipes,      verbose = FALSE, warn.conflicts = FALSE)
library(caret,        verbose = FALSE, warn.conflicts = FALSE)
sessionInfo()

rm(list=ls())
options(sipen = 100) # 지수승 표현 안하도록 
setwd("D:/RPrj/Rkaggle/Rkaggle/스터디_8월1일_caret발표/data")
# read.csv(), data.table::fread(), readr::read_csv()

####### 데이터 로딩 ###########
left_join( read.csv("X_train.csv") ->  X_train,
           read.csv("y_train.csv") ->  y_train ) %>%
          mutate(index = "train") -> train
read.csv("X_test.csv")  %>%  mutate(index = "test") ->  X_test
bind_rows(train, X_test) -> data

data$gender   <-  ifelse(data$gender   == "1", "남성", "여성")
data$gender %<>%  as.factor()
data$index  %<>%  as.factor()

# write.csv(data, "data.csv")
# read.csv("data.csv", stringsAsFactors =TRUE) -> data 

# 한글은 처리하기 불편하므로, 영어로 변경
# full <- janitor::clean_names(full)
data$'총구매액'   -> data$total 
data$'최대구매액' -> data$max
data$'환불금액'   -> data$refund
data$'주구매상품' -> data$product
data$'주구매지점' -> data$store
data$'내점일수'   -> data$days
data$'내점당구매건수' -> data$counts
data$'주말방문비율'   -> data$week
data$'구매주기'       -> data$cycle 

data %>%  dplyr::select(index, cust_id, gender, 
                 total, max, refund, product, store, 
                 days, counts, week, cycle) -> data 

data %>%  filter(index=="train") -> train
data %>%  filter(index=="test")  -> test 

# y값부터 탐색 
data$gender %>%  table() 
data %>% ggplot() + 
  geom_bar(aes(x=gender, fill = gender))

# 수치형 변수부터 탐색 
data$total %>%  hist()
data$total %>%  range()
(max(data$total) - min(data$total))/100
data %>% ggplot() + 
  geom_histogram(aes(x=total), binwidth=100000000)
data %>% ggplot() + 
  geom_density(aes(x=total))
data %>% ggplot() + geom_boxplot(aes(y=total))
# 대분의 값이 0이고 .. 우측으로 꼬리가 김

# RSiteSearch("skew")
# install.packages("utilities")
library(utilities,verbose = FALSE, warn.conflicts = FALSE)
data$total %>%  skewness()

data %>%  ggplot() + 
  geom_density(aes(x=total)) + facet_grid(gender ~ . )

data %>%  ggplot() + 
  geom_boxplot(aes(y=total)) + facet_grid(gender ~ . )

data %>%  ggplot() + 
  geom_boxplot(aes(y=total, group = gender, color = gender)) 

data$max %>%  hist()
data %>%  ggplot() + 
  geom_boxplot(aes(y=total, group = gender, color = gender)) 

train %>%  ggplot() + geom_point(aes(x=total, y=max, color = gender), size =0.4)

train$product %>%  table()
train$days %>%  hist()

# refund 결측치 보정하는 것 . 
train %>% is.na() %>%  colSums()/nrow(train)
test  %>% is.na() %>%  colSums()/nrow(test)

train$refund %>%  hist()
train$refund %>% range(na.rm=TRUE)
data$refund  %>% range(na.rm=TRUE)

data$refund %>%  hist()
data$refund %>%  median(na.rm=TRUE)

# refund 가 NA인 건.. 없다는 뜻이고, 그냥 0으로 보정하는 것이 좋을 듯
data$refund <-  ifelse(data$refund %>%  is.na(), 0, data$refund)

recipe(gender ~ ., data = data) %>%
  # step_log(total, max, refund, days, counts, week, cycle)  %>%
  # step_BoxCox(total, max, refund, days, counts, week, cycle)  %>%
  step_YeoJohnson(total, max, refund, days, counts, week, cycle)  %>%
  step_center(total, max, refund, days, counts, week, cycle)  %>% 
  step_scale(total, max, refund, days, counts, week, cycle)  %>% 
  prep() %>% juice() -> data2

data2 %>%  filter(index == "train") %>%  dplyr::select(-c(index)) -> train
data2 %>%  filter(index == "test")  %>%  dplyr::select(-index) -> test

data$total  %>%  skewness()
data2$total %>%  skewness(na.rm=TRUE)

data$total  %>%  hist()
data2$total %>%  hist()

data$max  %>%  skewness()
data2$max %>%  skewness(na.rm=TRUE)

data$max  %>%  hist()
data2$max %>%  hist()

# conrol은 공통으로 
ctrl <- trainControl(
    method = "repeatedcv",
    repeats = 3,    # (10 fold, repeated 3 times)
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    # sampling = "down",
    )

getModelInfo() %>%  names()
# caret vignette 에서 샘플 코드 찾아서 활용 
  ##### glm 
  modelLookup("glm")
  # model parameter     label forReg forClass probModel
  # 1   glm parameter parameter   TRUE     TRUE      TRUE
  
  set.seed(123)
  train(
    gender ~ .,
    data = train,
    method = "glm",
    trControl = ctrl,
    metric = "ROC"
  ) -> glmFit 
  glmFit
  # ROC        Sens       Spec     
  # 0.6798622  0.3733595  0.8441714
  glmFit %>%  summary()

  # ##### lda
  # modelLookup("lda")
  # # model parameter     label forReg forClass probModel
  # # 1   lda parameter parameter  FALSE     TRUE      TRUE
  # set.seed(123)
  # train(
  #   gender ~ .,
  #   data = train,
  #   method = "lda",
  #   trControl = ctrl,
  #   metric = "ROC"
  # ) -> ldaFit 
  # ldaFit 
  # # ROC        Sens    Spec     
  # # 0.6777653  0.3578  0.8517343
  # ladFit 
  # 
  ##### qda -> 에러 남 ㅠㅠㅠ
  modelLookup("qda")
  # model parameter     label forReg forClass probModel
  # 1   qda parameter parameter  FALSE     TRUE      TRUE
  # set.seed(123)
  # train(
  #   gender ~ .,
  #   data = train,
  #   method = "qda",
  #   trControl = ctrl,
  #   metric = "ROC"
  # ) -> qdaFit 
  # # Error -> 각 모델의 정확한 의미를 모를 경우 에러가 발생할 수 있음. 
  
  #### slda 
  modelLookup("slda")
  # model parameter label forReg forClass probModel
  # 1  slda parameter  none  FALSE     TRUE      TRUE
  # library(ipred)
  # set.seed(123)
  # train(
  #   gender ~ .,
  #   data = train,
  #   method = "slda",
  #   trControl = ctrl,
  #   metric = "ROC"
  # ) -> sldaFit 
  
  #### knn 
  modelLookup("knn")
  # model parameter      label forReg forClass probModel
  # 1   knn         k #Neighbors   TRUE     TRUE      TRUE
  set.seed(123)
  train(
    gender ~ .,
    data = train,
    method = "knn",
    trControl = ctrl,
    metric = "ROC"
  ) -> knnFit 
  knnFit 
  # k  ROC        Sens       Spec     
  # 5  0.4891084  0.2778799  0.7202414
  # 7  0.4911776  0.2507711  0.7487733
  # 9  0.4924157  0.2201056  0.7780913
  knnFit %>%  summary()
  knnFit %>%  plot()
  
  
  #### naiveBayes : naive_bayes
  modelLookup("naive_bayes")
  # model parameter                label forReg forClass probModel
  # 1 naive_bayes   laplace   Laplace Correction  FALSE     TRUE      TRUE
  # 2 naive_bayes usekernel    Distribution Type  FALSE     TRUE      TRUE
  # 3 naive_bayes    adjust Bandwidth Adjustment  FALSE     TRUE      TRUE
  
  # library(e1071)
  set.seed(123)
  train(
    gender ~ .,
    data = train, 
    method = "naive_bayes",
    trControl = ctrl,
    metric = "ROC"
  ) -> nbFit 
  nbFit
  # usekernel  ROC        Sens       Spec     
  # FALSE      0.6426785  0.3120807  0.8240571
  # TRUE      0.6609508  0.0000000  0.9990826
  nbFit %>%  summary()
  nbFit %>%  plot()
  
  #### svm linear : 
  modelLookup("svmLinear2")
  # model parameter label forReg forClass probModel
  # 1 svmLinear2      cost  Cost   TRUE     TRUE      TRUE
  # set.seed(123)
  # train(
  #   gender ~ .,
  #   data = train,
  #   method = "svmLinear2",
  #   trControl = ctrl,
  #   metric = "ROC"
  # ) -> svmlinearFit 
  # # WARNING: reaching max number of iterations
  # svmlinearFit
  # svmlinearFit %>%  summary()
  # svmlinearFit %>%  plot() 
   
  
  # #### svm linear weights : 
  # modelLookup("svmLinearWeights")
  # 
  # set.seed(123)
  # train(
  #   gender ~ .,
  #   data = train2,
  #   method = "svmLinearWeights",
  #   trControl = ctrl,
  #   metric = "ROC"
  # ) -> svmLinearWeightsFit 
  # 
  # ##### rf
  # library(randomForest)
  # 
  # 
  # 
  # set.seed(123)
  # rfFit <- train(
  #   gender ~ .,
  #   data = train2,
  #   method = "rf",
  #   # preProc = c("center", "scale"),
  #   tuneLength = 3,
  #   trControl = ctrl,
  #   metric = "ROC"
  # )
  # rfFit

  # #### rpart
  # library(rpart)
  # # Cross Validation & ROC Metric 을 위한 조치 필요 
  # ctrl <- trainControl(
  #   method = "repeatedcv", 
  #   # repeats = 3,    # (10 fold, repeated 3 times) 
  #   classProbs = TRUE, 
  #   summaryFunction = twoClassSummary,  
  #   sampling = "up"
  #   # sampling = "rose" 
  #   # sampling = "smote"
  # )
  # 
  # set.seed(123)
  # rpartFit <- train(
  #   gender ~ .,
  #   data = train2,
  #   method = "rpart",
  #   # preProc = c("center", "scale"),
  #   tuneLength = 15,
  #   trControl = ctrl,
  #   metric = "ROC"
  # )
  # rpartFit 
  # #                 cp 0.003419453일 때, 0.6377514
  # # sampling="up"   cp 0.005319149일 때, 0.6376410
  # # sampling="down" cp 0.005319149일 때, 0.6341834
  # rpartFit %>%  plot()
  
  #### rpart
  set.seed(123)
  rpartFit <- train(
    gender ~ .,
    data = train,
    method = "rpart",
    # preProc = c("center", "scale"),
    tuneLength = 15,
    trControl = ctrl,
    metric = "ROC"
  )
  rpartFit
  rpartFit %>%  summary()
  rpartFit %>%  plot()
  
  predict(rpartFit, test, type = "prob") -> pred_rpart
  
  # rpartFit %>%   predict(., test2, type = "raw") -> pred_rpart_raw
  # rfFit %>%   predict(., test2, type = "raw")
  
  bind_cols(X_test, pred_rpart ) %>%  
    select(cust_id, 남성) %>% 
     # rename(gender = "남성") %>%  
    write.csv(., "2017026.csv", row.names = FALSE)

  read.csv("2017026.csv")  %>%  head()
