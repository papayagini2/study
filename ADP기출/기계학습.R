# 기계학습(50점)
# 1.1 데이터 탐색
# 1.1.1 탐색적 데이터 분석 수행하시오(시각화 포함)
# 1.1.2 이상치 처리하시오
# 1.1.3 앞선 두 단계에서 얻은 향후 분석시 고려사항 작성

# 2.1 클래스 불균형을 처리하시오
# 2.1.1 업 샘플링 과정 설명하고 결과 작성
# 2.2.2 언더 샘플링 과정 설명하고 결과 작성
# 2.2.3 둘 중 선택하고 이유 설명

# 3.1 모델링 하시오
# 3.1.1 최소 3개 이상 알고리즘 제시하고 
# 정확도 측면의 모델 1개와 속도 측면의 모델 1개를 꼭 구현(총 2개 이상)
# 3.1.2 모델 비교하고 결과 설명
# 3.1.3 속도 개선을 위한 차원 축소 설명하고 수행, 예측 성능과 속도 비교하고 결과 작성

# 2. 통계분석(50점)

# 1.1 금속 성분 함유량 변수 1개. (1열 데이터) 
# 제품에 금속 재질 함유량의 분산이 1.3을 넘으면 불량이라고 보고있는데, 
# 제조사별로 차이가 난다고 제보를 받음 분산에 대해 검정을 수행하시오.
# 1.1.1 연구가설과 귀무가설 작성
# 1.1.2 양측 검정 어쩌고
# 1.1.3 검정통계량, 가설 채택


# 2.1 Lot별 불량 제품 수량 데이터. lot 번호와 불량제품수 두 개의 열. 
# 각 lot별 200개에 대한 불량제품 수.
# 2.1.1 p관리도에 따라 관리중심선(center line), 관리 상한선, 하한선 구하시오
# 2.1.2 관리도 시각화 하시오

# 3.1 데이터 없음. 표에 제품 1,2를 만드는데 사용되는 재료 a b c 컬럼 있고 
# 재료에 따라 최종 만들어지는 제품 두 개에 대한 수량 있음. 
# 최하단 행에는 수익이 있음. 
# 제품 수량을 최대로 뽑으면서 수익이 최적이 되도록 하라고 함.(10점)

# 4.1 데이터 없음. 상품 a와 b가 있을 때 다음과 같은 구매 패턴이 있다고 함. 
# aa bb bbbb aa aaa bb bbb aa bb a b 정확히 기억 안나지만 대충 비슷함. 
# 4.1.1 구매하는 패턴으로 봐서 두 상품이 연관이 있는지 가설 세우고 검정하시오
# 4.1.2 연구가설 귀무가설 세우시오
# 4.1.3 가설 채택하시오


############################ 문제 풀이 ##########################
# 1.1 데이터 탐색
# 1.1.1 탐색적 데이터 분석 수행하시오(시각화 포함)
# 1.1.2 이상치 처리하시오
# 1.1.3 앞선 두 단계에서 얻은 향후 분석시 고려사항 작성

library(tidyverse)
library(magrittr)
library(caret)
library(recipes)
library(funModeling)
library(knitr)

rm(list=ls())
setwd("D:/#000.자격증공부/#003. ADP실기/각종주제별코드/R과외/study/ADP기출/")

read.csv("data.csv") -> data 

data  %>%  glimpse()
data %<>%  mutate(Outcome = ifelse(Outcome == 0, "No", "Yes")) 
data$Outcome %<>%  as.factor()


# 목적변수 탐색 
data %>%  ggplot() + geom_bar() + 
  aes(x=Outcome, fill = Outcome)
data$Outcome %>% table() %>%  prop.table() %>%  round(3) 

# 탐색 결과 
# 분류 모델이다. 
# 0의 비율이 1에 비해 2배 많다. (Imbalanced data 이다.)

# 수치형변수의 분포 탐색 
data %>%  plot_num()
# 한쪽으로 치우친 데이터도 존재한다. 
# 이에 대한 변환(BoxCox 등) 등의 처리 고려 필요함 

# 목적변수와 수치형 변수간의 탐색 
data %>%  names()


featurePlot( as.matrix(data[, 1]),data$Outcome, "box")
featurePlot( as.matrix(data[, 2]),data$Outcome, "box")
featurePlot( as.matrix(data[, 3]),data$Outcome, "box")
featurePlot( as.matrix(data[, 4]),data$Outcome, "box")
featurePlot( as.matrix(data[, 5]),data$Outcome, "box")
featurePlot( as.matrix(data[, 6]),data$Outcome, "box")
featurePlot( as.matrix(data[, 7]),data$Outcome, "box")
featurePlot( as.matrix(data[, 8]),data$Outcome, "box")
# 0,1 여부에 따른 수치형 변수의 분포 차이 확인

# 결측치 처리 
data %>% is.na() %>%  colSums()
# 결측치 없음 

# 이상치 처리 

data %>%  ggplot() + geom_histogram() + 
  aes(x=Age)

data$Age %>%  range()

data$Age %>%  boxplot()

# box plot 상에서 나타나는 이상치가 실제 데이터를 처리할 만큼의 
# 이상한 데이터이니지에 대해서는 업무 담당자와 논의하여 처리가 필요 하다. 
# 여기서는 별도 이상치를 제거, 대체 등의 작업은 하지 않겠다. 

# near zero 여부 확인 
nearZeroVar(data[, -9], saveMetrics = TRUE)

# 탐색 결과에 향후 분석시 고려사항 작성
# 결측 없음 -> 별도 처리 X
# 이상치 -> 1.5*IQR 로 보이는 이상치가 보이나, 이에 대한 처리 X 
# 목적변수 -> 0,1 분류 모델 
# 각 변수 분포 -> 치우친 영역 존재하여 변환 필요 
# 목적변수 분포 2:1 -> Imbalanced data 처리 필요 

# 2.1 클래스 불균형을 처리하시오
# 2.1.1 업 샘플링 과정 설명하고 결과 작성
# 2.2.2 언더 샘플링 과정 설명하고 결과 작성
# 2.2.3 둘 중 선택하고 이유 설명

# [1] "Pregnancies"              "Glucose"                  "BloodPressure"           
# [4] "SkinThickness"            "Insulin"                  "BMI"                     
# [7] "DiabetesPedigreeFunction" "Age"                      "Outcome"

recipe(Outcome ~ . ,
      data = data) %>%  
  themis::step_downsample(Outcome) %>%  
  prep() %>%  
  juice() -> data_down 

recipe(Outcome ~ . ,
       data = data) %>%  
  themis::step_upsample(Outcome) %>%  
  prep() %>%  
  juice() -> data_up 

data$Outcome      %>%  table()
data_down$Outcome %>%  table()
data_up$Outcome   %>%  table()

# 이론적인 내용을 많이 적지는 못 했습니다. 
# 일단 편의상(학습 속도를 고려하여), (random) down sampling 을 선택하였습니다. 

# 3.1 모델링 하시오
# 3.1.1 최소 3개 이상 알고리즘 제시하고 
# 정확도 측면의 모델 1개와 속도 측면의 모델 1개를 꼭 구현(총 2개 이상)
# 3.1.2 모델 비교하고 결과 설명
# 3.1.3 속도 개선을 위한 차원 축소 설명하고 수행, 예측 성능과 속도 비교하고 결과 작성

# 데이터 탐색 결과에 따른 전처리 

recipe(Outcome ~ . ,
       data = data_down) %>%  
  step_YeoJohnson(all_predictors()) %>%  
  step_center(all_predictors()) %>%  
  step_scale(all_predictors()) %>%  
  prep() %>%  
  juice() -> data_down2 

data_down2 %>%  glimpse()

# train / test 분할 
set.seed(107)
inTrain <- createDataPartition(
  y = data_down2$Outcome,
  p = .75,
  list = FALSE
)

training <- data_down2[ inTrain,]
testing  <- data_down2[-inTrain,]


nrow(training)
# [1] 402
nrow(testing)
# [1] 134


training %>%  names()
# [1] "Pregnancies"              "Glucose"                  "BloodPressure"           
# [4] "SkinThickness"            "Insulin"                  "BMI"                     
# [7] "DiabetesPedigreeFunction" "Age"                      "Outcome"

# model 1: pls 
ctrl <- trainControl(method = "repeatedcv", repeats = 3, 
                     classProbs = TRUE, 
                     summaryFunction = twoClassSummary)

plsFit <- train(
  Outcome ~ .,
  data = training,
  method = "pls", 
  trControl = ctrl, 
  metric = "ROC"
)

plsFit
plsFit %>%  plot()

caret::getModelInfo() %>%  names()

# 모델 2: glm 
glmFit <- train(
  Outcome ~ .,
  data = training,
  method = "glm", 
  trControl = ctrl, 
  metric = "ROC"
)

glmFit

# 모델 3. randomforest 
rfFit <- train(
  Outcome ~ .,
  data = training,
  method = "rf", 
  trControl = ctrl, 
  metric = "ROC"
)

rfFit
rfFit %>%  plot()

# 3가지 모델을 사용하엿고,이에 대한 비교 
# 학습 성능 비교 
plsFit$results
glmFit$results
rfFit$results

# test 데이터의 성능 확인 
plsFit %>%  predict(testing) -> plsTest
glmFit %>%  predict(testing) -> glmTest
rfFit  %>%  predict(testing) ->  rfTest

plsTest %>% confusionMatrix(testing$Outcome) -> plsConf; plsConf
glmTest %>% confusionMatrix(testing$Outcome) -> glmConf; glmConf
 rfTest %>% confusionMatrix(testing$Outcome) ->  rfConf; rfConf

# plsConf -> Accuracy : 0.7537  
# glmConf -> Accuracy : 0.7612 
# rfConf  -> Accuracy : 0.7836   

# 따라서, rfConf 즉, RandomForest 모델이 제일 좋다고 할 수 있다. 


# 3.1.3 속도 개선에 대한의견 
# 여기서의 속도란, (1) 모델 학습 시간의 속도 (2) 예측에서의 속도 
#를 고려해야 하는데, 실제적으로 (1) 모델 학습 시간의 속도가 오래 걸리고
# 이는 어떠한 분석 모델(ML, 딥러닝 등)을 썼느냐, 하이퍼라라메터 튜닝 범위
# cross validation 횟수, feature 의 갯수 등에 상이핟. 
# 그러나 실질적으로 더 중요한 것은 (2) 예측에서의 속도가 더 중요하다. 
# 왜냐하면 학습 자체는 Batch 로이루어지고, 예측은 실제 데이터가 올 때 마다 
# 이에 대한 실시간 응답이 중요하기 때문이다.
# 예측의 속도는 분석 모형(머신러닝, 딥러닝)의 복잡성 보다도 
# 예측에 사용한 featrue 의 갯수가 더 중요하다. 예측 모델이 featrue를 사용하여
# 예측 결과를 내는 속도의 차이는 크지 않으나, 실제 해당 feature 가 
# 시스템 상에 생겨서 예측 모델까지 연결되는 속도는 차이가 있을 수 있기 때문이다. 
# 예를 들어, X1, X2 는 ERP에서 데이터를 가져오고, X3는 MES에서 가져옫나고 할 대
# ERP는 실시간으로 데이터 연계 속도 지연이 없으나, MES는 ETL 로 배치로 가져온다고 
# 했을 때 X3라는 변수를 사용함에 있어 모델 응답속도는 상이 할 수 있다. 
 

 
# 1.1 금속 성분 함유량 변수 1개. (1열 데이터) 
# 제품에 금속 재질 함유량의 분산이 1.3을 넘으면 불량이라고 보고있는데, 
# 제조사별로 차이가 난다고 제보를 받음 분산에 대해 검정을 수행하시오.
# 1.1.1 연구가설과 귀무가설 작성
# 1.1.2 양측 검정 어쩌고
# 1.1.3 검정통계량, 가설 채택
 