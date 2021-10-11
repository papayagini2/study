# install.packages("devtools")
# devtools::install_github("tjssu/Rstat")

library(tidyverse)
library(magrittr)
library(Rstat)
rm(list=ls())

####### EX 3.2 #######
# 주사위 두 개를 동시에 던지는 확률 게임 
# A: 눈금의 합이 짝수 
# B: 눈금의 합이 8이상
# C: 두 눈금의 차이가 1이하
# AnB, AnBnC, AnBc
# 벤다이어 그램 그리기. 



rolldie2(2) -> S 
S 

# X1, X2 순서가 의미가 없으므로, 큰 것을 X1, 작은 것을 X2로 
S %>%  filter(X1 >= X2) ->  S2
S2 %>%  nrow()

# A, B, C 정의 
S2 %>%  filter( (X1+X2) %% 2  == 0 ) -> A
S2 %>%  filter( (X1+X2) >= 8  ) -> B
S2 %>%  filter( (X1-X2) <= 1  ) -> C

# element 객체로 변환 
S2 %>%  element() -> S2e
A  %>%  element() -> Ae
B  %>%  element() -> Be
C  %>%  element() -> Ce

# AnB, AnBnC, AnBc
intersect2(Ae, Be)
intersect2(intersect2(Ae, Be),Ce) 
intersect2(Ae, setdiff2(S2e, Be) ) 

# 벤다이어 그램 그리기
library(gplots)
list(Ae, Be, Ce) %>%  venn()


# 3.6. 동전을 4회 던지는 실험. 앞면이 두 번 이상 나올 확률을 구하시오. 
tosscoin2(4) -> S
# 동전을 4회 던지니까..순서가 의미 있음. 
# 앞면이 나오는 회수를 계산
apply(S, 1, function(x) sum(x == "H") )
apply(S, 1, function(x) sum(x == "H") >=2 )
S[apply(S, 1, function(x) sum(x == "H") >=2 ), ]

S[apply(S, 1, function(x) sum(x == "H") >=2 ), ] %>%  nrow()
S %>%  nrow()
(S[apply(S, 1, function(x) sum(x == "H") >=2 ), ] %>%  nrow()) / (S %>%  nrow())

# 3.9. 네 개의 주사위를 동시에 던지는 실험 
# 네 개의 연속되는 숫자가 나올 확률 
# 순서 의미 있음. 
rolldie2(4) -> S; S

# 1 2 3 4
# 2 3 4 5 
# 1 3 2 4 도 되네..
is.straight <- function(x) all(diff(sort(x))==1) # arrange()는 안 됨. 
subset(S, apply(S, 1, is.straight)) -> straight
(straight %>%  nrow()) / (S %>%  nrow()) 

####### EX 3.11 문이과 교차지원 허용 학과 신입생 정원 100명의 분할표 #######
#     구분    남학생    여학생   소계 
#   문과출신    15        25      40
#   이과출신    40        20      60
#     소계      55        45     100

# (1) 한 학생을 선택했을 때, 문과출신 여학생일 확률 
# (2) 한 학생이 문과출신일 때, 그 학생이 여학생일 조건부 확률 
# (3) 한 학생이 문과출신일 사상(A)과 여학생일 사상(F)이 독립인가?

# (1) 
25/100
# (2) 
25/40
# (3) 
40/100 # P(A)
45/100 # P(F)
25/100 # P(AnF) 
40/100*45/100 # P(A)*P(F) 


####### Ex 3.13 4종류 무늬, 1번 ~ 13번 표기된 52장 카드 항아리 #######
# 4장의 카드 꺼냈을 때 모두 같은 무늬가 나올 확률 

# 전체 사상 
c("C", "D", "H", "S") %>%  rep(., 13) # 이걸로 해도 결과적으로는 같음 
c("C", "D", "H", "S") %>%  rep(., each = 13)
c("C", "D", "H", "S") %>%  rep(., each = 13) %>% NROW()

# 임의의 4장의 카드 선택 
c("C", "D", "H", "S") %>%  rep(., each = 13) %>%  
  urnsample2(4) -> S

S %>%  head()
S %>% tail()

S[1, ] %>%  unique() %>%  length()

sameshape <- function(x) x %>%  unique() %>%  length() == 1
subset(S, apply(S, 1, sameshape)) -> Flush

(Flush %>%  nrow())/(S %>%  nrow())

# 3.16 주사위 5개 굴렸을 때, 눈의 합이 짝수인 사상(A), 1과 6이 나오는 사상(B) 
# 두 사상이 독립인가?
rolldie2(5) -> S
S %>% head()
S %>% nrow()

# 눈의 합이 짝수
S[1, ] %>%  sum() %% 2 == 0 
even <-  function(x) ( sum(x) %% 2 )  == 0 
subset(S, apply(S, 1, even)) -> A
A %>% nrow()

# 1과 6이 나옴: max - min == 5 
S[100, ] 
S[100, ] %>%  min()
S[100, ] %>%  max()
(S[100, ] %>%  max()) - (S[100, ] %>%  min())

span <-  function(x) {
  ( max(x) - min(x) ) == 5
}

S[apply(S, 1, span ),] -> B

#P(A)
(A %>% nrow())/ (S %>%  nrow())

#P(B)
(B %>% nrow())/ (S %>%  nrow())

#P(A)*P(B) 
((A %>% nrow())/ (S %>%  nrow()))*(B %>% nrow())/ (S %>%  nrow())

# P(AnB) 
A %>%  element() -> Ae
B %>%  element() -> Be

(intersect2(Ae, Be) %>%  NROW())/(S %>%  nrow())

# P(A)*P(B)  = P(AnB) 이므로, A, B는 독립 사건 


####### EX. 3.17 한 제품 생산 라인 4개   #######
#  생산라인    A     B       C       D
#  생산비율   20%   40%     30%     10%  // 사전확률  
#  불량률    0.04   0.02   0.01     0.05 

#(1) 한 제품을 선택했을 때 불량일 확률 
(c(20, 40, 30, 10)/100    -> 생산비율) # 생산비율 = 사전확률 
(c(0.04, 0.02, 0.01, 0.05) -> 불량률) 
(생산비율*불량률) %>%  sum()

#(2) 불량품이 하나 나왔을 때, 생산라인 A, B, C, D 에서 생산되었을 확률
(생산비율*불량률) / ((생산비율*불량률) %>%  sum()) -> 사후확률 
사후확률
사전확률 <- 생산비율
bayes.plot(사전확률, 사후확률)

####### EX. 4.15 주사위 두 번 던지는 시행   #######
# 3이상 눈의 개수를 X, 짝의 눈의 개수를 Y
# X와 Y가 독립인가 ?
# 전체 사상 S 
rolldie2(2) -> S ; S
S %>%  head()

# X: 3이상 눈의 개수
apply(S, 1, function(x) sum(x>=3) ) -> X

# Y: 짝의 눈의 개수
apply(S, 1, function(x) sum(x %% 2 == 0)  ) -> Y

disc.ind2(X, Y)

####### EX. 5.1 확률변수의 기댓값  #######
# 동전을 세 번 던져 나온 (앞면의 개수 - 뒷면의 개수)만큼 100원씩 주고받는 게임dptjdml 
# 수익을 확률변수 X, X의 기댓값은?

# 확률변수 X 
x <- c(-3,-1,1,3)*100 
p <- c(1,3,3,1)/8
disc.exp(x,p) 


####### EX. 6.1 이산형 균일분포  #######
# 1~20 번호 적혀 있는 동일한 20개의 공이 들어 있는 상제어서 임의로 하나의 공을 꺼냈을 때
# 나온 번호를 X라고 할 때.
# (1) X의 확률분포 함수 
# (2) X의 기댓값과 분산 
# (3) 15이상의 번호가 나올 확률 

# (1) X의 확률분포 함수 
x <-  1:20 
p = rep(1,20)

# (2) X의 기댓값과 분산 
disc.exp(x,p) 

# (3) 15이상의 번호가 나올 확률 
sum(x>=15)/length(x)



####### EX. 6.2 이항분포  #######
# 성공확률 0.2, 0.5, 0.8 인 무한모집단에서 10개씩 표본 취했을 때 
# 나타나는 성공 횟수의 확률분포 그래프를 작성하고, 
# 기대값과 분산을 구하여 비교하시오. 

