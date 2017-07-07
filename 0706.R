install.packages('BayesFactor')
library(BayesFactor)

head(iris)

library(e1071)

zoo<-read.csv('zoo2.csv', header=F, stringsAsFactors = F)
zoo<-zoo[,-1]
str(zoo)
zoo<-as.data.frame(lapply(zoo, as.factor))
str(zoo)
model<-naiveBayes(zoo[1:16], zoo$V18, laplace=0)
result<-predict(model, zoo[101,-17])


all_result<-predict(model, zoo[,-17])
all_result_data<-cbind(zoo, all_result)

kbody2 <-read.csv('kbody2.csv', header=F, stringsAsFactors = F)
kbody2 <-na.omit(kbody2)
kbody2 <- kbody2[-1:-2]
str(kbody2)
kbody2<-as.data.frame(lapply(kbody2, as.factor))
model<-naiveBayes(kbody2[1:16], kbody2$V19, laplace=0)
result<-predict(model, kbody2[14015,-17:-18])
result
nrow(kbody2)



install.packages("Rcpp")
install.packages("wordcloud")

sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)
str(sms_raw)
sms_raw$type <- factor(sms_raw$type)



install.packages("tm")
library(tm)


sms_corpus <- Corpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:3])

# locale US 로 변경해주어야한다 
Sys.setlocale(category = "LC_ALL", locale = "us")

corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
#T_T
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

inspect(sms_corpus[1:3])
inspect(corpus_clean[1:3])


# 문서-용어 희소 매트릭스 생성 책 150page 표와 같은 형태로 변환
sms_dtm <- DocumentTermMatrix(corpus_clean)
sms_dtm

# 훈련과 테스트 데이터셋 생성
sms_raw_train <- sms_raw[1:4169, ]     #원래 데이터 (훈련)
sms_raw_test  <- sms_raw[4170:5559, ]  # 원래 데이터 (테스트)


sms_dtm_train <- sms_dtm[1:4169, ]     # 정제후의 표형태 변경한 데이터(훈련)
sms_dtm_test  <- sms_dtm[4170:5559, ]  #  정제후의 표형태 변경한 데이터 (테스트)

sms_corpus_train <- corpus_clean[1:4169]    # 정제후의 데이터 (훈련)
sms_corpus_test  <- corpus_clean[4170:5559] # 정제후의 데이터 (테스트)

# 스팸 비율 확인
prop.table(table(sms_raw_train$type))    # 훈련 데이터의 스팸:80% , 햄: 20%
prop.table(table(sms_raw_test$type))     # 테스트 데이터의 스팸 : 80% , 햄 : 20%

# 단어 클라우드 시각화(워드 클라우드로 햄의 단어가 무엇이고 스팸의 단어가 무엇인지 확인)
# R 을 2.15.3(2013-03-01) 로 설치하고 아래 패키지를 install 해야함

#install.packages("Rcpp")
#install.packages("wordcloud")

library(wordcloud)

wordcloud(sms_corpus_train, min.freq = 30, random.order = FALSE)

# 훈련 데이터를 스팸과 햄으로 구분
spam <- subset(sms_raw_train, type == "spam")
ham  <- subset(sms_raw_train, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

library(dplyr)

install.packages('Dictionary')
library(Dictionary)
library(tm)


Dictionary <- function(x) {
  if( is.character(x) ) {
    return (x)
  }
  stop('x is not a character vector')
}


# 빈번한 단어에 대한 속성 지시자
findFreqTerms(sms_dtm_train, 5) #5개 이상 사용된 단어만 다시 추출
sms_dict <- Dictionary(findFreqTerms(sms_dtm_train, 5))
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

# 개수를 팩터로 변환(라벨을 1과 0으로 변경하고 1과 0을 YES와 NO로 변경하는 작업)
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}


# apply() convert_counts()를 사용한 훈련/테스트 데이터 추출
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)

## 3 단계 : 데이터로 모델 훈련 ----
install.packages("e1071")
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier

## 4 단계 : 모델 성능 평가 ----
sms_test_pred <- predict(sms_classifier, sms_test)

install.packages("gmodels")
library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

## 5 단계 : 모델 성능 향상 ----
sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))


-0.60 * log2(0.60) - 0.40 * log2(0.40)
curve(-x * log2(x) - (1 - x) * log2(1 - x),
      col="red", xlab = "x", ylab = "Entropy", lwd=4)


x <- data.frame(
  cust_name=c('SCOTT','SMITH','ALLEN','JONES','WARD'),
  card_yn=c('Y','Y','N','Y','Y'),
  review_yn=c('Y','Y','N','N','Y'),
  before_buy_yn=c('Y','Y','Y','N','Y'),
  buy_yn=c('Y','Y','N','Y','Y') )

install.packages('FSelector')
library(FSelector)

weights <- information.gain(buy_yn~.,x)
weights
rm(weights)
skin <- read.csv('skin.csv',header = T)
skin
skin_re <- information.gain(cupon_react ~ .,skin)
skin_re

install.packages("rpart")
library(rpart)

tree1 <- rpart(cupon_react ~ . , skin, control = rpart.control(minsplit = 2))
plot(tree1,compress = T,uniform = T,margin = 0.1)
text(tree1, use.n = T , col = 'blue')

fatliver2<- read.csv("fatliver2.csv", header = TRUE)
fatliver2


fat <- information.gain(FATLIVER ~ . , fatliver2)
fat








##### 5장 : 결정 트리와 규칙(Decision tree and Rules)을 사용한 분류 -------------------
#### Part 1: 결정 트리 -------------------
## 결정 트리 이해 ----
# 두 부분 분류의 엔트로피 계산
-0.60 * log2(0.60) - 0.40 * log2(0.40)
curve(-x * log2(x) - (1 - x) * log2(1 - x),
      col="red", xlab = "x", ylab = "Entropy", lwd=4)
## 예제 : 위험 은행 대출 식별 ----
## 2 단계 : 데이터 준비와 살펴보기 ----
credit <- read.csv("credit.csv")
str(credit)
# 지원자의 두 특성 확인
table(credit$checking_balance)
table(credit$savings_balance)
# 대출의 두 특성 확인
summary(credit$months_loan_duration)
summary(credit$amount)
# 분류 변수 확인
table(credit$default)
# 훈련과 테스트 데이터에 대한 무작위 샘플 생성
# 예제와 같은 무작위 수열을 사용하기 위해 set.seed 사용
set.seed(12345)

runif(10)

credit_rand <- credit[order(runif(1000)), ]
# credit과 credit_rand 데이터 프레임간 비교
summary(credit$amount)
summary(credit_rand$amount)
head(credit$amount)
head(credit_rand$amount)
# 데이터 프레임 나누기
credit_train <- credit_rand[1:900, ]
credit_test  <- credit_rand[901:1000, ]
# 분류 변수의 비율 확인
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))
## 3 단계 : 데이터로 모델 훈련 ----
# 가장 단순한 결정 트리 생성
install.packages("C50")
library(C50)
credit_model <- C5.0(credit_train[-17], credit_train$default)
car_evaluation
car_evaluation$X<-as.factor(car_evaluation$X)
car_model <- C5.0(car_evaluation[-7],car_evaluation$X)
car_train <- car_evaluation[1:6]
car_train
car_pred <- predict(car_model,car_train)
car_evaluation$maint
CrossTable(car_evaluation[7],car_pred)
car_evaluation[7]
car_pred
nrow(car_pred)
nrow(car_evaluation[7])
# 트리 정보 출력
credit_model
# 트리에 대한 상세 정보 출력
summary(credit_model)
## 4 단계 : 모델 성능 평가 ----
# 테스트 데이터에 대한 예측 팩터 벡터 생성
credit_pred <- predict(credit_model, credit_test)
# 예측과 실제 분류의 교차표
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
## 5 단계 : 모델 성능 향상 ----
## 결정 트리의 정확성 부스팅
# 10 trials과 부스트드 결정 트리
credit_boost10 <- C5.0(credit_train[-17], credit_train$default,
                       trials = 10)
credit_boost10
summary(credit_boost10)
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
# 100 trials과 부스트드 결정 트리
credit_boost100 <- C5.0(credit_train[-17], credit_train$default,
                        trials = 100)
credit_boost_pred100 <- predict(credit_boost100, credit_test)
CrossTable(credit_test$default, credit_boost_pred100,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
## 가중치 매트릭스 생성
# 가중 비용 매트릭스
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)
error_cost
# 트리에 비용 매트릭스 적용
credit_cost <- C5.0(credit_train[-17], credit_train$default,
                    costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
#### Part 2: 규칙 학습기 -------------------



#car_evaluation 데이터로 의사결정트리 그리기
############################################################################

car_evaluation <- read.csv("car_evaluation.csv",head=T)
head(car_evaluation)

install.packages("C50")
library(C50)
x <- car_evaluation[,2:7]
y <- car_evaluation[,1]     #기준이 되는 컬럼
model <- C50::C5.0(x,y)
model
summary(model)
plot(model)


또는


x <- car_evaluation[,2:7]
build_tree <- C5.0(car_evaluation$buyingprice~.,data=x)
summary(build_tree)
plot(build_tree)



정확도:
  
p <- predict(model,x,type="class")
sum(p==y)/length(p)



#Decision tree 해석
#############################################################################

Decision tree:
  
  X = unacc: vhigh (1210/850)     #X=unacc는 1210개, 그 중 vhigh로 분류하지 않은 
데이터는 850개 
X in {good,vgood}: low (134/49)
X = acc:
  :...maint in {high,vhigh}:
  :...safety = low: high (0)
:   safety = high: med (102/56)
:   safety = med: low (75/39)
maint in {low,med}:
  :...safety in {high,low}: high (102/56)
safety = med: med (105/72)


Evaluation on training data (1728 cases):
  
  Decision Tree   
----------------  
  Size      Errors  

6 1122(64.9%)   <<
  
  
  (a)   (b)   (c)   (d)    <-classified as
----  ----  ----  ----
  46    13    49   324    (a): class high
121    53   258    (b): class low
10    75    79   268    (c): class med
46          26   360    (d): class vhigh


Attribute usage:
  
  100.00%	X
22.22%	maint
22.22%	safety

##################################################


badminton <- read.csv("badminton.csv",header=T)
install.packages("C50")
library(C50)
badminton
x <- badminton[1:5]
y <- badminton[1,5] #분할 기준이 되는 컬럼(buyingprice)
model <- C5.0(x,y) # C50::C5.0
model
summary(model)
plot(model)
p <- predict(model,x,type="class")
sum(p==y)/length(p)



#마지막 문제 
car_evaluation<-as.data.frame(lapply(car_evaluation, as.factor))
car_evaluation
x <- car_evaluation[2:7]
y <- car_evaluation[1]   #기준값 
as.factor()
model <- C5.0(x,y,trials = 10)


x <- badminton[,1:5]
build_tree <- C5.0(badminton$play~.,data=x)
summary(build_tree)
plot(build_tree)


CrossTable(x = car_evaluation$X, y = p,prop.chisq=FALSE)


x <- car_evaluation[,2:7]
y <- car_evaluation[,1]     #기준이 되는 컬럼
model <- C50::C5.0(x,y)

p <- predict(model,x)
CrossTable(p,y)

