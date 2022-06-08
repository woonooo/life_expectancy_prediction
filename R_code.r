library(tidyverse)
library(car)
library(GGally)
library(forecast)
library(MASS)
library(scales)
library(leaps)
library(readxl)


#1.자료확인

data <- read_excel("data name", sheet = 1)
str(data) 
data <- na.omit(data)   #na.omit()전체 결측값 제거
data <- data %>% filter(Status=="Developing")
dplyr::select(data, Year) %>% group_by(Year) %>% summarise(n=n())
data <- filter(data, Year==2011|Year==2012|Year==2013|Year==2014)
data_1 <- dplyr::select(data, -Country, -Status)

# 상관계수 확인
ggcorr(data_1, label = TRUE, label_round = 1) #상관계수

cor(data$thinness_10_19years, data$thinness_5_9years)
ggplot(data = data_1) + geom_density(aes(x=thinness_10_19years, fill="thinness_10_19years", alpha=.5)) + 
                        geom_density(aes(x=thinness_5_9years, fill="thinness_5_9years", alpha=.5))

cor(data_1$Hepatitis_B, data_1$Polio)
cor(data_1$Hepatitis_B, data_1$Diphtheria)
cor(data_1$Polio, data_1$Diphtheria)
ggplot(data = data_1) + geom_density(aes(x=Hepatitis_B, fill="Hepatitis_B", alpha=.5)) + 
  geom_density(aes(x=Polio, fill="Polio", alpha=.5)) +
  geom_density(aes(x=Diphtheria, fill="Diphtheria", alpha=.5))

# 상관계수 확인 후, 3개의 데이터 제거
data_1 <- dplyr::select(data_1, - thinness_5_9years, - Diphtheria, -Polio)
ggcorr(data_1, label = TRUE, label_round = 1)
ggpairs(data_1)

# 한쪽으로 치우친 데이터에 대한 그래프
ggplot(aes(x=infant_deaths), data=data_1) + geom_density(fill="pink")
ggplot(aes(x=Alcohol), data=data_1) + geom_density(fill="pink")
ggplot(aes(x=percentage_expenditure), data=data_1) + geom_density(fill="pink")
ggplot(aes(x=HIV_AIDS), data=data_1) + geom_density(fill="pink")
ggplot(aes(x=Measles), data=data_1) + geom_density(fill="pink")
ggplot(aes(x=thinness_10_19years), data=data_1) + geom_density(fill="pink")
  
# 로그변환 후 데이터 저장
data_2 <- data_1 %>% mutate(Year=factor(Year),
                            infant_deaths=log(infant_deaths+1), Alcohol=log(Alcohol+1),
                            percentage_expenditure=log(percentage_expenditure+1), HIV_AIDS=log(HIV_AIDS+1),
                            Measles = log(Measles+1), thinness_10_19years=log(thinness_10_19years+1))

ggpairs(data_2)

# 로그변환을 실시한 데이터에 대한 그래프
ggplot(aes(x=infant_deaths), data=data_2) + geom_density(fill="sky blue")
ggplot(aes(x=Alcohol), data=data_2) + geom_density(fill="sky blue")
ggplot(aes(x=percentage_expenditure), data=data_2) + geom_density(fill="sky blue")
ggplot(aes(x=HIV_AIDS), data=data_2) + geom_density(fill="sky blue")
ggplot(aes(x=Measles), data=data_2) + geom_density(fill="sky blue")
ggplot(aes(x=thinness_10_19years), data=data_2) + geom_density(fill="sky blue")

# 분석을 위한 train과 test데이터 나누기
train_df=filter(data_2, Year==2011|Year==2012|Year==2013) %>% dplyr::select(-Year)
test_df=filter(data_2, Year==2014) %>% dplyr::select(-Year)



#2.회귀모형 적합
fit_1=lm(Life_expectancy~1,train_df)  #절편
fit_full=lm(Life_expectancy~.,train_df) #전체
summary(fit_full)

# 모형선택 aic에 의한 단계별 선택
MASS::stepAIC(fit_1, scope=formula(fit_full), trace = F) #전진선택
MASS::stepAIC(fit_full, trace = F) #후진소거
# 선택된 모형 적합 결과 확인
fit1=MASS::stepAIC(fit_1, scope=formula(fit_full), trace = F) #전진선택
fit2=MASS::stepAIC(fit_full, trace = F) #후진소거
summary(fit1)
summary(fit2)

# bic에 의한 단계별 선택
MASS::stepAIC(fit_1, scope=formula(fit_full), k=log(nrow(train_df)), trace = F)
MASS::stepAIC(fit_full, k=log(nrow(train_df)), trace = F)
# 선택된 모형 적합 결과 확인
fit3=MASS::stepAIC(fit_full,k=log(nrow(train_df)),trace = F)
fit4=MASS::stepAIC(fit_full, k=log(nrow(train_df)), trace = F)
summary(fit3)
summary(fit4)

# 모든 가능한 회귀
fits=regsubsets(Life_expectancy~.,train_df)
plot(fits,scale = "adjr2")
plot(fits,scale = "Cp")
plot(fits) #디폴트는 BIC
car::subsets(fits, statistic="bic", legend=FALSE) # 가장 낮은 알파벳 찾는 방법

# 모형 선택
BIC(fit1,fit2,fit3,fit4)
AIC(fit1,fit2,fit3,fit4)

# 선택된 최종모형 
summary(fit3)

#3.모형진단
#가정확인(오차항의 동분산성, 정규성, 독립성, 선형성)
#잔차산점도, 정규성, 동분산성, 이상값
par(mfrow=c(2,2))
plot(fit4)         
par(mfrow=c(1,1))

#다중공선성 확인
vif(fit3) 

#이상값탐지
influencePlot(fit3)


#4.예측
pred=predict(fit3,newdata=test_df)
forecast::accuracy(pred,test_df$Life_expectancy)
caret::defaultSummary(data.frame(obs=test_df$Life_expectancy,pred=pred))

ggplot(data.frame(obs=test_df$Life_expectancy, pred=pred)) + 
  geom_point(aes(x=obs, y=pred)) + geom_abline(intercept = 0, slope = 1)

data.frame(obs=test_df$Life_expectancy, pred=pred) %>%
  rownames_to_column(var="country") %>% filter(abs(pred-obs)>=2)
