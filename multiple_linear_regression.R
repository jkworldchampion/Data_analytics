setwd("/Users/parkjuhwan/Documents/아주대학교/2021/data_anlyst_R")
#data = read.csv("Inner_Datasets_in_R.csv")
rm(list = ls())
#-------------------------------------------------------------------------------
suppressMessages(library(magrittr))
suppressMessages(library(dplyr)) %>% suppressWarnings()
suppressMessages(library(jtools)) %>% suppressWarnings()
suppressMessages(library(car)) %>% suppressWarnings()
suppressMessages(library(mice)) %>% suppressWarnings()
library(sjPlot)
library(QuantPsyc)
# 시작
df_culture = read.csv('6-dimensions-for-website-2015-08-16.csv', header = T, sep = ';')
df_inno = read.csv('Analysis_2021may.24161011.csv', header = T)
df_country = read.csv('countryContinent.csv', header = T)

# 전처리
head(df_culture) # 대략적 확인
df_culture[] = lapply(df_culture, function(x) gsub("#NULL!", NA, x)) # '#NULL!'값 제거
df_culture = df_culture[, -1] # 필요 행만 서브셋팅
df_inno = df_inno[,c(2, 5)] # 필요 행만 서브세팅
df_country = df_country[, c(1, 6)] # 필요 행만 서브세팅
colnames(df_inno) = c('country', 'Score') # 열 들의 이름 바꾸기

df_inno$Score = as.numeric(df_inno$Score) # 열 숫자로 바꾸기
rownames(df_culture) = df_culture$country ; df_culture = df_culture[,-1] # 나라 이름을 행으로 변환
df_culture[] = lapply(df_culture, function(x) as.numeric(x)) # 데이터 프레임 숫자로 바꾸기
df_culture$country = rownames(df_culture)

## 병합
df = inner_join(df_culture, df_country, by = 'country')
df = inner_join(df, df_inno, by = 'country')

## na 제거
#md.pattern(df) # 결측치에 대한 통계를 본다.
df = na.omit(df)

## 더미변수 생성
factor(df$continent) # 팩터형을 통해 5 대륙 중 빠진 것이 있나 확인
df$Africa = ifelse(df$continent == 'Africa', 1, 0)   # 아프리카 생성
df$Americas = ifelse(df$continent == 'Americas', 1, 0)  # 아메리카 생성
df$europe = ifelse(df$continent == 'Europe', 1, 0)  # 유럽 생성
df$Oceania = ifelse(df$continent == 'Oceania', 1, 0)  # 오세아니아 생성
# 준거집단은 아시아, 즉 위의 네가지가 0이면 아시아

str(df)
# 다중 선형 회귀
multi_linear_regression = lm(Score ~ pdi + idv + mas + uai + ltowvs + ivr + europe + Americas + Oceania + Africa, data = df)
summary(multi_linear_regression)
# 유의미한 것 
# ltowvs, ivr 1% 유의수준, 즉 장기 지향, 개인주의가 중요하다 
# 결정계수는 0.7819, 수정된 결정계수는 0.726으로 약 70%이상의 데이터를 설명한다.

# 다중공산성 확인
vif(multi_linear_regression) # 다중 공산성 확인 # 모두 괜춘 5 이하.

# 표준화 계수값 구하기
lm.beta(multi_linear_regression) # 가장 영향을 많이 미치는 변수는 ivr이다.

# sjPlot을 이용.
tab_model(multi_linear_regression)

plot_model(multi_linear_regression, type = "est", wrap.labels=5)

plot_model(multi_linear_regression, type = "std", sort.est = T, wrap.labels=5)

plot_model(multi_linear_regression, type = "diag")

plot(multi_linear_regression, 1) # 모형의 선형성이 인정됨.

plot(multi_linear_regression, 2) # 잔차의 정규성이 검증됨.
shapiro.test(multi_linear_regression$residuals) # 샤피로 테스트로 p값이 0.89이니 정규성 가정 만족.

plot(multi_linear_regression, 3) # 약간 아쉽지만 잔차의 등분산성이 만족.

plot(multi_linear_regression, 4) # 16번, 42번, 61번의 자료가 예측치에 비해 특히 벗이난다.

library(lmtest) 
dwtest(multi_linear_regression)

#회귀 다시 구하기
df_plus = df_plus[, -7]
df_plus$mid_age = as.numeric(df_plus$mid_age)
null = lm(Score ~ 1, data = df_plus)
multi_linear_regression_plus = lm(Score ~ ., data = df_plus)
front_model_plus = step(null, scope=list(lower=null, upper=multi_linear_regression_plus), direction="forward")

# 특정 변수를 추가해 회귀선의 R^2값을 키워볼 것이다.
data_3rd = read.csv("population_by_country_2020.csv") # 데이터 불러오기
data_3rd = data_3rd[, c(1, 9)] # 국가 이름과 국가별 나이의 중간 값을 가져온다.
colnames(data_3rd) = c("country", "mid_age")
df_plus = inner_join(df, data_3rd, by = 'country') # 병합

#회귀 다시 구하기
df_plus = df_plus[, -7]
df_plus$mid_age = as.numeric(df_plus$mid_age)
null_plus = lm(Score ~ 1, data = df_plus)
multi_linear_regression_plus = lm(Score ~ ., data = df_plus)
front_model_plus = step(null_plus, scope=list(lower=null_plus, upper=multi_linear_regression_plus), direction="forward")
