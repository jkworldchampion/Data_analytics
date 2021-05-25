setwd("/Users/parkjuhwan/Documents/아주대학교/2021/data_anlyst_R")
#data = read.csv("Inner_Datasets_in_R.csv")
rm(list = ls())
#-------------------------------------------------------------------------------
suppressMessages(library(magrittr))
suppressMessages(library(dplyr)) %>% suppressWarnings()

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
df = na.omit(df)

## 더미변수 생성
factor(df$continent) # 팩터형을 통해 5 대륙 중 빠진 것이 있나 확인
df$Africa = ifelse(df$continent == 'Africa', 1, 0)   # 아프리카 생성
df$Americas = ifelse(df$continent == 'Americas', 1, 0)  # 아메리카 생성
df$europe = ifelse(df$continent == 'Europe', 1, 0)  # 유럽 생성
df$Oceania = ifelse(df$continent == 'Oceania', 1, 0)  # 오세아니아 생성
# 준거집단은 아시아, 즉 위의 네가지가 0이면 아시아

df[df$continent=='Europe',df$europe]

str(df)
# 다중 선형 회귀
multi_linear_regression = lm(Score ~ pdi + idv + mas + uai + ltowvs + ivr, data = df)
summary(multi_linear_regression)




