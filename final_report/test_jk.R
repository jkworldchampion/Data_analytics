rm(list = ls())

library(glmnet)
library(dplyr)
library(lm.beta)
library(XML)
library(httr)
library(dplyr)
library(stringr)
library(QuantPsyc)
library(car)
library(sjPlot)
library(Epi)
library(caret)
options(scipen = 1000)

# 종속변수
url <- "https://worldpopulationreview.com/country-rankings/birth-rate-by-country"
html_source <- GET(url)
tabs <- readHTMLTable(rawToChar(html_source$content), stringsAsFactors=F)
df_birth_rate = as.data.frame(tabs)
df_birth_rate <- df_birth_rate[,-3]
colnames(df_birth_rate) <- c("Country", "Birth rate")
df_birth_rate[,1] <- tolower(df_birth_rate[,1])
df_birth_rate$Country <- gsub(" ", "", df_birth_rate$Country)

# 결혼 나이 
url_first_marriage <- "https://en.wikipedia.org/wiki/List_of_countries_by_age_at_first_marriage"
html_source <- GET(url_first_marriage)
tabs <- readHTMLTable(rawToChar(html_source$content), stringsAsFactors=F)
df_first_marriage = data.frame(Country = character(), Men = numeric(), Women = numeric(), 
                               Average = numeric(), AgeGap = numeric(), AgeRatio = numeric(),
                               Year = numeric(), Source = character())

for (i in 1:5){
  table = as.data.frame(tabs[i])
  colnames(table) = table[1,]
  table = table[-1,]
  colnames(df_first_marriage) <- colnames(table)
  df_first_marriage = rbind(df_first_marriage, table)
}
df_first_marriage <- df_first_marriage[, c(1,2,3)]
colnames(df_first_marriage) <- c("Country", "Men marry", "Women marry")
df_first_marriage$Country <- tolower(df_first_marriage$Country)
df_first_marriage$Country <- gsub(" ", "", df_first_marriage$Country)

# 교육수준 
url <- "https://en.wikipedia.org/wiki/Education_Index"
html_source <- GET(url)
tabs <- readHTMLTable(rawToChar(html_source$content), stringsAsFactors=F)
df_study = as.data.frame(tabs[1])
colnames(df_study) = df_study[1,] ; df_study = df_study[-1,]
df_study <- df_study[, c(1, 31)]
colnames(df_study) <- c("Country", "education")
df_study$Country <- tolower(df_study$Country)
df_study$Country <- gsub(" ", "", df_study$Country)

df_study[91,1] <- "southkorea"
df_study$Country <- gsub("bosniaandherzegovina", "bosnia", df_study$Country)
df_study$Country <- gsub("unitedkingdom", "england", df_study$Country)
df_study$Country <- gsub("tanzania(unitedrepublicof)", "tanzania", df_study$Country)

# 자살률
url <- "https://en.wikipedia.org/wiki/List_of_countries_by_suicide_rate"
html_source <- GET(url)
tabs <- readHTMLTable(rawToChar(html_source$content), stringsAsFactors=F, header = F)
df_suicide = as.data.frame(tabs[1])
colnames(df_suicide) = df_suicide[1,] ; df_suicide = df_suicide[-1,]
df_suicide <- df_suicide[,-2]
colnames(df_suicide) <- c("Country", "Men suicide", "Women suicide")
df_suicide$Country <- tolower(df_suicide$Country)
df_suicide$Country <- gsub(" ", "", df_suicide$Country)

df_suicide$Country <- gsub("[[:punct:]]", "", df_suicide$Country)
df_suicide[22,1] <- "bosnia"
df_suicide[174,1] <- "england"

# 육아비용 
url <- "https://www.numbeo.com/cost-of-living/prices_by_country.jsp?displayCurrency=USD&itemId=224"
html_source <- GET(url)
tabs <- readHTMLTable(rawToChar(html_source$content), stringsAsFactors=F)
df_childcost = as.data.frame(tabs[2])
df_childcost = df_childcost[,-1]
colnames(df_childcost) <- c("Country", "child cost")
df_childcost$Country <- tolower(df_childcost$Country)
df_childcost$Country <- gsub(" ", "", df_childcost$Country)

df_childcost[45,1] <- "england"
df_childcost[61,1] <- "bosnia"



# 65세 이상 인구 비율
url <- "https://en.wikipedia.org/wiki/List_of_countries_by_age_structure"
html_source <- GET(url)
tabs <- readHTMLTable(rawToChar(html_source$content), stringsAsFactors=F)
df_oldratio = as.data.frame(tabs[1])
colnames(df_oldratio) = df_oldratio[1,] ; df_oldratio = df_oldratio[-1,]
df_oldratio = df_oldratio[-1,c(-2:-3)]
colnames(df_oldratio) <- c("Country", "old ratio")
df_oldratio$Country <- tolower(df_oldratio$Country)
df_oldratio$Country <- gsub(" ", "", df_oldratio$Country)

df_oldratio$Country <- gsub("bosniaandherzegovina", "bosnia", df_oldratio$Country)
df_oldratio$Country <- gsub("republicofthecongo", "congo", df_oldratio$Country)
df_oldratio$Country <- gsub("unitedkingdom", "england", df_oldratio$Country)



# 행복지수 
df_happiness <- read.csv("https://raw.githubusercontent.com/jkworldchampion/Data_analytics/ahyoung/%EB%82%98%EB%9D%BC%EB%B3%84%20%ED%96%89%EB%B3%B5%EC%A7%80%EC%88%98.csv")
df_happiness <- df_happiness[,2:3]
colnames(df_happiness) <- c("Country", "happy score")
df_happiness$Country <- tolower(df_happiness$Country)
df_happiness$Country <- gsub(" ", "", df_happiness$Country)

df_happiness$Country <- gsub("bosniaandherzegovina", "bosnia", df_happiness$Country)
df_happiness$Country <- gsub("congo(brazzaville)", "congo", df_happiness$Country)
df_happiness$Country <- gsub("unitedkingdom", "england", df_happiness$Country)
df_happiness$Country <- gsub("southsudan", "sudan", df_happiness$Country)



# 지니계수 
df_gini <- read.csv("https://raw.githubusercontent.com/jkworldchampion/Data_analytics/main/final_report/data_set/%EC%A7%80%EB%8B%88%EA%B3%84%EC%88%98.csv", fileEncoding = 'euc-kr')
str(df_gini) 
df_gini[] <- lapply(df_gini, function(x){gsub("-", NA, x)})
rownames(df_gini) <- df_gini[,1]
df_gini[] <- lapply(df_gini, function(x){as.numeric(x)})
gini_mean <- round(rowMeans(df_gini, na.rm = T),2)
df_gini <- cbind(df_gini, gini_mean)
df_gini[,1] <- rownames(df_gini)
df_gini <- df_gini[,c(1,12)]
colnames(df_gini) <- c("Country", "gini_score")
df_gini$Country <- tolower(df_gini$Country)
df_gini$Country <- gsub(" ", "", df_gini$Country)


# 낙태 합법인 국가 
df_abortionlegal <- read.csv("https://raw.githubusercontent.com/jkworldchampion/Data_analytics/main/abortionlegal.csv", header=FALSE)
df_abortionlegal <- df_abortionlegal[-219,-3]
df_abortionlegal <- as.data.frame(df_abortionlegal)
colnames(df_abortionlegal) = c("Country", "legal")
df_abortionlegal$legal[df_abortionlegal$legal != 'yes'] <- 'no'
df_abortionlegal$Country <- tolower(df_abortionlegal$Country)
df_abortionlegal$Country <- gsub(" ", "", df_abortionlegal$Country)


# 많은 데이터 
vary <- read.csv("https://raw.githubusercontent.com/jkworldchampion/Data_analytics/main/final_report/data_set/%E1%84%82%E1%85%A1%E1%84%85%E1%85%A1%E1%84%83%E1%85%A6%E1%84%8B%E1%85%B5%E1%84%90%E1%85%A5.csv", header = FALSE)
colnames(vary) <- vary[1,]
vary <- vary[-1,]
df_vary <- as.data.frame(vary)
colnames(df_vary)[1] <- c("Country")
df_vary$Country <- tolower(df_vary$Country)
df_vary$Country <- gsub(" ", "", df_vary$Country)
df_vary$Country <- gsub("[[:punct:]]", "", df_vary$Country)
df_vary <- df_vary[,c(-2,-40,-41)]



# GDP
url <- "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)"
html_source <- GET(url)
tabs <- readHTMLTable(rawToChar(html_source$content), stringsAsFactors=F)

df_gdp = as.data.frame(tabs[3])
df_gdp <- df_gdp[5:194,-3]
colnames(df_gdp) <- c("Country", "gdp")
df_gdp$Country <- tolower(df_gdp$Country)
df_gdp$Country <- gsub(" ", "", df_gdp$Country)
df_gdp$Country <- gsub("[[:punct:]]", "", df_gdp$Country)


df_gdp[2,1] <- "china"
df_gdp$Country <- gsub("bosniaandherzegovina", "bosnia", df_gdp$Country)
df_gdp$Country <- gsub("solomonislands", "island", df_gdp$Country)
df_gdp$Country <- gsub("lebanon2020", "lebanon", df_gdp$Country)
df_gdp$Country <- gsub("moldovan8", "moldova", df_gdp$Country)
df_gdp$Country <- gsub("ukrainen5", "ukraine", df_gdp$Country)
df_gdp$Country <- gsub("unitedkingdom", "england", df_gdp$Country)
df_gdp$gdp<- gsub(",", "", df_gdp$gdp)



# 나라별 평등지수 
url <- "https://en.wikipedia.org/wiki/Global_Gender_Gap_Report"
html_source <- GET(url)
tabs <- readHTMLTable(rawToChar(html_source$content), stringsAsFactors=F)
df_equality = as.data.frame(tabs[3])
df_equality <- df_equality[-1,]
df_equality <- df_equality[-1,c(1,17)]
colnames(df_equality) = c("Country", "equality")

df_equality$Country <- tolower(df_equality$Country)
df_equality$Country <- gsub(" ", "", df_equality$Country)
df_equality$Country <- gsub("[[:punct:]]", "", df_equality$Country)

df <- full_join(df_gini, df_equality, by = 'Country')
df_equality$Country <- gsub("korearep", "southkorea", df_equality$Country)
df_equality$Country <- gsub("kyrgyzrepublic", "kyrgyzstan", df_equality$Country)
df_equality$Country <- gsub("bosniaandherzegovina", "bosnia", df_equality$Country)
df_equality$Country <- gsub("unitedkingdom", "england", df_equality$Country)
df_equality$Country <- gsub("democraticrepublicofthecongo", "congo", df_equality$Country)


# OECD 국가
df_oecd <- read.csv("https://raw.githubusercontent.com/jkworldchampion/Data_analytics/main/final_report/data_set/OECD%20country.csv")
df_oecd <- cbind(df_oecd, 1)
colnames(df_oecd) <- c("Country", "OECD")


#=======================================================================================================================================
df <- inner_join(df_gini, df_abortionlegal, by = 'Country')
df <- inner_join(df, df_study, by = 'Country')
df <- inner_join(df, df_oldratio, by='Country')
df <- inner_join(df, df_happiness, by='Country')
df <- inner_join(df, df_childcost, by='Country')
df <- inner_join(df, df_suicide, by = "Country")
df <- inner_join(df, df_birth_rate, by = "Country")
df <- inner_join(df, df_gdp, by = 'Country')
df <- inner_join(df, df_equality, by='Country')
df <- inner_join(df, df_vary, by='Country')
df$Country <- str_to_title(df$Country)

colnames(df)[13:58] <- c("surface area", "population in thousands", "population density",
                         "sex ratio", "gdp_", "gdp growth rate", "gdp per capita", "agriculture", "industry",
                         "services and other activity", "agriculture employed", "industry employed",
                         "service employed", "unemployed", "labour force", "agriculture production", "food production",
                         "exports", "imports", "trade balance", "balance of paments", "population growth rate", "urban population",  
                         "urban population growth rate", "fertility rate", "life expectancy at birth", 
                         "population age distribution", "international migrant", "refugees", "infant mortality rate", 
                         "health expenditure", "physicians", "education government expenditure",
                         "primary education", "secondary education", "tertiary education",
                         "women politician", "internet using", "threatened species",
                         "forested area", "CO2 emission", "energy production", "energy supply",
                         "water", "sanitation facilities", "net official")

# sex ratio 여자 백명당 남자 몇명 
# gdp growth rate 연간 gdp 성장율 
# gdp per capita 국민 1인당 gdp 
# agriculture 농업 경제 % of GVA
# industry 산업 경제 % of GVA
# services and other activity 서비스 산업 % of GVA
# agriculture employed 농업 종사자 비율 
# industry employed 산업 종사자 비율 
# service employed 서비스 종사자 비율 
# unemployed 실업자 비율 
# labour force (여자/남자 인구 %)
# agriculture production 농업 생산지수
# food production 식품 생산지수  
# exports 수출 
# imports 수입 
# trade balance 무역수지 
# balance of payments 소비 
# population growth rate 인구 증가 평균 연간 비율 
# urban population 도시 인구 비율 
# urban population growth rate 도시 인구 증가 평균 연간 비율
# fertility rate 여성 한 명당 출생률 
# life expectancy at birth 기대수명 여자/남자 년도 
# population age distribution (0-14, 60+ %)
# international migrant 이민자 비율 
# refugees 1000명당 난민이나  UNHCR에서 고려해야할 사람 수
# infant mortality 유아 사망률 
# health expenditure 건강에 쓰는 비용 % of GDP
# physicians 1000명당 의사
# education government expenditure 정부 총 교육 비용 
# primary education 초등교육 성별 비율 100명당 여자 남자 
# secondary education 중등교육 성별 비율 100명당 여자 남자 
# tertiary education 고등교육 성별 비율 100명당 여자 남자 
# women politician 국회 여자 비율 
# internet using 거주지 100 당 인터넷 쓰는 사람 수 
# threatened species 멸종위기종수
# forested area 숲으로 된 지역 퍼센트 
# CO2 emission 이산화탄소 예상 배출량 
# energy production 에너지 생산량 
# energy supply 인당 에너지 공급 
# water 개선된 식수 마시는 비율 (도시/시골)
# sanitation facilities 위생시설 비율 (도시/시골)
# net official 공적 개발 원조 받은 비율 % of GNI
#=======================================================================================================================================
# 낙태 합법 변수 재부호화
df$legal <- ifelse(df$legal == 'yes', 1, 0)

# 65 세 이상 변수 퍼센트 없애기
df$`old ratio` <- gsub("[[:punct:]]", "", df$`old ratio`) # %없애주기
df$`old ratio` <- gsub("\u00A0", "", df$`old ratio`) # %없애주기

# 숫자형으로 변환
rownames(df) <- df$Country
df <- df[,-1]  # 나라이름 없애기
df[] <- lapply(df, function(x){as.numeric(x)})
no_na <- colSums(is.na(df))
na_col_list = vector()

for (i in 1:length(df)){
  if (no_na[[i]] != 0){
    na_col_list = append(na_col_list, i)
  }
}
df <- df[,na_col_list*-1]
df <- df[,-41]
df <- df[,c(-16, -32)]

# Elasticnet 정규화
df_std <- as.data.frame(scale(df))
sapply(df_std, sd) # 표준편차
sapply(df_std, mean) # 평균


# 다중공선성에 의한 변수 삭제
vif_model <- lm(`Birth rate` ~ ., data = df_std)
vif_value <- vif(vif_model) > 5
df_vif <- df_std[, vif_value]


# 표준화 계수 구하기
df_regression <- lm(`Birth rate` ~ ., data=df_vif)
null <- lm(`Birth rate` ~ 1, data=df_vif)

step_df <- step(null, scope=list(lower=null, upper=df_regression),
                direction="forward")
summary(step_df)

lm.beta(step_df)
round(lm.beta(step_df), 3)


### 검증 
# 다중공선성 진단을 위한 VIF 값 구하기
vif(step_df)

# sjplot
set_theme(axis.title.size = 1.0, axis.textsize = 1.0)
plot_model(step_df, type = "est", wrap.labels=5)
# plot_model(step_df, type = "std", wrap.labels=5) # 에러남
plot_model(step_df, type = "diag", wrap.labels=5)

# plot으로 알아보기
#정규성 검정
par(mfrow = c(2,2))# plot 4개를 동시에 표시하기 위함
plot(step_df)

# [등분산성] 독립변수 old ratio, Infant mortality rate 값이 증가할 수록 birth rate 값이 선형관계라면
# 예측값(predicted values)과 잔차(Residuals)가 일정한 관계를 가지면 안된다. 빨간 줄이 오른쪽으로 갈 수록 위로 솟아지고 일정 부분에 직선 성향을 띠기에 낮은 선형관계에 있다고 한다

# [정규성 검증] 잔차가 정규성을 갖기 위해서는 세로축의 Standardized residuals 값이 가로축의 Quantiles이 증가하여도
# 일정하게 45도 각도로 증가하여야 한다. 선이 약간씩 벗어나지만 일직선이므로 잔차의 분포는 정규성을 띠었다 볼 수 있다.

# [등분산성] 분산이 일정하다면 값이 무작위로 찍혀야 한다. 선이 오른쪽으로 갈 수록 위로 솟아지지만 대략적으로 무작위로 찍혔다.

# [영향점] 개개 관측치의 이상치, 통계 모형 계수에 영향을 줄 수 있는 관측치를 확인하기 위해 Cook's distance 가 빨간 점선으로 표시된다.
# 특정 값이 통계 모형 계수에 영향을 준다.

# Elasticnet 해석하기
set.seed(12345) # 난수 생성_ 무작위 숫자 추출  
df_elastic <- cv.glmnet(as.matrix(df_vif[,-28]),
                        df_vif$OECD,
                        family = "gaussian", alpha = .5,
                        nfolds = 4, type.measure = "mse")

# 정규분포를 따르는 선형회귀분석 시행  
# cv.glmnet 함수로 x와 y 정의하고 ElasticNet Regression 방법으로 분석할 것이기 때문에 alpha값은 0.5로 지정해 비중을 조정해주었다.성과의 기준치는 평균 제곱근 오차(mse) 설정하여  SSE 최소화  

plot(df_elastic)      # 최적의 lambda값 확인
log(df_elastic$lambda.min)     # MSE가 가장 작을때의 lambda
log(df_elastic$lambda.1se)     # 가장 작을때 1표준편차 안에서 가장 간결한 모델부분의 lambda.
plot(df_elastic$glmnet.fit, xvar = "lambda")   # 변수가 추가될 때마다 모수 추정값의 변화를 볼 수 있다.
coef.elastic <- coef(df_elastic, s = "lambda.min")[,1]  # 계수확인 
coef.elastic
mse.min.elastic <- df_elastic$cvm[df_elastic$lambda == df_elastic$lambda.min]
mse.min.elastic #  elestic 모델에서 최적의 mse값

r2.min.elastic <- df_elastic$glmnet.fit$dev.ratio[df_elastic$lambda == df_elastic$lambda.min]
r2.min.elastic
#elestic 모델에서 최적의 R-squared값 

# lm모델 출력 비교
res.lm <- lm(`Birth rate` ~ ., data=df_std)
summary(res.lm)
library(dvmisc)
get_mse(res.lm)

par(mfrow = c(1,1))
#===================================================================================================


# OECD 추가하고 분석


# df_vif이용 (다중공선성, 정규화 해결된 데이터)
Country <- rownames(df_vif)
df_vif <- cbind(df_vif, Country)
df_vif <- left_join(df_vif, df_oecd, by='Country')
df_vif$OECD[is.na(df_vif$OECD)] <- 0  # OECD여부에 따라 맞으면1, 아니면0
rownames(df_vif) <- df_vif[,28]
df_vif <- df_vif[,-28] # Country 삭제
df_vif$OECD <- as.numeric(df_vif$OECD)

# 다중공선성에 의한 변수 삭제
vif_model <- lm(`OECD` ~ ., data = df_vif)
vif_value <- vif(vif_model) > 5
df_vif <- df_vif[, !vif_value]


df_log_reg <- glm(OECD ~ ., family = binomial, data = df_vif, maxit = 100) # 변수 초과에 의해 정답률이 1이 나온다.
summary(df_log_reg)

df_log_reg_Null = glm(OECD ~ 1, family = binomial, data = df_vif, maxit = 100)
summary(df_log_reg_Null)

anova(df_log_reg_Null, df_log_reg, test="LRT")

step_df_vif_log_reg <- step(df_log_reg_Null, scope=list(lower=df_log_reg_Null, upper=df_log_reg),
                            direction="forward")
summary(step_df_vif_log_reg)


coef(step_df_vif_log_reg)

vif(step_df_vif_log_reg)

# 평가
df_log_reg_graph <- ROC(form = `OECD` ~ `old ratio`+
                          `infant mortality rate`+
                          `industry employed`+
                          `gini_score`+
                          `Women suicide`,
                        data = df_vif,
                        plot = "ROC")

df_log_reg_graph$res[round(df_log_reg_graph$res$lr.eta,3) == 0.674,]
df_log_reg_graph$AUC
df_log_reg_graph$lr

# caret
confusionMatrix(
  as.factor(ifelse(predict(step_df_vif_log_reg, type = "response")>0.674,0,1)),
  as.factor(df_log_reg$y),
  positive = '1')



#=============================================================================
# 나이브 베이즈
mygroup = function (y,k=4){
  count = length(y)
  z = rank(y,ties.method = "min")
  return(floor((z-1)/(count/k))+1)
}
df_vif_factor <- data.frame()
df_vif_factor <- lapply(df_vif, function(x){mygroup(x,10)})
df_vif_factor <- as.data.frame(df_vif_factor)

df_vif_factor$OECD <- ifelse(df_vif_factor$OECD == '6', 1, 0)

train_idx = sample(73, 73*3/4)
df_train = df_vif_factor[train_idx,]
df_test = df_vif_factor[-train_idx,]
df_train_labels = df_vif_factor[train_idx,]$OECD
df_test_labels = df_vif_factor[-train_idx,]$OECD

df_train = df_train[,-29] # OECD 열을 삭제

prop.table(table(df_train_labels))
prop.table(table(df_test_labels))
# 비슷하다.

str(df_train_labels)
str(df_train)

# 모델 생성
library(e1071)
library(descr)
df_classifier = naiveBayes(df_train, df_train_labels)
df_test_pred = predict(df_classifier, df_test)

CrossTable(df_test_pred, df_test_labels,
           prop.chisq = F, prop.c = F, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

confusionMatrix(as.factor(df_test_pred),
                as.factor(df_test_labels), 
                positive = '1')

# MSE 구하기
pred = predict(df_regression, data=df)
mse(df$`Birth rate`, pred)


