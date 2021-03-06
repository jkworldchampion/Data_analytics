# 출산률 - 국가적 차원에서의 고찰
# 서론 
## 우리의 궁극적 목표

# 본론
## 1 데이터 전처리
### 1.1 필요 라이브러리 불러오기
rm(list = ls())
suppressMessages(library(tidyverse))
suppressMessages(library(glmnet)) %>% suppressWarnings()
suppressMessages(library(dplyr)) %>% suppressWarnings()
suppressMessages(library(lm.beta)) %>% suppressWarnings()
suppressMessages(library(XML)) %>% suppressWarnings()
suppressMessages(library(httr)) %>% suppressWarnings()
suppressMessages(library(dplyr)) %>% suppressWarnings()
suppressMessages(library(stringr)) %>% suppressWarnings()
suppressMessages(library(QuantPsyc)) %>% suppressWarnings()
suppressMessages(library(car)) %>% suppressWarnings()
suppressMessages(library(sjPlot)) %>% suppressWarnings()
suppressMessages(library(Epi)) %>% suppressWarnings()
suppressMessages(library(caret)) %>% suppressWarnings()
library(dvmisc)
library(Metrics)
library(yardstick) 
options(scipen = 1000) # 옵션으로 숫자 형태 조절


### 1.2 데이터 불러오기

# 출산율
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


### 1.3 데이터 합치기, 이름바꾸기  

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
                         "sex ratio", "gdp_", "gdp growth rate", "gdp per capita", "agriculture",
                         "industry","services and other activity", "agriculture employed",
                         "industry employed","service employed", "unemployed", "labour force",
                         "agriculture production", "food production","exports", "imports",
                         "trade balance", "balance of paments", "population growth rate", "urban population",
                         "urban population growth rate", "fertility rate", "life expectancy at birth", 
                         "population age distribution", "international migrant", "refugees",
                         "infant mortality rate","health expenditure", "physicians",
                         "education government expenditure", "primary education", 
                         "secondary education", "tertiary education",
                         "women politician", "internet using", "threatened species",
                         "forested area", "CO2 emission", "energy production", "energy supply",
                         "water", "sanitation facilities", "net official")



### 1.4 데이터 변형

# 낙태 합법 변수 재부호화
df$legal <- ifelse(df$legal == 'yes', 1, 0)

# 65 세 이상 변수 퍼센트 없애기
df$`old ratio` <- gsub("[[:punct:]]", "", df$`old ratio`)     # %없애주기
df$`old ratio` <- gsub("\u00A0", "", df$`old ratio`)          # 나머지 부분 없애주기

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
head(sapply(df_std, sd))     # 표준편차
head(sapply(df_std, mean))   # 평균

# 다중공선성에 걸리는 변수 삭제
vif_model <- lm(`Birth rate` ~ ., data = df_std)
vif_value <- vif(vif_model) > 5
df_vif <- df_std[, vif_value]

# 다중공선성 확인 그래프 하나 추가


## 2 국가별 출산율 분석
### 2.1 다중선형 회귀분석

df_regression <- lm(`Birth rate` ~ ., data=df_vif)
df_regression_null <- lm(`Birth rate` ~ 1, data=df_vif)

step_df <- step(df_regression_null, scope=list(lower=df_regression_null, upper=df_regression),
                direction="forward")

summary(step_df) # 요약본 보기
round(lm.beta(step_df), 3)    # 표준화 계수확인 


### 2.2 검증

vif(step_df) # 다중공선성 확인

# sjplot
set_theme(axis.title.size = 1.0, axis.textsize = 1.0)
plot_model(step_df, type = "est", wrap.labels=5)
plot_model(step_df, type = "diag", wrap.labels=5)

# plot으로 알아보기
#정규성 검정
par(mfrow = c(2,2))# plot 4개를 동시에 표시하기 위함
plot(step_df)
par(mfrow = c(1,1))


### 2.3 Elasticnet 분석
set.seed(12345) # 시드설정 
df_elastic <- cv.glmnet(as.matrix(df_vif[,-7]),
                        df_vif$`Birth rate`,
                        family = "gaussian", alpha = .5,
                        nfolds = 4, type.measure = "mse")

plot(df_elastic)      # 최적의 lambda값 확인
log(df_elastic$lambda.min)     # MSE가 가장 작을때의 lambda
log(df_elastic$lambda.1se)     # 가장 작을때 1표준편차 안에서 가장 간결한 모델일 때 lambda.
plot(df_elastic$glmnet.fit, xvar = "lambda")   # 변수가 추가될 때마다 모수 추정값의 변화를 볼 수 있다.
coef.elastic <- coef(df_elastic, s = "lambda.min")[,1] 
coef.elastic       # 계수확인 
mse.min.elastic <- df_elastic$cvm[df_elastic$lambda == df_elastic$lambda.min]
mse.min.elastic    # elestic 모델에서 최적의 mse값
r2.min.elastic <- df_elastic$glmnet.fit$dev.ratio[df_elastic$lambda == df_elastic$lambda.min]
r2.min.elastic     # elestic 모델에서 최적의 R-squared값 

# step_df 분석치
step_df_mse <- sum( (step_df$residuals)^2 )/nrow(df_vif)
step_df_mse


## 3 OECD 회원국 분석
### 3.1 로지스틱 회귀분석
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
vif_value <- vif(vif_model) > 10
df_vif_modify <- df_vif[, !vif_value]

df_log_reg <- glm(`OECD` ~ ., family = binomial, data = df_vif_modify, maxit = 100) # 변수 초과에 의해 정답률이 1이 나온다.
summary(df_log_reg)

df_log_reg_Null = glm(OECD ~ 1, family = binomial, data = df_vif_modify, maxit = 100)
summary(df_log_reg_Null)

# anova(df_log_reg_Null, df_log_reg, test="LRT")

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
                        data = df_vif_modify,
                        plot = "ROC")

df_log_reg_graph$res[round(df_log_reg_graph$res$lr.eta,3) == 0.674,]
df_log_reg_graph$AUC
df_log_reg_graph$lr

# caret
confusionMatrix(
  as.factor(ifelse(predict(step_df_vif_log_reg, type = "response")>0.674,0,1)),
  as.factor(df_log_reg$y),
  positive = '1')




# 결론

