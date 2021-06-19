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

df_study[92,1] <- "southkorea"
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



# 혁신데이터 
df_innovation <- read.csv("")

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

#=======================================================================================================================================

#다중회귀분석 

# 1. 나라별 결혼 나이는 출산율에 영향을 미칠 것이다.  
# 2. 나라별 교육 수준의 차이는 출산율에 영향을 미칠 것이다.  
# 3. 나라별 자살율은 출산율에 영향을 미칠 것이다.  
# 4. 나라별 육아비용은 출산율에 영향을 미칠 것이다.  
# 5. 나라별 65새 이상 인구 비율은 출산율에 영향을 미칠 것이다.  
# 6. 나라별 행복지수는 출산율에 영향을 미칠 것이다.  
# 7. 나라별 지니계수는 출산율에 영향을 미칠 것이다.  
# 8. 나라별 육아비용은 출산율에 영향을 미칠 것이다.  
# 9. 나라별 낙태 현황 현황은 출산율에 영향을 미칠 것이다.  


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


# 표준화 계수 구하기
df_regression <- lm(`Birth rate` ~ ., data=df_std)
null <- lm(`Birth rate` ~ 1, data=df_std)

step_df <- step(null, scope=list(lower=null, upper=df_regression),
                direction="forward")
summary(step_df)

lm.beta(step_df)
round(lm.beta(step_df), 3)

# 다중공선성 진단을 위한 VIF 값 구하기
vif(step_df)

# ‘sjPlot’ 패키지를 이용한 결과표 및 도표 출력
library(sjPlot)
tab_model(regression, show.se = T, show.ci = F, show.stat= T)

# 한글을 사용
tab_model(regression, show.se = T, show.ci = F, show.stat= T,
          pred.labels = c("(Intercept)", "지니계수", "낙태 합법", "교육수준",
                          "고령화", "행복지수","양육 비용","남성 자살율","여성 자살율", "gdp", "평등지수"), dv.labels = c("출산율"), file =
            "multiple_regression.html")
file.show("multiple_regression.html")


