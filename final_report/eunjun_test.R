rm(list = ls())

library(XML)
library(httr)
library(dplyr)
library(stringr)

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


# 육아비용 
url <- "https://www.numbeo.com/cost-of-living/prices_by_country.jsp?displayCurrency=USD&itemId=224"
html_source <- GET(url)
tabs <- readHTMLTable(rawToChar(html_source$content), stringsAsFactors=F)
df_childcost = as.data.frame(tabs[2])
df_childcost = df_childcost[,-1]
colnames(df_childcost) <- c("Country", "child cost")
df_childcost$Country <- tolower(df_childcost$Country)
df_childcost$Country <- gsub(" ", "", df_childcost$Country)

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

# 행복지수 
df_happiness <- read.csv("https://raw.githubusercontent.com/jkworldchampion/Data_analytics/ahyoung/%EB%82%98%EB%9D%BC%EB%B3%84%20%ED%96%89%EB%B3%B5%EC%A7%80%EC%88%98.csv")
df_happiness <- df_happiness[,2:3]
colnames(df_happiness) <- c("Country", "happy score")
df_happiness$Country <- tolower(df_happiness$Country)
df_happiness$Country <- gsub(" ", "", df_happiness$Country)


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


#=======================================================================================================================================
df <- inner_join(df_gini, df_abortionlegal, by = 'Country')

df <- inner_join(df_gini, df_study, by = 'Country')

df <- inner_join(df_gini, df_suicide, by='Country')

df <- inner_join(df_gini, df_childcost, by='Country')

df <- inner_join(df_gini, df_oldratio, by='Country')

df <- inner_join(df_gini, df_happiness, by='Country')

df <- inner_join(df_gini, df_gini, by='Country')

df <- inner_join(df_gini, df_abortionlegal, by='Country')
