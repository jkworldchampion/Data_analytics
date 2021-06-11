rm(list = ls())

library(XML)
library(httr)


# 종속변수
url <- "https://worldpopulationreview.com/country-rankings/birth-rate-by-country"
html_source <- GET(url)
tabs <- readHTMLTable(rawToChar(html_source$content), stringsAsFactors=F)
df_birth_rate = as.data.frame(tabs)

# 첫번째 결혼 나이 
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

# 교육수준 
url <- "https://en.wikipedia.org/wiki/Education_Index"
html_source <- GET(url)
tabs <- readHTMLTable(rawToChar(html_source$content), stringsAsFactors=F)
df_study = as.data.frame(tabs[1])
colnames(df_study) = df_study[1,] ; df_study = df_study[-1,]

# 자살률
url <- "https://en.wikipedia.org/wiki/List_of_countries_by_suicide_rate"
html_source <- GET(url)
tabs <- readHTMLTable(rawToChar(html_source$content), stringsAsFactors=F)
suicide = as.data.frame(tabs[1])
colnames(suicide) = suicide[1,] ; suicide = suicide[-1,]

# 육아비용 
url <- "https://www.numbeo.com/cost-of-living/prices_by_country.jsp?displayCurrency=USD&itemId=224"
html_source <- GET(url)
tabs <- readHTMLTable(rawToChar(html_source$content), stringsAsFactors=F)
child_cost = as.data.frame(tabs[2])
child_cost = child_cost[,-1]

# 65세 이상 인구 비율
url <- "https://en.wikipedia.org/wiki/List_of_countries_by_age_structure"
html_source <- GET(url)
tabs <- readHTMLTable(rawToChar(html_source$content), stringsAsFactors=F)
aged_rate = as.data.frame(tabs[1])
colnames(aged_rate) = aged_rate[1,] ; aged_rate = aged_rate[-1,]
aged_rate = aged_rate[-1,c(-2:-3)]

# 행복지수 
happiness <- read.csv("https://raw.githubusercontent.com/jkworldchampion/Data_analytics/ahyoung/%EB%82%98%EB%9D%BC%EB%B3%84%20%ED%96%89%EB%B3%B5%EC%A7%80%EC%88%98.csv")
happiness <- happiness[,2:3]

# 지니계수
genie <- read.csv("https://raw.githubusercontent.com/jkworldchampion/Data_analytics/main/%EC%A7%80%EB%8B%88%EA%B3%84%EC%88%98.csv")
genie 

#낙태 합법인 국가
abortionlegal <- read.csv("https://raw.githubusercontent.com/jkworldchampion/Data_analytics/main/abortionlegal.csv")
abortionlegal <- abortionlegal[-219,-3]
abortionlegal <- as.data.frame(abortionlegal)
colnames(abortionlegal) = c("country", "legal")
abortionlegal$legal[abortionlegal$legal != 'yes'] <- 'no'

