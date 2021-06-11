rm(list = ls())

library(XML)
library(httr)

url <- "https://worldpopulationreview.com/country-rankings/birth-rate-by-country"
html_source <- GET(url)
tabs <- readHTMLTable(rawToChar(html_source$content), stringsAsFactors=F)
# 종속변수
df_birth_rate = as.data.frame(tabs)


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


url <- "https://en.wikipedia.org/wiki/Education_Index"
html_source <- GET(url)
tabs <- readHTMLTable(rawToChar(html_source$content), stringsAsFactors=F)
# 교육수준 
df_study = as.data.frame(tabs[1])
colnames(df_study) = df_study[1,] ; df_study = df_study[-1,]


url <- "https://en.wikipedia.org/wiki/List_of_countries_by_suicide_rate"
html_source <- GET(url)
tabs <- readHTMLTable(rawToChar(html_source$content), stringsAsFactors=F)
# 자살률
suicide = as.data.frame(tabs[1])
colnames(suicide) = suicide[1,] ; suicide = suicide[-1,]

url <- "https://www.numbeo.com/cost-of-living/prices_by_country.jsp?displayCurrency=USD&itemId=224"
html_source <- GET(url)
tabs <- readHTMLTable(rawToChar(html_source$content), stringsAsFactors=F)
# 육아비용 
child_cost = as.data.frame(tabs[2])
child_cost = child_cost[,-1]

happy <- read.csv("world-happiness-report-2021.csv")

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_age_structure"
html_source <- GET(url)
tabs <- readHTMLTable(rawToChar(html_source$content), stringsAsFactors=F)
# 65세 이상 인구 비율
aged_rate = as.data.frame(tabs[1])
colnames(aged_rate) = aged_rate[1,] ; aged_rate = aged_rate[-1,]
aged_rate = aged_rate[-1,c(-2:-3)]


