setwd("/Users/parkjuhwan/Documents/아주대학교/2021/data_anlyst_R")
#data = read.csv("Inner_Datasets_in_R.csv")
rm(list = ls())
#-------------------------------------------------------------------------------
suppressMessages(library(magrittr))
suppressMessages(library()) %>% suppressWarnings()

df_culture = read.csv('6-dimensions-for-website-2015-08-16.csv', header = T, sep = ';')
df_inno = read.csv('Analysis_2021may.24161011.csv', header = T)
df_country = read.csv('countryContinent.csv')

head(df_culture) # 대략적 확인
df_culture[] = lapply(df_culture, function(x) gsub("#NULL!", '0', x)) # '#NULL!'값 제거
df_culture = df_culture[, -1] # 필요행만 서브셋팅
df_inno = df_inno[,c(2, 5)]
df_country = df_country[, c(1, 6)]
