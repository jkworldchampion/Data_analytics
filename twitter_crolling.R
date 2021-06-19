library(twitteR)
library(dplyr)

source(file = "Code/TwitterAPIKey(temp).R", echo = FALSE)
keyword_ko <- enc2utf8("재기")

tweets <- searchTwitter(keyword_ko, n=100, lang="ko")#, since="2021-02-11", until="2021-03-30")
search_twee
keyword_ko <- enc2utf8("아이즈원")      ## 오늘은 한글 키워드만 모아서 시각화까지 해봅시다
izone_ko <- search_tweets(keyword_ko, n=1000, lang="ko", since="2020-01-01", until="2020-06-30")











rm(list = ls())
library(RSelenium)


remDr <- remoteDriver(port = 4445L, browserName="chrome") 
remDr$open()



remDr$navigate(paste0("https://twitter.com/search?q=%ED%8E%98%EB%AF%B8%EB%8B%88%EC%A6%98%20until%3A2021-01-31%20since%3A2021-01-01%20-filter%3Alinks%20-filter%3Areplies&src=typed_query")) 
path = "/html/body/div/div/div/div[2]/main/div/div/div/div[1]/div/div[2]/div/div/section/div/div/div[1]/div/div/article/div/div/div/div[2]/div[2]/div[2]/div[1]/div/span[1]"
path1= "/html/body/div/div/div/div[2]/main/div/div/div/div[1]/div/div[2]/div/div/section/div/div/div[2]/div/div/article/div/div/div/div[2]/div[2]/div[2]/div[1]/div/span[3]"
webElem <- remDr$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end"))
test = remDr$findElement(using="xpath", value=path1)


word_text = vector()
for (i in 1:100){
  path = "/html/body/div/div/div/div[2]/main/div/div/div/div[1]/div/div[2]/div/div/section/div/div/div["
  path_end = "]/div/div/article/div/div/div/div[2]/div[2]/div[2]/div[1]/div/span[1]"
  path = paste0(path, i)
  path = paste0(path, path_end)
  test = remDr$findElement(using="xpath", value=path)
  word_text = append(test$getElementText(), word_text)
  webElem <- remDr$findElement("css", "body")
  webElem$sendKeysToElement(list(key = "down_arrow"))
  
}
length(word_text)







Hate_Word_Count <- function(url_path) {
  remDr$navigate(paste0(url_path)) 
  word_text = vector()
  for (i in 1:100){
    path = "/html/body/div/div/div/div[2]/main/div/div/div/div[1]/div/div[2]/div/div/section/div/div/div["
    path_end = "]/div/div/article/div/div/div/div[2]/div[2]/div[2]/div[1]/div/span[1]"
    path = paste0(path, i)
    path = paste0(path, path_end)
    test = remDr$findElement(using="xpath", value=path)
    word_text = append(test$getElementText(), word_text)
  }
  len = length(word_text)
  return (len)
}





