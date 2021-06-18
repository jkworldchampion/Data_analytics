setwd("/Users/parkjuhwan/Documents/아주대학교/2021/data_anlyst_R/card_man")
#data = read.csv("Inner_Datasets_in_R.csv")
rm(list = ls())
#-------------------------------------------------------------------------------
suppressMessages(library(magrittr))
suppressMessages(library(sjPlot)) %>% suppressWarnings()
suppressMessages(library(dplyr)) %>% suppressWarnings()
suppressMessages(library(tidyverse)) %>% suppressWarnings()
suppressMessages(library(tidymodels)) %>% suppressWarnings()

#-------------------------------------------------------------------------------


card <- read.csv("train.csv") 



#-----------------------------------------------------------------------------------------------------------
# 귀무가설 : 성별에 따라 신용등급은 같을 것이다.
# 카이제곱 검정

# 카이제곱으로 검정하기 위해 신용등급의 0,1,2를 좋다, 나쁘다로 바꾼다.


credibility <- vector(length = length(card[,1])) # 벡터 생성

# 0과 1이 좋은거, 2가 나쁜 것
for (i in 1:length(card[,1])) {
  if (card$credit[i] == 2) {
    credibility[i] <- 'bad' 
  } else {
    credibility[i] <- 'good' 
  }
  rm(i)
}

card <- cbind(card, credibility) # 추가해주기

gender_credibility <- card[,c(2,21)]

sjt.xtab(gender_credibility$credibility, gender_credibility$gender, 
         show.col.prc = T, 
         show.exp = T, var.labels=c("신용도", "성별"), 
         encoding="UTF-8" )


plot_xtab(gender_credibility$credibility, gender_credibility$gender, 
          type="bar",
          y.offset = 0.01,
          margin = "col", coord.flip = F, wrap.labels = 7,
          geom.colors = "Set2", show.summary = T, show.total = F,
          axis.titles = "Credibility",
          #axis.labels = c(),
          legend.title = "Gender")


# 결론 : p값이 매우 크기 때문에 귀무가설을 기각할 수 없다.



#------------------------------------------------------------------------------------------------------
# 귀무가설 : 남여의 수입은 같을 것이다.
# t 검정 중 독립표본 T검정.

man <- data.frame(1, 2)
colnames(man) <- c('gender', 'income_total')
girl <- data.frame(1, 2)
colnames(girl) <- c('gender', 'income_total')
gender_income <- card[,c(2,6)]

for (i in 1:length(gender_income[,1])) {
  if (gender_income[i, 1] == 'M') {
    man <<- rbind(man, gender_income[i,])
  } else {
    girl <<- rbind(girl, gender_income[i,])
  }
}

t.test(man$income_total, girl$income_total)

# 결론 : P 값이 매우 작다. 따라서 남여의 임금은 다르다는 대립가설을 채택한다.

#-------------------------------------------------------------------------------------------------------
# 귀무가설 : 차량 소유 여부와 상관없이 신용등급은 같을 것이다. 
# 카이제곱 검정 

car_credibility <- card[, c(3, 21)]

sjt.xtab(car_credibility$credibility, car_credibility$car, 
         show.col.prc = T, 
         show.exp = T, var.labels=c("신용도", "차 소유"), 
         encoding="UTF-8" )

plot_xtab(car_credibility$credibility, car_credibility$car,
          type="bar",
          y.offset = 0.01,
          margin = "col", coord.flip = F, wrap.labels = 7,
          geom.colors = "Set2", show.summary = T, show.total = F,
          axis.titles = "Credibility",
          #axis.labels = c(),
          legend.title = "Car")

# 수치 상으로는 p값이 0.023이다. 우리는 유의수준을 1%로 하기 때문에 기각 불가능.

#---------------------------------------------------------------------------------------------------------
# 귀무가설 : 부동산 자산 소유 여부와 상관없이 신용등급을 같다.
# 카이제곱 검정

reality_credibility <- card[, c(4, 21)]

sjt.xtab(reality_credibility$credibility, reality_credibility$reality,
         show.col.prc = T, 
         show.exp = T, var.labels=c("신용도", "부동산 소유"), 
         encoding="UTF-8" )

plot_xtab(reality_credibility$credibility, reality_credibility$reality, 
          type="bar",
          y.offset = 0.01,
          margin = "col", coord.flip = F, wrap.labels = 7,
          geom.colors = "Set2", show.summary = T, show.total = F,
          axis.titles = "Credibility",
          #axis.labels = c(),
          legend.title = "Reality")
# p-value = 0.01,  기각할 수 없다.

#------------------------------------------------------------------------------------------------------
# 귀무가설 : 아이 수와 신용 등급은 같을 것이다.
# 카이제곱 검정

child_num_credibility <- card[, c(5, 21)]

sjt.xtab(child_num_credibility$credibility, child_num_credibility$child_num,
         show.col.prc = T, 
         show.exp = T, var.labels=c("신용도", "자녀 수"), 
         encoding="UTF-8" )

# 여기서 문제는 자녀의 수가 4 이상 부터는 너무나 작은 수가 나왔기 때문에 신뢰하기가 힘들다.
# 때문에 4이상을 없애겠다.

child_num_credibility_revice <- child_num_credibility[child_num_credibility[,1] < 4,]

sjt.xtab(child_num_credibility_revice$credibility, child_num_credibility_revice$child_num,
         show.col.prc = T, 
         show.exp = T, var.labels=c("신용도", "자녀 수"), 
         encoding="UTF-8" )
 
plot_xtab(child_num_credibility_revice$credibility, child_num_credibility_revice$child_num, 
          type="bar",
          y.offset = 0.01,
          margin = "col", coord.flip = F, wrap.labels = 7,
          geom.colors = "Set2", show.summary = T, show.total = F,
          axis.titles = "Credibility",
          #axis.labels = c(),
          legend.title = "Child_number")
# 결론 : 기각할 수 없다.

#--------------------------------------------------------------------------------------
# 귀무가설 : 수입 수치 등급과는 신용도의 차이가 없다.

income_credibility <- card[, c(6, 21)]   #여기선 위안으로 계산 됨.
max(income_credibility[,1])
min(income_credibility[,1])
mean(income_credibility[,1])  # 187306.5위안 = 약 3000만원

# 오늘 위안/원 환율을 통해 값을 원화 값으로 변환
# 1 위안 = 171 원
income_credibility[,1] <- income_credibility[,1] * 171
options(scipen=999) # 수학이 아닌 실제 값으로 표현
plot(income_credibility[,1])
# 구간을 국내 전체 가구 평균기준, 분위수의 값으로 변환시킨다.
# 2019년 10월 데이터이기 때문에 2019년 4분기의 데이터를 가지고 기준을 정했다.
income_level <- read.csv("소득10분위별__가구당_가계수지__전국_2인이상__20210425191731.csv", fileEncoding = 'euc-kr', header = T)
income_level <- income_level[-1,]
# 위의 각 데이터는 분위수 별 소득의 평균치 이다. 때문에 우리는 각 분위 마다 같은 분포를 갖고 있을 것을 가정하고 양쪽의 평균값으로 단계의 기준을 정했다.
sep <- vector() # 분위수 결정치를 위한 벡터 설정

for (i in 1:(length(income_level[,3]))) {
  sep[i] <- (as.numeric(income_level[i, 3]) + as.numeric(income_level[i+1, 3]))/2
}
# 이 수치는 월수익이다. 연수익으로 교체
sep <- sep*12

income_group <- vector()
for (i in income_credibility[,1]) {
  if (i <= sep[1]) {
    income_group <- append(income_group, 1)
  } else if (i <= sep[2]) {
    income_group <- append(income_group, 2)
  } else if (i <= sep[3]) {
    income_group <- append(income_group, 3)
  } else if (i <= sep[4]) {
    income_group <- append(income_group, 4)
  } else if (i <= sep[5]) {
    income_group <- append(income_group, 5)
  } else if (i <= sep[6]) {
    income_group <- append(income_group, 6)
  } else if (i <= sep[7]) {
    income_group <- append(income_group, 7)
  } else if (i <= sep[8]) {
    income_group <- append(income_group, 8)
  } else if (i <= sep[9]) {
    income_group <- append(income_group, 9)
  } else {
    income_group <- append(income_group, 10)
  }
}
head(income_group)
income_credibility <- cbind(income_credibility, income_group)

sjt.xtab(income_credibility$credibility, income_credibility$income_group,
         show.col.prc = T, 
         show.exp = T, var.labels=c("신용도", "수입"), 
         encoding="UTF-8" )

plot_xtab(income_credibility$credibility, income_credibility$income_group, 
          type="bar",
          y.offset = 0.01,
          margin = "col", coord.flip = F, wrap.labels = 7,
          geom.colors = "Set", show.summary = T, show.total = F,
          axis.titles = "Credibility",
          #axis.labels = c(),
          legend.title = "income_type")

#-----------------------------------------------------------------------------------
# 귀무가설 : 수입 타입에 따라 신용도는 변화하지 않는다.

income_type_credibility <- card[, c(7, 21)]

sjt.xtab(income_type_credibility$credibility, income_type_credibility$income_type,
         show.col.prc = T, 
         show.exp = T, var.labels=c("신용도", "수입 타입"), 
         encoding="UTF-8" )

plot_xtab(income_type_credibility$credibility, income_type_credibility$income_type, 
          type="bar",
          y.offset = 0.01,
          margin = "col", coord.flip = F, wrap.labels = 7,
          geom.colors = "Set2", show.summary = T, show.total = F,
          axis.titles = "Credibility",
          #axis.labels = c(),
          legend.title = "income_type")

# 결론 : 기각할 수 없다.

#-----------------------------------------------------------------------------------
# 귀무가설 : 교육 벙식에 따라 신용도는 변하지 않는다.

academic_type_credibility <- card[,c(8, 21)]

sjt.xtab(academic_type_credibility$credibility, academic_type_credibility$edu_type,
         show.col.prc = T, 
         show.exp = T, var.labels=c("신용도", "교육방식"), 
         encoding="UTF-8" )

plot_xtab(academic_type_credibility$credibility, academic_type_credibility$edu_type, 
          type="bar",
          y.offset = 0.01,
          margin = "col", coord.flip = F, wrap.labels = 7,
          geom.colors = "Set2", show.summary = T, show.total = F,
          axis.titles = "Credibility",
          #axis.labels = c(),
          legend.title = "edu_type")
# 결론 : 기각 할 수 없다.

#-----------------------------------------------------------------------------------
# 귀무가설 : 가족 타입에 따라 신용도는 변화하지 않는다.

family_type_credibility <- card[, c(9, 21)]

sjt.xtab(family_type_credibility$credibility, family_type_credibility$family_type,
         show.col.prc = T, 
         show.exp = T, var.labels=c("신용도", "가족 타입"), 
         encoding="UTF-8" )

plot_xtab(family_type_credibility$credibility, family_type_credibility$family_type, 
          type="bar",
          y.offset = 0.01,
          margin = "col", coord.flip = F, wrap.labels = 7,
          geom.colors = "Set2", show.summary = T, show.total = F,
          axis.titles = "Credibility",
          #axis.labels = c(),
          legend.title = "family_type")
# 결론 : 가족 타입에 따라 크게 유의미한 차이는 없다.
#------------------------------------------------------------------------------------
# 귀무가설 : 집 종류에 따라

house_type_credibility <- card[,c(10, 21)] 

sjt.xtab(family_type_credibility$credibility, family_type_credibility$family_type,
         show.col.prc = T, 
         show.exp = T, var.labels=c("신용도", "집 종류"), 
         encoding="UTF-8" )

plot_xtab(family_type_credibility$credibility, family_type_credibility$family_type, 
          type="bar",
          y.offset = 0.01,
          margin = "col", coord.flip = F, wrap.labels = 7,
          geom.colors = "Set2", show.summary = T, show.total = F,
          axis.titles = "Credibility",
          #axis.labels = c(),
          legend.title = "House_type")

# 결론 : 유의미 x
#------------------------------------------------------------------------------------
# 귀무가설 : 나이 대에 따라 신용등급의 유의미한 차이는 없다.

# 코드 가공 - 이아영
birth <- card[,c(11)]
gender <- card[,c(2)]
age <- round(birth/365*-1)
age_data <- cbind(gender, age)
age_data

age_dae <- vector() #나이 대에 따른 새로운 벡터 생성

min(age_data[,2])  #21
max(age_data[,2])  #69 이기 때문에 20대부터 60대까지 데이터를 정리하였다.
for(i in 1:length(age_data[,1])){
  if(age_data[i,2] <= 29){
    age_dae[i] <- 20
  } else if(age_data[i,2] <= 39){
    age_dae[i] <- 30
  } else if(age_data[i,2] <= 49){
    age_dae[i] <- 40
  } else if(age_data[i,2] <= 59){
    age_dae[i] <- 50
  } else if(age_data[i,2] <=69){
    age_dae[i] <- 60
  }
}

age_data <- cbind(age_data,age_dae)
age_data <-as.data.frame(age_data)
age_data <- cbind(age_data, card$credibility)
age_credibility <- age_data[, c(3,4)]


sjt.xtab(age_credibility$`card$credibility`, age_credibility$age_dae,
         show.col.prc = T, 
         show.exp = T, var.labels=c("신용도", "나이 대"), 
         encoding="UTF-8" )

plot_xtab(age_credibility$`card$credibility`, age_credibility$age_dae, 
          type="bar",
          y.offset = 0.01,
          margin = "col", coord.flip = F, wrap.labels = 7,
          geom.colors = "Set2", show.summary = T, show.total = F,
          axis.titles = "Credibility",
          #axis.labels = c(),
          legend.title = "Age")

# 유의미한 차이라고 보기는 약간 힘들다. 하지만 차이는 있다.

#--------------------------------------------------------------------------------------
# 핸드폰의 유무 
card_credibility <- card[, c(15, 21)]

sjt.xtab(card_credibility$credibility, card_credibility$phone,
         show.col.prc = T, 
         show.exp = T, var.labels=c("신용도", "핸드폰 유무"), 
         encoding="UTF-8" )

plot_xtab(card_credibility$credibility, card_credibility$phone, 
          type="bar",
          y.offset = 0.01,
          margin = "col", coord.flip = F, wrap.labels = 7,
          geom.colors = "Set2", show.summary = T, show.total = F,
          axis.titles = "Credibility",
          #axis.labels = c(),
          legend.title = "phone")
# 무의미하다.

#--------------------------------------------------------------------------------------
# 이메일의 유무
emial_credibility <- card[, c(16, 21)]

sjt.xtab(emial_credibility$credibility, emial_credibility$email,
         show.col.prc = T, 
         show.exp = T, var.labels=c("신용도", "이메일 여부"), 
         encoding="UTF-8" )

plot_xtab(emial_credibility$credibility, emial_credibility$email, 
          type="bar",
          y.offset = 0.01,
          margin = "col", coord.flip = F, wrap.labels = 7,
          geom.colors = "Set2", show.summary = T, show.total = F,
          axis.titles = "Credibility",
          #axis.labels = c(),
          legend.title = "email")

# 무의미
#--------------------------------------------------------------------------------------
# 직업  # 19 종류라서 수정 필요

occpy_credibility <- card[, c(17, 21)]

sjt.xtab(occpy_credibility$credibility, occpy_credibility$occyp_type,
         show.col.prc = T, 
         show.exp = T, var.labels=c("신용도", "직업"), 
         encoding="UTF-8" )

plot_xtab(occpy_credibility$credibility, occpy_credibility$occyp_type, 
          type="bar",
          y.offset = 0.01,
          margin = "col", coord.flip = F, wrap.labels = 7,
          geom.colors = "Set2", show.summary = T, show.total = F,
          axis.titles = "Credibility",
          #axis.labels = c(),
          legend.title = "occpy_type")

#----------------------------------------------------------------------------------------
# 가입 날짜 

being_credibility <- card[, c(19, 21)]
being_credibility[,1] <- round(being_credibility[,1] /12 * -1, 0)

sjt.xtab(being_credibility$credibility, being_credibility$begin_month,
         show.col.prc = T, 
         show.exp = T, var.labels=c("신용도", "가입 연수"), 
         encoding="UTF-8" )

plot_xtab(being_credibility$credibility, being_credibility$begin_month, 
          type="bar",
          y.offset = 0.01,
          margin = "col", coord.flip = F, wrap.labels = 7,
          geom.colors = "Set2", show.summary = T, show.total = F,
          axis.titles = "Credibility",
          #axis.labels = c(),
          legend.title = "being_year")
#-------------------------------------------------------------------------------------
# 나의 모든 것을 건 imcome_total


####################################################
# t 검정 - 남여, 차, 부동산, 핸드폰, 이메일
# gender
girl_c <- card_unique[card_unique$gender == 'F', c(1, 18)]
man_c <- card_unique[card_unique$gender == 'M', c(1, 18)]

t.test(man_c$credit, girl_c$credit)


# car
car_y <- card_unique[card_unique$car == 'Y', c(2, 18)]
car_n <- card_unique[card_unique$car == 'N', c(2, 18)]

t.test(car_y$credit, car_n$credit)


# reality
rea_y <- card_unique[card_unique$reality == 'Y', c(3, 18)]
rea_n <- card_unique[card_unique$reality == 'N', c(3, 18)]

t.test(rea_n$credit, rea_y$credit)


# email
email_n <- card_unique[card_unique$email== '0', c(15, 18)]
email_y <- card_unique[card_unique$email == '1', c(15, 18)]

t.test(email_n$credit, email_y$credit)


# phone
phone_y <- card_unique[card_unique$phone == '1', c(14, 18)]
phone_n <- card_unique[card_unique$phone == '0', c(14, 18)]

t.test(phone_y$credit, phone_n$credit)

# 결론 모두 무의미.

## 일원 분산 분석 anova
income_type_c_u <- card_unique[,c(6, 18)] #income_type()
oneway.test(credit ~ income_type, data = income_type_c_u)

edu_type_c_u <- card_unique[,c(7, 18)] #edu_type
oneway.test(credit ~ edu_type, data = edu_type_c_u)


family_type_c_u <- card_unique[,c(8, 18)] #family_type
oneway.test(credit ~ family_type, data = family_type_c_u)

house_type_c_u <- card_unique[,c(9, 18)] #house_type
oneway.test(credit ~ house_type, data = house_type_c_u)

occyp_type_c_u <- card_unique[,c(16, 18)] #occyp_type
oneway.test(credit ~ occyp_type, data = occyp_type_c_u)

# income_group 재생성
in_to_c_u <- card_unique[, 5]
in_to_c_u <- in_to_c_u * 171 # 한화 기준으로 바꿔주기


income_group <- vector()
for (i in in_to_c_u) {
  if (i <= sep[1]) {
    income_group <- append(income_group, 1)
  } else if (i <= sep[2]) {
    income_group <- append(income_group, 2)
  } else if (i <= sep[3]) {
    income_group <- append(income_group, 3)
  } else if (i <= sep[4]) {
    income_group <- append(income_group, 4)
  } else if (i <= sep[5]) {
    income_group <- append(income_group, 5)
  } else if (i <= sep[6]) {
    income_group <- append(income_group, 6)
  } else if (i <= sep[7]) {
    income_group <- append(income_group, 7)
  } else if (i <= sep[8]) {
    income_group <- append(income_group, 8)
  } else if (i <= sep[9]) {
    income_group <- append(income_group, 9)
  } else {
    income_group <- append(income_group, 10)
  }
}

in_to_c_u <- cbind(in_to_c_u, income_group)
in_to_c_u <- data.frame(in_to_c_u)
in_to_c_u <- cbind(in_to_c_u, card_unique$credit)

oneway.test(card_unique$credit ~ income_group, data = in_to_c_u)

#birth_day
card_unique$DAYS_BIRTH <- card_unique$DAYS_BIRTH/365 * -1

old_dae <- vector()
for (i in card_unique$DAYS_BIRTH) {
  if (i <= 30) {
    old_dae <- append(old_dae, 20)  # 최저값이 21
  } else if (i <= 40) {
    old_dae <- append(old_dae, 30)
  } else if (i <= 50) {
    old_dae <- append(old_dae, 40)
  } else if (i <= 60) {
    old_dae <- append(old_dae, 50)
  } else {
    old_dae <- append(old_dae, 60)   # 최대값이 68
  }
}

birth_type_c_d <- cbind(old_dae, card_unique$credit)
oneway.test(card_unique$credit ~ old_dae, birth_type_c_d)


# begin
begin_m <- card[,19]
begin_m <- round(begin_m/12 * -1,0)

begin_c_d <- cbind(begin_m, card$credit)
begin_c_d <- data.frame(begin_c_d)
colnames(begin_c_d) <- c("begin_y", 'credit')

oneway.test(credit ~ begin_m, data = begin_c_d)

### 등분산성
bartlett.test(credit ~ begin_m, data = begin_c_d)
# 등분산성 만족 따라서 유의마한 결과이다.


###사후 검정
library(agricolae)

begin_c_d$begin_y <- factor(begin_c_d$begin_y) #
str(begin_c_d)

aov(credit ~ begin_y, data = begin_c_d) %>% 
  TukeyHSD %>% 
  tidy

aov(credit ~ begin_y, data = begin_c_d) %>%
  scheffe.test("begin_y", alpha = 0.01, console = T)





#-----------------------------------------------------------------------------------------------------------
library(dplyr)
head(card)

card_dup <- card %>%
  group_by(gender, income_total, income_type, DAYS_BIRTH, DAYS_EMPLOYED) %>%
  summarise(freq = names(which.max(table(credit))))
card_dup <- data.frame(card_dup)

credibility <- vector(length = length(card_dup[,1])) # 벡터 생성

# 0과 1이 좋은 것, 2가 나쁜 것
for (i in 1:length(card_dup[,1])) {
  if (card_dup$freq[i] == 2) {
    credibility[i] <- 'bad' 
  } else {
    credibility[i] <- 'good' 
  }
  rm(i)
}

card_dup <- cbind(card_dup, credibility) # 추가해주기




# 카이제곱 검정

income_credibility_dup <- card_dup[, c(2,7)]
income_credibility_dup[,1] <- income_credibility_dup[,1] * 171

income_level <- read.csv(file="소득10분위별__가구당_가계수지__전국_2인이상__20210425191731.csv", fileEncoding = 'euc-kr')
income_level <- income_level[-1,]
# 위의 각 데이터는 분위수 별 소득의 평균치 이다. 때문에 우리는 각 분위 마다 같은 분포를 갖고 있을 것을 가정하고 양쪽의 평균값으로 단계의 기준을 정했다.
sep <- vector() # 분위수 결정치를 위한 벡터 설정

for (i in 1:(length(income_level[,3]))) {
  sep[i] <- (as.numeric(income_level[i, 3]) + as.numeric(income_level[i+1, 3]))/2
}
# 이 수치는 월수익이다. 연수익으로 교체
sep <- sep*12

income_group <- vector()
for (i in income_credibility_dup[,1]) {
  if (i <= sep[1]) {
    income_group <- append(income_group, 1)
  } else if (i <= sep[2]) {
    income_group <- append(income_group, 2)
  } else if (i <= sep[3]) {
    income_group <- append(income_group, 3)
  } else if (i <= sep[4]) {
    income_group <- append(income_group, 4)
  } else if (i <= sep[5]) {
    income_group <- append(income_group, 5)
  } else if (i <= sep[6]) {
    income_group <- append(income_group, 6)
  } else if (i <= sep[7]) {
    income_group <- append(income_group, 7)
  } else if (i <= sep[8]) {
    income_group <- append(income_group, 8)
  } else if (i <= sep[9]) {
    income_group <- append(income_group, 9)
  } else {
    income_group <- append(income_group, 10)
  }
}
head(income_group)
income_credibility_dup <- cbind(income_credibility_dup, income_group)


sjt.xtab(income_credibility_dup$credibility, income_credibility_dup$income_group, 
         show.col.prc = T, 
         show.exp = T, var.labels=c("신용도", "성별"), 
         encoding="UTF-8" )


plot_xtab(income_credibility_dup$credibility, income_credibility_dup$income_group, 
          type="bar",
          y.offset = 0.01,
          margin = "col", coord.flip = F, wrap.labels = 7,
          geom.colors = "set1", show.summary = T, show.total = F,
          axis.titles = "Credibility",
          #axis.labels = c(),
          legend.title = "Gender")
#--------------------------------------------------#--------------------------------------------------


# T 검정
# 신용도 0,1을 좋다 그룹, 2를 나쁘다 그룹화 시켰다.
# 신용도에 따라 평균 수입이 다를 것인가?
money_credit <- card_dup[,c(2,7)]
money_credit[,2] <- factor(money_credit[,2])
var.test(income_total ~ credibility, data=money_credit)
#두개의 분산이 같지 않다.

#정규성 검정
hist(money_credit$income_total[money_credit$credibility == "good"])
hist(money_credit$income_total[money_credit$credibility == "bad"])
shapiro.test(money_credit$income_total[money_credit$credibility == "good"])
length(money_credit$income_total[money_credit$credibility == "bad"]) #5668
shapiro.test(money_credit$income_total[money_credit$credibility == "bad"][300:5299]) #5000이 한계, 양쪽 300개를 빼줌
# 정규성을 만족 x

# 로그변환, 제곱근 변환을 해본다.
money_credit_log <- log(money_credit[,1] + 1)
money_credit_sqrt <- sqrt(money_credit[,1])

## Q-Q plot
qqnorm(money_credit$income_total, main="Q-Q plot of INCOME")
qqline(money_credit$income_total)
#처음 값은 정규분포를 안 띄는 것이 확인

qqnorm(money_credit_log, main="Q-Q plot of INCOME_log")
qqline(money_credit_log)
# 로그 변환하니 봐줄만 하다.

qqnorm(money_credit_sqrt, main="Q-Q plot of INCOME_sqrt")
qqline(money_credit_sqrt)
# 조금 올라간 형태이다.


#로그변환이 올바르다.
money_credit$income_total <- log(money_credit$income_total + 1)
hist(money_credit$income_total[money_credit$credibility == 'good'], main = "plot of income_total_log_good")
hist(money_credit$income_total[money_credit$credibility == "bad"], main = "plot of income_total_log_bad")
qqnorm(money_credit$income_total[money_credit$credibility == 'bad'], main="Q-Q plot of INCOME_log_bad")
qqline(money_credit$income_total[money_credit$credibility == 'bad'])
qqnorm(money_credit$income_total[money_credit$credibility == 'good'], main="Q-Q plot of INCOME_log_good")
qqline(money_credit$income_total[money_credit$credibility == 'good'])
# log변환 뒤 둘다 정규분포의 모습이라고 가정할 수 있게 되었다.

# 분산이 다르니 Welch’s test를 한다.
t.test(income_total ~ credibility, var.equal = F, data = money_credit)
# p값이 0.07로 기각할 수 없다. 따라서 두 개의 신용도에 따라 연간 수입은 바뀐다고 할 수는 없다.
library(psych)
describeBy(money_credit$income_total, money_credit$credibility)
library(gplots)
plotmeans(income_total ~ credibility, data = money_credit,
          xlab="Credibility",
          ylab="income_total", 
          ci.label=TRUE, mean.label=TRUE, 
          barwidth=5, 
          main="Annual income based on credibility", digits=3, pch="*")
#--------------------------------------------------------------------------------------------------------
# T 검정
## 나이대와 신용도

#>> 데이터 수집 당시 (0)부터 역으로 셈, 즉, -1은 데이터 수집일 하루 전에 태어났음을 의미
library(MASS)
library(car)
age_dup_New <- cbind(DAYS_BIRTH_dup,age_dae)
hist(age_dup_New$age)


qqnorm(age_dup_New$age)
qqline(age_dup_New$age)
qqnorm(log(age_dup_New$age + 1))
qqnorm(sqrt(age_dup_New$age))


boxcox(age_dup_New$age ~ 1)

p <- powerTransform(age_dup_New$age)

age_dup_New$age <- bcPower(age_dup_New$age, p$lambda)

qqnorm(age)
qqline(age)

var.test(age ~ credibility_1, data = age_dup_New)
# 유의수준 1%에서 분산이 동일함을 가정할 수 있다.

t.test(age ~ credibility_1, data = age_dup_New, var.equal = T)
# 결론 : 신용도에 따라 나이가 다르다고 할 수 있다.
library(psych)
describeBy(age_dup_New$age, age_dup_New$credibility_1)
library(gplots)
plotmeans(age ~ credibility_1, data = age_dup_New,
          xlab="Credibility",
          ylab="age", 
          ci.label=TRUE, mean.label=TRUE, 
          barwidth=5, 
          main="Age based on credibility", digits=3, pch="*")



#--------------------------------------------------------------------------------------------------------
# 아노바 검정

# 그룹을 신용등급 0,1,2에 따라 분류한 뒤 일원 분산 분석을 하였다.
money_credit_aov <- card_dup[,c(2,6)]

# 아까와 같이 로그 변환을 해준다.
money_credit_aov$income_total <- log(1 + money_credit_aov$income_total)
hist(money_credit_aov$income_total)
qqnorm(money_credit_aov$income_total)
qqline(money_credit_aov$income_total)
# 정규성 확인 

money_aov <- aov(income_total ~ freq, data = money_credit_aov)
#역시나 마찬가지. 유의도는 0.06577
anova(money_aov)

set_theme(axis.textsize=1.2, geom.label.size=4.5) 
sjp.aov1(money_credit_aov$income_total, money_credit_aov$freq,
         geom.size = 1.5, wrap.labels = 7,
         axis.lim = c(10, 14), meansums = T,
         show.summary = T, show.p = F,
         title = "Annual income based on credibility", axis.labels=c("very good","good","bad"))

# ‘gplots’ 패키지를 이용한 분산분석 그래프 만들기 library(gplots)
plotmeans(income_total ~ freq, data = money_credit_aov,
          xlab="credibility", ylab="income_total", ci.label=T, ylim = c(11.94, 12.04),
          mean.label=T, barwidth=5, digits=2, col="brown", pch=1, barcol="red",
          legends = c("very good", "good", "bad"), main="Annual income based on credibility")









