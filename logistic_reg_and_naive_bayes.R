rm(list = ls())

df = read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")
df$Churn = ifelse(df$Churn == 'Yes', 1, 0)

#EDA
df = df[,-1]
df = na.omit(df)
df$MultipleLines = as.factor(df$MultipleLines)
df$InternetService = as.factor(df$InternetService)
df$OnlineSecurity = as.factor(df$OnlineSecurity)
df$OnlineBackup = as.factor(df$OnlineBackup)
df$DeviceProtection = as.factor(df$DeviceProtection)
df$TechSupport = as.factor(df$TechSupport)
df$StreamingTV = as.factor(df$StreamingTV)
df$StreamingMovies = as.factor(df$StreamingMovies)
str(df)
# logistic regression
logreg1 = glm(Churn ~ ., family = binomial, data = df)
summary(logreg1)
logreg1Null = glm(Churn ~ 1, family = binomial, data = df)
anova(logreg1Null, logreg1, test = 'LRT') # 확실히 잘 설명한다.
# 사용할 변수 명 
# tenure, Contract, PaperlessBilling, TotalCharges
logreg1_Modify = glm(Churn ~ tenure + Contract + PaperlessBilling + TotalCharges, family = binomial, data = df)
anova(logreg1Null, logreg1_Modify, test = 'LRT')

coef(logreg1_Modify)
exp(coef(logreg1_Modify)) # 'odds가 이 숫자배수 만큼 증가함'

# 성능평가 ROC
library(Epi)
graph1 = ROC(form = Churn ~ tenure + Contract + PaperlessBilling + TotalCharges, data = df, plot = 'ROC')
head(graph1$res)
tail(graph1$res)
graph1$res[round(graph1$res$lr.eta, 3) == 0.313,]  # 아마 3번째 값이 가장 .313에 가깝다.
graph1$AUC   # 정확도
graph1$lr    # Null 모델 기준 얼마나 맞는지 

library(caret)
confusionMatrix(
  as.factor(ifelse(predict(logreg1_Modify, type = "response")>0.313,1,0)),
  as.factor(logreg1_Modify$y),
  positive = '1')

logreg1_Modify_F1 = 2*0.8111*0.4906/(0.8111+0.4906)   # 0.6113938  


## 나이브 베이즈 
library(e1071)
mygroup = function (y,k=4){
  count = length(y)
  z = rank(y,ties.method = "min")
  return(floor((z-1)/(count/k))+1)
}
df$MonthlyCharges_group = mygroup(df$MonthlyCharges, 20)  # MonthlyCharges 값이 낮으면: 1 ~ 높으면: 10
table(df$MonthlyCharges_group)  # 그룸이 5개로 나눠짐
df$tenure_group = mygroup(df$tenure, 15)  # tenure 값을 7개로 범주화
table(df$tenure_group)
df$TotalCharges_group = mygroup(df$TotalCharges, 20)  # TotalCharges 값을 10로 범주화
table(df$TotalCharges_group)

df = df[, -c(5, 18, 19)]
str(df)

# 데이터 분할
train_idx = sample(7032, 7032*3/4)
df_train = df[train_idx,]
df_test = df[-train_idx,]
df_train_labels = df[train_idx,]$Churn
df_test_labels = df[-train_idx,]$Churn

df_train = df_train[,-17]

prop.table(table(df_train_labels))
prop.table(table(df_test_labels))
# 비슷하다.

str(df_train_labels)
str(df_train)

# 모델 생성
df_classifier = naiveBayes(df_train, df_train_labels)
df_test_pred = predict(df_classifier, df_test)
library(descr)
CrossTable(df_test_pred, df_test_labels,
           prop.chisq = F, prop.c = F, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

confusionMatrix(as.factor(df_test_pred),
                as.factor(df_test_labels), 
                positive = '1')

# 라플라스 적용
df_classifier_laplace = naiveBayes(df_train, df_train_labels, laplace = 1)
df_test_pred_laplace = predict(df_classifier_laplace, df_test)

CrossTable(df_test_pred_laplace, df_test_labels,
           prop.chisq = F, prop.c = F, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

confusionMatrix(as.factor(df_test_pred_laplace),
                as.factor(df_test_labels), 
                positive = '1')
# 별 다른것이 없다.



library(gbm)
set.seed(123)
df$tenure = as.numeric(df$tenure)
df$Contract = as.factor(df$Contract)
df$PaperlessBilling = as.factor(df$PaperlessBilling)
str(df)

df_gbm <- gbm(
  formula = Churn ~ tenure + Contract + PaperlessBilling + TotalCharges,
  distribution = "huberized", 
  data = df,
  #weights,
  var.monotone = NULL,
  n.trees = 100,
  interaction.depth = 3, 
  n.minobsinnode = 10,
  shrinkage = 0.1,       
  bag.fraction = 0.5,
  train.fraction = 0.7,
  cv.folds = 2,
  keep.data = TRUE,
  verbose = TRUE,
  n.cores = NULL
)

print(df_gbm)
summary(
  df_gbm, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

gbm.perf(df_gbm, plot.it = T, oobag.curve = T, method = 'test')
predict(df_gbm, df_train)


