rm(list = ls())
options(stringsAsFactors = F)
load(file = 'data/split_data.rdata')
load('data/mul_model.rdata')

library(caret)
# 去掉缺失值
library(tidyr)
df$group <- as.factor(df$group)
rm('model_log','model_nb',
   'model_adaboost','model_rf')

# methy risk score
# m <- predict(model_svmRadial,newdata = df)
m <- predict(model_svmRadial,newdata = df,type = 'prob')
head(m)
df$m=m$case


## -----------------
# 结合临床数据
load(file = 'data/RA_cli.rdata')
table(rp$group)
cli=rp

cli <- cli[rownames(df),]
identical(rownames(cli),rownames(df))

table(df$group)
table(cli$group)
colnames(cli)
cli <- cli[,-13]

trainData <- cbind(cli[rownames(trainData),],df[rownames(trainData),])
testData <- cbind(cli[rownames(testData),],df[rownames(testData),])
df <- cbind(cli[rownames(df),],df)

# 建立模型 245traindata
library(caret)
# Define the training control 重抽样方法
fitControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 10,                      # 重抽样的迭代次数
  savePredictions = 'final',       # 只保存最优的参数
  classProbs = T,                  # 生成分类概率，而不是按照默认0.5进行分类
  summaryFunction=twoClassSummary  # results summary function
) 

set.seed(100)
table(trainData$group)
levels(trainData$group) <- make.names(levels(factor(trainData$group)))
table(trainData$group)
colnames(trainData)

# Train the model using svm
svmRadial = train(group ~ LY+`Age at diagnosis`+m,
                  data=trainData,
                  method='svmRadial',
                  trControl = fitControl)
svmRadial
# 
save(
  svmRadial,
  file = 'data/svmmul_model.rdata')

load('data/svmmul_model.rdata')
# # 自我验证模型
# # predict traindata 风险评分
m <- predict(svmRadial,newdata = trainData) #0.8382  0.8529
trainData$group <- as.factor(trainData$group)
confusionMatrix(reference=trainData$group,data=m,
                mode='everything',
                positive = 'case')

# 外部验证模型
# # predict traindata 风险评分
m <- predict(svmRadial,newdata = testData) #0.6603 0.6731
testData$group <- as.factor(testData$group)
confusionMatrix(reference=testData$group,data=m,
                mode='everything',
                positive = 'case')

# 全数据验证模型
# predict traindata 风险评分
m <- predict(svmRadial,newdata = df)#0.7143 0.7277
# m <- predict(svmRadial,newdata = df,type = 'prob') #0.7143 0.7277
df$group <- as.factor(df$group)
confusionMatrix(reference=df$group,data=m,
                mode='everything',
                positive = 'case')







