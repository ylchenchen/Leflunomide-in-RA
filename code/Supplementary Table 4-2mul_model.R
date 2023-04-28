rm(list = ls())
options(stringsAsFactors = F)
load(file = 'data/split_data.rdata')


# 探索位点间的相关性
{
# Highly correlated CpGs (Pearson’s correlation coefficient r > 0.5) were excluded
library(corrplot)
colnames(df)
data <- df[,-8]
colnames(data)
colnames(data) <- c("cg17330251","cg19814518",      
                    "cg20124410" , "cg21109666",     
                    "cg22572476","cg23403192",
                    "cg24432675" )
tdc <- cor(data, method="pearson")
#默认参数
corrplot(tdc) # 符合标准 小于0.5
}

## 建立模型 245traindata-----
library(caret)
# Define the training control 重抽样方法
fitControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 10,                      # 重抽样的迭代次数
  savePredictions = 'final',       # 只保存最优的参数
  classProbs = T,                  # 生成分类概率，而不是按照默认0.5进行分类
  summaryFunction=twoClassSummary  # results summary function
) 



# Train the model using rf
model_rf = train(group ~ ., 
                 data=trainData, 
                 method='rf', 
                 tuneLength=5, 
                 trControl = fitControl)
model_rf

set.seed(100)

# Train the model using svm
model_svmRadial = train(group ~ ., 
                        data=trainData, 
                        method='svmRadial', 
                        tuneLength=15, 
                        trControl = fitControl)
model_svmRadial

set.seed(100)

# Train the model using adaboost
model_adaboost = train(group ~ ., 
                       data=trainData, 
                       method='adaboost', 
                       tuneLength=2, 
                       trControl = fitControl)
model_adaboost

# Train the model using nb
model_nb = train(group ~ ., 
                       data=trainData, 
                       method='nb', 
                       trControl = fitControl)
model_nb

# Train the model using log
model_log = train(group ~ ., 
                 data=trainData, 
                 method='glm', 
                 family= 'binomial',
                 trControl = fitControl)
model_log

# Compare model performances using resample()
models_compare <- resamples(list(ADABOOST=model_adaboost, 
                                 RF=model_rf, 
                                 #XGBDART=model_xgbDART, 
                                 #MARS=model_mars3, 
                                 SVM=model_svmRadial))

# Summary of the models performances
summary(models_compare)
save(model_adaboost,model_rf,model_svmRadial,
     model_nb,model_log,
     file = 'data/mul_model.rdata')

load(file = 'data/mul_model.rdata')
# 自我验证模型
# predict traindata 风险评分
trainData$group <- as.factor(trainData$group)

m <- predict(model_svmRadial,newdata = trainData)
m <- predict(model_adaboost,newdata = trainData)
m <- predict(model_rf,newdata = trainData)
m <- predict(model_nb,newdata = trainData)
m <- predict(model_log,newdata = trainData)

confusionMatrix(reference=trainData$group,data=m,
                mode='everything',
                positive = 'case')

# 外部验证模型
# predict traindata 风险评分

m <- predict(model_svmRadial,newdata = testData)
m <- predict(model_adaboost,newdata = testData)
m <- predict(model_rf,newdata = testData)
m <- predict(model_nb,newdata = testData)
m <- predict(model_log,newdata = testData)

testData$group <- as.factor(testData$group)
confusionMatrix(reference=testData$group,data=m,
                mode='everything',
                positive = 'case')

# 全部验证模型
# predict traindata 风险评分

m <- predict(model_svmRadial,newdata = df)
m <- predict(model_adaboost,newdata = df)
m <- predict(model_rf,newdata = df)
m <- predict(model_nb,newdata = df)
m <- predict(model_log,newdata = df)

df$group <- as.factor(df$group)
confusionMatrix(reference=df$group,data=m,
                mode='everything',
                positive = 'case')


