rm(list = ls())
options(stringsAsFactors = F)
# data input
load('data/hub_methy.rdata')
head(hub_methy)

# hubgene
g <- c('cg17330251','cg19814518',
       'cg20124410','cg21109666',
       'cg22572476','cg23403192',
       'cg24432675','group')
gene=g[-8]
df <- hub_methy[,g] # 验证
df=na.omit(df)

# 拆分数据
# Create the training and test datasets
library(caret)
set.seed(100)

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(df$group, p=0.7, list=FALSE)

# Step 2: Create the training  dataset
trainData <- df[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- df[-trainRowNumbers,]

save(df,testData,trainData,file = 'data/split_data.rdata')

