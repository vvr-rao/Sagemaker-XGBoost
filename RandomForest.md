```R
install.packages(c('e1071'),
      repo = 'http://cran.rstudio.com',
      dependencies = TRUE)
```


```R
library(caret)
library(e1071)
```


```R
train_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url  <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
rawTrainData <- read.csv(url(train_url))
rawTestData <- read.csv(url(test_url))
dim(rawTrainData)
dim(rawTestData)
```


```R
## - check Variance of columns
apply(rawTrainData, 2, var)
##remove columns with near zero variance
nearZeroVarCols <- nearZeroVar(rawTrainData)
training <- rawTrainData[,-nearZeroVarCols]
testing <- rawTestData[,-nearZeroVarCols]
dim(training)
dim(testing)
```


```R
# 2 - remove data with NA
training <- training[, colSums(is.na(training)) == 0] 
testing <- testing[, colSums(is.na(testing)) == 0] 
dim(training)
dim(testing)
head(training)
```


```R
# 3 - remove first 5 columns - user_name, raw_timestamp_part_1, raw_timestamp_part_2,cvtd_timestamp
training <-training[,-c(1:5)]
testing <- testing[,-c(1:5)]
dim(training)
dim(testing)
head(training)
```


```R
subSamples <- createDataPartition(y=training$classe, p=0.70, list=FALSE)
subTraining <- training[subSamples, ] 
subValidation <- training[-subSamples, ]
```


```R
##Train Using Decision Tree
mod_DT <- train(classe ~ ., data = subTraining, method="rpart")
pred_DT <-  predict(mod_DT, subValidation)
confMat_DT <- table(subValidation$classe,pred_DT)
accuracy_DT <- sum(diag(confMat_DT))/sum(confMat_DT)
oose_DT <- 1 - accuracy_DT
confMat_DT
accuracy_DT
```


```R
## Train using Random Forest
mod_RF <- train(classe ~ ., data = subTraining, method = "rf", ntree = 100)
pred_RF <- predict(mod_RF, subValidation)
confMat_RF <- table(subValidation$classe,pred_RF)
accuracy_RF <- sum(diag(confMat_RF))/sum(confMat_RF)
oose_RF <- 1 - accuracy_RF
confMat_RF
accuracy_RF
```


```R

```
