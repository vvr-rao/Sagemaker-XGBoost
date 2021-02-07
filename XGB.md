```R
install.packages(c('e1071','xgboost'),
      repo = 'http://cran.rstudio.com',
      dependencies = TRUE)
```


```R
library(caret)
library(e1071)
library(xgboost)
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
### Train in XGB
tune_grid <- expand.grid(nrounds = 100,
                        max_depth = 6,
                        eta = 0.05,
                        gamma = 0.01,
                        colsample_bytree = 1,
                        min_child_weight = 0.5,
                        subsample = 0.5)


```


```R
trctrl <- trainControl(method = "cv", number = 5)
```


```R
mod_RF <- train(classe ~ ., data = subTraining, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)

pred_RF <- predict(mod_RF, subValidation)
confMat_RF <- table(subValidation$classe,pred_RF)
accuracy_RF <- sum(diag(confMat_RF))/sum(confMat_RF)
oose_RF <- 1 - accuracy_RF
confMat_RF
accuracy_RF

```


```R
mod_RF

```


```R

```
