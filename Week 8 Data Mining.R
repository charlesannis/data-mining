suppressWarnings(RNGversion("3.5.3"))
library(readxl)
library(caret)
library(rpart)
library(rpart.plot)
library(forecast)
library(gains)
library(pROC)
library(randomForest)
library(adabag)

# Problem 1
partition1 <- myData[myData$x1 <= 252, ]
partition2 <- myData[myData$x1 > 252, ]
mean_partition1 <- mean(partition1$y)
mean_partition2 <- mean(partition2$y)
mse_partition1 <- mean((partition1$y - mean_partition1)^2)
mse_partition2 <- mean((partition2$y - mean_partition2)^2)
n1 <- nrow(partition1)
n2 <- nrow(partition2)
total_n <- n1 + n2
weighted_mse <- (n1 / total_n) * mse_partition1 + (n2 / total_n) * mse_partition2
cat("MSE for partition 1 (x1 <= 252):", round(mse_partition1, 4), "\n")
cat("MSE for partition 2 (x1 > 252):", round(mse_partition2, 4), "\n")
cat("Weighted overall MSE for the split:", round(weighted_mse, 4), "\n")
partition1 <- myData[myData$x2 <= 92.5, ]
partition2 <- myData[myData$x2 > 92.5, ]
mean_partition1 <- mean(partition1$y)
mean_partition2 <- mean(partition2$y)
mse_partition1 <- mean((partition1$y - mean_partition1)^2)
mse_partition2 <- mean((partition2$y - mean_partition2)^2)
n1 <- nrow(partition1)
n2 <- nrow(partition2)
total_n <- n1 + n2
weighted_mse <- (n1 / total_n) * mse_partition1 + (n2 / total_n) * mse_partition2
cat("MSE for partition 1 (x2 <= 92.5):", round(mse_partition1, 4), "\n")
cat("MSE for partition 2 (x2 > 92.5):", round(mse_partition2, 4), "\n")
cat("Weighted overall MSE for the split:", round(weighted_mse, 4), "\n")
partition1 <- myData[myData$x3 <= 14.25, ]
partition2 <- myData[myData$x3 > 14.25, ]
mean_partition1 <- mean(partition1$y)
mean_partition2 <- mean(partition2$y)
mse_partition1 <- mean((partition1$y - mean_partition1)^2)
mse_partition2 <- mean((partition2$y - mean_partition2)^2)
n1 <- nrow(partition1)
n2 <- nrow(partition2)
total_n <- n1 + n2
weighted_mse <- (n1 / total_n) * mse_partition1 + (n2 / total_n) * mse_partition2
cat("MSE for partition 1 (x3 <= 14.25):", round(mse_partition1, 4), "\n")
cat("MSE for partition 2 (x3 > 14.25):", round(mse_partition2, 4), "\n")
cat("Weighted overall MSE for the split:", round(weighted_mse, 4), "\n")

# Problem 2")
suppressWarnings(RNGversion("3.5.3"))
set.seed(1)
myIndex <- createDataPartition(myData$y, p=0.7, list=FALSE)
trainSet <- myData[myIndex,]
validationSet <- myData[-myIndex,]
set.seed(1)
default_tree <- rpart(y ~ ., data=trainSet, method="anova")
summary(default_tree)
prp(default_tree, type=1, extra=1, under=TRUE)
set.seed(1)
full_tree <- rpart(y ~ ., data=trainSet, method="anova", cp=0, minsplit=2, minbucket=1)
prp(full_tree, type=1, extra=1, under=TRUE)
printcp(full_tree)
set.seed(1)
pruned_tree <- prune(full_tree, cp=0.0881871675)
prp(pruned_tree, type=1, extra=1, under=TRUE)
predicted_value <- predict(pruned_tree, validationSet)
accuracy(predicted_value, validationSet$y)

# Problem 3
myData$y <- as.factor(myData$y)
set.seed(1)
myIndex <- createDataPartition(myData$y, p=0.6, list=FALSE)
trainSet <- myData[myIndex,]
validationSet <- myData[-myIndex,]
set.seed(1)
randomforest_tree <- randomForest(y ~ ., data=trainSet, ntree=100, mtry=2, importance=TRUE)
predicted_class <- predict(randomforest_tree, validationSet)
confusionMatrix(predicted_class, validationSet$y, positive="1")
predicted_prob <- predict(randomforest_tree, validationSet, type = "prob")
validationSet$y <- as.numeric(as.character(validationSet$y))
gains_table <- gains(validationSet$y, predicted_prob[, 2])
gains_table
roc_object <- roc(validationSet$y, predicted_prob[, 2])
plot.roc(roc_object)
auc(roc_object)
varImpPlot(randomforest_tree, type=1)
x1 <- 3.45
x2 <- 1
x3 <- 18
x4 <- 5.80
myScoreData <- data.frame(x1, x2, x3, x4)
predicted_class_score <- predict(randomforest_tree, myScoreData, type="class")
predicted_class_score
predicted_class_prob <- predict(randomforest_tree, myScoreData, type="prob")
predicted_class_prob

# Problem 4
myData$Disease <- as.factor(myData$Disease)
set.seed(1)
myIndex <- createDataPartition(myData$Disease, p=0.6, list=FALSE)
trainSet <- myData[myIndex,]
validationSet <- myData[-myIndex,]
set.seed(1)
bagging_tree <- randomForest(Disease ~ ., data=trainSet, ntree=100, mtry=4, importance=TRUE)
predicted_class <- predict(bagging_tree, validationSet)
confusionMatrix(predicted_class, validationSet$Disease, positive="1")
myData <- data.frame(myData)
myData$Disease <- as.factor(myData$Disease)
set.seed(1)
myIndex <- createDataPartition(myData$Disease, p=0.6, list=FALSE)
trainSet <- myData[myIndex,]
validationSet <- myData[-myIndex,]
set.seed(1)
boosting_tree <- boosting(Disease ~ ., data=trainSet, mfinal=100)
prediction <- predict(boosting_tree, validationSet)
confusionMatrix(as.factor(prediction$class), validationSet$Disease, positive = "1")
predicted_prob <- predict(bagging_tree, validationSet, type = "prob")
validationSet$Disease <- as.numeric(as.character(validationSet$Disease))
roc_object <- roc(validationSet$Disease, predicted_prob[, 2])
plot.roc(roc_object)
auc(roc_object)

