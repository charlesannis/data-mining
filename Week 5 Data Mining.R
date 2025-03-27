# Question 1
# Ensure R version compatibility for reproducibility
suppressWarnings(RNGversion("3.5.3"))

# Install required packages if not already installed
install.packages(c("caret", "gains", "pROC"))
library(caret)
library(gains)
library(pROC)

# Load and preprocess the data
myData$y <- as.factor(myData$y)  # Ensure target variable is a factor

# Standardize numerical predictors
myData1 <- data.frame(scale(myData[, c("x1", "x2")]))  
myData1$y <- myData$y  # Reattach the target variable

# Train-validation split
set.seed(1)
myIndex <- createDataPartition(myData1$y, p=0.6, list = FALSE)
trainSet <- myData1[myIndex,]
validationSet <- myData1[-myIndex,]

# Define cross-validation control
myCtrl <- trainControl(method = "cv", number = 10)

# Define grid search for k from 1 to 10
myGrid <- expand.grid(.k = 1:10)

# Train the KNN model
set.seed(1)
KNN_fit <- train(y ~ ., data = trainSet, method = "knn", trControl = myCtrl, tuneGrid = myGrid)

# Print best k value
print(KNN_fit)

# Get predictions on validation set
KNN_Class <- predict(KNN_fit, newdata = validationSet)

# Confusion matrix and model performance
cm <- confusionMatrix(KNN_Class, validationSet$y, positive = '1')
print(cm)

# Extract key metrics
accuracy <- cm$overall["Accuracy"]
specificity <- cm$byClass["Specificity"]
sensitivity <- cm$byClass["Sensitivity"]
precision <- cm$byClass["Pos Pred Value"]

# Print metrics
cat("Optimal k:", KNN_fit$bestTune$.k, "\n")
cat("Accuracy:", round(accuracy, 2), "\n")
cat("Specificity:", round(specificity, 2), "\n")
cat("Sensitivity:", round(sensitivity, 2), "\n")
cat("Precision:", round(precision, 2), "\n")

#Question2


# Convert Admit to a factor
admit_data$Admit <- as.factor(admit_data$Admit)

# Standardize numerical predictors
admit_data_scaled <- data.frame(scale(admit_data[, c("GPA", "SAT")]))
admit_data_scaled$Admit <- admit_data$Admit  # Reattach target variable

# Train-validation split
set.seed(1)
myIndex <- createDataPartition(admit_data_scaled$Admit, p=0.6, list=FALSE)
trainSet <- admit_data_scaled[myIndex,]
validationSet <- admit_data_scaled[-myIndex,]

# Define cross-validation control
myCtrl <- trainControl(method="cv", number=10)

# Define grid search for k
myGrid <- expand.grid(.k=1:10)

# Train the KNN model
set.seed(1)
KNN_fit <- train(Admit ~ ., data=trainSet, method="knn", trControl=myCtrl, tuneGrid=myGrid)

# Print best k value
print(KNN_fit)

# Get predictions on validation set
KNN_Class <- predict(KNN_fit, newdata=validationSet)

# Confusion matrix and model performance
cm <- confusionMatrix(KNN_Class, validationSet$Admit, positive='1')
print(cm)

# Extract key metrics
accuracy <- cm$overall["Accuracy"]
specificity <- cm$byClass["Specificity"]
sensitivity <- cm$byClass["Sensitivity"]
precision <- cm$byClass["Pos Pred Value"]

# Print metrics
cat("Optimal k:", KNN_fit$bestTune$.k, "\n")
cat("Accuracy:", round(accuracy, 2), "\n")
cat("Specificity:", round(specificity, 2), "\n")
cat("Sensitivity:", round(sensitivity, 2), "\n")
cat("Precision:", round(precision, 2), "\n")

# Compute AUC for ROC curve
KNN_Prob <- predict(KNN_fit, newdata=validationSet, type='prob')
roc_curve <- roc(validationSet$Admit, KNN_Prob[,2])
auc_value <- auc(roc_curve)
cat("AUC:", round(auc_value, 4), "\n")

# Calculate Naive Rule Accuracy
most_common_class <- names(sort(table(trainSet$Admit), decreasing=TRUE))[1]
naive_accuracy <- mean(validationSet$Admit == most_common_class)
cat("Naive Rule Accuracy:", round(naive_accuracy * 100, 2), "%\n")

# Predict outcomes for new applicants
admit_score_scaled <- data.frame(scale(admit_score))
KNN_Score_Predictions <- predict(KNN_fit, newdata=admit_score_scaled)
print(KNN_Score_Predictions)


#Question3





# Install and load required packages
install.packages("e1071")
library(e1071)

# Convert y to a factor with correct levels
naive_data$y <- factor(naive_data$y, levels = c("No", "Yes"))

# Train-validation split
set.seed(1)
myIndex <- createDataPartition(naive_data$y, p=0.6, list=FALSE)
trainSet <- naive_data[myIndex,]
validationSet <- naive_data[-myIndex,]

# Train the Naïve Bayes model
set.seed(1)
nb_model <- train(y ~ ., data=trainSet, method="naive_bayes", trControl=trainControl(method="cv", number=10))

# Print model summary
print(nb_model)

# Predict on validation set
nb_pred <- predict(nb_model, newdata=validationSet)

# Confusion matrix and model performance
cm <- confusionMatrix(nb_pred, validationSet$y, positive='Yes')
print(cm)

# Extract key metrics
accuracy <- cm$overall["Accuracy"]
specificity <- cm$byClass["Specificity"]
sensitivity <- cm$byClass["Sensitivity"]
precision <- cm$byClass["Pos Pred Value"]

# Print metrics
cat("Accuracy:", round(accuracy, 2), "\n")
cat("Specificity:", round(specificity, 2), "\n")
cat("Sensitivity:", round(sensitivity, 2), "\n")
cat("Precision:", round(precision, 2), "\n")

# Generate ROC Curve and AUC
nb_prob <- predict(nb_model, newdata=validationSet, type='prob')
roc_curve <- roc(validationSet$y, nb_prob[,2])
auc_value <- auc(roc_curve)
cat("AUC:", round(auc_value, 4), "\n")

# Generate Lift Chart
gains_table <- gains(as.numeric(validationSet$y == "Yes"), nb_prob[,2])
print(gains_table)

# Lift value of the first decile
lift_value <- gains_table$Lift[1]
cat("Lift value of the first decile:", round(lift_value, 2), "\n")

# Predict new applicants
nb_score_pred <- predict(nb_model, newdata=naive_score)
print(nb_score_pred[1:3])  # Print first 3 predictions



#Question4




# Bin Age into [22, 45) and [45, 85)
myData$AgeGroup <- cut(myData$Age, 
                       breaks = c(22, 45, Inf),  # Use Inf instead of 85
                       labels = c("22-44", "45-85"),  # Adjust label
                       right = FALSE, 
                       include.lowest = TRUE)

# Bin Income into [0, 85000) and [85000, 300000)
myData$IncomeGroup <- cut(myData$Income, breaks = c(0, 85000, 300000), labels = c("0-84999", "85000-299999"), right = FALSE)

# Display bin numbers for the first two observations
cat("First two observations - Age Group:", myData$AgeGroup[1:2], "\n")
cat("First two observations - Income Group:", myData$IncomeGroup[1:2], "\n")

# Convert Rental to a factor
myData$Rental <- factor(myData$Rental, levels = c(0,1), labels = c("No", "Yes"))

# Train-validation split
set.seed(1)
myIndex <- createDataPartition(myData$Rental, p=0.6, list=FALSE)
trainSet <- myData[myIndex,]
validationSet <- myData[-myIndex,]

# Train the Naïve Bayes model
set.seed(1)
nb_model <- train(Rental ~ Own + Children + AgeGroup + IncomeGroup, data=trainSet, method="naive_bayes", trControl=trainControl(method="cv", number=10))





# Predict on validation set
nb_pred <- predict(nb_model, newdata=validationSet)

# Confusion matrix and model performance
cm <- confusionMatrix(nb_pred, validationSet$Rental, positive='Yes')
print(cm)

# Extract key metrics
accuracy <- cm$overall["Accuracy"]
specificity <- cm$byClass["Specificity"]
sensitivity <- cm$byClass["Sensitivity"]
precision <- cm$byClass["Pos Pred Value"]

# Print metrics
cat("Accuracy:", round(accuracy, 2), "\n")
cat("Specificity:", round(specificity, 2), "\n")
cat("Sensitivity:", round(sensitivity, 2), "\n")
cat("Precision:", round(precision, 2), "\n")

# Compute AUC for ROC curve
nb_prob <- predict(nb_model, newdata=validationSet, type='prob')
roc_curve <- roc(validationSet$Rental, nb_prob[,2])
auc_value <- auc(roc_curve)
cat("AUC:", round(auc_value, 4), "\n")

# Generate Lift Chart
gains_table <- gains(as.numeric(validationSet$Rental == "Yes"), nb_prob[,2])
print(gains_table)

# Lift value of the first decile
lift_value <- as.numeric(gains_table$Lift[1])
cat("Lift value of the first decile:", round(lift_value, 2), "\n")

