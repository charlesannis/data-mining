#1:
pca_cov <- prcomp(myData[, c("x1", "x2", "x3")], center = TRUE, scale. = FALSE)
summary(pca_cov)
prop_var_cov <- pca_cov$sdev^2 / sum(pca_cov$sdev^2)
cum_var_cov <- cumsum(prop_var_cov)
num_comp_cov <- min(which(cum_var_cov >= 0.90))
cat("Covariance PCA:\n")
cat("Number of components retained:", num_comp_cov, "\n")
cat("Cumulative variance explained:", round(cum_var_cov[num_comp_cov], 4), "\n")
cat("Weights for PC1:\n")
print(round(pca_cov$rotation[,1], 4))

pca_cor <- prcomp(myData[, c("x1", "x2", "x3")], center = TRUE, scale. = TRUE)
summary(pca_cor)
prop_var_cor <- pca_cor$sdev^2 / sum(pca_cor$sdev^2)
cum_var_cor <- cumsum(prop_var_cor)
num_comp_cor <- min(which(cum_var_cor >= 0.90))
cat("\nCorrelation PCA:\n")
cat("Number of components retained:", num_comp_cor, "\n")
cat("Cumulative variance explained:", round(cum_var_cor[num_comp_cor], 4), "\n")
cat("Weights for PC1:\n")
print(round(pca_cor$rotation[,1], 4))

#2:
mean_x1 <- 5.8; sd_x1 <- 2.4; mean_x2 <- 380.5; sd_x2 <- 123.4
obs1 <- c(x1 = 7.30, x2 = 342.90)
obs2 <- c(x1 = 3.20, x2 = 258.60)
z_obs1 <- round((obs1 - c(mean_x1, mean_x2)) / c(sd_x1, sd_x2), 4)
z_obs2 <- round((obs2 - c(mean_x1, mean_x2)) / c(sd_x1, sd_x2), 4)
cat("Z-scores for Observation 1:\n"); print(z_obs1)
cat("Z-scores for Observation 2:\n"); print(z_obs2)
pc_weights <- matrix(c(-0.81, -0.59, 0.59, -0.81), nrow = 2, byrow = FALSE)
rownames(pc_weights) <- c("x1", "x2")
colnames(pc_weights) <- c("PC1", "PC2")
pc1_obs1 <- round(sum(pc_weights[,"PC1"] * z_obs1), 4)
pc2_obs2 <- round(sum(pc_weights[,"PC2"] * z_obs2), 4)
cat("\nPC1 score for Observation 1:", pc1_obs1, "\n")
cat("PC2 score for Observation 2:", pc2_obs2, "\n")

#3:
unique_days <- sort(unique(myData$Days))
candidate_splits_days <- (unique_days[-length(unique_days)] + unique_days[-1]) / 2
lowest_split_days <- round(min(candidate_splits_days), 1)
highest_split_days <- round(max(candidate_splits_days), 1)
cat("Days - Lowest possible split:", lowest_split_days, "\n")
cat("Days - Highest possible split:", highest_split_days, "\n")
unique_precip <- sort(unique(myData$Precipitation))
candidate_splits_precip <- (unique_precip[-length(unique_precip)] + unique_precip[-1]) / 2
lowest_split_precip <- round(min(candidate_splits_precip), 1)
highest_split_precip <- round(max(candidate_splits_precip), 1)
cat("Precipitation - Lowest possible split:", lowest_split_precip, "\n")
cat("Precipitation - Highest possible split:", highest_split_precip, "\n")

#4:
suppressWarnings(RNGversion("3.5.3"))
library(readxl)
library(caret)
library(rpart)
library(rpart.plot)
library(gains)
library(pROC)
myData$Edu <- as.factor(myData$Edu)
myData$Own <- as.factor(myData$Own)
myData$Pet <- as.factor(myData$Pet)
myData$City <- as.factor(myData$City)
myData$ContinueEdu <- as.factor(myData$ContinueEdu)
set.seed(1)
index <- createDataPartition(myData$ContinueEdu, p = 0.7, list = FALSE)
trainSet <- myData[index, ]
validSet <- myData[-index, ]
default_tree <- rpart(ContinueEdu ~ ., data = trainSet, method = "class")
prp(default_tree, type = 1, extra = 1, under = TRUE)
num_leaf_default <- sum(default_tree$frame$var == "<leaf>")
cat("a)\n")
cat("   Number of leaf nodes in default tree:", num_leaf_default, "\n")
first_split_var <- default_tree$frame$var[1]
first_split_value <- default_tree$splits[1, 1]
cat("   First split variable:", first_split_var, "\n")
# Split value: 140,000
full_tree <- rpart(ContinueEdu ~ ., data = trainSet, method = "class", cp = 0, minsplit = 2, minbucket = 1)
printcp(full_tree)
cp_table <- full_tree$cptable
min_xerror_row <- which.min(cp_table[, "xerror"])
cp_lowest <- cp_table[min_xerror_row, "CP"]
cat("b-1)\n")
cat("   cp value for lowest CV error:", round(cp_lowest, 4), "\n")
min_splits <- cp_table[min_xerror_row, "nsplit"]
cat("   Number of splits in the minimum error tree:", min_splits, "\n\n")
min_xerror <- cp_table[min_xerror_row, "xerror"]
xstd <- cp_table[min_xerror_row, "xstd"]
threshold <- min_xerror + xstd
candidate_rows <- which(cp_table[, "xerror"] <= threshold)
if(length(candidate_rows) > 0) {
  best_row <- candidate_rows[1]
  best_cp <- cp_table[best_row, "CP"]
  simpler_tree <- TRUE
} else {
  simpler_tree <- FALSE
}
cat("c-1)\n")
cat("   Is there a simpler tree within 1 SE of min CV error? ", ifelse(simpler_tree, "Yes", "No"), "\n")
cat("c-2)\n")
if(simpler_tree) {
  cat("   cp value for best-pruned tree:", round(best_cp, 4), "\n\n")
} else {
  cat("   cp value for best-pruned tree: N/A\n\n")
}
if(simpler_tree) {
  pruned_tree <- prune(full_tree, cp = best_cp)
} else {
  pruned_tree <- prune(full_tree, cp = cp_lowest)
}
prp(pruned_tree, type = 1, extra = 1, under = TRUE)
num_leaf_pruned <- sum(pruned_tree$frame$var == "<leaf>")
cat("d)\n")
cat("   Number of leaf nodes in the pruned tree:", num_leaf_pruned, "\n\n")
pred_class <- predict(pruned_tree, validSet, type = "class")
cm <- confusionMatrix(pred_class, validSet$ContinueEdu, positive = "1")
acc <- round(cm$overall["Accuracy"], 2)
sens <- round(cm$byClass["Sensitivity"], 2)
spec <- round(cm$byClass["Specificity"], 2)
prec <- round(cm$byClass["Pos Pred Value"], 2)
cat("e)\n")
cat("   Accuracy:", acc, "\n")
cat("   Sensitivity:", sens, "\n")
cat("   Specificity:", spec, "\n")
cat("   Precision:", prec, "\n\n")
pred_prob <- predict(pruned_tree, validSet, type = "prob")
pred_class_cutoff <- ifelse(pred_prob[, "1"] > 0.1, "1", "0")
pred_class_cutoff <- factor(pred_class_cutoff, levels = levels(validSet$ContinueEdu))
cm_cutoff <- confusionMatrix(pred_class_cutoff, validSet$ContinueEdu, positive = "1")
acc_c <- round(cm_cutoff$overall["Accuracy"], 2)
sens_c <- round(cm_cutoff$byClass["Sensitivity"], 2)
spec_c <- round(cm_cutoff$byClass["Specificity"], 2)
prec_c <- round(cm_cutoff$byClass["Pos Pred Value"], 2)
cat("f)\n")
cat("   (Cutoff = 0.1) Accuracy:", acc_c, "\n")
cat("   Sensitivity:", sens_c, "\n")
cat("   Specificity:", spec_c, "\n")
cat("   Precision:", prec_c, "\n\n")
true_valid <- as.numeric(as.character(validSet$ContinueEdu))
gains_table <- gains(true_valid, pred_prob[, "1"])
lift_leftmost <- round(gains_table$mean.resp[1] / mean(true_valid), 2)
cat("g)\n")
cat("   Lift value of leftmost bar (decile 1):", lift_leftmost, "\n\n")
roc_obj <- roc(validSet$ContinueEdu, pred_prob[, "1"])
auc_val <- round(auc(roc_obj), 4)
cat("h)\n")
cat("   Area under the ROC curve (AUC):", auc_val, "\n\n")
myScoreData$Edu <- as.factor(myScoreData$Edu)
myScoreData$Own <- as.factor(myScoreData$Own)
myScoreData$Pet <- as.factor(myScoreData$Pet)
myScoreData$City <- as.factor(myScoreData$City)
score_prob <- predict(pruned_tree, myScoreData, type = "prob")
prob_first <- round(score_prob[1, "1"], 4)
prob_second <- round(score_prob[2, "1"], 4)
cat("i)\n")
cat("   Probability of first community member enrolling:", prob_first, "\n")
cat("   Probability of second community member enrolling:", prob_second, "\n")
