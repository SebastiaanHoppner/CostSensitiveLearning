# Kaggle - Creditcard Data ------------------------------------------------------------------------
# https://www.kaggle.com/mlg-ulb/creditcardfraud


# - Instance-dependent cost-sensitive logistic regression with lasso regularization
# - Instance-dependent cost-sensitive gradient tree boosting
# - Regular logistic regression
# - Extreme gradient boosted trees

# TODO:
#  - cs.cslogit for lambda
#  - glmnet instead of glm
#  - fixed_cost?
#  - nrounds?
#  - base_score?



rm(list = ls())
# Setup -------------------------------------------------------------------------------------------

# save results as - - - - - - - - - - - - - - - -
version <- "3-10"
save_results_as <- paste0("Creditcard_", gsub("-", "", Sys.Date()), "_", version, ".RData")
setwd("~/Desktop/PhD/PhD KUL/CostSensitiveLearning/experiments")


# fixed cost - - - - - - - - - - - - - - - - - - -
fixed_cost <- 10 # <=== !!! # sum(creditcard$Amount[creditcard$Class == 1]) / nrow(creditcard) = 0.2124798


# procedure - - - - - - - - - - - - - - - - - - -
nruns  <- 5
nfolds <- 2



# Load packages -----------------------------------------------------------------------------------
library(csboost)
library(cslogit)
library(ggplot2)
library(xgboost)
library(gridExtra)
library(lubridate)
library(splitstackshape)



# Load and preprocess data ------------------------------------------------------------------------
load("../data/creditcard.RData") # creditcard <- read.csv("../data/creditcard.csv")

creditcard <- creditcard[-which(creditcard$Amount == 0), ] # remove transactions with zero Amount
creditcard <- creditcard[, -1] # remove variable Time
creditcard$Class <- factor(creditcard$Class) # set Class to factor variable
rownames(creditcard) <- 1:nrow(creditcard)

amount <- creditcard$Amount

creditcard$LogAmount <- log(creditcard$Amount) # log-transformation of Amount
creditcard <- creditcard[, c(1:28, 31, 30)] # rearrange columns
creditcard[, -30] <- scale(creditcard[, -30]) # scale predictors to zero mean and unit variance


# Amount category, used for stratification:
print(quantile(amount[creditcard$Class == 1], probs = c(1/3, 2/3)))
amount_category <- rep("high", length(amount))
amount_category[amount < quantile(amount[creditcard$Class == 1], probs = 2/3)] <- "middle"
amount_category[amount < quantile(amount[creditcard$Class == 1], probs = 1/3)] <- "low"
amount_category <- factor(amount_category, levels = c("low", "middle", "high"))

print(table(amount_category))
print(prop.table(table(amount_category)))
print(table(Class = creditcard$Class, amount_category = amount_category))



# Create cost matrix ------------------------------------------------------------------------------
cost_matrix <- matrix(nrow = nrow(creditcard), ncol = 2)
cost_matrix[, 1] <- ifelse(creditcard$Class == 1, fixed_cost, 0)
cost_matrix[, 2] <- ifelse(creditcard$Class == 1, amount, fixed_cost)



# Create folds (nruns x nfolds) for cross-validation ----------------------------------------------
set.seed(2019)
cvfolds <- list()
for (k in 1:nruns) {
  folds <- stratified(indt = cbind.data.frame(Class = creditcard$Class,
                                              amount_category = amount_category),
                      group = c("Class", "amount_category"),
                      size  = 0.5, # 2-fold cross validation (= 1/nfolds)
                      bothSets = TRUE,
                      keep.rownames = TRUE)
  names(folds) <- c("Fold1", "Fold2")
  folds[[1]] <- sort(as.numeric(folds[[1]]$rn))
  folds[[2]] <- sort(as.numeric(folds[[2]]$rn))
  cvfolds[[k]] <- folds
}



# Start cross-validation procedure ----------------------------------------------------------------
results_cslogit <- c()
results_logit   <- c()
results_csboost <- c()
results_xgboost <- c()

time_cslogit <- c()
time_logit   <- c()
time_csboost <- c()
time_xgboost <- c()


t_start <- proc.time()

for (k_run in 1:nruns) {
  for (j_fold in 1:nfolds) {
    # k_run = 5
    # j_fold = 2
    
    cat(paste0("\n Run ", k_run, "/", nruns, " - fold ", j_fold, "/", nfolds, "\n"))
    
    
    
    # Create training and test part ---------------------------------------------------------------
    train <- creditcard[-cvfolds[[k_run]][[j_fold]], ]
    test  <- creditcard[ cvfolds[[k_run]][[j_fold]], ]
    
    cost_matrix_train <- cost_matrix[-cvfolds[[k_run]][[j_fold]], ]
    cost_matrix_test  <- cost_matrix[ cvfolds[[k_run]][[j_fold]], ]
    
    amount_train <- amount[-cvfolds[[k_run]][[j_fold]]]
    amount_test  <- amount[ cvfolds[[k_run]][[j_fold]]]
    
    
    # Remove transfers from training set whose amount is belows the fixed cost:
    low_amount_train <- which(amount_train < fixed_cost)
    train             <-             train[-low_amount_train, ]
    cost_matrix_train <- cost_matrix_train[-low_amount_train, ]
    amount_train      <-      amount_train[-low_amount_train]
    
    
    # Construct xgb.DMatrix objects of training and test sets for xgboost:
    dtrain <- xgb.DMatrix(data  = model.matrix(Class ~ . - 1, data = train),
                          label = ifelse(train$Class == 1, 1, 0))
    dtest <- xgb.DMatrix(data  = model.matrix(Class ~ . - 1, data = test),
                         label = ifelse(test$Class == 1, 1, 0))
    
    
    
    # Logistic regression -------------------------------------------------------------------------
    cat("  - logit...\n")
    t_start_logit <- proc.time()
    logit <- glm(formula = Class ~ ., data = train, family = "binomial")
    t_end_logit <- proc.time() - t_start_logit
    time_logit <- c(time_logit, t_end_logit[3])
    
    
    
    # Instance-dependent cost-sensitive logistic regression with lasso regularization -------------
    cat("  - cslogit...\n")
    cslogitL1 <- cslogit(formula     = Class ~ .,
                         data        = train,
                         cost_matrix = cost_matrix_train,
                         lambda      = 0,
                         options     = list(check_data = FALSE,
                                            start = logit$coefficients))
    time_cslogit <- c(time_cslogit, cslogitL1$time)
    
    
    
    # Instance-dependent cost-sensitive gradient tree boosting ------------------------------------
    cat("  - csboost...\n")
    set.seed(2019)
    csbtree <- csboost(formula           = Class ~ . - 1,
                       train             = train,
                       cost_matrix_train = cost_matrix_train,
                       booster           = "gbtree",
                       nrounds           = 20,
                       base_score        = 0.01,
                       verbose           = 0)
    time_csboost <- c(time_csboost, csbtree$time)
    
    
    
    # eXtreme Gradient Boosting -------------------------------------------------------------------
    cat("  - xgboost...\n")
    t_start_xgboost <- proc.time()
    set.seed(2019)
    xgbtree <- xgb.train(data        = dtrain,
                         watchlist   = list(train = dtrain),
                         booster     = "gbtree",
                         objective   = "binary:logistic",
                         eval_metric = "logloss",
                         nrounds     = 20,
                         base_score  = 0.01,
                         verbose     = 0)
    t_end_xgboost <- proc.time() - t_start_xgboost
    time_xgboost <- c(time_xgboost, t_end_xgboost[3])
    
    
    
    # Performance of the models -------------------------------------------------------------------
    
    # Scores - - - - - - - - - - - - - - - - - - -
    scores_cslogit_test <- predict(cslogitL1, newdata = test)
    scores_logit_test   <- predict(logit,     newdata = test, type = "response")
    scores_csboost_test <- predict(csbtree,   newdata = test)
    scores_xgboost_test <- predict(xgbtree,   newdata = dtest)
    
    
    # Thresholds - - - - - - - - - - - - - - - - -
    threshold_test <- fixed_cost / amount_test
    
    
    # Performance - - - - - - - - - - - - - - - -
    performanceMeasures <- function (scores, threshold, true_classes, cost_matrix) {
      predicted_classes <- ifelse(scores > threshold, 1, 0)
      metrics      <-     cslogit::performance(scores, predicted_classes, true_classes)$metrics
      cost_metrics <- cslogit::costPerformance(scores, predicted_classes, true_classes, cost_matrix)
      spearman_correlation <- cor(scores[true_classes == 1], amount_test[true_classes == 1], method = "spearman")
      return(cbind.data.frame(metrics, cost_metrics, cor = spearman_correlation))
    }
    
    results_cslogit <- rbind.data.frame(results_cslogit, performanceMeasures(scores_cslogit_test, threshold_test, test$Class, cost_matrix_test))
    results_logit   <- rbind.data.frame(results_logit,   performanceMeasures(scores_logit_test,   threshold_test, test$Class, cost_matrix_test))
    results_csboost <- rbind.data.frame(results_csboost, performanceMeasures(scores_csboost_test, threshold_test, test$Class, cost_matrix_test))
    results_xgboost <- rbind.data.frame(results_xgboost, performanceMeasures(scores_xgboost_test, threshold_test, test$Class, cost_matrix_test))
    
    cat(paste("\n  - cslogit: savings =", round(results_cslogit$savings[length(results_cslogit$savings)], 4)))
    cat(paste("\n  -   logit: savings =", round(  results_logit$savings[length(results_logit$savings)],   4)))
    cat(paste("\n  - csboost: savings =", round(results_csboost$savings[length(results_csboost$savings)], 4)))
    cat(paste("\n  - xgboost: savings =", round(results_xgboost$savings[length(results_xgboost$savings)], 4), "\n"))
    
    cat(paste("\n  - cslogit: cor =", round(results_cslogit$cor[length(results_cslogit$cor)], 4)))
    cat(paste("\n  -   logit: cor =", round(  results_logit$cor[length(results_logit$cor)],   4)))
    cat(paste("\n  - csboost: cor =", round(results_csboost$cor[length(results_csboost$cor)], 4)))
    cat(paste("\n  - xgboost: cor =", round(results_xgboost$cor[length(results_xgboost$cor)], 4), "\n"))
    
    
    # Lift curve - - - - - - - - - - - - - - - - -
    scores_cslogit_test_fraud <- scores_cslogit_test[test$Class == 1]
    scores_logit_test_fraud   <- scores_logit_test[test$Class == 1]
    scores_csboost_test_fraud <- scores_csboost_test[test$Class == 1]
    scores_xgboost_test_fraud <- scores_xgboost_test[test$Class == 1]
    
    amount_test_fraud <- amount_test[test$Class == 1]
    
    plot(scores_cslogit_test_fraud[order(scores_cslogit_test_fraud, decreasing = TRUE)],
         cumsum(amount_test_fraud[order(scores_cslogit_test_fraud, decreasing = TRUE)] / sum(amount_test_fraud)),
         type = "l", lwd = 2, las = 1, ylab = "% detected of all fraud amount", xlab = "fraud score",
         main = "cslogit (solid), logit (dashed)", xlim = c(1, 0), cex.lab = 1.3)
    lines(scores_logit_test_fraud[order(scores_logit_test_fraud, decreasing = TRUE)],
          cumsum(amount_test_fraud[order(scores_logit_test_fraud, decreasing = TRUE)] / sum(amount_test_fraud)),
          lwd = 2, las = 1, lty = 2)
    
    plot(scores_csboost_test_fraud[order(scores_csboost_test_fraud, decreasing = TRUE)],
         cumsum(amount_test_fraud[order(scores_csboost_test_fraud, decreasing = TRUE)] / sum(amount_test_fraud)),
         type = "l", lwd = 2, las = 1, ylab = "% detected of all fraud amount", xlab = "fraud score",
         main = "csboost (solid), xgboost (dashed)", xlim = c(1, 0), cex.lab = 1.3)
    lines(scores_xgboost_test_fraud[order(scores_xgboost_test_fraud, decreasing = TRUE)],
          cumsum(amount_test_fraud[order(scores_xgboost_test_fraud, decreasing = TRUE)] / sum(amount_test_fraud)),
          lwd = 2, las = 1, lty = 2)
    
    
    # Elapsed time - - - - - - - - - - - - - - - - -
    t_end <- seconds_to_period(round((proc.time() - t_start)[3]))
    cat(paste0("\n ", round(100 * (j_fold + (k_run - 1) * nfolds) / (nruns * nfolds)),
               "% - ", sprintf("%02d:%02d:%02d", t_end@hour, minute(t_end), second(t_end)), "\n\n"))
  }
}

t_end <- seconds_to_period(round((proc.time() - t_start)[3]))
cat(paste("\nTime elapsed:",
          sprintf("%02d:%02d:%02d", t_end@hour, minute(t_end), second(t_end)), "\n\n"))



# Collect and save results ------------------------------------------------------------------------
methods <- c("cslogit", "logit", "csboost", "xgboost")
methods_vec <- rep(methods, nruns * nfolds)[order(match(rep(methods, nruns * nfolds), methods))]


# results - - - - - - - - - - - - - - - - - - - -
results <- rbind.data.frame(results_cslogit,
                            results_logit,
                            results_csboost,
                            results_xgboost)

average_results <- aggregate(results, by = list(methods_vec), FUN = mean, na.rm = TRUE)
average_results <- average_results[order(match(average_results$Group.1, methods)), ]

results <- cbind.data.frame(results, method = methods_vec)


# execution times - - - - - - - - - - - - - - - -
times <- data.frame(time = c(time_cslogit, time_logit, time_csboost, time_xgboost))
average_times <- aggregate(times, by = list(methods_vec), FUN = mean)
average_times <- average_times[order(match(average_times$Group.1, methods)), ]
times$method <- methods_vec


save.image(file = paste0("results Creditcard/", save_results_as))



# Study results -----------------------------------------------------------------------------------
library(ggplot2)
library(gridExtra)

createBoxplots <- function (df, ylabel, ylimit, average_measures) {
  fontfaces <- rep("plain", length(methods))
  fontfaces[which.max(average_measures)] <- "bold"
  if (any(average_measures > 100) | grepl("time", ylabel)) {
    label_average <- round(average_measures, 2)
  } else {
    label_average <- paste0(round(average_measures, 2), "%")
  }
  boxplots <- ggplot(data = df, mapping = aes(x = method, y = measure, fill = method)) +
    stat_boxplot(geom = "errorbar", width = 0.4) + geom_boxplot() +
    ylab(ylabel) + xlab("") + ylim(ylimit) + scale_x_discrete(limits = unique(df$method)) +
    stat_summary(fun.y = mean, geom = "point", shape = 18, size = 5, col = "black") +
    geom_text(data = data.frame(method = unique(df$method), average_measures = average_measures),
              mapping = aes(x = method, y = min(ylimit, na.rm = TRUE),
                            label = label_average), size = 5,
              fontface = fontfaces) +
    theme_bw() + theme(text = element_text(size = 15), legend.position = "none")
  return(boxplots)
}


box_savings <- createBoxplots(ylabel = "Savings (%)",
                              df = data.frame(measure = 100 * results$savings,
                                              method  = results$method),
                              ylimit = c(100 * min(results$savings, na.rm = TRUE) - 10,
                                         min(100 * max(results$savings, na.rm = TRUE) + 10, 100)),
                              average_measures = 100 * average_results$savings)

box_expected_savings <- createBoxplots(ylabel = "Expected savings (%)",
                                       df = data.frame(measure = 100 * results$expected_savings,
                                                       method  = results$method),
                                       ylimit = c(100 * min(results$expected_savings, na.rm = TRUE) - 10,
                                                  min(100 * max(results$expected_savings, na.rm = TRUE) + 10, 100)),
                                       average_measures = 100 * average_results$expected_savings)

box_precision <- createBoxplots(ylabel = "Precision (%)",
                                df = data.frame(measure = 100 * results$Precision,
                                                method  = results$method),
                                ylimit = c(100 * min(results$Precision, na.rm = TRUE) - 10,
                                           min(100 * max(results$Precision, na.rm = TRUE) + 10, 100)),
                                average_measures = 100 * average_results$Precision)

box_recall <- createBoxplots(ylabel = "Recall (%)",
                             df = data.frame(measure = 100 * results$Recall,
                                             method  = results$method),
                             ylimit = c(100 * min(results$Recall, na.rm = TRUE) - 10,
                                        min(100 * max(results$Recall, na.rm = TRUE) + 10, 100)),
                             average_measures = 100 * average_results$Recall)

box_F1 <- createBoxplots(ylabel = "F1 (%)",
                         df = data.frame(measure = 100 * results$F1,
                                         method  = results$method),
                         ylimit = c(100 * min(results$F1, na.rm = TRUE) - 10,
                                    min(100 * max(results$F1, na.rm = TRUE) + 10, 100)),
                         average_measures = 100 * average_results$F1)

box_AUCpr <- createBoxplots(ylabel = "AUC-PR (%)",
                            df = data.frame(measure = 100 * results$AUC_pr,
                                            method  = results$method),
                            ylimit = c(100 * min(results$AUC_pr, na.rm = TRUE) - 10,
                                       min(100 * max(results$AUC_pr, na.rm = TRUE) + 10, 100)),
                            average_measures = 100 * average_results$AUC_pr)

box_dfar <- createBoxplots(ylabel = "Detected fraud amount ratio (%)",
                           df = data.frame(measure = 100 * results$detected_fraud_amount_ratio,
                                           method  = results$method),
                           ylimit = c(100 * min(results$detected_fraud_amount_ratio, na.rm = TRUE) - 10,
                                      min(100 * max(results$detected_fraud_amount_ratio, na.rm = TRUE) + 10, 100)),
                           average_measures = 100 * average_results$detected_fraud_amount_ratio)

box_cor <- createBoxplots(ylabel = "Spearman correlation (%) - Amount vs Score (frauds)",
                          df = data.frame(measure = 100 * results$cor,
                                          method  = results$method),
                          ylimit = c(100 * min(results$cor, na.rm = TRUE) - 10,
                                     min(100 * max(results$cor, na.rm = TRUE) + 10, 100)),
                          average_measures = 100 * average_results$cor)

box_cost <- createBoxplots(ylabel = "Cost",
                              df = data.frame(measure = results$average_cost,
                                              method  = results$method),
                              ylimit = c(min(results$average_cost, na.rm = TRUE),
                                         min(max(results$average_cost, na.rm = TRUE))),
                              average_measures = average_results$average_cost)



# Boxplots - - - - - - - - - - - - - - - - - - - -
# grid.arrange(box_expected_savings, box_savings, box_dfar,
#              box_precision, box_recall, box_F1, box_AUCpr,
#              layout_matrix = rbind(c(1, 5), c(2, 6), c(3, 7), c(4, 8)))
#

grid.arrange(box_expected_savings, box_savings,
             box_precision, box_recall, box_F1,
             layout_matrix = rbind(c(1, 3), c(1, 3), c(1, 4), c(2, 4), c(2, 5), c(2, 5)))

# grid.arrange(box_expected_savings, box_savings,
#              layout_matrix = rbind(c(1, 2)))



# Boxplot execution times - - - - - - - - - - - -
# box_time <- createBoxplots(ylabel = "Execution time (sec.)",
#                            df = data.frame(measure = times$time,
#                                            method  = times$method),
#                            ylimit = c(0, max(times$time)),
#                            average_measures = average_times$time)
# plot(box_time)



# Importance plots - - - - - - - - - - - - - - -
# xgb.plot.importance(xgb.importance(model = csbtree$xgbmodel))
# xgb.plot.importance(xgb.importance(model = xgbtree))


print(cbind.data.frame(total_fraud_amount = results_cslogit$total_fraud_amount,
                       total_fixed_cost = results_cslogit$total_fixed_cost,
                       check = results_cslogit$total_fraud_amount < results_cslogit$total_fixed_cost))

