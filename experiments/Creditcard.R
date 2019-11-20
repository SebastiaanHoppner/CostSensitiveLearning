# Kaggle - Creditcard Data ------------------------------------------------------------------------
# https://www.kaggle.com/mlg-ulb/creditcardfraud


# - Instance-dependent cost-sensitive logistic regression with lasso regularization
# - Instance-dependent cost-sensitive gradient tree boosting
# - Regular logistic regression
# - Extreme gradient boosted trees



rm(list = ls())
# Setup -------------------------------------------------------------------------------------------

# save results as - - - - - - - - - - - - - - - -
version <- "1-13"
save_results_as <- paste0("Creditcard_", gsub("-", "", Sys.Date()), "_", version, ".RData")
setwd("~/Desktop/PhD/PhD KUL/CostSensitiveLearning/experiments")


# fixed cost - - - - - - - - - - - - - - - - - - -
fixed_cost <- 10


# procedure - - - - - - - - - - - - - - - - - - -
nruns  <- 5
nfolds <- 2 # fixed!
seed   <- 2019


# cslogit parameters - - - - - - - - - - - - - - -
print_level    <- 0
lambda_cslogit <- 0
algorithm      <- "SLSQP"
maxeval        <- 10000
ftol_rel       <- 1e-8
xtol_rel       <- 1e-5
lb             <- -50
ub             <- 50


# csboost parameters - - - - - - - - - - - - - - -
verbose <- 1
print_every_n <- 10

nrounds               <- 500 # 20 # <=== !!!
early_stopping_rounds <- 10
hessian_type          <- "solution2" #"solution1", "solution2" # <=== !!!
hessian_constant      <- NULL

booster           <- "gbtree"
eta               <- 0.3
gamma             <- 0
max_depth         <- 6
min_child_weight  <- 1
max_delta_step    <- 0
subsample         <- 1
colsample_bytree  <- 1
colsample_bylevel <- 1
colsample_bynode  <- 1
lambda_csboost    <- 1
alpha             <- 0
scale_pos_weight  <- 1
base_score        <- 0.5 # <=== !!!
nthread           <- 1


# xgboost parameters - - - - - - - - - - - - - - -
objective    <- "binary:logistic"
eval_metric  <- "logloss"
maximize     <- FALSE



# Load packages -----------------------------------------------------------------------------------
library(csboost)
library(cslogit)
library(xgboost)
library(gridExtra)
library(tidyverse)
library(splitstackshape)



# Load and preprocess data ------------------------------------------------------------------------
load("../data/creditcard.RData") # creditcard <- read.csv("../data/creditcard.csv")

creditcard <- creditcard[-which(creditcard$Amount == 0), ] # remove transactions with zero Amount
creditcard <- creditcard[, -1]  # remove variable Time
creditcard$Class <- factor(creditcard$Class)  # set Class to factor variable
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
set.seed(seed)
cvfolds <- list()
for (k in 1:nruns) {
  folds <- splitstackshape::stratified(indt = cbind.data.frame(Class = creditcard$Class,
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

# Check folds:
print(nrow(creditcard[-cvfolds[[1]][[1]], ]) / nrow(creditcard)) # training part
print(nrow(creditcard[ cvfolds[[1]][[1]], ]) / nrow(creditcard)) # testing part
print(table(Class = creditcard$Class[-cvfolds[[1]][[1]]],
            amount_category = amount_category[-cvfolds[[1]][[1]]]))
print(table(Class = creditcard$Class[ cvfolds[[1]][[1]]],
            amount_category = amount_category[ cvfolds[[1]][[1]]]))
print(100 * prop.table(table(Class = creditcard$Class[-cvfolds[[1]][[1]]],
                             amount_category = amount_category[-cvfolds[[1]][[1]]])))
print(100 * prop.table(table(Class = creditcard$Class[ cvfolds[[1]][[1]]],
                             amount_category = amount_category[ cvfolds[[1]][[1]]])))



# Start cross-validation procedure ----------------------------------------------------------------
results_cslogit <- c()
results_logit   <- c()
results_csboost <- c()
results_xgboost <- c()

time_cslogit <- c()
time_logit   <- c()
time_csboost <- c()
time_xgboost <- c()

iter_cslogit <- c()
iter_logit   <- c()
iter_csboost <- c()
iter_xgboost <- c()


t_start <- proc.time()

for (k_run in 1:nruns) {
  for (j_fold in 1:nfolds) {
    # k_run = 5
    # j_fold = 2
    t_start_round <- proc.time()
    cat(paste0("\n Run ", k_run, "/", nruns, " - fold ", j_fold, "/", nfolds, "\n"))



    # Create training and test part ---------------------------------------------------------------
    train <- creditcard[-cvfolds[[k_run]][[j_fold]], ]
    test  <- creditcard[ cvfolds[[k_run]][[j_fold]], ]

    cost_matrix_train <- cost_matrix[-cvfolds[[k_run]][[j_fold]], ]
    cost_matrix_test  <- cost_matrix[ cvfolds[[k_run]][[j_fold]], ]

    amount_train <- amount[-cvfolds[[k_run]][[j_fold]]]
    amount_test  <- amount[ cvfolds[[k_run]][[j_fold]]]


    #low_amount_frauds_train <- which(amount_train < fixed_cost & train$Class == 1)
    low_amount_train <- which(amount_train < fixed_cost)
    train             <-             train[-low_amount_train, ]
    cost_matrix_train <- cost_matrix_train[-low_amount_train, ]
    amount_train      <-      amount_train[-low_amount_train]



    # Logistic regression -------------------------------------------------------------------------
    cat("  - logit...\n")
    t_start_logit <- proc.time()
    logit <- glm(formula = Class ~ ., data = train, family = "binomial")
    t_end_logit <- proc.time() - t_start_logit
    time_logit <- c(time_logit, t_end_logit[3])
    iter_logit <- c(iter_logit, logit$iter)



    # Instance-dependent cost-sensitive logistic regression with lasso regularization -------------
    cat("  - cslogit...\n")
    cslogitL1 <- cslogit(formula     = Class ~ .,
                         data        = train,
                         cost_matrix = cost_matrix_train,
                         lambda      = lambda_cslogit,
                         options     = list(
                           algorithm   = algorithm,
                           maxeval     = maxeval,
                           ftol_rel    = ftol_rel,
                           xtol_rel    = xtol_rel,
                           lb          = lb,
                           ub          = ub,
                           print_level = print_level,
                           check_data  = FALSE,
                           start       = logit$coefficients))
    time_cslogit <- c(time_cslogit, cslogitL1$time)
    iter_cslogit <- c(iter_cslogit, cslogitL1$iterations)



    # Instance-dependent cost-sensitive gradient tree boosting ------------------------------------
    cat("  - csboost...\n")
    set.seed(seed)
    csbtree <- csboost(formula               = Class ~ . - 1,
                       train                 = train,
                       test                  = NULL,
                       cost_matrix_train     = cost_matrix_train,
                       cost_matrix_test      = NULL,

                       hessian_type          = hessian_type,
                       hessian_constant      = hessian_constant,

                       booster               = booster,
                       eta                   = eta,
                       gamma                 = gamma,
                       max_depth             = max_depth,
                       min_child_weight      = min_child_weight,
                       max_delta_step        = max_delta_step,
                       subsample             = subsample,
                       colsample_bytree      = colsample_bytree,
                       colsample_bylevel     = colsample_bylevel,
                       colsample_bynode      = colsample_bynode,
                       lambda                = lambda_csboost,
                       alpha                 = alpha,
                       scale_pos_weight      = scale_pos_weight,
                       base_score            = base_score,
                       nthread               = nthread,
                       nrounds               = nrounds,
                       early_stopping_rounds = early_stopping_rounds,
                       verbose               = verbose,
                       print_every_n         = print_every_n)
    time_csboost <- c(time_csboost, csbtree$time)
    iter_csboost <- c(iter_csboost, csbtree$xgbmodel$best_iteration)



    # eXtreme Gradient Boosting -------------------------------------------------------------------
    cat("  - xgboost...\n")
    dtrain <- xgb.DMatrix(data  = model.matrix(Class ~ . - 1, data = train),
                          label = ifelse(train$Class == 1, 1, 0))
    dtest <- xgb.DMatrix(data  = model.matrix(Class ~ . - 1, data = test),
                         label = ifelse(test$Class == 1, 1, 0))

    t_start_xgboost <- proc.time()
    set.seed(seed)
    xgbtree <- xgb.train(data              = dtrain,
                         watchlist         = list(train = dtrain),

                         verbose           = verbose,
                         print_every_n     = print_every_n,
                         nthread           = nthread,
                         nrounds           = nrounds,
                         early_stopping_rounds = early_stopping_rounds,

                         booster           = booster,
                         eta               = eta,
                         gamma             = gamma,
                         max_depth         = max_depth,
                         min_child_weight  = min_child_weight,
                         max_delta_step    = max_delta_step,
                         subsample         = subsample,
                         colsample_bytree  = colsample_bytree,
                         colsample_bylevel = colsample_bylevel,
                         colsample_bynode  = colsample_bynode,
                         lambda            = lambda_csboost,
                         alpha             = alpha,
                         scale_pos_weight  = scale_pos_weight,
                         base_score        = base_score,
                         objective         = objective,
                         eval_metric       = eval_metric,
                         maximize          = maximize)
    t_end_xgboost <- proc.time() - t_start_xgboost
    time_xgboost <- c(time_xgboost, t_end_xgboost[3])
    iter_xgboost <- c(iter_xgboost, xgbtree$best_iteration)



    # Performance of the models -------------------------------------------------------------------
    scores_cslogit_test <- predict(cslogitL1, newdata = test)
    scores_logit_test   <- predict(logit,     newdata = test, type = "response")
    scores_csboost_test <- predict(csbtree,   newdata = test)
    scores_xgboost_test <- predict(xgbtree,   newdata = dtest)


    threshold_test <- fixed_cost / amount_test


    performanceMeasures <- function (scores, threshold, true_classes, cost_matrix) {
      predicted_classes <- ifelse(scores > threshold, 1, 0)
      metrics      <-     cslogit::performance(scores, predicted_classes, true_classes)$metrics
      cost_metrics <- cslogit::costPerformance(scores, predicted_classes, true_classes, cost_matrix)
      return(cbind.data.frame(metrics, cost_metrics))
    }


    results_cslogit <- rbind.data.frame(results_cslogit, performanceMeasures(scores_cslogit_test, threshold_test, test$Class, cost_matrix_test))
    results_logit   <- rbind.data.frame(results_logit,   performanceMeasures(scores_logit_test,   threshold_test, test$Class, cost_matrix_test))
    results_csboost <- rbind.data.frame(results_csboost, performanceMeasures(scores_csboost_test, threshold_test, test$Class, cost_matrix_test))
    results_xgboost <- rbind.data.frame(results_xgboost, performanceMeasures(scores_xgboost_test, threshold_test, test$Class, cost_matrix_test))


    # scatterplot of scores
    if (nrow(results_cslogit) == 1) {
      par(mfrow = c(2, 2))
      #colors <- ifelse(test$Class == 1, "red", "dodgerblue")
      #plot(scores_cslogit_test, col = colors, pch = 19, ylim = c(0, 1), main = "cslogit", ylab = "Fraud score")
      #plot(scores_logit_test,   col = colors, pch = 19, ylim = c(0, 1), main = "logit",   ylab = "Fraud score")
      #plot(scores_csboost_test, col = colors, pch = 19, ylim = c(0, 1), main = "csboost", ylab = "Fraud score")
      #plot(scores_xgboost_test, col = colors, pch = 19, ylim = c(0, 1), main = "xgboost", ylab = "Fraud score")
      boxplot(score ~ label, data = data.frame(score = scores_cslogit_test, label = test$Class), ylim = c(0, 1), main = "cslogit", ylab = "Fraud score", col = "olivedrab4")
      boxplot(score ~ label, data = data.frame(score = scores_logit_test,   label = test$Class), ylim = c(0, 1), main = "logit",   ylab = "Fraud score", col = "olivedrab4")
      boxplot(score ~ label, data = data.frame(score = scores_csboost_test, label = test$Class), ylim = c(0, 1), main = "csboost", ylab = "Fraud score", col = "olivedrab4")
      boxplot(score ~ label, data = data.frame(score = scores_xgboost_test, label = test$Class), ylim = c(0, 1), main = "xgboost", ylab = "Fraud score", col = "olivedrab4")
      par(mfrow = c(1, 1))
    }


    cat(paste("\n  - cslogit: savings =", round(results_cslogit$savings[length(results_cslogit$savings)], 4)))
    cat(paste("\n  -   logit: savings =", round(  results_logit$savings[length(results_logit$savings)],   4)))
    cat(paste("\n  - csboost: savings =", round(results_csboost$savings[length(results_csboost$savings)], 4)))
    cat(paste("\n  - xgboost: savings =", round(results_xgboost$savings[length(results_xgboost$savings)], 4), "\n"))


    t_end_round <- proc.time() - t_start_round
    cat(paste0("\n ", round(100 * (j_fold + (k_run - 1) * nfolds) / (nruns * nfolds)), "% - ",
               round(t_end_round[3]), " seconds \n"))
  }
}
t_end <- proc.time() - t_start
cat(paste("\nTime elapsed:", round(t_end[3]/60, 1), "minutes\n\n"))



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


# iterations - - - - - - - - - - - - - - - - - - -
iterations <- data.frame(iter = c(iter_cslogit, iter_logit, iter_csboost, iter_xgboost))
average_iterations <- aggregate(iterations, by = list(methods_vec), FUN = mean)
average_iterations <- average_iterations[order(match(average_iterations$Group.1, methods)), ]
iterations$method <- methods_vec


save.image(file = paste0("results Creditcard/", save_results_as))



# Study results -----------------------------------------------------------------------------------
library(ggplot2)
library(gridExtra)

createBoxplots <- function (df, ylabel, ylimit, average_measures) {
  fontfaces <- rep("plain", length(methods))
  fontfaces[which.max(average_measures)] <- "bold"
  if (any(average_measures > 100) | grepl("time", ylabel) | grepl("iterations", ylabel)) {
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



# Boxplots - - - - - - - - - - - - - - - - - - - -
# grid.arrange(box_expected_savings, box_savings, box_dfar,
#              box_precision, box_recall, box_F1, box_AUCpr,
#              layout_matrix = rbind(c(1, 5), c(2, 6), c(3, 7), c(4, 8)))
#
# grid.arrange(box_expected_savings, box_savings,
#              box_precision, box_recall, box_F1,
#              layout_matrix = rbind(c(1, 3), c(1, 3), c(1, 4), c(2, 4), c(2, 5), c(2, 5)))

grid.arrange(box_expected_savings, box_savings,
             layout_matrix = rbind(c(1, 2)))


# shapiro.test(results_cslogit$savings)
# shapiro.test(results_logit$savings)
# shapiro.test(results_csboost$savings)
# shapiro.test(results_xgboost$savings)
#
# var.test(x = results_cslogit$savings, y = results_logit$savings)
# var.test(x = results_csboost$savings, y = results_xgboost$savings)

print(t.test(x = results_cslogit$savings, y = results_logit$savings,
             paired = TRUE, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95))
print(t.test(x = results_csboost$savings, y = results_xgboost$savings,
             paired = TRUE, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95))

# print(sd(results_cslogit$savings))
# print(sd(results_logit$savings))
# print(sd(results_csboost$savings))
# print(sd(results_xgboost$savings))



# Boxplot execution times - - - - - - - - - - - -
# box_time <- createBoxplots(ylabel = "Execution time (sec.)",
#                            df = data.frame(measure = times$time,
#                                            method  = times$method),
#                            ylimit = c(0, max(times$time)),
#                            average_measures = average_times$time)
# plot(box_time)



# Boxplot number of iterations - - - - - - - - -
# box_iter <- createBoxplots(ylabel = "Number of iterations",
#                            df = data.frame(measure = iterations$iter,
#                                            method  = iterations$method),
#                            ylimit = c(0, max(iterations$iter)) - c(30, 0),
#                            average_measures = average_iterations$iter)
# plot(box_iter)



# Importance plots - - - - - - - - - - - - - - -
# xgb.plot.importance(xgb.importance(model = csbtree$xgbmodel))
# xgb.plot.importance(xgb.importance(model = xgbtree))


