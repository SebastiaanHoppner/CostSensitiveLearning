# Kaggle - Creditcard Data ------------------------------------------------------------------------
# https://www.kaggle.com/mlg-ulb/creditcardfraud


# - Instance-dependent cost-sensitive logistic regression with lasso regularization
# - Instance-dependent cost-sensitive gradient tree boosting
# - Regular logistic regression
# - Extreme gradient boosted trees



rm(list = ls())
# Setup -------------------------------------------------------------------------------------------

# save results as - - - - - - - - - - - - - - - -
version <- "1-1"
save_results_as <- paste0("Creditcard_", gsub("-", "", Sys.Date()), "_", version, ".RData")
setwd("~/Desktop/PhD/PhD KUL/CostSensitiveLearning/experiments")


# procedure - - - - - - - - - - - - - - - - - - -
nruns  <- 1
nfolds <- 5
seed   <- 2019


# fixed cost - - - - - - - - - - - - - - - - - - -
fixed_cost <- 10


# cslogit parameters - - - - - - - - - - - - - - -
print_level    <- 1
lambda_cslogit <- 0 #0.00001
algorithm      <- "SLSQP"
maxeval        <- 10000
ftol_rel       <- 1e-8
xtol_rel       <- 1e-5
lb             <- -50
ub             <- 50


# csboost parameters - - - - - - - - - - - - - - -
verbose <- 1
print_every_n <- 10

nrounds               <- 100
early_stopping_rounds <- 10
hessian_type          <- "solution1"
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
base_score        <- 0.3
nthread           <- 1


# xgboost parameters - - - - - - - - - - - - - - -
objective    <- "binary:logistic"
eval_metric  <- "logloss"
maximize     <- FALSE



# Load packages -----------------------------------------------------------------------------------
library(caret)
library(csboost)
library(cslogit)
library(xgboost)
library(gridExtra)
library(tidyverse)



# Load and preprocess data ------------------------------------------------------------------------
load("../data/creditcard.RData") # creditcard <- read.csv("../data/creditcard.csv")

####
creditcard <- creditcard[-which(creditcard$Amount == 0), ] # remove transactions with zero Amount
rownames(creditcard) <- 1:nrow(creditcard)
####

amount <- creditcard$Amount

creditcard <- creditcard[, -1]  # remove variable Time
creditcard$Class  <- factor(creditcard$Class)

creditcard_original <- creditcard   # used by cslogit and logit
creditcard_processed <- creditcard  # used by csboost and xgboost
rm(creditcard)

creditcard_processed$Amount <- log(creditcard_processed$Amount + 0.01)
creditcard_processed[, -30] <- scale(creditcard_processed[, -30])



# Create cost matrix ------------------------------------------------------------------------------
cost_matrix <- matrix(nrow = nrow(creditcard_original), ncol = 2)
cost_matrix[, 1] <- ifelse(creditcard_original$Class == 1, fixed_cost, 0)
cost_matrix[, 2] <- ifelse(creditcard_original$Class == 1, amount, fixed_cost)

# cost_matrix[, 1] <- ifelse(creditcard_original$Class == 1, fixed_cost, 0)
# cost_matrix[, 2] <- ifelse(creditcard_original$Class == 1, amount, 10 * fixed_cost)

# cost_matrix[, 1] <- ifelse(creditcard_original$Class == 1, log(fixed_cost), 0)
# cost_matrix[, 2] <- ifelse(creditcard_original$Class == 1, log(amount), log(fixed_cost))
# cost_matrix[, 1] <- ifelse(creditcard_original$Class == 1, log(fixed_cost), 0)
# cost_matrix[, 2] <- ifelse(creditcard_original$Class == 1, log(amount), log(10 * fixed_cost))



# Create folds (nruns x nfolds) for cross-validation ----------------------------------------------
set.seed(seed)
cvfolds <- list()
for (k in 1:nruns) {
  cvfolds[[k]] <- caret::createFolds(y = creditcard_original$Class, k = nfolds)
}

# Check folds:
# nrow(creditcard_original[-cvfolds[[1]][[1]], ]) / nrow(creditcard_original) # training part
# nrow(creditcard_original[ cvfolds[[1]][[1]], ]) / nrow(creditcard_original) # validation part
# table(creditcard_original$Class[-cvfolds[[1]][[1]]])
# table(creditcard_original$Class[ cvfolds[[1]][[1]]])
# 100 * prop.table(table(creditcard_original$Class[-cvfolds[[1]][[1]]]))
# 100 * prop.table(table(creditcard_original$Class[ cvfolds[[1]][[1]]]))



# Start cross-validation procedure ----------------------------------------------------------------
results_cslogit   <- c()
results_logit     <- c()
results_csboost   <- c()
results_xgboost   <- c()

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
    # k_run = 1
    # j_fold = 1
    t_start_round <- proc.time()
    cat(paste0("\n Run ", k_run, "/", nruns, " - fold ", j_fold, "/", nfolds, "\n"))



    # Create training and validation part ---------------------------------------------------------
    train_original <- creditcard_original[-cvfolds[[k_run]][[j_fold]], ]
    valid_original <- creditcard_original[ cvfolds[[k_run]][[j_fold]], ]

    train_processed <- creditcard_processed[-cvfolds[[k_run]][[j_fold]], ]
    valid_processed <- creditcard_processed[ cvfolds[[k_run]][[j_fold]], ]

    cost_matrix_train <- cost_matrix[-cvfolds[[k_run]][[j_fold]], ]
    cost_matrix_valid <- cost_matrix[ cvfolds[[k_run]][[j_fold]], ]

    amount_train <- amount[-cvfolds[[k_run]][[j_fold]]]
    amount_valid <- amount[ cvfolds[[k_run]][[j_fold]]]



    # Logistic regression -------------------------------------------------------------------------
    cat("  - Logistic regression...\n")
    t_start_logit <- proc.time()
    logit <- glm(formula = Class ~ ., data = train_processed, family = "binomial")
    t_end_logit <- proc.time() - t_start_logit
    time_logit <- c(time_logit, t_end_logit[3])
    iter_logit <- c(iter_logit, logit$iter)



    # Instance-dependent cost-sensitive logistic regression with lasso regularization -------------
    cat("  - Cost-sensitive logistic regression...\n")
    cslogitL1 <- cslogit(formula     = Class ~ .,
                         data        = train_processed,
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
    cat("  - Cost-sensitive xgboost...\n")
    set.seed(seed)
    csbtree <- csboost(formula               = Class ~ . - 1,
                       train                 = train_original,
                       test                  = valid_original,
                       cost_matrix_train     = cost_matrix_train,
                       cost_matrix_test      = cost_matrix_valid,

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
    cat("  - XGBoost...\n")
    dtrain_original <- xgb.DMatrix(data  = model.matrix(Class ~ . - 1, data = train_original),
                                   label = ifelse(train_original$Class == 1, 1, 0))
    dvalid_original <- xgb.DMatrix(data  = model.matrix(Class ~ . - 1, data = valid_original),
                                   label = ifelse(valid_original$Class == 1, 1, 0))

    t_start_xgboost <- proc.time()
    set.seed(seed)
    xgbtree <- xgb.train(data              = dtrain_original,
                         watchlist         = list(train = dtrain_original, test = dvalid_original),
                         #watchlist         = list(train = dtrain_original),

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
    scores_cslogit <- predict(cslogitL1, newdata = valid_processed)
    scores_logit   <- predict(logit,     newdata = valid_processed, type = "response")
    scores_csboost <- predict(csbtree,   newdata = valid_original)
    scores_xgboost <- predict(xgbtree,   newdata = dvalid_original)

    thresholds <- fixed_cost / amount_valid

    performanceMeasures <- function (scores, thresholds, true_classes, cost_matrix) {
      predicted_classes <- ifelse(scores > thresholds, 1, 0)
      metrics      <-     cslogit::performance(scores, predicted_classes, true_classes)$metrics
      cost_metrics <- cslogit::costPerformance(scores, predicted_classes, true_classes, cost_matrix)
      curve_metrics <- cslogit::liftcurve(scores, true_classes, cost_matrix, show = FALSE)
      return(cbind.data.frame(metrics, cost_metrics, curve_metrics))
    }

    results_cslogit <- rbind.data.frame(results_cslogit, performanceMeasures(scores_cslogit, thresholds, valid_processed$Class, cost_matrix_valid))
    results_logit   <- rbind.data.frame(results_logit,   performanceMeasures(scores_logit,   thresholds, valid_processed$Class, cost_matrix_valid))
    results_csboost <- rbind.data.frame(results_csboost, performanceMeasures(scores_csboost, thresholds, valid_original$Class,  cost_matrix_valid))
    results_xgboost <- rbind.data.frame(results_xgboost, performanceMeasures(scores_xgboost, thresholds, valid_original$Class,  cost_matrix_valid))


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

average_results <- aggregate(results, by = list(methods_vec), FUN = mean)
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

box_mac <- createBoxplots(ylabel = "Maximum avoided costs",
                          df = data.frame(measure = results$max_avoided_costs,
                                          method  = results$method),
                          ylimit = range(results$max_avoided_costs) - c(500, 0),
                          average_measures = average_results$max_avoided_costs)



# Boxplots - - - - - - - - - - - - - - - - - - - -
grid.arrange(box_expected_savings, box_savings, box_dfar, box_mac,
             box_precision, box_recall, box_F1, box_AUCpr,
             layout_matrix = rbind(c(1, 5), c(2, 6), c(3, 7), c(4, 8)))

grid.arrange(box_expected_savings, box_savings,
             box_precision, box_recall, box_F1,
             layout_matrix = rbind(c(1, 3), c(1, 3), c(1, 4), c(2, 4), c(2, 5), c(2, 5)))



# Scatterplot of scores - - - - - - - - - - - - -
par(mfrow = c(2, 2))
colors <- ifelse(valid_processed$Class == 1, "red", "dodgerblue")
plot(scores_cslogit, col = colors, pch = 19, ylim = c(0, 1), main = "cslogit", ylab = "Fraud score")
plot(scores_logit,   col = colors, pch = 19, ylim = c(0, 1), main = "logit",   ylab = "Fraud score")
plot(scores_csboost, col = colors, pch = 19, ylim = c(0, 1), main = "csboost", ylab = "Fraud score")
plot(scores_xgboost, col = colors, pch = 19, ylim = c(0, 1), main = "xgboost", ylab = "Fraud score")
par(mfrow = c(1, 1))



# Gains curves - - - - - - - - - - - - - - - - - -
par(mfrow = c(2, 2))
liftcurve(scores_cslogit, valid_processed$Class, cost_matrix_valid, which = "gains")
liftcurve(scores_logit,   valid_processed$Class, cost_matrix_valid, which = "gains")
liftcurve(scores_csboost, valid_original$Class,  cost_matrix_valid, which = "gains")
liftcurve(scores_xgboost, valid_original$Class,  cost_matrix_valid, which = "gains")
par(mfrow = c(1, 1))



# Lift curves - - - - - - - - - - - - - - - - - -
par(mfrow = c(2, 2))
liftcurve(scores_cslogit, valid_processed$Class, cost_matrix_valid, which = "lift")
liftcurve(scores_logit,   valid_processed$Class, cost_matrix_valid, which = "lift")
liftcurve(scores_csboost, valid_original$Class,  cost_matrix_valid, which = "lift")
liftcurve(scores_xgboost, valid_original$Class,  cost_matrix_valid, which = "lift")
par(mfrow = c(1, 1))



# Costs curves - - - - - - - - - - - - - - - - - -
par(mfrow = c(2, 2))
liftcurve(scores_cslogit, valid_processed$Class, cost_matrix_valid, which = "costs")
liftcurve(scores_logit,   valid_processed$Class, cost_matrix_valid, which = "costs")
liftcurve(scores_csboost, valid_original$Class,  cost_matrix_valid, which = "costs")
liftcurve(scores_xgboost, valid_original$Class,  cost_matrix_valid, which = "costs")
par(mfrow = c(1, 1))



# Boxplot execution times - - - - - - - - - - - -
box_time <- createBoxplots(ylabel = "Execution time (sec.)",
                           df = data.frame(measure = times$time,
                                           method  = times$method),
                           ylimit = c(0, max(times$time)),
                           average_measures = average_times$time)
plot(box_time)



# Boxplot execution times - - - - - - - - - - - -
box_iter <- createBoxplots(ylabel = "Number of iterations",
                           df = data.frame(measure = iterations$iter,
                                           method  = iterations$method),
                           ylimit = c(0, max(iterations$iter)) - c(30, 0),
                           average_measures = average_iterations$iter)
plot(box_iter)


