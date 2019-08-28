# Testing procedure for Transfer Fraud Detection Model --------------------------------------------
# Written by Sebastiaan Höppner, 2019



rm(list = ls())
# Setup -------------------------------------------------------------------------------------------

# save results as - - - - - - - - - - - - - - - -
version <- "1-3"
save_results_as <- paste0("results_", gsub("-", "", Sys.Date()), "_", version, ".RData")
setwd("~/Desktop/bnppf")


# procedure - - - - - - - - - - - - - - - - - - - 
nruns <- 5


# fixed cost - - - - - - - - - - - - - - - - - - -
fixed_cost <- 10


# xgboost with gbtree - - - - - - - - - - - - - - 
seed <- 2019
verbose <- 1
print_every_n <- 10


early_stopping_rounds <- 10
nrounds <- 100

booster           <- "gbtree"
eta               <- 0.10
gamma             <- 0
max_depth         <- 8
min_child_weight  <- 3
max_delta_step    <- 0
subsample         <- 0.5
colsample_bytree  <- 1
colsample_bylevel <- 1
colsample_bynode  <- 1
lambda            <- 1
alpha             <- 0
scale_pos_weight  <- 1
base_score        <- 0.01
objective         <- "binary:logistic"
eval_metric       <- "logloss"
maximize          <- FALSE
nthread           <- 2


# predictor variables - - - - - - - - - - - - - - 
predictors <- c("category",
                "country_cat",
                "age_cat",
                "time_CI_flag",
                "white_merchant_flag",
                "above_mean_amount_flag",
                "zscore_logamt",
                "freq_past_merc",
                "merchant_rel_degree",
                "prob_merchantID",
                "prob_category",
                "prob_country_cat",
                "prob_time_CI_flag",
                "prob_geq_amount",
                "recency_category")



# Load packages -----------------------------------------------------------------------------------
library(cslogit)
library(xgboost)



# Start procedure ---------------------------------------------------------------------------------
results <- c()
results_best_ntreelimit <- c()


t_start <- proc.time()
for (k_run in 1:nruns) {
  #k_run = 1
  t_start_run <- proc.time()
  cat(paste0("\n Run ", k_run, "/", nruns, " -------------------------------------------------\n"))
  
  
  
  # Load data -------------------------------------------------------------------------------------
  train <- read.csv(paste0("train", k_run, ".csv"))
  test  <- read.csv(paste0("test",  k_run, ".csv"))
  
  cat(paste0("\nOverlapping: ", length(intersect(train$transactionID, test$transactionID)), "\n"))
  print(100 * prop.table(table(train$isFraud)))
  print(100 * prop.table(table(test$isFraud)))
  
  amount_train <- train$amount
  amount_test  <- test$amount
  
  train <- train[, c("isFraud", predictors)]
  test  <-  test[, c("isFraud", predictors)]
  
  
  
  # Preprocess data -------------------------------------------------------------------------------
  train$isFraud                <- as.factor(train$isFraud)
  train$time_CI_flag           <- as.factor(train$time_CI_flag)
  train$white_merchant_flag    <- as.factor(train$white_merchant_flag)
  train$above_mean_amount_flag <- as.factor(train$above_mean_amount_flag)
  train$category               <- factor(train$category, levels = c("low", "mid", "high"))
  train$freq_past_merc         <- as.numeric(scale(log(1e-5 + train$freq_past_merc)))
  train$merchant_rel_degree    <- as.numeric(scale(log(1e-5 + train$merchant_rel_degree)))
  
  test$isFraud                <- as.factor(test$isFraud)
  test$time_CI_flag           <- as.factor(test$time_CI_flag)
  test$white_merchant_flag    <- as.factor(test$white_merchant_flag)
  test$above_mean_amount_flag <- as.factor(test$above_mean_amount_flag)
  test$category               <- factor(test$category, levels = c("low", "mid", "high"))
  test$freq_past_merc         <- as.numeric(scale(log(1e-5 + test$freq_past_merc)))
  test$merchant_rel_degree    <- as.numeric(scale(log(1e-5 + test$merchant_rel_degree)))
  
  
  
  # Create cost matrix ----------------------------------------------------------------------------
  cost_matrix_train <- matrix(NA, nrow = nrow(train), ncol = 2)
  cost_matrix_train[, 1] <- ifelse(train$isFraud == 1, fixed_cost, 0)
  cost_matrix_train[, 2] <- ifelse(train$isFraud == 1, amount_train, fixed_cost)
  
  cost_matrix_test <- matrix(NA, nrow = nrow(test), ncol = 2)
  cost_matrix_test[, 1] <- ifelse(test$isFraud == 1, fixed_cost, 0)
  cost_matrix_test[, 2] <- ifelse(test$isFraud == 1, amount_test, fixed_cost)
  
  
  
  # Logistic regression ---------------------------------------------------------------------------
  #cat("  - Logistic regression...\n")
  #logit <- glm(formula = isFraud ~ ., data = train, family = "binomial")
  
  
  
  # eXtreme Gradient Boosting ---------------------------------------------------------------------
  cat("  - XGBoostTree...\n")
  dtrain <- xgb.DMatrix(data  = model.matrix(isFraud ~ . - 1, data = train),
                        label = ifelse(train$isFraud == 1, 1, 0))
  dtest <- xgb.DMatrix(data  = model.matrix(isFraud ~ . - 1, data = test),
                       label = ifelse(test$isFraud == 1, 1, 0))
  set.seed(seed)
  xgbtree <- xgb.train(data              = dtrain,
                       watchlist         = list(train = dtrain, test = dtest),
                       #watchlist         = list(train = dtrain),
                       
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
                       lambda            = lambda,
                       alpha             = alpha,
                       scale_pos_weight  = scale_pos_weight,
                       
                       base_score        = base_score,
                       objective         = objective,
                       eval_metric       = eval_metric,
                       maximize          = maximize)
  
  
  
  # Measure performance  --------------------------------------------------------------------------
  #scores_test <- predict(logit,   newdata = test, type = "response")
  scores_test <- predict(xgbtree, newdata = dtest)
  
  threshold <- fixed_cost / amount_test
  #threshold <- 0.5
  predictions_test <- ifelse(scores_test > threshold, 1, 0)
  
  
  metrics      <-     cslogit::performance(scores_test, predictions_test, test$isFraud)$metrics
  cost_metrics <- cslogit::costPerformance(scores_test, predictions_test, test$isFraud, cost_matrix_test)
  results      <- rbind.data.frame(results, cbind.data.frame(metrics, cost_metrics))
  results_best_ntreelimit <- c(results_best_ntreelimit, xgbtree$best_ntreelimit)
  
  
  t_end_run <- proc.time() - t_start_run
  cat(paste0("\n ", round(100 * k_run / nruns), "% - ",
             round(t_end_run[3]), " seconds (", round(t_end_run[3]/60, 1), " minutes) \n"))
}
t_end <- proc.time() - t_start
cat(paste("\nTime elapsed:", round(t_end[3]/60, 1), "minutes\n\n"))

results <- rbind.data.frame(results, colMeans(results, na.rm = TRUE))
rownames(results) <- c(paste0("test", 1:nruns), "average")



# Collect and save results ------------------------------------------------------------------------
save.image(file = save_results_as)
stop("PERFECT! Testing procedure was conducted successfully and results are saved!", call. = FALSE)



# Study results -----------------------------------------------------------------------------------
print(results_best_ntreelimit)
print(summary(results_best_ntreelimit))

{
  results_subset1 <- 100 * results[, c("alert_rate", "Precision", "Recall", "F1", "AUC", "AUC_pr")]
  n1_metrics <- ncol(results_subset1)
  ylimit1 <- c(min(results_subset1, na.rm = TRUE) - 10, min(max(results_subset1, na.rm = TRUE) + 10, 100))
  colors1 <- hcl(h = seq(15, 375, length = n1_metrics + 1), l = 65, c = 100)[1:n1_metrics]
  boxplot(results_subset1, ylim = ylimit1, col = colors1)
  points(as.numeric(results_subset1[nrow(results), ]), pch = 18, cex = 1.5)
  text(x = 1:n1_metrics, y = ylimit1[1],
       labels = paste0(round(results_subset1[nrow(results), ], 2), "%"))
}

{
  results_subset2 <- 100 * results[, c("detected_fraud_amount_ratio", "savings", "expected_savings")]
  n2_metrics <- ncol(results_subset2)
  ylimit2 <- c(min(results_subset2, na.rm = TRUE) - 10, min(max(results_subset2, na.rm = TRUE) + 10, 100))
  colors2 <- hcl(h = seq(15, 375, length = n2_metrics + 1), l = 65, c = 100)[1:n2_metrics]
  boxplot(results_subset2, ylim = ylimit2, col = colors2)
  points(as.numeric(results_subset2[nrow(results), ]), pch = 18, cex = 1.5)
  text(x = 1:n2_metrics, y = ylimit2[1],
       labels = paste0(round(results_subset2[nrow(results), ], 2), "%"))
}


