# Cross validation procedure for Transfer Fraud Detection Model -----------------------------------
# Written by Sebastiaan Höppner, 2019



rm(list = ls())
# Setup -------------------------------------------------------------------------------------------

# save results as - - - - - - - - - - - - - - 
save_results_as <- paste0("cv_results_", gsub("-", "", Sys.Date()), "_1-1.RData")


# cross-validation procedure - - - - - - - - -
nruns  <- 5
nfolds <- 10
seed   <- 2019


# fixed cost - - - - - - - - - - - - - - - - -
fixed_cost <- 10


# xgboost with gbtree - - - - - - - - - - - - 
verbose           <- 0
print_every_n     <- 1

nthread           <- 2
nrounds           <- 50
early_stopping_rounds <- 10

booster           <- "gbtree"
eta               <- 0.30
gamma             <- 0
max_depth         <- 6
min_child_weight  <- 1
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



# Load packages -----------------------------------------------------------------------------------
library(caret)
library(cslogit)
library(xgboost)
library(reshape2)
library(gridExtra)



# Load and preprocess data ------------------------------------------------------------------------
setwd("~/Desktop/PhD/PhD KUL/CSLogit/experiments")
creditcard <- read.csv("../data/transferfraud_extended.csv")

amount <- creditcard$amount


# select predictor variables - - - - - - - - -
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

creditcard <- creditcard[, c("isFraud", predictors)]


# preprocessing - - - - - - - - - - - - - - - 
creditcard$isFraud                <- as.factor(creditcard$isFraud)
creditcard$time_CI_flag           <- as.factor(creditcard$time_CI_flag)
creditcard$white_merchant_flag    <- as.factor(creditcard$white_merchant_flag)
creditcard$above_mean_amount_flag <- as.factor(creditcard$above_mean_amount_flag)
creditcard$category               <- factor(creditcard$category, levels = c("low", "mid", "high"))

creditcard$freq_past_merc         <- as.numeric(scale(log(1e-5 + creditcard$freq_past_merc)))
creditcard$merchant_rel_degree    <- as.numeric(scale(log(1e-5 + creditcard$merchant_rel_degree)))



# Create cost matrix ------------------------------------------------------------------------------
cost_matrix <- matrix(NA, nrow = nrow(creditcard), ncol = 2)
cost_matrix[, 1] <- ifelse(creditcard$isFraud == 1, fixed_cost, 0)
cost_matrix[, 2] <- ifelse(creditcard$isFraud == 1, amount, fixed_cost)



# Create folds (nruns x nfolds) for cross-validation ----------------------------------------------
set.seed(seed)
cvfolds <- list()
for (k in 1:nruns) {
  cvfolds[[k]] <- caret::createFolds(y = creditcard$isFraud, k = nfolds)
}

# nrow(creditcard[-cvfolds[[1]][[1]], ]) / nrow(creditcard)  # training part
# nrow(creditcard[ cvfolds[[1]][[1]], ]) / nrow(creditcard)  # validation part
# table(creditcard$isFraud[-cvfolds[[1]][[1]]])
# table(creditcard$isFraud[ cvfolds[[1]][[1]]])
# 100 * prop.table(table(creditcard$isFraud[-cvfolds[[1]][[1]]]))
# 100 * prop.table(table(creditcard$isFraud[ cvfolds[[1]][[1]]]))



# Start cross-validation procedure ----------------------------------------------------------------
cv_results <- c()


t_start <- proc.time()
for (k_run in 1:nruns) {
  for (j_fold in 1:nfolds) {
    #k_run = 1; j_fold = 1
    t_start_round <- proc.time()
    cat(paste0("\n Run ", k_run, "/", nruns, " - fold ", j_fold, "/", nfolds, "\n"))
    
    
    
    # Create training and validation part ---------------------------------------------------------
    train <- creditcard[-cvfolds[[k_run]][[j_fold]], ]
    valid <- creditcard[ cvfolds[[k_run]][[j_fold]], ]
    
    amount_train <- amount[-cvfolds[[k_run]][[j_fold]]]
    amount_valid <- amount[ cvfolds[[k_run]][[j_fold]]]
    
    cost_matrix_train <- cost_matrix[-cvfolds[[k_run]][[j_fold]], ]
    cost_matrix_valid <- cost_matrix[ cvfolds[[k_run]][[j_fold]], ]
    
    
    
    # Logistic regression -------------------------------------------------------------------------
    #cat("  - Logistic regression...\n")
    #logit <- glm(formula = isFraud ~ ., data = train, family = "binomial")
    
    
    
    # eXtreme Gradient Boosting - gbtree ----------------------------------------------------------
    cat("  - XGBoostTree...\n")
    dtrain <- xgb.DMatrix(data  = model.matrix(isFraud ~ . - 1, data = train),
                          label = ifelse(train$isFraud == 1, 1, 0))
    dvalid <- xgb.DMatrix(data  = model.matrix(isFraud ~ . - 1, data = valid),
                          label = ifelse(valid$isFraud == 1, 1, 0))
    set.seed(seed)
    xgbtree <- xgb.train(data              = dtrain,
                         watchlist         = list(train = dtrain, valid = dvalid),
                         
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
    
    
    
    # Performance of the models -------------------------------------------------------------------
    #scores_valid <- predict(logit,   newdata = valid, type = "response")
    scores_valid <- predict(xgbtree, newdata = dvalid)
    
    threshold <- fixed_cost / amount_valid
    #threshold <- 0.5
    predictions_valid <- ifelse(scores_valid > threshold, 1, 0)
    
    
    metrics      <- cslogit::performance(    scores_valid, predictions_valid, valid$isFraud)$metrics
    cost_metrics <- cslogit::costPerformance(scores_valid, predictions_valid, valid$isFraud, cost_matrix_valid)
    cv_results   <- rbind.data.frame(cv_results, cbind.data.frame(metrics, cost_metrics))
    
    
    t_end_round <- proc.time() - t_start_round
    cat(paste0("\n ", round(100*(j_fold + (k_run - 1) * nfolds) / (nruns * nfolds)), "% - ",
               round(t_end_round[3]), " seconds (", round(t_end_round[3]/60, 1), " minutes) \n"))
  }
}
t_end <- proc.time() - t_start
cat(paste("\nTime elapsed:", round(t_end[3]/60, 1), "minutes\n\n"))



# Collect and save results ------------------------------------------------------------------------
save.image(file = paste0("~/Desktop/", save_results_as))
stop("PERFECT! Cross validation was conducted successfully and results are saved!", call. = FALSE)



# Study results -----------------------------------------------------------------------------------
cv_results_subset <- 100 * cv_results[, c("Precision", "Recall", "F1", "savings", "expected_savings")]
n_metrics <- ncol(cv_results_subset)
ylimit <- c(min(cv_results_subset, na.rm = TRUE) - 10, min(max(cv_results_subset, na.rm = TRUE) + 10, 100))
colors <- hcl(h = seq(15, 375, length = n_metrics + 1), l = 65, c = 100)[1:n_metrics]
boxplot(cv_results_subset, ylim = ylimit, col = colors)
points(colMeans(cv_results_subset, na.rm = TRUE), pch = 18, cex = 1.5)
text(x = 1:n_metrics, y = ylimit[1],
     labels = paste0(round(colMeans(cv_results_subset, na.rm = TRUE), 2), "%"))


