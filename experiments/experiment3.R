# Kaggle - Creditcard Data ------------------------------------------------------------------------
# https://www.kaggle.com/mlg-ulb/creditcardfraud



rm(list = ls())
# Setup -------------------------------------------------------------------------------------------

# - Instance-dependent cost-sensitive logistic regression with lasso regularization
# - Regular logistic regression
# - Stepwise logistic regression
# - Lasso-regularized logistic regression
# - Ridge-regularized logistic regression
# - Logistic regression with elastic net penalty
# - Linear discriminant analysis



# save results as ----------------------------
save_results_as <- paste0("experiment3_", Sys.Date(), "_1.RData")



# cross-validation procedure -----------------
nruns  <- 1
nfolds <- 5
seed   <- 2019



# fixed cost ---------------------------------
fixed_cost <- 10



# cslogit ------------------------------------
lambda    <- 0
algorithm <- "SLSQP"
maxeval   <- 10000
ftol_rel  <- 1e-8
xtol_rel  <- 1e-5
lb        <- -50
ub        <- 50



# Load packages -----------------------------------------------------------------------------------
library(MASS)
library(caret)
library(glmnet)
library(scales)
library(cslogit)
library(gridExtra)
library(tidyverse)



# Load and preprocess data ------------------------------------------------------------------------
setwd("~/Desktop/PhD/PhD KUL/FraudLogit/experiments")
load("../data/creditcard.RData") # creditcard <- read.csv("../data/creditcard.csv")

#creditcard <- creditcard[-which(creditcard$Amount == 0), ] # remove transactions with zero Amount
#rownames(creditcard) <- 1:nrow(creditcard)

amount <- creditcard$Amount

creditcard$Class <- factor(creditcard$Class)
creditcard$Logamount <- log(creditcard$Amount + 0.01)
creditcard <- creditcard[, -c(1, 30)] # remove variables Time and Amount
creditcard <- creditcard[, c(1:28, 30, 29)]
creditcard[, -30] <- scale(creditcard[, -30])



# Create cost matrix ------------------------------------------------------------------------------
cost_matrix <- matrix(NA, nrow = nrow(creditcard), ncol = 2)
cost_matrix[, 1] <- ifelse(creditcard$Class == 1, fixed_cost, 0)
cost_matrix[, 2] <- ifelse(creditcard$Class == 1, amount, fixed_cost)



# Create folds (nruns x nfolds) for cross-validation ----------------------------------------------
set.seed(seed)
cvfolds <- list()
for (k in 1:nruns) {
  cvfolds[[k]] <- caret::createFolds(y = creditcard$Class, k = nfolds)
}

# nrow(creditcard[-cvfolds[[1]][[1]], ]) / nrow(creditcard) # training part
# nrow(creditcard[ cvfolds[[1]][[1]], ]) / nrow(creditcard) # validation part
# table(creditcard$Class[-cvfolds[[1]][[1]]])
# table(creditcard$Class[ cvfolds[[1]][[1]]])
# 100 * prop.table(table(creditcard$Class[-cvfolds[[1]][[1]]]))
# 100 * prop.table(table(creditcard$Class[ cvfolds[[1]][[1]]]))



# Start cross-validation procedure ----------------------------------------------------------------
results_cslogitL1 <- c()
results_logit     <- c()
#results_steplogit <- c()
#results_lasso     <- c()
#results_ridge     <- c()
#results_elnet     <- c()
results_lda       <- c()


t_start <- proc.time()
for (k in 1:nruns) {
  for (j in 1:nfolds) {
    #k = 1
    #j = 1
    t_start_round <- proc.time()
    cat(paste0("\n Run ", k, "/", nruns, " - fold ", j, "/", nfolds, "\n"))
    
    
    
    # Create training and validation part ---------------------------------------------------------
    train <- creditcard[-cvfolds[[k]][[j]], ]
    valid <- creditcard[ cvfolds[[k]][[j]], ]
    
    amount_train <- amount[-cvfolds[[k]][[j]]]
    amount_valid <- amount[ cvfolds[[k]][[j]]]
    
    cost_matrix_train <- cost_matrix[-cvfolds[[k]][[j]], ]
    cost_matrix_valid <- cost_matrix[ cvfolds[[k]][[j]], ]
    
    
    
    # Logistic regression -------------------------------------------------------------------------
    cat("  - Logistic regression...\n")
    logit <- glm(formula = Class ~ ., data = train, family = "binomial")
    
    
    
    # Instance-dependent cost-sensitive logistic regression with lasso regularization -------------
    cat("  - Cost-sensitive logistic regression...\n")
    cslogitL1 <- cslogit(formula     = Class ~ .,
                         data        = train,
                         cost_matrix = cost_matrix_train,
                         lambda      = lambda,
                         options     = list(
                           algorithm   = algorithm,
                           maxeval     = maxeval,
                           ftol_rel    = ftol_rel,
                           xtol_rel    = xtol_rel,
                           lb          = lb,
                           ub          = ub,
                           print_level = 0,
                           check_data  = FALSE,
                           start       = logit$coefficients))
    
    
    
    # Stepwise logstic regression -----------------------------------------------------------------
    #cat("  - Stepwise logistic regression...\n")
    #set.seed(seed)
    #steplogit <- stepAIC(logit, scope = list(upper = ~ ., lower ~ 1), direction = "both",
    #                     steps = 1000, trace = 0)
    
    
    
    # Lasso-regularized logistic regression -------------------------------------------------------
    #cat("  - Lasso-regularized logistic regression...\n")
    #set.seed(seed)
    #lassoCV <- cv.glmnet(x            = model.matrix(Class ~ ., data = train)[, -1],
    #                     y            = train$Class,
    #                     family       = "binomial",
    #                     alpha        = 1,
    #                     intercept    = TRUE,
    #                     standardize  = FALSE,
    #                     type.measure = "deviance",
    #                     nfolds       = nfolds)
    
    
    
    # Ridge-regularized logistic regression -------------------------------------------------------
    #cat("  - Ridge-regularized logistic regression...\n")
    #set.seed(seed)
    #ridgeCV <- cv.glmnet(x            = model.matrix(Class ~ ., data = train)[, -1],
    #                     y            = train$Class,
    #                     family       = "binomial",
    #                     alpha        = 0,
    #                     intercept    = TRUE,
    #                     standardize  = FALSE,
    #                     type.measure = "deviance",
    #                     nfolds       = nfolds)
    
    
    
    # Logistic regression with elastic net penalty ------------------------------------------------
    #cat("  - Logistic regression with elastic net penalty...\n")
    #set.seed(seed)
    #elnetCV <- cv.glmnet(x            = model.matrix(Class ~ ., data = train)[, -1],
    #                     y            = train$Class,
    #                     family       = "binomial",
    #                     alpha        = 0.5,
    #                     intercept    = TRUE,
    #                     standardize  = FALSE,
    #                     type.measure = "deviance",
    #                     nfolds       = nfolds)
    
    
    
    # Linear discriminant analysis ----------------------------------------------------------------
    cat("  - Linear discriminant analysis...\n")
    linda <- lda(formula = Class ~ ., data = train, method = "moment")
    
    
    
    # Performance of the models -------------------------------------------------------------------
    #validGlmnet <- model.matrix(Class ~ ., data = valid)[, -1]
    
    scores_cslogitL1 <- predict(cslogitL1, newdata = valid)
    scores_logit     <- predict(logit,     newdata = valid,    type = "response")
    #scores_steplogit <- predict(steplogit, newdata = valid,    type = "response")
    #scores_lasso     <- predict(lassoCV,   newx = validGlmnet, type = "response", s = "lambda.1se")
    #scores_ridge     <- predict(ridgeCV,   newx = validGlmnet, type = "response", s = "lambda.1se")
    #scores_elnet     <- predict(elnetCV,   newx = validGlmnet, type = "response", s = "lambda.1se")
    scores_lda       <- predict(linda,     newdata = valid)$posterior[, 2]
    
    
    performanceMeasures <- function (scores, thresholds, true_classes, cost_matrix) {
      predicted_classes <- ifelse(scores > thresholds, 1, 0)
      metrics           <-     performance(scores, predicted_classes, true_classes)$metrics
      cost_metrics      <- costPerformance(scores, predicted_classes, true_classes, cost_matrix)
      return(cbind.data.frame(metrics, cost_metrics))
    }
    
    thresholds <- fixed_cost / amount_valid
    
    results_cslogitL1 <- rbind.data.frame(results_cslogitL1,
                                          performanceMeasures(scores_cslogitL1, thresholds,
                                                              valid$Class, cost_matrix_valid))
    results_logit     <- rbind.data.frame(results_logit,
                                          performanceMeasures(scores_logit, thresholds,
                                                              valid$Class, cost_matrix_valid))
    #results_steplogit <- rbind.data.frame(results_steplogit,
    #                                      performanceMeasures(scores_steplogit, thresholds,
    #                                                          valid$Class, cost_matrix_valid))
    #results_lasso     <- rbind.data.frame(results_lasso,
    #                                      performanceMeasures(scores_lasso, thresholds,
    #                                                          valid$Class, cost_matrix_valid))
    #results_ridge     <- rbind.data.frame(results_ridge,
    #                                      performanceMeasures(scores_ridge, thresholds,
    #                                                          valid$Class, cost_matrix_valid))
    #results_elnet     <- rbind.data.frame(results_elnet,
    #                                      performanceMeasures(scores_elnet, thresholds,
    #                                                          valid$Class, cost_matrix_valid))
    results_lda       <- rbind.data.frame(results_lda,
                                          performanceMeasures(scores_lda, thresholds,
                                                              valid$Class, cost_matrix_valid))
    
    t_end_round <- proc.time() - t_start_round
    cat(paste0("\n ", round(100*(j + (k-1)*nfolds)/(nruns*nfolds)), "% - ",
               round(t_end_round[3]), " seconds \n"))
  }
}
t_end <- proc.time() - t_start
cat(paste("\nTime elapsed:", round(t_end[3]/60, 1), "minutes\n\n"))



# Collect and save results ------------------------------------------------------------------------
methods <- c("cslogitL1", "logit", #"steplogit", "lasso", "ridge", "elasticnet",
             "LDA")
methods_vec <- rep(methods, nruns * nfolds)[order(match(rep(methods, nruns * nfolds), methods))]

results <- rbind.data.frame(results_cslogitL1,
                            results_logit,
                            #results_steplogit,
                            #results_lasso,
                            #results_ridge,
                            #results_elnet,
                            results_lda)

average_results <- aggregate(results, by = list(methods_vec), FUN = mean, na.rm = TRUE)
average_results <- average_results[order(match(average_results$Group.1, methods)), ]

results <- cbind.data.frame(results, method = methods_vec)

rm(creditcard, train, valid, cost_matrix, cost_matrix_train, cost_matrix_valid,
   amount_train, amount_valid, cvfolds)
save.image(file = paste0("Results/", save_results_as))



# Study results -----------------------------------------------------------------------------------
createBoxplots <- function (df, ylabel, ylimit, average_measures) {
  fontfaces <- rep("plain", length(methods))
  fontfaces[which.max(average_measures)] <- "bold"
  boxplots <- ggplot(data = df, mapping = aes(x = method, y = measure, fill = method)) +
    stat_boxplot(geom = "errorbar", width = 0.4) + geom_boxplot() +
    ylab(ylabel) + xlab("") + ylim(ylimit) + scale_x_discrete(limits = unique(df$method)) +
    stat_summary(fun.y = mean, geom = "point", shape = 18, size = 5, col = "black") +
    geom_text(data = data.frame(method = unique(df$method), average_measures = average_measures),
              mapping = aes(x = method, y = min(ylimit, na.rm = TRUE),
                            label = paste0(round(average_measures, 2), "%")), size = 5,
              fontface = fontfaces) +
    theme_bw() + theme(text = element_text(size = 15), legend.position = "none")
  return(boxplots)
}


p_savings <- createBoxplots(df = data.frame(measure = 100 * results$savings,
                                            method  = results$method),
                            ylabel = "Savings (%)",
                            ylimit = c(100 * min(results$savings, na.rm = TRUE) - 10,
                                       min(100 * max(results$savings, na.rm = TRUE) + 10, 100)),
                            average_measures = 100 * average_results$savings)

p_expected_savings <- createBoxplots(df = data.frame(measure = 100 * results$expected_savings,
                                                     method  = results$method),
                                     ylabel = "Expected savings (%)",
                                     ylimit = c(100 * min(results$expected_savings, na.rm = TRUE) - 10,
                                                min(100 * max(results$expected_savings, na.rm = TRUE) + 10, 100)),
                                     average_measures = 100 * average_results$expected_savings)

p_precision <- createBoxplots(df = data.frame(measure = 100 * results$Precision,
                                              method  = results$method),
                              ylabel = "Precision (%)",
                              ylimit = c(100 * min(results$Precision, na.rm = TRUE) - 10,
                                         min(100 * max(results$Precision, na.rm = TRUE) + 10, 100)),
                              average_measures = 100 * average_results$Precision)

p_recall <- createBoxplots(df = data.frame(measure = 100 * results$Recall,
                                           method  = results$method),
                           ylabel = "Recall (%)",
                           ylimit = c(100 * min(results$Recall, na.rm = TRUE) - 10,
                                      min(100 * max(results$Recall, na.rm = TRUE) + 10, 100)),
                           average_measures = 100 * average_results$Recall)

p_F1 <- createBoxplots(df = data.frame(measure = 100 * results$F1,
                                       method  = results$method),
                       ylabel = "F1 (%)",
                       ylimit = c(100 * min(results$F1, na.rm = TRUE) - 10,
                                  min(100 * max(results$F1, na.rm = TRUE) + 10, 100)),
                       average_measures = 100 * average_results$F1)



# Boxplots -----------------------------------
grid.arrange(p_savings, p_expected_savings, p_F1, p_precision, p_recall,
             layout_matrix = rbind(c(2, 4), c(1, 5), c(1, 3)))

# grid.arrange(p_savings, p_expected_savings,
#              layout_matrix = rbind(c(1, 2)))
#
# grid.arrange(p_F1, p_precision, p_recall,
#              layout_matrix = rbind(c(1, 2), c(1, 3)))



# Density of scores --------------------------
colors <- scales::hue_pal()(length(methods))[c(1, 3, 2)]#[c(1, 5, 7, 3, 6, 2, 4)]
plot( density(scores_cslogitL1, from = 0, to = 1, bw = 0.01), col = colors[1], lwd = 2, lty = 2,
      main = "Density plot of fraud scores on validation set", xlab = "Fraud score")
lines(density(scores_logit,     from = 0, to = 1, bw = 0.01), col = colors[2], lwd = 2)
#lines(density(scores_steplogit, from = 0, to = 1, bw = 0.01), col = colors[3], lwd = 2)
#lines(density(scores_lasso,     from = 0, to = 1, bw = 0.01), col = colors[4], lwd = 2)
#lines(density(scores_ridge,     from = 0, to = 1, bw = 0.01), col = colors[5], lwd = 2)
#lines(density(scores_elnet,     from = 0, to = 1, bw = 0.01), col = colors[6], lwd = 2)
lines(density(scores_lda,       from = 0, to = 1, bw = 0.01), col = colors[3], lwd = 2)#col = colors[7], lwd = 2)
legend("top", legend = methods, fill = colors)



# Fitted models ------------------------------
summary(logit)
#summary(steplogit)
plot(cslogitL1)
summary(cslogitL1)


