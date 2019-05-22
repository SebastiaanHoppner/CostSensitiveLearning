performance <- function (scores, predicted_classes, true_classes)
{
  # Compute classification performance measures
  # -----------------------------------------------------------------------------------------------
  # Arguments:
  #   scores              a vector of predicted probabilities
  #   predicted_classes   a vector of predicted labels; can be either a factor, or in numeric form
  #   true_classes        a vector of true labels; can be either a factor, or in numeric form
  # -----------------------------------------------------------------------------------------------
  # Value:
  #   performance returns a list containing the following:
  #   pr_curve          precision-recall curve; can be NULL if pr.curve gives an error
  #   confusionmatrix   confusion matrix based on predicted labels and true labels
  #   metrics           a data frame containing the following classification metrics:
  #
  #                     alerts       number of instances predicted as positive
  #                     alert_rate   fraction of instances predicted as positive
  #                     TP           number of true positives
  #                     FP           number of false positives
  #                     TN           number of true negatives
  #                     FN           number of false negatives
  #                     TPR          true positive rate
  #                     FPR          false positive rate
  #                     ER           error rate
  #                     MER          minimum error rate; can be NA if HMeasure gives an error
  #                     Precision    precision
  #                     Recall       recall
  #                     F1           F1-scores
  #                     AUC          Area under ROC curve; can be NA if HMeasure gives an error
  #                     AUC_pr       Area under precision-recall curve; can be NA if pr.curve
  #                                  gives an error
  # -----------------------------------------------------------------------------------------------
  # Written by Sebastiaan Höppner, 2019
  # -----------------------------------------------------------------------------------------------

  # check inputs
  if (missing(scores)) {
    stop("argument 'scores' is missing, with no default")
  }
  if (missing(predicted_classes)) {
    stop("argument 'predicted_classes' is missing, with no default")
  }
  if (missing(true_classes)) {
    stop("argument 'true_classes' is missing, with no default")
  }

  # process inputs
  Y <- true_classes
  Yhat <- predicted_classes

  if ((length(scores) != length(Y)) | (length(scores) != length(Yhat)) |
      (length(Y) != length(Yhat))) {
    stop("all arguments must have the same length")
  }

  allYare0 <- FALSE
  allYare1 <- FALSE
  if (length(unique(Y)) == 1) {
    if (all(Y == 0)) {
      Y <- rep(0, length(Y))
      allYare0 <- TRUE
      warning("only class 0 is present in argument 'true_classes'")
    } else if (all(Y == 1)) {
      Y <- rep(1, length(Y))
      allYare1 <- TRUE
      warning("only class 1 is present in argument 'true_classes'")
    } else {
      stop("only one unknown class is present in argument 'true_classes'")
    }
  } else {
    Y <- try(hmeasure::relabel(true_classes), silent = TRUE)
    if (class(Y) == "try-error") {
      Y <- ifelse(true_classes == 0, 0, 1)
    }
  }
  if (length(unique(Yhat)) == 1) {
    if (all(Yhat == 0)) {
      Yhat <- rep(0, length(Yhat))
      warning("only class 0 is present in argument 'predicted_classes'")
    } else if (all(Yhat == 1)) {
      Yhat <- rep(1, length(Yhat))
      warning("only class 1 is present in argument 'predicted_classes'")
    } else {
      stop("only one unknown class is present in argument 'predicted_classes'")
    }
  } else {
    Yhat <- try(hmeasure::relabel(predicted_classes), silent = TRUE)
    if (class(Yhat) == "try-error") {
      Yhat <- ifelse(predicted_classes == 0, 0, 1)
    }
  }

  # metrics
  alerts <- sum(Yhat)
  alert_rate <- mean(Yhat)
  TP <- sum(Y == 1 & Yhat == 1)
  FP <- sum(Y == 0 & Yhat == 1)
  TN <- sum(Y == 0 & Yhat == 0)
  FN <- sum(Y == 1 & Yhat == 0)
  TPR <- TP / (TP + FN)
  FPR <- FP / (FP + TN)
  ER <- (FP + FN) / (TP + FP + TN + FN)
  Precision <- TP / (TP + FP)
  Recall <- TPR
  F1 <- 2 * (Precision * Recall) / (Precision + Recall)

  # Minimum Error Rate and AUC
  MER_AUC <- try(hmeasure::HMeasure(true.class = Y, scores = scores)$metrics[c("MER", "AUC")],
                 silent = TRUE)
  metrics <- cbind.data.frame(alerts, alert_rate, TP, FP, TN, FN, TPR, FPR, ER,
                              MER = NA, Precision, Recall, F1, AUC = NA)
  if (class(MER_AUC) != "try-error") {
    metrics$MER <- MER_AUC$MER
    metrics$AUC <- MER_AUC$AUC
  }

  # confusion matrix
  CM <- matrix(c(TN, FP, FN, TP), ncol = 2, nrow = 2)
  colnames(CM) <- c("0", "1")
  rownames(CM) <- c("0", "1")
  names(dimnames(CM)) <- c("Prediction", "Reference")
  CM <- as.table(CM)

  # Precision-Recall curve - Note: scores.class0 and scores.class1 must be switched!
  pr_curve <- try(PRROC::pr.curve(scores.class0 = scores[which(Y == 1)],
                                  scores.class1 = scores[which(Y == 0)],
                                  curve = TRUE), silent = TRUE)
  AUC_pr <- NA
  if (class(pr_curve) != "try-error") {
    AUC_pr <- pr_curve$auc.integral
  }
  metrics <- cbind.data.frame(metrics, AUC_pr)

  return(list(pr_curve = pr_curve, confusionmatrix = CM, metrics = metrics))
}
