performance <- function (scores, predicted_classes, true_classes, show = FALSE)
{
  Y <- true_classes
  Yhat <- predicted_classes

  if ((length(scores) != length(Y)) | (length(scores) != length(Yhat)) | (length(Y) != length(Yhat)))
    stop("all arguments must have the same length")

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
    if (class(Y) == "try-error")
      Y <- ifelse(true_classes == 0, 0, 1)
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
    if (class(Yhat) == "try-error")
      Yhat <- ifelse(predicted_classes == 0, 0, 1)
  }

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

  metrics <- try(HMeasure(true.class = Y, scores = scores)$metrics[c("MER", "AUC")], silent = TRUE)
  if (class(metrics) == "try-error") {
    metrics <- cbind.data.frame(alerts, alert_rate, TP, FP, TN, FN, TPR, FPR, ER,
                                MER = NA, Precision, Recall, F1, AUC = NA)
  } else {
    metrics <- cbind.data.frame(alerts, alert_rate, TP, FP, TN, FN, TPR, FPR, ER,
                                MER = metrics$MER, Precision, Recall, F1, AUC = metrics$AUC)
  }

  CM <- matrix(c(TN, FP, FN, TP), ncol = 2, nrow = 2)
  colnames(CM) <- c("0", "1")
  rownames(CM) <- c("0", "1")
  names(dimnames(CM)) <- c("Prediction", "Reference")
  CM <- as.table(CM)

  # Note: scores.class0 and scores.class1 must be switched!
  pr_curve <- try(PRROC::pr.curve(scores.class0 = scores[which(Y == 1)],
                                  scores.class1 = scores[which(Y == 0)],
                                  curve = TRUE), silent = TRUE)
  if (class(pr_curve) == "try-error")
    AUC_pr <- NA
  else
    AUC_pr <- pr_curve$auc.integral
  metrics <- cbind.data.frame(metrics, AUC_pr)

  if (show) {
    cat("\nConfusion matrix:\n")
    print(CM)
    cat("\nPerformance:\n")
    print(metrics)

    if (!is.na(AUC_pr))
      plot(pr_curve)

    density_scores0 <- list()
    density_scores1 <- list()
    density_scores0$y <- 0
    density_scores1$y <- 0
    if (allYare1 == FALSE)
      density_scores0 <- density(scores[which(Y == 0)], from = 0, to = 1)
    if (allYare0 == FALSE)
      density_scores1 <- density(scores[which(Y == 1)], from = 0, to = 1)
    ylimit <- c(0, max(c(density_scores0$y, density_scores1$y)))
    if (allYare0 == TRUE) {
      plot(density_scores0, col = "dodgerblue", lwd = 2, ylim = ylimit,
           xlab = "Fraud propensity", main = "Density of predicted fraud propensities")
    } else if (allYare1 == TRUE) {
      plot(density_scores1, col = "red", lwd = 2, ylim = ylimit,
           xlab = "Fraud propensity", main = "Density of predicted fraud propensities")
    } else {
      plot(density_scores0, col = "dodgerblue", lwd = 2, ylim = ylimit,
           xlab = "Fraud propensity", main = "Density of predicted fraud propensities")
      lines(density_scores1, col = "red", lwd = 2)
    }
    legend("top", c("fraud", "no-fraud"), fill = c("red", "dodgerblue"))
  }

  return(list(pr_curve = pr_curve, confusionmatrix = CM, metrics = metrics))
}
