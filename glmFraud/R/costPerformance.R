costPerformance <- function (scores, predicted_classes, true_classes, amounts, fixed_cost)
{
  Y <- true_classes
  Yhat <- predicted_classes

  nscores <- length(scores)
  nY <- length(Y)
  nYhat <- length(Yhat)
  namounts <- length(amounts)

  if ((nscores != nY) | (nscores != nYhat) | (nscores!= namounts) |
      (nY != nYhat) | (nY != namounts) | (nYhat != namounts))
    stop("all arguments must have the same length")
  if (fixed_cost < 0)
    stop("argument 'fixed_cost' must be a non-negative number")

  if (length(unique(Y)) == 1) {
    if (all(Y == 0)) {
      Y <- rep(0, length(Y))
      warning("only class 0 is present in argument 'true_classes'")
    } else if (all(Y == 1)) {
      Y <- rep(1, length(Y))
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

  total_fraud_amount <- sum(Y * amounts)
  detected_fraud_amount <- sum(Y * Yhat * amounts)
  detected_fraud_amount_ratio <- detected_fraud_amount / total_fraud_amount

  cost_no_model <- total_fraud_amount
  minimum_cost <- sum(Y * fixed_cost)
  maximum_savings <- (cost_no_model - minimum_cost) / cost_no_model

  cost <- sum(Y * (1 - Yhat) * amounts + Yhat * fixed_cost)
  average_cost <- cost / length(Y)

  expected_cost <- sum(Y * (1 - scores) * amounts + scores * fixed_cost)
  average_expected_cost <- expected_cost / length(Y)

  savings <- (cost_no_model - cost) / cost_no_model
  expected_savings <- (cost_no_model - expected_cost) / cost_no_model

  metrics <- cbind.data.frame(total_fraud_amount,
                              detected_fraud_amount,
                              detected_fraud_amount_ratio,
                              cost_no_model,
                              minimum_cost,
                              maximum_savings,
                              cost,
                              average_cost,
                              expected_cost,
                              average_expected_cost,
                              savings,
                              expected_savings)
  return(metrics)
}
