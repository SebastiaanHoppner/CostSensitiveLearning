costPerformance <- function (scores, predicted_classes, true_classes, cost_matrix) {
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
  if (missing(cost_matrix)) {
    stop("argument 'cost_matrix' is missing, with no default")
  }
  if (!is.matrix(cost_matrix) | any(dim(cost_matrix) != c(length(scores), 2))) {
    stop("argument 'cost_matrix' must be a matrix of dimension n x 2")
  }

  # process inputs
  Y <- true_classes
  Yhat <- predicted_classes

  nscores <- length(scores)
  nY <- length(Y)
  nYhat <- length(Yhat)

  if ((nscores != nY) | (nscores != nYhat) | (nY != nYhat)) {
    stop("all arguments must have the same length")
  }

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

  # rearrange cost matrix
  cost_matrix[Y == 0, ] <- cost_matrix[Y == 0, c(2, 1)]

  # cost-related metrics
  total_fraud_amount <- sum(cost_matrix[, 2])
  detected_fraud_amount <- sum(cost_matrix[, 2] * Yhat)
  detected_fraud_amount_ratio <- detected_fraud_amount / total_fraud_amount

  cost_no_model <- total_fraud_amount
  minimum_cost <- sum(cost_matrix[, 1] * Y)
  maximum_savings <- (cost_no_model - minimum_cost) / cost_no_model

  cost <- sum(cost_matrix * matrix(c(Yhat, 1 - Yhat), ncol = 2))
  average_cost <- cost / length(Y)

  expected_cost <- sum(cost_matrix * matrix(c(scores, 1 - scores), ncol = 2))
  average_expected_cost <- expected_cost / length(Y)

  savings <- (cost_no_model - cost) / cost_no_model
  expected_savings <- (cost_no_model - expected_cost) / cost_no_model

  # output
  cost_metrics <- cbind.data.frame(total_fraud_amount,
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
  return(cost_metrics)
}
