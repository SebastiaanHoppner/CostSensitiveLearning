findOptimalThreshold <- function (scores, true_classes, thresholds = NULL, metric,
                                  cost_matrix = NULL, plot = TRUE) {
  # check inputs
  checkInputs(scores, predicted_classes = NULL, true_classes, cost_matrix, plot,
              thresholds, metric)

  if (is.null(thresholds)) {
    # take all unique scores as thresholds
    thresholds <- sort(unique(scores))
  } else if (length(thresholds) == 1 & thresholds[1] > 1) {
    # select some scores as thresholds
    nThresholds <- as.integer(thresholds)
    unique_scores_sorted <- sort(unique(scores))
    indices <- as.integer(seq(from = 1, to = length(unique_scores_sorted),
                              length.out = nThresholds + 2))
    thresholds <- unique_scores_sorted[indices[2:(nThresholds + 1)]]
  } else {
    # sort all specified thresholds
    thresholds <- sort(thresholds)
  }

  # names can confuse and are otherwise useless - remove them
  names(scores) <- NULL
  names(true_classes) <- NULL
  names(cost_matrix) <- NULL
  names(thresholds) <- NULL

  # relabel classes to numeric version
  y <- relabel(factor(as.character(true_classes)), "true_classes")

  # rearrange cost matrix
  if (metric %in% c("Savings", "Cost", "AverageCost")) {
    cost_matrix[y == 0, ] <- cost_matrix[y == 0, c(2, 1)]
  }
  if (metric == "Savings") {
    CostNoModel <- min(sum(cost_matrix[, 1]), sum(cost_matrix[, 2]))
  }

  # start looping over threshold sequence
  measures <- rep(NA, length(thresholds))

  for (i in 1:length(thresholds)) {

    yhat <- ifelse(scores > thresholds[i], 1, 0)

    # confusion matrix related metrics
    if (metric == "Alerts") {
      measures[i] <- sum(yhat)

    } else if (metric == "AlertRate") {
      measures[i] <- mean(yhat)

    } else if (metric == "TP") {
      measures[i] <- sum(y == 1 & yhat == 1)

    } else if (metric == "FP") {
      measures[i] <- sum(y == 0 & yhat == 1)

    } else if (metric == "TN") {
      measures[i] <- sum(y == 0 & yhat == 0)

    } else if (metric == "FN") {
      measures[i] <- sum(y == 1 & yhat == 0)

    } else if (metric == "Precision") {
      measures[i] <- TP / (TP + FP)

    } else if (metric %in% c("TPR", "HitRate", "Sensitivity")) {
      TP <- sum(y == 1 & yhat == 1)
      FN <- sum(y == 1 & yhat == 0)
      measures[i] <- TP / (TP + FN)

    } else if (metric %in% c("FPR", "Specificty")) {
      FP <- sum(y == 0 & yhat == 1)
      TN <- sum(y == 0 & yhat == 0)
      if (metric == "FPR") {
        measures[i] <- FP / (FP + TN)
      } else if (metric == "Specificity") {
        measures[i] <- TN / (FP + TN)
      }

    } else if (metric %in% c("ER", "Accuracy", "Youden", "F1")) {
      TP <- sum(y == 1 & yhat == 1)
      FN <- sum(y == 1 & yhat == 0)
      FP <- sum(y == 0 & yhat == 1)
      TN <- sum(y == 0 & yhat == 0)
      if (metric == "ER") {
        measures[i] <- (FP + FN) / (TP + FP + TN + FN)
      } else if (metric == "Accuracy") {
        measures[i] <- (TP + TN) / (TP + FP + TN + FN)
      } else if (metric == "Youden") {
        measures[i] <- TP / (TP + FN) + TN / (FP + TN) - 1
      } else if (metric == "F1") {
        Precision <- TP / (TP + FP)
        Recall <- TP / (TP + FN)
        measures[i] <- 2 * (Precision * Recall) / (Precision + Recall)
      }

      # cost-related metrics
    } else if (metric == "Cost") {
      measures[i] <- sum(cost_matrix * matrix(c(yhat, 1 - yhat), ncol = 2))

    } else if (metric == "AverageCost") {
      measures[i] <- mean(cost_matrix * matrix(c(yhat, 1 - yhat), ncol = 2))

    } else if (metric == "Savings") {
      Cost <- sum(cost_matrix * matrix(c(yhat, 1 - yhat), ncol = 2))
      measures[i] <- (CostNoModel - Cost) / CostNoModel

    }
  } # end for-loop over threshold sequence

  # find optimal threshold value and corresponding metric value
  if (metric %in% c("Alerts", "AlertRate", "FP", "FN", "FPR", "ER", "Cost", "AverageCost")) {
    i_optimal <- which.min(measures)
  } else {
    i_optimal <- which.max(measures)
  }
  optimal_threshold <- thresholds[i_optimal]
  optimal_measure <- measures[i_optimal]

  if (plot) {
    # plot computed metric values vs thresholds and show optimal threshold
    plot(thresholds, measures, ylab = metric, xlab = "Threshold", type = "l", lwd = 2, las = 1)
    grid(col = "gray90")
    lines(thresholds, measures, lwd = 2)
    points(optimal_threshold, optimal_measure, pch = 18, cex = 2, col = "red")
  }

  # output
  output <- list(metric            = metric,
                 thresholds        = thresholds,
                 measures          = measures,
                 optimal_measure   = optimal_measure,
                 optimal_threshold = optimal_threshold)
  class(output) <- "findOptimalThreshold"
  return(output)
}
