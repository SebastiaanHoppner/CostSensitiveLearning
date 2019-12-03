findOptimalThreshold <- function (scores, true_classes, metric, maximize, thresholds,
                                  cost_matrix = NULL, show = TRUE) {
  # original call
  call <- match.call()

  # check inputs
  if (missing(scores)) {
    stop("argument 'scores' is missing, with no default")
  }
  if (missing(true_classes)) {
    stop("argument 'true_classes' is missing, with no default")
  }
  if (missing(metric)) {
    stop("argument 'metric' is missing, with no default")
  }
  if (missing(maximize)) {
    stop("argument 'maximize' is missing, with no default")
  }
  if (missing(thresholds)) {
    stop("argument 'thresholds' is missing, with no default")
  }
  if (!is.null(cost_matrix)) {
    if (!is.matrix(cost_matrix) | any(dim(cost_matrix) != c(length(scores), 2))) {
      stop("argument 'cost_matrix' must be a matrix of dimension n x 2")
    }
  }
  if (class(show) != "logical") {
    stop("'show' must be TRUE or FALSE")
  }

  metrics <- c("alerts", "alert_rate", "TP", "FP", "TN", "FN", "TPR", "FPR", "ER",
               "Precision", "Recall", "F1")
  cost_metrics <- c("detected_fraud_amount", "detected_fraud_amount_ratio", "cost",
                    "average_cost", "savings")

  if (!metric %in% c(metrics, cost_metrics)) {
    stop("argument 'metric' is not specified correctly.\n",
         "Possible options are:\n - ", paste(metrics, collapse = "\n - "), "\n",
         "If argument 'cost_matrix' is provided, then other options are:\n - ",
         paste(cost_metrics, collapse = "\n - "), "\n")
  }
  if (metric %in% cost_metrics & is.null(cost_matrix)) {
    stop("argument 'cost_matrix' is not provided while it's needed to compute '", metric, "'")
  }

  # start looping over threshold sequence
  measures <- c()
  for (i in 1:length(thresholds)) {
    predicted_classes <- ifelse(scores > thresholds[i], 1, 0)
    if (metric %in% metrics) {
      measures[i] <- as.numeric(performance(scores, predicted_classes,
                                            true_classes)$metrics[metric])
    } else if (metric %in% cost_metrics) {
      measures[i] <- as.numeric(costPerformance(scores, predicted_classes,
                                                true_classes, cost_matrix)[metric])
    }
  }
  if (maximize){
    optimal_threshold <- thresholds[which.max(measures)]
    optimal_measure <- max(measures)
  } else {
    optimal_threshold <- thresholds[which.min(measures)]
    optimal_measure <- min(measures)
  }

  if (show) {
    plot(thresholds, measures, ylab = metric, type = "l", lwd = 2)
    points(optimal_threshold, optimal_measure, pch = 18, cex = 2, col = "red")
  }

  return(list(call = call, thresholds = thresholds, measures = measures,
              optimal_threshold = optimal_threshold, optimal_measure = optimal_measure))
}
