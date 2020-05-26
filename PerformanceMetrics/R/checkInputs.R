checkInputs <- function (scores, predicted_classes, true_classes, cost_matrix, plot,
                         thresholds, metric) {
  if (missing(scores)) {
    stop("Argument 'scores' is missing, with no default.", call. = FALSE)
  }
  if (missing(true_classes)) {
    stop("Argument 'true_classes' is missing, with no default.", call. = FALSE)
  }
  if (missing(predicted_classes)) {
    stop("Argument 'predicted_classes' is missing, with no default.", call. = FALSE)
  }
  if (is.matrix(scores) | is.data.frame(scores)) {
    stop("Argument 'scores' should be a vector, not a matrix or data frame.", call. = FALSE)
  }
  if (is.matrix(true_classes) | is.data.frame(true_classes)) {
    stop("Argument 'true_classes' should be a vector, not a matrix or data frame.", call. = FALSE)
  }
  if (is.matrix(predicted_classes) | is.data.frame(predicted_classes)) {
    stop("Argument 'predicted_classes' should be a vector, not a matrix or data frame.", call. = FALSE)
  }
  if (any(is.na(scores))) {
    stop("Missing values in 'scores' are not allowed.", call. = FALSE)
  }
  if (any(is.na(true_classes))) {
    stop("Missing values in 'true_classes' are not allowed.", call. = FALSE)
  }
  if (any(is.na(predicted_classes))) {
    stop("Missing values in 'predicted_classes' are not allowed.", call. = FALSE)
  }
  if (any(scores < 0) | any(scores > 1)) {
    stop("All 'scores' must be inside the unit interval.", call. = FALSE)
  }
  if (length(unique(true_classes)) == 1) {
    stop(paste("Only one class is present in 'true_classes',",
               "but needs both classes to be represented."), call. = FALSE)
  }
  if (length(unique(true_classes)) > 2) {
    stop(paste("More than two classes present in 'true_classes',",
               "but code can only handle binary classification."), call. = FALSE)
  }
  if (length(unique(predicted_classes)) > 2) {
    stop(paste("More than two classes present in 'predicted_classes',",
               "but code can only handle binary classification."), call. = FALSE)
  }
  if (!is.null(predicted_classes)) {
    if (!all(levels(factor(predicted_classes)) %in% levels(factor(true_classes)))) {
      stop(paste("Argument 'predicted_classes' contains unknown classes that are not present",
                 "in 'true_classes'."), call. = FALSE)
    }
  }
  if (!is.null(predicted_classes)) {
    if ((length(scores) != length(true_classes)) | (length(scores) != length(predicted_classes)) |
        (length(true_classes) != length(predicted_classes))) {
      stop("Arguments 'scores', 'predicted_classes' and 'true_classes' must have the same length.",
           call. = FALSE)
    }
  } else {
    if (length(scores) != length(true_classes)) {
      stop("Arguments 'scores' and 'true_classes' must have the same length.", call. = FALSE)
    }
  }
  if (!is.null(cost_matrix)) {
    if (!is.matrix(cost_matrix) | any(dim(cost_matrix) != c(length(scores), 2))) {
      stop("Argument 'cost_matrix' must be a matrix of dimension length(scores) x 2.",
           call. = FALSE)
    }
    if (any(is.na(cost_matrix))) {
      stop("Missing values in 'cost_matrix' are not allowed.", call. = FALSE)
    }
  }
  if (!is.logical(plot)) {
    stop("Argument 'plot' must be TRUE or FALSE.", call. = FALSE)
  }
  if (missing(thresholds)) {
    stop("Argument 'thresholds' is missing, with no default.", call. = FALSE)
  }
  if (missing(metric)) {
    stop("Argument 'metric' is missing, with no default.", call. = FALSE)
  }
  metrics <- c("F1", "ER", "Accuracy", "Youden", "TPR", "FPR", "HitRate", "Precision", "Recall",
               "Sensitivity", "Specificity", "TP", "FP", "TN", "FN", "Alerts", "AlertRate")
  cost_metrics <- c("Savings", "Cost", "AverageCost")
  if (!is.null(metric)) {
    if (!metric %in% c(metrics, cost_metrics)) {
      stop("Argument 'metric' is not specified correctly.\n",
           "Possible options are:\n - ", paste(metrics, collapse = "\n - "), "\n",
           "If argument 'cost_matrix' is provided, then other options are:\n - ",
           paste(cost_metrics, collapse = "\n - "), "\n")
    }
    if (metric %in% cost_metrics & is.null(cost_matrix)) {
      stop("Argument 'cost_matrix' is not provided while it's needed to compute '", metric, "'.")
    }
  }
  if (!is.null(thresholds)) {
    if (any(is.na(scores))) {
      stop("Missing values in 'thresholds' are not allowed.", call. = FALSE)
    }
    if (length(thresholds) == 1) {
      if (thresholds[1] < 1) {
        stop("Argument 'thresholds' must either be a positive integer, ",
             "a sequence of values inside the unit interval or NULL.")
      }
    } else if (any(thresholds < 0) | any(thresholds > 1)) {
      stop("All 'thresholds' must be inside the unit interval.", call. = FALSE)
    }
  }
}
