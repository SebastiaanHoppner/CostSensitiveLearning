optimal_threshold <- function (scores, true_classes, cost_matrix, thresholds)
{
  # original call
  call <- match.call()

  # check inputs
  if (missing(scores)) {
    stop("argument 'scores' is missing, with no default")
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
  if (missing(thresholds)) {
    stop("argument 'thresholds' is missing, with no default")
  }

  # start looping over threshold sequence
  savings <- c()
  for (i in 1:length(thresholds)) {
    predicted_classes <- ifelse(scores > thresholds[i], 1, 0)
    savings[i] <- costPerformance(scores, predicted_classes, true_classes, cost_matrix)$savings
  }
  optimal_threshold = thresholds[which.max(savings)]

  return(list(call = call, thresholds = thresholds, savings = savings,
              optimal_threshold = optimal_threshold))
}
