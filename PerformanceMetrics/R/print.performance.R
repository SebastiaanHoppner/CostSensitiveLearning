print.performance <- function (x, ...) {
  if (missing(x)) {
    stop("Argument 'x' is missing, with no default.")
  }
  if (class(x) != "performance") {
    stop("Argument 'x' must be of class 'performance'.")
  }

  metrics <- round(t(x$metrics[1:24]), 6)
  colnames(metrics) <- "value"

  cat("\nPerformance:\n")
  print(metrics)
  if (!is.null(x$metrics$Savings)) {
    cost_metrics <- round(t(x$metrics[-(1:24)]), 6)
    colnames(cost_metrics) <- "value"
    cat("\nCost-related performance:\n")
    print(cost_metrics)
  }
  cat("\nConfusion matrix:\n")
  print(x$confusionmatrix)
  invisible(x)
}
