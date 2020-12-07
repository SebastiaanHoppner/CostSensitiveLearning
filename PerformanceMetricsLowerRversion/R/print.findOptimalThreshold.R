print.findOptimalThreshold <- function (x, ...) {
  if (missing(x)) {
    stop("Argument 'x' is missing, with no default.")
  }
  if (class(x) != "findOptimalThreshold") {
    stop("Argument 'x' must be of class 'findOptimalThreshold'.")
  }

  # optim_indicator <- rep("", length(x$thresholds))
  # optim_indicator[which(x$thresholds == x$optimal_threshold)] <- "<---"
  # results <- cbind.data.frame(x$thresholds, x$measures, optim_indicator)
  # names(results) <- c("threshold", x$metric, "")
  # print(results)
  cat(paste0("Optimal threshold: ", round(x$optimal_threshold, 6)))
  cat(paste0("\nOptimal ", x$metric, ": ", round(x$optimal_measure, 6)))
  invisible(x)
}
