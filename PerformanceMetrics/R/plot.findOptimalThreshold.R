plot.findOptimalThreshold <- function (x, ...) {
  # check inputs
  if (missing(x)) {
    stop("Argument 'x' is missing, with no default.")
  }
  if (class(x) != "findOptimalThreshold") {
    stop("Argument 'x' must be of class 'findOptimalThreshold'.")
  }
  # plot computed metric values vs thresholds and show optimal threshold
  plot(x$thresholds, x$measures, ylab = x$metric, xlab = "Threshold", type = "l", lwd = 2, las = 1)
  grid(col = "gray90")
  lines(x$thresholds, x$measures, lwd = 2)
  points(x$optimal_threshold, x$optimal_measure, pch = 18, cex = 2, col = "red")
}
