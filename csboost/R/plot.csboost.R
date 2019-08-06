plot.csboost <- function (x, ...) {
  # check inputs
  if (missing(x)) {
    stop("argument 'x' is missing, with no default")
  }
  if (class(x) != "csboost") {
    stop("argument 'x' must be of class 'csboost'")
  }

  # plot expected savings versus iteration
  evallog <- x$xgbmodel$evaluation_log
  plot(evallog$iter, unlist(evallog[, 2]), type = "l", lwd = 2,
       ylim = range(evallog[, 2:NCOL(evallog)]), ylab = "expected savings", xlab = "iteration")
  if (NCOL(evallog) == 3) {
    lines(evallog$iter, unlist(evallog[, 3]), lty = 2, lwd = 2)
    legend("right", legend = c("train", "test"), lty = c(1, 2), lwd = 2)
  }
  abline(v = x$xgbmodel$best_iteration, lty = 3, lwd = 2)
}
