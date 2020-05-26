plot.csboost <- function (x, legend_position = NULL, ...) {
  # check inputs
  if (missing(x)) {
    stop("argument 'x' is missing, with no default")
  }
  if (class(x) != "csboost") {
    stop("argument 'x' must be of class 'csboost'")
  }
  if (!is.null(legend_position)) {
    if (!legend_position %in% c("bottomright", "bottom", "bottomleft", "left", "topleft",
                                "top", "topright", "right", "center")) {
      stop(paste("'legend_position' should be one of \"bottomright\", \"bottom\", \"bottomleft\",",
                 "\"left\", \"topleft\", \"top\", \"topright\", \"right\", \"center\""))
    }
  }

  # plot average expected cost versus iteration
  evallog <- x$xgbmodel$evaluation_log
  ylimit <- range(evallog[, 2:NCOL(evallog)])

  plot(evallog$iter, unlist(evallog[, 2]), type = "l", ylim = ylimit,
       ylab = "average expected cost", xlab = "iteration", ...)

  if (NCOL(evallog) == 3) {
    lines(evallog$iter, unlist(evallog[, 3]), lty = 2, ...)
    if (is.null(legend_position)) {
      legend_position <- "top"
    }
    abline(v = x$xgbmodel$best_iteration, lty = 4, ...)
    legend(legend_position, legend = c("train", "test"), lty = c(1, 2), lwd = 2)
  }
}
