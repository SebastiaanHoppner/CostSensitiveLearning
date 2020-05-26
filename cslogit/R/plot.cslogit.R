plot.cslogit <- function (x, which = "all", show_legend = FALSE, legend_position = NULL, ...) {
  # check inputs
  if (missing(x)) {
    stop("argument 'x' is missing, with no default")
  }
  if (class(x) != "cslogit") {
    stop("argument 'x' must be of class 'cslogit'")
  }
  if (!which %in% c("all", "objective", "coefficients")) {
    stop("'which' should be one of \"all\", \"objective\", \"coefficients\"")
  }
  if (class(show_legend) != "logical") {
    stop("'show_legend' must be either TRUE or FALSE")
  }
  if (!is.null(legend_position)) {
    if (!legend_position %in% c("bottomright", "bottom", "bottomleft", "left", "topleft",
                                "top", "topright", "right", "center")) {
      stop(paste("'legend_position' should be one of \"bottomright\", \"bottom\", \"bottomleft\",",
                 "\"left\", \"topleft\", \"top\", \"topright\", \"right\", \"center\""))
    }
  }

  n <- length(x$objective_path)
  opt_iteration_path <- 1
  opt_objective_path <- x$objective_path[1]
  opt_objective <- x$objective_path[1]
  for (i in 2:n) {
    if (x$objective_path[i] < opt_objective) {
      opt_iteration_path <- c(opt_iteration_path, i)
      opt_objective_path <- c(opt_objective_path, x$objective_path[i])
      opt_objective <- x$objective_path[i]
    }
  }

  op <- par(ask = (which == "all"))
  # plot objective versus iteration
  if (which %in% c("all", "objective")) {
    plot((1:n)[-opt_iteration_path], x$objective_path[-opt_iteration_path],
         col = "grey75", xlim = c(0, n), ylim = range(opt_objective_path),
         xlab = "iteration", ylab = "objective")
    lines(opt_iteration_path, opt_objective_path, col = "red", lwd = 3)
  }

  # plot estimated regression parameters versus iteration
  if (which %in% c("all", "coefficients")) {
    colors <- rainbow(NCOL(x$betas_path))
    ylimit <- max(abs(x$betas_path[opt_iteration_path,]))
    ylimit <- c(-ylimit, ylimit)

    for (j in 1:NCOL(x$betas_path)) {
      if (j == 1) {
        plot((1:n)[opt_iteration_path], x$betas_path[opt_iteration_path, 1],
             xlim = c(0, n), ylim = ylimit, type = "l", lty = 5, lwd = 2, col = colors[1],
             xlab = "iteration", ylab = "coefficients")
        abline(h = 0, lwd = 2, lty = 2, col = "black")
        abline(h = min(x$options$lb[!is.infinite(x$options$lb)]), lwd = 2, lty = 2, col = "black")
        abline(h = max(x$options$ub[!is.infinite(x$options$ub)]), lwd = 2, lty = 2, col = "black")
      } else {
        lines((1:n)[opt_iteration_path], x$betas_path[opt_iteration_path, j],
              lwd = 2, col = colors[j])
      }
    }
    if (show_legend) {
      if (is.null(legend_position)) {
        legend_position <- ifelse(which.max(abs(ylimit)) == 1, "bottomleft", "topleft")
      }
      legend(legend_position, colnames(x$betas_path), lwd = 2, col = colors,
             lty = c(2, rep(1, NCOL(x$betas_path) - 1)))
    }
    text(x = n, y = x$coefficients[1], label = "(Intercept)", pos = 3)
  }
  par(op)
}
