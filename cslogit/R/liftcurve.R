liftcurve <- function (scores, true_classes, cost_matrix = NULL, show = TRUE, which = "all") {
  # check inputs
  if (missing(scores)) {
    stop("argument 'scores' is missing, with no default")
  }
  if (missing(true_classes)) {
    stop("argument 'true_classes' is missing, with no default")
  }
  if (!is.null(cost_matrix)) {
    if (!is.matrix(cost_matrix) | any(dim(cost_matrix) != c(length(scores), 2))) {
      stop("argument 'cost_matrix' must be a matrix of dimension n x 2")
    }
  }
  if (!which %in% c("all", "gains", "lift", "costs")) {
    stop("'which' should be one of \"all\", \"gains\", \"lift\", \"costs\"")
  }
  if (which == "costs" & is.null(cost_matrix)) {
    stop("the costs curve cannot be plotted without a cost matrix")
  }

  # process inputs
  Y <- true_classes

  if (length(scores) != length(Y)) {
    stop("all arguments must have the same length")
  }

  if (length(unique(Y)) == 1) {
    if (all(Y == 0)) {
      Y <- rep(0, length(Y))
      warning("only class 0 is present in argument 'true_classes'")
    } else if (all(Y == 1)) {
      Y <- rep(1, length(Y))
      warning("only class 1 is present in argument 'true_classes'")
    } else {
      stop("only one unknown class is present in argument 'true_classes'")
    }
  } else {
    Y <- try(hmeasure::relabel(true_classes), silent = TRUE)
    if (class(Y) == "try-error") {
      Y <- ifelse(true_classes == 0, 0, 1)
    }
  }

  # rearrange cost matrix
  cost_matrix[Y == 0, ] <- cost_matrix[Y == 0, c(2, 1)]

  n <- length(scores)
  n1 <- sum(Y)
  n0 <- n - n1
  fraction <- n1 / n
  fraction_of_population <- 100 * (1:n) / n

  order_scores <- order(scores, decreasing = TRUE)
  cumulative_true_classes <- cumsum(Y[order_scores])
  cost_matrix <- cost_matrix[order_scores, ]

  gains <- cumulative_true_classes / n
  lift <- (cumulative_true_classes / n1) / ((1:n) / n)

  avoided_costs <- cumsum(cost_matrix[, 2] - cost_matrix[, 1])
  optimal_population_fraction <- fraction_of_population[which.max(avoided_costs)] / 100

  # Area under Gains curve
  auc_gains_model   <- DescTools::AUC(x = fraction_of_population, y = 100 * gains)
  auc_gains_perfect <- DescTools::AUC(x = fraction_of_population,
                                      y = 100 * cumsum(c(rep(1, n1), rep(0, n0))) / n)
  auc_gains <- auc_gains_model / auc_gains_perfect

  # Area under Lift curve
  auc_lift_model   <- DescTools::AUC(x = fraction_of_population, y = lift)
  auc_lift_perfect <- DescTools::AUC(x = fraction_of_population,
                                     y = (cumsum(c(rep(1, n1), rep(0, n0))) / n1) / ((1:n) / n))
  auc_lift <- auc_lift_model / auc_lift_perfect

  # Area under Costs curve
  if (!is.null(cost_matrix)) {
    auc_costs <- DescTools::AUC(x = fraction_of_population/100, y = avoided_costs)
  }

  op <- par(ask = (which == "all"))
  # Gains curve
  if (which %in% c("all", "gains") & show == TRUE) {
    plot(fraction_of_population, 100 * gains, lwd = 2, type = "l",
         xlab = "% of population", ylab = "% positive instances",
         main = paste0("Gains curve (AUC = ", round(100 * auc_gains, 2), "%)"))
    polygon(x = c(fraction_of_population, 100), y = c(100 * gains, 100 * gains[1]),
            col = "grey", border = NA)
    lines(fraction_of_population, 100 * gains, lwd = 2)
    lines(fraction_of_population, 100 * fraction * (1:n) / n, lty = 2)
    lines(fraction_of_population, 100 * cumsum(c(rep(1, n1), rep(0, n0))) / n, lty = 3)
    legend("bottomright", legend = c("model", "perfect", "random"), lty = c(1, 3, 2),
           lwd = c(2, 1, 1))
  }

  # Lift curve
  if (which %in% c("all", "lift") & show == TRUE) {
    plot(fraction_of_population, lift, lwd = 2, type = "l",
         ylim = range(c(lift, (cumsum(c(rep(1, n1), rep(0, n0))) / n1) / ((1:n) / n))),
         xlab = "% of population", ylab = "Lift",
         main = paste0("Lift curve (AUC = ", round(100 * auc_lift, 2), "%)"))
    polygon(x = c(0, fraction_of_population), y = c(1, lift),
            col = "grey", border = NA)
    lines(fraction_of_population, lift, lwd = 2)
    lines(fraction_of_population, rep(1, n), lty = 2, lwd = 2)
    lines(fraction_of_population, (cumsum(c(rep(1, n1), rep(0, n0))) / n1) / ((1:n) / n), lty = 3)
    legend("topright", legend = c("model", "perfect", "random"), lty = c(1, 3, 2),
           lwd = c(2, 1, 1))
  }

  # Costs curve
  if (which %in% c("all", "costs") & !is.null(cost_matrix) & show == TRUE) {
    plot(fraction_of_population, avoided_costs, lwd = 2, type = "l",
         xlab = "% of population", ylab = "Avoided costs",
         main = paste0("Costs curve (AUC = ", round(auc_costs, 2), ")"))
    polygon(x = c(fraction_of_population, 100), y = c(avoided_costs, avoided_costs[1]),
            col = "grey", border = NA)
    lines(fraction_of_population, avoided_costs, lwd = 2)
    segments(x0 = 100 * optimal_population_fraction, y0 = avoided_costs[1],
             x1 = 100 * optimal_population_fraction, y1 = max(avoided_costs), lty = 2, lwd = 2)
    legend("topleft", legend = c(paste0("best % of population: ", 100 * optimal_population_fraction,
                                        "%"),
                                 paste0("Max. avoided costs: ", max(avoided_costs))), pch = 16)
  }
  par(op)

  if (is.null(cost_matrix)) {
    return(list(auc_gains = auc_gains,
                auc_lift = auc_lift))
  } else {
    return(data.frame(auc_gains = auc_gains,
                      auc_lift = auc_lift,
                      auc_costs = auc_costs,
                      max_avoided_costs = max(avoided_costs),
                      optimal_population_fraction = optimal_population_fraction))
  }
}
