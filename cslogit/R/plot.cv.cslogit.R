plot.cv.cslogit <- function (x)
{
  # Plot results from cross-validation with cslogit
  # -----------------------------------------------------------------------------------------------
  # Arguments:
  #   x   an object of class "cv.cslogit"
  # -----------------------------------------------------------------------------------------------
  # Value:
  #   plot.cv.cslogit creates boxplots of the cross-validation results for a lambda sequence
  # -----------------------------------------------------------------------------------------------
  # Written by Sebastiaan Höppner, 2019
  # -----------------------------------------------------------------------------------------------

  # check inputs
  if (missing(x)) {
    stop("argument 'x' is missing, with no default")
  }
  if (class(x) != "cv.cslogit") {
    stop("argument 'x' must be of class 'cv.cslogit'")
  }

  # boxplot of cross-validation results
  nlambda <- length(x$lambda_path)
  optimal_index <- which(x$lambda_path == x$optimal_lambda)
  box_colors <- rep("grey90", nlambda)
  box_colors[optimal_index] <- "grey65"
  pchs <- rep(21, nlambda)
  pchs[optimal_index] <- 23
  point_colors <- rep("red", nlambda)
  point_colors[optimal_index] <- "orange"

  boxplot(x$cv_results, xlab = "log(lambda)", ylab = "average expected cost",
          names = format(round(log(x$lambda_path), 2), nsmall = 2),
          col = box_colors, las = 2, boxwex = 0.5)
  lines(1:nlambda, colMeans(x$cv_results), col = "red", lwd = 2)
  points(1:nlambda, colMeans(x$cv_results),
         pch = pchs, col = "darkred", bg = point_colors, cex = 1.5)
  legend("topleft", legend = paste0("optimal lambda = ", round(x$optimal_lambda, 6)),
         text.font = 2, bg = "grey90")
}
