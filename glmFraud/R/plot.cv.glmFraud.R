plot.cv.glmFraud <- function (x, show_legend = TRUE)
{
  # check inputs
  if (missing(x))
    stop("argument 'x' is missing, with no default")
  if (class(x) != "cv.glmFraud")
    stop("argument 'x' must be of class 'cv.glmFraud'")

  # plot estimated regression parameters versus log(lambda_path)
  lambda_path    <- x$lambda_path
  coef_mat       <- x$coef_mat
  variable_names <- x$variable_names
  intercept_flag <- x$intercept_flag

  if (intercept_flag == 1)
    coef_mat <- coef_mat[, -1]
  colors <- rainbow(NCOL(coef_mat))
  ylimit <- range(coef_mat)
  legend_position <- ifelse(which.max(abs(ylimit)) == 1, "bottomright", "topright")

  for (jj in 1:NCOL(coef_mat)) {
    if (jj == 1)
      plot(log(lambda_path), coef_mat[, 1], col = colors[1], ylim = ylimit, type = "l",
           xlab = "Log lambda", ylab = "Coefficients")
    else
      lines(log(lambda_path), coef_mat[, jj], col = colors[jj])
  }
  if (show_legend)
    legend(legend_position, variable_names, col = colors, lty = 1)
}
