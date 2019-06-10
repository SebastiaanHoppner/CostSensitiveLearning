summary.cslogit <- function (object, ...) {
  # check inputs
  if (missing(object)) {
    stop("argument 'object' is missing, with no default")
  }
  if (class(object) != "cslogit") {
    stop("argument 'object' must be of class 'cslogit'")
  }

  # print summary
  predictor_variables <- attr(object$terms, "dataClasses")[-1]
  factor_variables  <- names(which(predictor_variables == "factor"))
  numeric_variables <- names(which(predictor_variables != "factor"))
  n_factors  <- length(factor_variables)
  n_numerics <- length(numeric_variables)
  df_coefficients <- data.frame(coefficients = round(object$coefficients, 5))

  cat("SETTINGS -------------------------------------------------------------------------------\n")
  cat(paste("  - lambda =", object$lambda, "\n"))
  cat(paste("  - cost matrix (example) = \n"))
  print(object$example_cost_matrix)
  cat(paste("  - factor variables:\n      "))
  cat(paste(factor_variables), sep = "\n      ")
  cat(paste("  - numeric variables:\n      "))
  cat(paste(numeric_variables), sep = "\n      ")
  cat("\n")
  cat("OPTIONS --------------------------------------------------------------------------------\n")
  cat(paste("  - algorithm =",   object$options$algorithm,   "\n"))
  cat(paste("  - maxeval =",     object$options$maxeval,     "\n"))
  cat(paste("  - ftol_rel =",    object$options$ftol_rel,    "\n"))
  cat(paste("  - xtol_rel =",    object$options$xtol_rel,    "\n"))
  cat(paste("  - print_level =", object$options$print_level, "\n"))
  cat(paste("  - check_data =",  object$options$check_data,  "\n"))
  cat(paste("  - lower bounds = "))
  cat(object$options$lb)
  cat(paste("\n  - upper bounds = "))
  cat(object$options$ub)
  cat("\n\n")
  cat("RESULTS --------------------------------------------------------------------------------\n")
  cat(paste0("  - message = ",  object$message, " (status ", object$status, ")\n"))
  cat(paste("  - iterations =", object$iterations, "\n"))
  cat(paste("  - time =",       object$time, "seconds\n"))
  cat(paste("  - objective =",  round(object$objective, 8), "\n"))
  cat(paste("  - average expected cost =",  round(object$average_expected_cost, 8), "\n"))
  cat(paste("  - coefficients = \n"))
  print(df_coefficients)
  cat("\n")
}
