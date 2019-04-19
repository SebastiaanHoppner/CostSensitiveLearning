summary.glmFraud <- function (object)
{
  # check inputs
  if (missing(object))
    stop("argument 'object' is missing, with no default")
  if (class(object) != "glmFraud")
    stop("argument 'object' must be of class 'glmFraud'")

  # print summary
  predictor_variables <- attr(object$terms, "dataClasses")[-1]
  factor_variables  <- names(which(predictor_variables == "factor"))
  numeric_variables <- names(which(predictor_variables != "factor"))
  n_factors  <- length(factor_variables)
  n_numerics <- length(numeric_variables)

  cat("SETTINGS -------------------------------------------------------------------------------\n")
  cat(paste("  - fixed cost =", object$fixed_cost, "\n"))
  cat(paste("  - lambda =", object$lambda, "\n"))
  cat(paste("  - factor variables:\n"))
  for (i in 1:ceiling(n_factors / 5))
    cat(paste0("      ", paste0(factor_variables[(5*(i-1)+1):min((5*i), n_factors)], collapse = ", "),
               ifelse(i == ceiling(n_factors / 5), "\n", ",\n")))
  cat(paste("  - numeric variables:\n"))
  for (i in 1:ceiling(n_numerics / 5))
    cat(paste0("      ", paste0(numeric_variables[(5*(i-1)+1):min((5*i), n_numerics)], collapse = ", "),
               ifelse(i == ceiling(n_numerics / 5), "\n", ",\n")))
  cat("\n")
  cat("OPTIONS --------------------------------------------------------------------------------\n")
  cat(paste("  - algorithm =",   object$options$algorithm,   "\n"))
  cat(paste("  - maxeval =",     object$options$maxeval,     "\n"))
  cat(paste("  - ftol_rel =",    object$options$ftol_rel,    "\n"))
  cat(paste("  - xtol_rel =",    object$options$xtol_rel,    "\n"))
  cat(paste("  - print_level =", object$options$print_level, "\n"))
  cat(paste("  - check_data =",  object$options$check_data,  "\n"))
  cat(paste("  - std_errors =",  object$options$std_errors,  "\n"))
  cat("\n")
  cat("RESULTS --------------------------------------------------------------------------------\n")
  cat(paste0("  - message = ",   object$message, " (status = ", object$status, ")\n"))
  cat(paste("  - iterations =",  object$iterations,          "\n"))
  cat(paste("  - time =",        object$time,         "seconds\n"))
  cat(paste("  - objective =",   round(object$objective, 8), "\n"))
  cat(paste("  - coefficients = \n"))
  print(round(object$coefficients, 5))
  if (object$options$std_errors == TRUE) {
    cat(paste("  - standard errors = \n"))
    print(round(object$std_errors, 5))
  }
  cat("\n")
  options(scipen = 0)
}
