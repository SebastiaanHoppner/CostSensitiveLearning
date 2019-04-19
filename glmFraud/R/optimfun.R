optimfun <- function (start, intercept_flag, Y, X, amounts, fixed_cost, lambda, options)
{
  # solve optimization problem by using nloptr
  if (options$print_level > 0)
    cat("\n(4) Search for optimal regression parameters...\n\n")

  fit <- nloptr(x0             = start,
                eval_f         = objectivefun,
                eval_grad_f    = gradientfun,
                intercept_flag = intercept_flag,
                Y              = Y,
                X              = X,
                amounts        = amounts,
                fixed_cost     = fixed_cost,
                lambda         = lambda,
                opts           = list(
                  algorithm   = options$algorithm,
                  maxeval     = options$maxeval,
                  ftol_rel    = options$ftol_rel,
                  xtol_rel    = options$xtol_rel,
                  print_level = ifelse(options$print_level == 2, 3, options$print_level)
                ))
  names(fit$solution) <- names(start)

  # estimate standard errors of regression parameters (if required)
  std_errors <- NA
  if (options$std_errors == TRUE) {
    if (options$print_level > 0)
      cat("\n(5) Estimate standard errors... \n")
    hessian <- try(optimHess(par            = fit$solution,
                             fn             = objectivefun,
                             gr             = gradientfun,
                             intercept_flag = intercept_flag,
                             Y              = Y,
                             X              = X,
                             amounts        = amounts,
                             fixed_cost     = fixed_cost,
                             lambda         = lambda))
    cov_mat <- try(solve(hessian))
    std_errors <- try(sqrt(diag(cov_mat)))
    if (class(hessian) == "try-error" |
        class(cov_mat) == "try-error" |
        class(std_errors) == "try-error")
      std_errors <- NA
  }

  # output
  output <- list(fitted_values = as.numeric( 1 / (1 + exp(-fit$solution %*% t(X))) ),
                 objective     = fit$objective,
                 coefficients  = fit$solution,
                 std_errors    = std_errors,
                 diagnostics   = list(status     = fit$status,
                                      message    = fit$message,
                                      iterations = fit$iterations))
  return(output)
}
