optimfun <- function (start, intercept_flag, Y, X, Yname, amounts, fixed_cost, lambda, options)
{
  if (options$print_level > 0)
    cat("\n(4) Search for optimal regression parameters...\n\n")

  # solve optimization problem by using nloptr
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
                  ranseed     = options$seed,
                  print_level = ifelse(options$print_level == 2, 3, options$print_level)
                ))

  # estimate standard errors of regression parameters (if required)
  if (options$std_errors == TRUE) {
    hessian <- try(optimHess(par            = fit$solution,
                             fn             = objectivefun,
                             gr             = gradientfun,
                             intercept_flag = intercept_flag,
                             Y              = Y,
                             X              = X,
                             amounts        = amounts,
                             fixed_cost     = fixed_cost,
                             lambda         = lambda),
                   silent = FALSE)
    if (class(hessian) == "try-error") {
      std_errors <- NA
    } else {
      cov_mat <- try(solve(hessian), silent = FALSE)
      if (class(cov_mat) == "try-error") {
        std_errors <- NA
      } else {
        std_errors <- try(sqrt(diag(cov_mat)), silent = FALSE)
        if (class(std_errors) == "try-error")
          std_errors <- NA
      }
    }
  } else {
    std_errors <- NA
  }

  # derive non-zero regression parameters
  significant_variables <- colnames(X)[which(abs(fit$solution) > options$xtol_rel)]
  significant_variables[which(significant_variables == "(Intercept)")] <- "1"
  significant_formula <- as.formula(paste(Yname, "~", paste(significant_variables, collapse = " + ")),
                                    env = NULL)

  # output
  output <- list(fitted_values         = as.numeric( 1 / (1 + exp(-fit$solution %*% t(X))) ),
                 objective             = fit$objective,
                 coefficients          = fit$solution,
                 std_errors            = std_errors,
                 significant_variables = setdiff(significant_variables, "1"),
                 significant_formula   = significant_formula,
                 diagnostics           = list(status     = fit$status,
                                              message    = fit$message,
                                              iterations = fit$iterations))
  return(output)
}
