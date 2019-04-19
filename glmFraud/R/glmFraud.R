glmFraud <- function (formula, data, amounts, fixed_cost, lambda, options = list())
{
  # start timer
  t_start <- proc.time()

  # original call
  call <- match.call()

  # check inputs
  check <- check.inputs(formula, data, amounts, fixed_cost, lambda, options)
  options <- check$options

  # extract response variable and model matrix
  Y <- hmeasure::relabel(model.response(check$mf))
  X <- model.matrix(formula, data)
  intercept_flag <- attr(check$mt, "intercept")

  # starting values for the regression parameters
  if (!is.null(options$start)) {
    if (options$print_level > 0)
      cat("\n(3) Check starting values...")
    if (length(options$start) != NCOL(X))
      stop("Some starting values for the regression parameters are missing")
  } else {
    if (options$print_level > 0)
      cat("\n(3) Fit logistic regression model to get starting values...")
    logit <- suppressWarnings(glm(formula, family = binomial(link = "logit"), data))
    options$start <- coef(logit)
  }

  # fit
  fit <- optimfun(start = options$start, intercept_flag = intercept_flag, Y = Y, X = X,
                  amounts = amounts, fixed_cost = fixed_cost, lambda = lambda, options = options)
  if (fit$diagnostics$status != 0)
    warning(fit$diagnostics$message)

  # extract significant variables
  xlevels <- lapply(.getXlevels(check$mt, check$mf), function (x) {x[-1]})
  significant_factor_vars <- c()
  if (length(xlevels) > 0) {
    for (i in 1:length(xlevels)) {
      xlevels_names <- paste0(names(xlevels)[i], xlevels[[i]])
      factor_regression_params<- fit$coefficients[which(colnames(X) %in% xlevels_names)]
      if (any(abs(factor_regression_params) > options$xtol_rel))
        significant_factor_vars <- c(significant_factor_vars, names(xlevels)[i])
    }
  }
  numeric_vars <- names(which(attr(check$mt, "dataClasses") != "factor"))
  significant_numeric_vars <- NULL
  if (length(numeric_vars) > 0) {
    numeric_vars_idx <- which(colnames(X) %in% numeric_vars)
    significant_numeric_vars_idx <- which(abs(fit$coefficients[numeric_vars_idx]) > options$xtol_rel)
    significant_numeric_vars <- numeric_vars[significant_numeric_vars_idx]
  }
  significant_variables <- c(significant_factor_vars, significant_numeric_vars)
  if (intercept_flag == 1 & abs(fit$coefficients[1]) > options$xtol_rel)
    significant_variables <- c("1", significant_variables)
  else
    significant_variables <- c("0", significant_variables)
  Yname <- names(attr(check$mt, "dataClasses"))[1]
  significant_formula <- formula(paste(Yname, "~", paste(significant_variables, collapse = " + ")),
                                 env = NULL)
  significant_variables <- setdiff(significant_variables, c("0", "1"))

  # end timer
  t_end <- proc.time() - t_start
  if (options$print_level > 0)
    cat(paste("\nTime elapsed:", round(t_end[3], 3), "seconds\n\n"))

  # output
  output <- c(list(call = call, formula = formula, fixed_cost = fixed_cost, lambda = lambda,
                   options = options, terms = check$mt), fit[1:4],
              list(significant_variables = significant_variables,
                   significant_formula = significant_formula),
              fit$diagnostics, list(time = round(t_end[3], 3)))
  class(output) <- "glmFraud"
  return(output)
}
