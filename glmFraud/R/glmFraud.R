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
  fit <- optimfun(start = options$start, intercept_flag = attr(check$mt, "intercept"),
                  Yname = names(check$mf[1]), Y = Y, X = X, amounts = amounts,
                  fixed_cost = fixed_cost, lambda = lambda, options = options)
  if (fit$diagnostics$status != 0)
    warning(fit$diagnostics$message)

  # end timer
  t_end <- proc.time() - t_start
  if (options$print_level > 0)
    cat(paste("\nTime elapsed:", round(t_end[3], 3), "seconds\n\n"))

  # output
  output <- c(list(call = call, formula = formula, fixed_cost = fixed_cost, lambda = lambda,
                   options = options, terms = check$mt), fit, list(time = t_end[3]))
  class(output) <- "glmFraud"
  return(output)
}
