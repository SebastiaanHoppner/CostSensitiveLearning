cv.glmFraud <- function (formula, data, amounts, fixed_cost, options = list(),
                         nfolds = 5, nlambda = 100, lambda_max = 1, lambda_min = 1e-5)
{
  # start timer
  t_start <- proc.time()

  # original call
  call <- match.call()

  # check inputs
  check <- check.inputs(formula, data, amounts, fixed_cost, lambda, options)
  options <- check$options
  options$check_data <- FALSE
  options$print_level <- 0
  options$std_errors <- FALSE

  # extract response variable and model matrix
  Y <- hmeasure::relabel(model.response(check$mf))
  X <- model.matrix(formula, data)

  # starting values for the regression parameters
  if (!is.null(options$start)) {
    if (length(options$start) != NCOL(X))
      stop("Some starting values for the regression parameters are missing")
  } else {
    cat("\n(3) Fit logistic regression model to get starting values...")
    logit <- suppressWarnings(glm(formula, family = binomial(link = "logit"), data))
    options$start <- coef(logit)
  }

  # create progress bar
  cat(paste("\n(4) Loop over", nlambda, "lambda values...\n"))
  pb <- txtProgressBar(min = 0, max = nlambda, style = 3)

  # create lambda sequence
  lambda_path <- exp(seq(log(lambda_min), log(lambda_max), length.out = nlambda))

  # for-loop over lambda values
  significant_formula_list <- list()
  coef_mat <- matrix(NA, nrow = nlambda, ncol = ncol(X))

  for (ii in 1:nlambda) {
    cslogit_lambda <- suppressWarnings(glmFraud(formula     = formula,
                                                data        = data,
                                                amounts     = amounts,
                                                fixed_cost  = fixed_cost,
                                                lambda      = lambda_path[ii],
                                                options     = options))
    significant_formula_list[[ii]] <- cslogit_lambda$significant_formula
    coef_mat[ii, ] <- cslogit_lambda$coefficients
    setTxtProgressBar(pb, ii)
  }

  close(pb)
  variable_names <- attr(cslogit_lambda$terms, "term.labels")
  intercept_flag <- attr(cslogit_lambda$terms, "intercept")
  formula_list   <- unique(significant_formula_list)
  options$start  <- NULL

  # create folds for cross-validation
  if (!is.null(options$seed))
    set.seed(options$seed)
  folds <- caret::createFolds(y = factor(Y), k = nfolds)

  # create progress bar
  cat(paste("\n(5) Loop over", length(formula_list), "formulas and", nfolds, "folds...\n"))
  pb <- txtProgressBar(min = 0, max = length(formula_list) * nfolds, style = 3)
  step <- 0

  # start looping over formulas and folds
  cv_results <- c()
  for (FORMULA in formula_list) {

    results_formula <- c()
    for (k in 1:nfolds) {

      # create training & validation set
      train <- data[-folds[[k]], ]
      valid <- data[ folds[[k]], ]

      amounts_train <- amounts[-folds[[k]]]
      amounts_valid <- amounts[ folds[[k]]]

      # fit on training set
      cslogit <- suppressWarnings(glmFraud(formula    = FORMULA,
                                           data       = train,
                                           amounts    = amounts_train,
                                           fixed_cost = fixed_cost,
                                           lambda     = 0,
                                           options    = options))

      # validate on validation set
      scores <- predict.glmFraud(cslogit, newdata = valid)

      predicted_classes <- classify(scores     = scores,
                                    threshold  = 0.5, # threshold does not matter
                                    amounts    = amounts_valid,
                                    fixed_cost = fixed_cost)

      metrics <- suppressWarnings(performance(scores            = scores,
                                              predicted_classes = predicted_classes,
                                              true_classes      = Y[folds[[k]]])$metrics)
      metrics <- metrics[, c("MER", "AUC", "AUC_pr")]

      cost_metrics <- suppressWarnings(costPerformance(scores            = scores,
                                                       predicted_classes = predicted_classes,
                                                       true_classes      = Y[folds[[k]]],
                                                       amounts           = amounts_valid,
                                                       fixed_cost        = fixed_cost))
      cost_metrics <- cost_metrics[, c("average_expected_cost", "expected_cost", "expected_savings")]

      perf <- cbind.data.frame(cost_metrics, metrics)

      results_formula <- rbind.data.frame(results_formula, perf)

      # update progress bar
      step <- step + 1
      setTxtProgressBar(pb, step)
    }

    cv_results <- rbind.data.frame(cv_results, colMeans(results_formula))
  }
  close(pb)
  colnames(cv_results) <- names(perf)

  # end timer
  t_end <- proc.time() - t_start
  cat(paste0("\nTotal elapsed time: ", round(t_end[3]/60, 2), " min.\n"))

  # output
  output <- list(call = call, cv_results = cv_results, lambda_path = lambda_path,
                 coef_mat = coef_mat, formula_list = formula_list,
                 variable_names = variable_names, intercept_flag = intercept_flag, time = t_end[3])
  class(output) <- "cv.glmFraud"
  return(output)
}
