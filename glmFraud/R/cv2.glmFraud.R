cv2.glmFraud <- function (formula, data, amounts, fixed_cost, options = list(),
                          nfolds = 5, nlambda = 100, lambda_max = 1, lambda_min = 1e-5, seed = NULL)
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

  # create lambda sequence (include zero)
  lambda_path <- c(0, exp(seq(log(lambda_min), log(lambda_max), length.out = nlambda)))

  # create folds for cross-validation
  if (!is.null(seed))
    set.seed(seed)
  folds <- caret::createFolds(y = factor(Y), k = nfolds)

  # create progress bar
  cat(paste("\n(5) Loop over", nlambda+1, "lambda values and", nfolds, "folds...\n"))
  pb <- txtProgressBar(min = 0, max = (nlambda+1) * nfolds, style = 3)
  step <- 0

  # start looping over formulas and folds
  cv_results <- c()

  for (LAMBDA in lambda_path) {

    results_lambda <- c()
    for (k in 1:nfolds) {

      # create training & validation set
      train <- data[-folds[[k]], ]
      valid <- data[ folds[[k]], ]

      amounts_train <- amounts[-folds[[k]]]
      amounts_valid <- amounts[ folds[[k]]]

      # fit on training set
      cslogit <-suppressWarnings(glmFraud(formula    = formula,
                                          data       = train,
                                          amounts    = amounts_train,
                                          fixed_cost = fixed_cost,
                                          lambda     = LAMBDA,
                                          options    = options))

      # validate on validation set
      scores <- predict(cslogit, newdata = valid)

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
      results_lambda <- rbind.data.frame(results_lambda, perf)

      # update progress bar
      step <- step + 1
      setTxtProgressBar(pb, step)
    }

    cv_results <- rbind.data.frame(cv_results, colMeans(results_lambda))
  }

  close(pb)
  colnames(cv_results) <- names(perf)

  # end timer
  t_end <- proc.time() - t_start
  cat(paste0("\nTotal elapsed time: ", round(t_end[3]/60, 2), " min.\n"))

  # output
  output <- list(call = call, cv_results = cv_results, lambda_path = lambda_path,
                 time = round(t_end[3], 3))
  class(output) <- "cv.glmFraud"
  return(output)
}
