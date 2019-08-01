cv.cslogit <- function (formula, data, cost_matrix, options = list(),
                        nfolds = 10L, lambda_path = NULL, seed = NULL) {
  # start timer
  t_start <- proc.time()

  # original call
  call <- match.call()

  # check inputs
  if (!is.list(options)) {
    stop("argument 'options' must be a list")
  }
  options$print_level <- 1
  check <- checkInputsCSlogit(formula, data, cost_matrix, lambda = 0, options)
  options <- check$options
  options$print_level <- 0
  options$check_data <- FALSE
  if (nfolds < 2L | nfolds > nrow(data)) {
    stop("'nfolds' must be an integer between 2 and the sample size")
  } else {
    nfolds <- as.integer(nfolds)
  }

  # extract response variable
  Y <- hmeasure::relabel(model.response(check$mf))

  # lambda sequence
  if (is.null(lambda_path)) {
    lambda_path <- c(0, exp(seq(log(1e-5), log(1), length.out = 100)))
  } else if (any(lambda_path < 0)) {
    stop("each element in argument 'lambda_path' must be non-negative")
  }

  # create folds for cross-validation
  cat(paste("- Create", nfolds, "folds...\n"))
  if (!is.null(seed)) {
    set.seed(seed)
  }
  folds <- caret::createFolds(y = factor(Y), k = nfolds)

  # create progress bar
  cat(paste0("- Loop over ", length(lambda_path), " lambda values and ", nfolds, " folds... ",
             "(started at ", Sys.time(), ")\n"))
  pb <- txtProgressBar(min = 0, max = length(lambda_path) * nfolds, style = 3)
  step <- 0

  # start looping over lambda sequence and folds
  cv_results <- c()
  for (LAMBDA in lambda_path) {

    results_lambda <- c()
    for (k in 1:nfolds) {

      # create training & validation set
      train <- data[-folds[[k]], ]
      valid <- data[ folds[[k]], ]

      cost_matrix_train <- cost_matrix[-folds[[k]], ]
      cost_matrix_valid <- cost_matrix[ folds[[k]], ]

      # fit on training set
      cslogit_lambda <-suppressWarnings(cslogit(formula     = formula,
                                                data        = train,
                                                cost_matrix = cost_matrix_train,
                                                lambda      = LAMBDA,
                                                options     = options))

      # validate on validation set
      scores <- predict(cslogit_lambda, newdata = valid)

      Y_valid <- Y[folds[[k]]]
      cost_matrix_valid[Y_valid == 0, ] <- cost_matrix_valid[Y_valid == 0, c(2, 1)]
      average_expected_cost <- 2 * mean(cost_matrix_valid * matrix(c(scores, 1 - scores), ncol = 2))

      results_lambda <- rbind(results_lambda, average_expected_cost)

      # update progress bar
      step <- step + 1
      setTxtProgressBar(pb, step)
    }

    cv_results <- cbind(cv_results, results_lambda)
  }
  close(pb)

  # find optimal lambda
  colnames(cv_results) <- paste0("Lambda", 1:length(lambda_path))
  optimal_lambda <- lambda_path[which.min(colMeans(cv_results))]

  # end timer
  t_end <- proc.time() - t_start
  cat(paste0("\nTime elapsed: ", round(t_end[3]/60, 2), " minutes\n\n"))

  # output
  output <- list(call = call, formula = formula, nfolds = nfolds, lambda_path = lambda_path,
                 options = options, seed = seed, cv_results = cv_results,
                 optimal_lambda = optimal_lambda, time = round(t_end[3], 3))
  class(output) <- "cv.cslogit"
  return(output)
}
