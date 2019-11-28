csboost <- function (formula, train, test = NULL,
                     cost_matrix_train, cost_matrix_test = NULL,
                     hessian_type, hessian_constant = NULL,
                     nrounds, params = list(),
                     verbose = 1, print_every_n = 1L, early_stopping_rounds = NULL,
                     save_period = NULL, save_name = "xgboost.model",
                     xgb_model = NULL, ...) {
  # start timer
  t_start <- proc.time()

  # original call
  call <- match.call()

  # check inputs
  check <- checkInputsCSboost(formula, train, test, cost_matrix_train, cost_matrix_test,
                              hessian_type, hessian_constant)

  # convert data to xgb.DMatrix & build watchlist
  labels_train <- hmeasure::relabel(model.response(check$mf_train))
  dtrain <- xgboost::xgb.DMatrix(data = model.matrix(formula, train), label = labels_train)
  watchlist <- list(train = dtrain)
  if (!is.null(test)) {
    labels_test <- hmeasure::relabel(model.response(check$mf_test))
    dtest <- xgboost::xgb.DMatrix(data = model.matrix(formula, test), label = labels_test)
    watchlist <- c(watchlist, list(test = dtest))
  } else {
    dtest <- NULL
  }

  # rearrange cost matrix & define auxilary vectors
  cost_matrix_train[labels_train == 0, ] <- cost_matrix_train[labels_train == 0, c(2, 1)]
  cost_matrix_train_col2 <- cost_matrix_train[, 2]
  cost_no_model_train <- sum(cost_matrix_train_col2)
  diff_costs_train <- cost_matrix_train[, 1] - cost_matrix_train[, 2]
  if (!is.null(test)) {
    cost_matrix_test[labels_test == 0, ] <- cost_matrix_test[labels_test == 0, c(2, 1)]
    cost_matrix_test_col2 <- cost_matrix_test[, 2]
    cost_no_model_test <- sum(cost_matrix_test_col2)
    diff_costs_test <- cost_matrix_test[, 1] - cost_matrix_test[, 2]
  }

  # example cost matrix (used by summary.csboost)
  example_cost_matrix <- matrix(c(cost_matrix_train[which(labels_train == 0)[1], c(2, 1)],
                                  cost_matrix_train[which(labels_train == 1)[1], c(2, 1)]),
                                nrow = 2, ncol = 2)
  colnames(example_cost_matrix) <- rownames(example_cost_matrix) <- c("0", "1")
  names(dimnames(example_cost_matrix)) <- c("      Prediction", "Reference")

  # define objective function
  averageExpectedCostObj <- function (scores, dtrain) {
    scores <- 1 / (1 + exp(-scores))

    grad <- scores * (1 - scores) * diff_costs_train
    hess <- (1 - 2 * scores) * grad

    labels <- getinfo(dtrain, "label")
    i0 <- which(labels == 0)
    i1 <- which(labels == 1)
    print(summary(hess[i0]))
    print(summary(hess[i1]))
    cat("\n")

    if (hessian_type == "exact") {
      hess <- (1 - 2 * scores) * grad
    } else if (hessian_type == "solution1") {
      hess <- (1 - 2 * scores) * grad
      hess[which(hess < 0)] <- 0
    } else if (hessian_type == "solution2") {
      hess <- abs((1 - 2 * scores) * grad)
    } else if (hessian_type == "constant") {
      hess <- rep(hessian_constant, length(scores))
    }
    print(summary(hess[i0]))
    print(summary(hess[i1]))
    cat("\n\n")
    return(list(grad = grad, hess = hess))
  }

  # define evaluation function
  expectedSavings <- function (scores, DMatrix) {
    scores <- 1 / (1 + exp(-scores))
    if (length(scores) == NROW(dtrain)) {
      cost_matrix_col2 <- cost_matrix_train_col2
      cost_no_model <- cost_no_model_train
      diff_costs <- diff_costs_train
    } else if (length(scores) == NROW(dtest)) {
      cost_matrix_col2 <- cost_matrix_test_col2
      cost_no_model <- cost_no_model_test
      diff_costs <- diff_costs_test
    }
    expected_cost <- sum(cost_matrix_col2 + scores * diff_costs)
    expected_savings <- (cost_no_model - expected_cost) / cost_no_model
    return(list(metric = "expected savings", value = expected_savings))
  }

  # fit xgboost
  params$objective <- averageExpectedCostObj
  params$eval_metric <- expectedSavings

  xgbmodel <- xgboost::xgb.train(params, dtrain, nrounds, watchlist,
                                 verbose = verbose, print_every_n = print_every_n,
                                 early_stopping_rounds = early_stopping_rounds, maximize = TRUE,
                                 save_period = save_period, save_name = save_name,
                                 xgb_model = xgb_model)
  xgbmodel <- xgboost::xgb.train(params, dtrain, nrounds, watchlist,
                                 verbose = verbose, print_every_n = print_every_n,
                                 early_stopping_rounds = early_stopping_rounds, maximize = TRUE,
                                 save_period = save_period, save_name = save_name,
                                 xgb_model = xgb_model, ...)

  # end timer
  t_end <- proc.time() - t_start

  # output
  xgbmodel$params <- c(xgbmodel$params, list(formula               = formula,
                                             hessian_type          = hessian_type,
                                             hessian_constant      = hessian_constant,
                                             nrounds               = nrounds,
                                             early_stopping_rounds = early_stopping_rounds,
                                             example_cost_matrix   = example_cost_matrix))
  output <- list(call     = call,
                 time     = round(t_end[3], 3),
                 xgbmodel = xgbmodel)
  class(output) <- "csboost"
  return(output)
}
