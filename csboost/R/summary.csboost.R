summary.csboost <- function (object, ...) {
  # check inputs
  if (missing(object)) {
    stop("argument 'object' is missing, with no default")
  }
  if (class(object) != "csboost") {
    stop("argument 'object' must be of class 'csboost'")
  }

  # print summary
  xgbmodel <- object$xgbmodel
  params <- xgbmodel$params
  best_performance <- xgbmodel$evaluation_log[xgbmodel$best_iteration, ]

  cat("SETTINGS -------------------------------------------------------------------------------\n")
  if (!is.null(params$nrounds)) {
    cat(paste("  - nrounds =",               params$nrounds,               "\n"))
  }
  if (!is.null(params$early_stopping_rounds)) {
    cat(paste("  - early_stopping_rounds =", params$early_stopping_rounds, "\n"))
  }
  if (!is.null(params$booster)) {
    cat(paste("  - booster =",               params$booster,               "\n"))
  }
  if (!is.null(params$eta)) {
    cat(paste("  - etas =",                  params$eta,                   "\n"))
  }
  if (!is.null(params$gamma)) {
    cat(paste("  - gamma =",                 params$gamma,                 "\n"))
  }
  if (!is.null(params$max_depth)) {
    cat(paste("  - max_depth =",             params$max_depth,             "\n"))
  }
  if (!is.null(params$min_child_weight)) {
    cat(paste("  - min_child_weight =",      params$min_child_weight,      "\n"))
  }
  if (!is.null(params$max_delta_step)) {
    cat(paste("  - max_delta_step =",        params$max_delta_step,        "\n"))
  }
  if (!is.null(params$subsample)) {
    cat(paste("  - subsample =",             params$subsample,             "\n"))
  }
  if (!is.null(params$colsample_bytree)) {
    cat(paste("  - colsample_bytree =",      params$colsample_bytree,      "\n"))
  }
  if (!is.null(params$colsample_bylevel)) {
    cat(paste("  - colsample_bylevel =",     params$colsample_bylevel,     "\n"))
  }
  if (!is.null(params$colsample_bynode)) {
    cat(paste("  - colsample_bynode =",      params$colsample_bynode,      "\n"))
  }
  if (!is.null(params$lambda)) {
    cat(paste("  - lambda =",                params$lambda,                "\n"))
  }
  if (!is.null(params$alpha)) {
    cat(paste("  - alpha =",                 params$alpha,                 "\n"))
  }
  if (!is.null(params$scale_pos_weight)) {
    cat(paste("  - scale_pos_weight =",      params$scale_pos_weight,      "\n"))
  }
  if (!is.null(params$base_score)) {
    cat(paste("  - base_score =",            params$base_score,            "\n"))
  }
  if (!is.null(params$nthread)) {
    cat(paste("  - nthread =",               params$nthread,               "\n"))
  }
  cat(paste("  - cost matrix (example) = \n"))
  print(params$example_cost_matrix)
  cat(paste("  -", xgbmodel$nfeatures, "features:\n      "))
  cat(paste(xgbmodel$feature_names), sep = "\n      ")

  cat("\n")
  cat("RESULTS --------------------------------------------------------------------------------\n")
  cat(paste("  - iterations =",      xgbmodel$niter, "\n"))
  cat(paste("  - time =",            object$time, "seconds\n"))
  cat(paste("  - best iteration =",  xgbmodel$best_iteration, "\n"))
  cat(paste("  - best ntreelimit =", xgbmodel$best_ntreelimit, "\n"))
  cat(paste("  - best average expected cost (train) =",
            round(best_performance$train_AEC, 6), "\n"))
  if (NCOL(xgbmodel$evaluation_log) == 3) {
    cat(paste("  - best average expected cost (test) =",
              round(best_performance$test_AEC, 6), "\n"))
  }
  cat("\n")
}
