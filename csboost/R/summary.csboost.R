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
  hessian_constant <- params$hessian_constant
  hessian_constant <- ifelse(is.null(hessian_constant), "NULL", hessian_constant)
  best_performance <- xgbmodel$evaluation_log[xgbmodel$best_iteration, ]

  cat("SETTINGS -------------------------------------------------------------------------------\n")
  cat(paste("  - hessian_type =",          params$hessian_type,          "\n"))
  cat(paste("  - hessian_constant =",      hessian_constant,             "\n"))
  cat(paste("  - nrounds =",               params$nrounds,               "\n"))
  cat(paste("  - early_stopping_rounds =", params$early_stopping_rounds, "\n"))
  cat(paste("  - booster =",               params$booster,               "\n"))
  cat(paste("  - etas =",                  params$eta,                   "\n"))
  cat(paste("  - gamma =",                 params$gamma,                 "\n"))
  cat(paste("  - max_depth =",             params$max_depth,             "\n"))
  cat(paste("  - min_child_weight =",      params$min_child_weight,      "\n"))
  cat(paste("  - max_delta_step =",        params$max_delta_step,        "\n"))
  cat(paste("  - subsample =",             params$subsample,             "\n"))
  cat(paste("  - colsample_bytree =",      params$colsample_bytree,      "\n"))
  cat(paste("  - colsample_bylevel =",     params$colsample_bylevel,     "\n"))
  cat(paste("  - colsample_bynode =",      params$colsample_bynode,      "\n"))
  cat(paste("  - lambda =",                params$lambda,                "\n"))
  cat(paste("  - alpha =",                 params$alpha,                 "\n"))
  cat(paste("  - scale_pos_weight =",      params$scale_pos_weight,      "\n"))
  cat(paste("  - base_score =",            params$base_score,            "\n"))
  cat(paste("  - nthread =",               params$nthread,               "\n"))
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
  cat(paste("  - best expected savings (train) =",
            round(best_performance$`train_expected savings`, 6), "\n"))
  if (NCOL(xgbmodel$evaluation_log) == 3) {
    cat(paste("  - best expected savings (test) =",
              round(best_performance$`test_expected savings`, 6), "\n"))
  }
  cat("\n")
}
