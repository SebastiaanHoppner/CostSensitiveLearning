predict.csboost <- function (object, newdata, ntreelimit = NULL, ...) {
  # check inputs
  if (missing(object)) {
    stop("argument 'object' is missing, with no default")
  } else if (class(object) != "csboost") {
    stop("argument 'object' must be of class 'csboost'")
  }
  if (missing(newdata)) {
    stop("argument 'newdata' is missing, with no default")
  } else if(!is.data.frame(newdata)) {
    stop("argument 'newdata' must be a data frame")
  }

  # predict scores
  dnewdata <- xgboost::xgb.DMatrix(data = model.matrix(object$xgbmodel$params$formula, newdata))
  etas <- predict(object$xgbmodel, newdata = dnewdata, ntreelimit = ntreelimit, ...)
  scores <- 1 / (1 + exp(-etas))
  return(scores)
}
