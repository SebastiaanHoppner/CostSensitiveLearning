predict.cslogit <- function (object, newdata = NULL, ...) {
  # check inputs
  if (missing(object)) {
    stop("argument 'object' is missing, with no default")
  }
  if (class(object) != "cslogit") {
    stop("argument 'object' must be of class 'cslogit'")
  }

  # predict fraud scores
  if (is.null(newdata)) {
    return(object$fitted_values)
  } else {
    X <- model.matrix(delete.response(object$terms), newdata)
    scores <- as.numeric( 1 / (1 + exp(-object$coefficients %*% t(X))) )
    return(scores)
  }
}
