predict.cslogit <- function (object, newdata = NULL)
{
  # Predict method for cslogit fits
  # -----------------------------------------------------------------------------------------------
  # Arguments:
  #   object    a fitted object of class "cslogit"
  #   newdata   optionally, a data frame in which to look for variables with which to predict;
  #             if omitted, the fitted coefficients are used.
  # -----------------------------------------------------------------------------------------------
  # Value:
  #   predict.cslogit returns a vector of predicted probabilities
  # -----------------------------------------------------------------------------------------------
  # Written by Sebastiaan Höppner, 2019
  # -----------------------------------------------------------------------------------------------

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
