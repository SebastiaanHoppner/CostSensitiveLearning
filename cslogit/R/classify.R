classify <- function (scores, threshold, amounts = NULL, fixed_cost = NULL)
{
  # Convert scores into a binary label based on a threshold
  # -----------------------------------------------------------------------------------------------
  # Arguments:
  #   scores       vector of predicted probabilities
  #   threshold    either "BMR" (Bayes Minimum Risk) or a value between 0 and 1
  #   amounts      vector of positive, non-zero values; only needs to be specified when
  #                the threshold is "BMR"
  #   fixed_cost   a non-negative number; only needs to be speciefied when the threshold is "BMR"
  # -----------------------------------------------------------------------------------------------
  # Value:
  #   classify returns a vector containing binary labels 0 and 1
  # -----------------------------------------------------------------------------------------------
  # Written by Sebastiaan Höppner, 2019
  # -----------------------------------------------------------------------------------------------

  if (class(threshold) == "numeric" ) {
    if (threshold < 0 | threshold > 1) {
      stop("argument 'threshold' must be 'BMR' or a value between 0 and 1")
    }
  } else if (class(threshold) == "character") {
    if (threshold != "BMR") {
      stop("argument 'threshold' must be 'BMR' or a value between 0 and 1")
    }
    if (is.null(amounts) | is.null(fixed_cost)) {
      stop("when 'threshold' is 'BMR' then arguments 'amounts' and 'fixed_cost' must be specified")
    }
    if (any(amounts <= 0)) {
      stop("argument 'amounts' must contain positive, non-zero values")
    }
    if (fixed_cost < 0) {
      stop("argument 'fixed_cost' must be a non-negative number")
    }
  } else {
    stop("argument 'threshold' must be 'BMR' or a value between 0 and 1")
  }

  if (class(threshold) == "numeric") {
    thresholds <- threshold
  } else if (threshold == "BMR") {
    thresholds <- fixed_cost / amounts
  }

  predicted_classes <- ifelse(scores > thresholds, 1, 0)

  return(predicted_classes)
}
