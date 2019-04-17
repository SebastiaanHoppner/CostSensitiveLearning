classify <- function (scores, threshold, amounts = NULL, fixed_cost = NULL)
{
  if (class(threshold) == "numeric" ) {
    if (threshold < 0 | threshold > 1)
      stop("argument 'threshold' must be 'BMR' or a value between 0 and 1")
  } else if (class(threshold) == "character") {
    if (threshold != "BMR")
      stop("argument 'threshold' must be 'BMR' or a value between 0 and 1")
    if (is.null(amounts) | is.null(fixed_cost))
      stop("when 'threshold' is 'BMR' then arguments 'amounts' and 'fixed_cost' must be specified")
    if (any(amounts <= 0))
      stop("argument 'amounts' must contain positive, non-zero values")
    if (fixed_cost < 0)
      stop("argument 'fixed_cost' must be a non-negative number")
  } else {
    stop("argument 'threshold' must be 'BMR' or a value between 0 and 1")
  }

  if (class(threshold) == "numeric")
    thresholds <- threshold
  if (threshold == "BMR")
    thresholds <- fixed_cost / amounts

  predicted_classes <- ifelse(scores > thresholds, 1, 0)

  return(predicted_classes)
}
