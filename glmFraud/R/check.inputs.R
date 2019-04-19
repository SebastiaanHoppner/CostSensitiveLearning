check.inputs <- function(formula, data, amounts, fixed_cost, lambda = NULL, options = list())
{
  # check inputs
  if (missing(formula))
    stop("argument 'formula' is missing, with no default")
  if (missing(data))
    stop("argument 'data' is missing, with no default")
  if (missing(amounts))
    stop("argument 'amounts' is missing, with no default")
  if (missing(fixed_cost))
    stop("argument 'fixed_cost' is missing, with no default")
  if (missing(lambda))
    stop("argument 'lambda' is missing, with no default")

  # check options
  possible_options <- c("check_data", "print_level", "algorithm", "maxeval",
                        "ftol_rel", "xtol_rel", "start", "std_errors")
  possible_algorithms <- c("NLOPT_LD_MMA", "NLOPT_LD_SLSQP",
                           "NLOPT_LD_VAR1", "NLOPT_LD_VAR2", "NLOPT_LD_LBFGS")
  if (!is.list(options))
    stop("argument 'options' must be a list")
  if (!"check_data" %in% names(options))
    options$check_data <- TRUE
  if (!"start" %in% names(options))
    options$start <- NULL
  if (!"std_errors" %in% names(options))
    options$std_errors <- FALSE
  if (!"maxeval" %in% names(options))
    options$maxeval <- 1000
  if (!"ftol_rel" %in% names(options))
    options$ftol_rel <- 1e-8
  if (!"xtol_rel" %in% names(options))
    options$xtol_rel <- 1e-5
  if (!"print_level" %in% names(options))
    options$print_level <- 1
  else if (!options$print_level %in% c(0, 1, 2))
    stop("argument 'print_level' must be either 0, 1, or 2")
  if (!"algorithm" %in% names(options))
    options$algorithm <- "NLOPT_LD_MMA"
  else if (!options$algorithm %in% possible_algorithms)
    stop("argument 'algorithm' must be one of the following:\n - ",
         paste(possible_algorithms, collapse = "\n - "))
  unused_options <- setdiff(names(options), possible_options)
  if (length(unused_options) > 0)
    warning("Following arguments are not used: ", paste(unused_options, collapse = ", "), "\n",
            "Possible options are:\n - ", paste(possible_options, collapse = "\n - "), "\n")

  # check data
  if (options$check_data) {
    if (options$print_level > 0)
      cat("\n(1) Check if data contains NA, Inf or -Inf values...")
    if (any(is.na(data)))
      stop("argument 'data' contains NA values")
    if (any(is.infinite(unlist(data))))
      stop("argument 'data' contains Inf or -Inf values")
  }

  # build model frame
  mf <- model.frame(formula, data)
  mt <- attr(mf, "terms")

  # check all variables
  if (options$check_data) {
    if (options$print_level > 0)
      cat("\n(2) Check all variables...")
    if (length(unique(mf[, 1])) == 1L)
      stop("response variable has no variation")
    if (!is.factor(mf[, 1])) {
      stop("response variable is not a factor")
    } else if (length(levels(mf[, 1])) < 2L) {
      stop("response variable has only ", length(levels(mf[, 1])), " level(s)")
    }
    if (NCOL(mf) > 1) {
      for (j in 2:NCOL(mf)) {
        if (length(unique(mf[, j])) == 1L)
          stop("predictor variable ", names(mf[j]), " is constant")
        if (is.character(mf[, j])) {
          mf[, j] <- as.factor(mf[, j])
          warning("character variable ", names(mf[j]), " was converted to a factor")
        } else if (is.logical(mf[, j])) {
          mf[, j] <- as.factor(mf[, j])
          warning("logical variable ", names(mf[j]), " was converted to a factor")
        } else if (class(mf[, j])[1] == "Date") {
          stop("variable type \"Date\" of variable ", names(mf[j])," is not supported")
        }
      }
    }
  }

  return(list(options = options, mf = mf, mt = mt))
}
