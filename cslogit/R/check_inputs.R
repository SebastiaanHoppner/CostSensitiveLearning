check_inputs <- function (formula, data, cost_matrix, lambda, options = list())
{
  # Check input arguments for cslogit
  # -----------------------------------------------------------------------------------------------
  # Arguments:
  #   formula           an object of class "formula": a symbolic description of the model to be
  #                     fitted; an intercept must be included
  #   data              a data frame containing the variables in the model
  #   cost_matrix       a matrix of dimension nrow(data) x 2:
  #                     for each instance, the first/second column contains the cost of
  #                     correctly/incorrectly predicting the binary class of the instance
  #   lambda            value that controls the L1-penalty of the regression coefficients
  #   options           a list of options that control details of the cslogit algorithm:
  #
  #       algorithm     algorithm used by the nloptr function; available algorithms:
  #                     "SLSQP" = Sequential Least-Squares Quadratic Programming (default)
  #                     "MMA" = Method of Moving Asymptotes
  #       maxeval       maximum number of function evaluations (default is 10000)
  #       ftol_rel      obtain the minimum of the objective function to within a relative tolerance
  #                     (default is 1e-8)
  #       xtol_rel      obtain optimal regression coefficients to within a relative tolerance
  #                     (default is 1e-5)
  #       start         starting values for the coefficients in the linear predictor;
  #                     by default a logistic regression model is fitted in order to
  #                     use the coefficients as starting values
  #       lb            vector with lower bounds of the coefficients (use -Inf for coefficients
  #                     without lower bound); by default the intercept is unbounded and remaining
  #                     coefficients have a lower bound of -max(50, abs(options$start[-1]));
  #                     if a single number is provided, the vector is initialized as
  #                     c(-Inf, rep(options$lb, length(options$start) - 1))
  #       ub            vector with upper bounds of the coefficients (use Inf for coefficients
  #                     without upper bound); by default the intercept is unbounded and remaining
  #                     coefficients have an upper bound of max(50, abs(options$start[-1]));
  #                     if a single number is provided, the vector is initialized as
  #                     c(Inf, rep(options$ub, length(options$start) - 1))
  #       check_data    should the data and variables be checked for NA, Inf, -Inf values, etc.
  #                     (default is TRUE) or not (FALSE)
  #       print_level   controls how much output is shown during the optimization process;
  #                     possible values:
  #                     0             no output
  #                     1 (default)   show iteration number and value of objective function
  #                     2             1 + show value of coefficients
  # -----------------------------------------------------------------------------------------------
  # Value:
  #   check_inputs is called by cslogit and returns a list containing the following:
  #   options   a list of arguments supplied, with defaults filled in
  #   mf        the model frame used
  #   mt        the terms object used
  # -----------------------------------------------------------------------------------------------
  # Written by Sebastiaan Höppner, 2019
  # -----------------------------------------------------------------------------------------------

  # check inputs
  if (missing(formula)) {
    stop("argument 'formula' is missing, with no default")
  } else if (class(formula) != "formula") {
    stop("argument 'formula' must be a formula")
  }
  if (missing(data)) {
    stop("argument 'data' is missing, with no default")
  } else if(!is.data.frame(data)) {
    stop("argument 'data' must be a data frame")
  }
  if (missing(cost_matrix)) {
    stop("argument 'cost_matrix' is missing, with no default")
  } else if (!is.matrix(cost_matrix) | any(dim(cost_matrix) != c(nrow(data), 2))) {
    stop("argument 'cost_matrix' must be a matrix of dimension n x 2")
  }
  if (missing(lambda)) {
    stop("argument 'lambda' is missing, with no default")
  } else if (lambda < 0) {
    stop("argument 'lambda' must be non-negative")
  }


  # check options
  possible_options <- c("algorithm", "maxeval", "ftol_rel", "xtol_rel", "lb", "ub",
                        "check_data", "print_level", "start")
  if (!is.list(options)) {
    stop("argument 'options' must be a list")
  }
  if (!"algorithm" %in% names(options)) {
    options$algorithm <- "SLSQP"
  } else if (!options$algorithm %in% c("SLSQP", "MMA")) {
    stop("argument 'algorithm' must be either 'SLSQP' or 'MMA'")
  }
  if (!"maxeval" %in% names(options)) {
    options$maxeval <- 10000L
  } else if (options$maxeval <= 0L) {
    stop("'maxeval' must be a positive integer")
  } else {
    options$maxeval <- as.integer(options$maxeval)
  }
  if (!"ftol_rel" %in% names(options)) {
    options$ftol_rel <- 1e-8
  } else if (options$ftol_rel <= 0) {
    stop("'ftol_rel' must be positive")
  }
  if (!"xtol_rel" %in% names(options)) {
    options$xtol_rel <- 1e-5
  } else if (options$xtol_rel <= 0) {
    stop("'xtol_rel' must be positive")
  }
  if (!"check_data" %in% names(options)) {
    options$check_data <- TRUE
  } else if (class(options$check_data) != "logical") {
    stop("'check_data' must be TRUE or FALSE")
  }
  if (!"print_level" %in% names(options)) {
    options$print_level <- 1L
  } else if (!options$print_level %in% c(0L, 1L, 2L)) {
    stop("argument 'print_level' must be either 0, 1 or 2")
  }

  unused_options <- setdiff(names(options), possible_options)
  if (length(unused_options) > 0) {
    stop("Following arguments are not used: ", paste(unused_options, collapse = ", "), "\n",
         "Possible options are:\n - ", paste(possible_options, collapse = "\n - "), "\n")
  }

  # check data
  if (options$check_data) {
    if (options$print_level > 0) {
      cat("\n- Check if data contains NA, Inf or -Inf values...\n")
    }
    if (any(is.na(data))) {
      stop("argument 'data' contains NA values")
    }
    if (any(is.infinite(unlist(data)))) {
      stop("argument 'data' contains Inf or -Inf values")
    }
  }

  # build model frame
  mf <- model.frame(formula, data)
  mt <- attr(mf, "terms")

  # check all variables
  if (attr(mt, "intercept") == 0) {
    stop("argument 'formula' must contain an intercept")
  }
  if (options$check_data) {
    if (options$print_level > 0) {
      cat("- Check all variables...\n")
    }
    if (length(unique(mf[, 1])) == 1L) {
      stop("response variable has no variation")
    }
    if (!is.factor(mf[, 1])) {
      stop("response variable is not a factor")
    } else if (length(levels(mf[, 1])) != 2L) {
      stop("response variable has ", length(levels(mf[, 1])), " level(s) instead of 2")
    }

    if (NCOL(mf) > 1L) {
      for (j in 2:NCOL(mf)) {
        if (length(unique(mf[, j])) == 1L) {
          stop("predictor variable ", names(mf[j]), " is constant")
        }
        if (is.character(mf[, j])) {
          mf[, j] <- as.factor(mf[, j])
          warning("character variable ", names(mf[j]), " was converted to a factor")
        } else if (is.logical(mf[, j])) {
          mf[, j] <- as.factor(mf[, j])
          warning("logical variable ", names(mf[j]), " was converted to a factor")
        } else if (!class(mf[, j])[1] %in% c("numeric", "integer", "factor")) {
          stop("type '", class(mf[, j])[1] ,"' of variable ", names(mf[j])," is not supported")
        }
      }
    }
  }

  return(list(options = options, mf = mf, mt = mt))
}
