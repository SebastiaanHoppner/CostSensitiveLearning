cslogit <- function (formula, data, cost_matrix, lambda, options = list())
{
  # Instance-Dependent Cost-Sensitive Logistic Regression
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
  #   cslogit returns an object of class "cslogit" which is a list containing the following:
  #   call                    the matched call
  #   formula                 the formula supplied
  #   lambda                  the lambda value supplied
  #   options                 the list of arguments supplied, with defaults filled in
  #   terms                   the terms object used
  #   example_cost_matrix     2x2 cost matrix based on the first class-0 and class-1 instances in
  #                           the data; used by the summary.cslogit function
  #   objective_path          the vector containing the objective value for each iteration
  #   betas_path              the matrix containing the coefficients for each iteration
  #   fitted_values           the fitted probabilities
  #   average expected cost   the average expected cost of the solution fitted on the supplied data
  #   objective               the value of the objective function in the solution
  #   coefficients            the vector of fitted coefficients
  #   status                  integer value with the status of the optimization
  #   message                 character string produced by NLopt and giving additional information
  #   iterations              the number of iterations that were executed
  #   time                    the number of seconds passed to execute the cslogit algorithm
  # -----------------------------------------------------------------------------------------------
  # Written by Sebastiaan Höppner, 2019
  # -----------------------------------------------------------------------------------------------

  # start timer
  t_start <- proc.time()

  # original call
  call <- match.call()

  # check inputs
  check <- check_inputs(formula, data, cost_matrix, lambda, options)
  options <- check$options

  # extract response variable and model matrix
  Y <- hmeasure::relabel(model.response(check$mf))
  X <- model.matrix(formula, data)
  ncolX <- NCOL(X)

  # starting values for the regression parameters
  if (options$print_level > 0) {
    cat("- Get starting values...\n")
  }
  if (is.null(options$start)) {
    options$start <- glm.fit(X, Y, family = binomial(link = "logit"))$coefficients
  } else if (length(options$start) != ncolX) {
    stop(paste("length of 'start' should equal", ncolX))
  }

  # lower bounds for the regression parameters
  if (options$print_level > 0) {
    cat("- Get lower and upper bounds...\n")
  }
  if (is.null(options$lb)) {
    lb <- -max(50, abs(options$start[-1]))
    options$lb <- c(-Inf, rep(lb, ncolX - 1))
  } else {
    if (length(options$lb) == 1) {
      options$lb <- c(-Inf, rep(options$lb, ncolX - 1))
    } else if (length(options$lb) != ncolX) {
      stop(paste("length of 'lb' should equal", ncolX))
    }
    if (any(options$start < options$lb)) {
      stop("at least one element in 'start' < 'lb'")
    }
  }

  # upper bounds for the regression parameters
  if (is.null(options$ub)) {
    ub <- max(50, abs(options$start[-1]))
    options$ub <- c(Inf, rep(ub, ncolX - 1))
  } else {
    if (length(options$ub) == 1) {
      options$ub <- c(Inf, rep(options$ub, ncolX - 1))
    } else if (length(options$ub) != ncolX) {
      stop(paste("length of 'ub' should equal", ncolX))
    }
    if (any(options$start > options$ub)) {
      stop("at least one element in 'start' > 'ub'")
    }
  }

  # rearrange cost matrix
  cost_matrix[Y == 0, ] <- cost_matrix[Y == 0, c(2, 1)]

  # example cost matrix (used by summary.cslogit)
  example_cost_matrix <- matrix(c(cost_matrix[which(Y == 0)[1], c(2, 1)],
                                  cost_matrix[which(Y == 1)[1], c(2, 1)]), nrow = 2, ncol = 2)
  colnames(example_cost_matrix) <- rownames(example_cost_matrix) <- c("0", "1")
  names(dimnames(example_cost_matrix)) <- c("      Prediction", "Reference")

  # fit instance-dependent cost-sensitive logistic regression
  if (options$print_level > 0) {
    cat("- Search for optimal regression parameters...\n\n")
  }
  fit_reap <- reap(nloptr(x0               = options$start,
                          eval_f           = sower(objective_gradient_fun),
                          lb               = options$lb,
                          ub               = options$ub,
                          X                = X,
                          tX               = t(X),
                          cost_matrix      = cost_matrix,
                          diff_cost_matrix = 1/NROW(X) * (cost_matrix[, 1] - cost_matrix[, 2]),
                          lambda           = lambda,
                          opts             = list(
                            algorithm   = paste0("NLOPT_LD_", options$algorithm),
                            maxeval     = options$maxeval,
                            ftol_rel    = options$ftol_rel,
                            xtol_rel    = options$xtol_rel,
                            print_level = ifelse(options$print_level == 2, 3, options$print_level)
                          )))
  fit <- fit_reap[[1]]
  names(fit$solution) <- colnames(X)
  objective_path <- as.numeric(fit_reap[[2]][which(names(fit_reap[[2]]) == "objective")])
  betas_path <- do.call(rbind, fit_reap[[2]][which(names(fit_reap[[2]]) == "betas")])
  rownames(betas_path) <- 1:nrow(betas_path)
  if (fit$status == 5) {
    warning(fit$message) # maxeval was reached
  }
  scores <- as.numeric( 1 / (1 + exp(-fit$solution %*% t(X))) )
  average_expected_cost <- 2 * mean(cost_matrix * matrix(c(scores, 1 - scores), ncol = 2))

  # end timer
  t_end <- proc.time() - t_start
  if (options$print_level > 0) {
    cat(paste("\nTime elapsed:", round(t_end[3], 3), "seconds\n\n"))
  }

  # output
  output <- list(call = call, formula = formula, example_cost_matrix = example_cost_matrix,
                 lambda = lambda, options = options, terms = check$mt,
                 objective_path = objective_path, betas_path = betas_path,
                 fitted_values = scores, average_expected_cost = average_expected_cost,
                 objective = fit$objective, coefficients = fit$solution,
                 status = fit$status, message = fit$message, iterations = fit$iterations,
                 time = round(t_end[3], 3))
  class(output) <- "cslogit"
  return(output)
}
