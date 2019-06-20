cslogit <- function (formula, data, cost_matrix, lambda, options = list()) {
  # start timer
  t_start <- proc.time()

  # original call
  call <- match.call()

  # check inputs
  check <- checkInputs(formula, data, cost_matrix, lambda, options)
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
  fit_reap <- reap(nloptr(x0          = options$start,
                          eval_f      = sower(computeObjectiveGradient),
                          lb          = options$lb,
                          ub          = options$ub,
                          X           = X,
                          tX          = t(X),
                          cost_matrix = cost_matrix,
                          diff_costs  = (cost_matrix[, 1] - cost_matrix[, 2]) / NROW(X),
                          nrowX       = NROW(X),
                          lambda      = lambda,
                          opts        = list(
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
  scores <- 1 / (1 + exp(-as.numeric(fit$solution %*% t(X))))
  average_expected_cost <- fit$objective - lambda * sum(abs(fit$solution[-1]))

  # end timer
  t_end <- proc.time() - t_start
  if (options$print_level > 0) {
    cat(paste("\nTime elapsed:", round(t_end[3], 3), "seconds\n\n"))
  }

  # output
  output <- list(coefficients          = fit$solution,
                 objective             = fit$objective,
                 average_expected_cost = average_expected_cost,
                 fitted_values         = scores,
                 objective_path        = objective_path,
                 betas_path            = betas_path,
                 status                = fit$status,
                 message               = fit$message,
                 iterations            = fit$iterations,
                 time                  = round(t_end[3], 3),
                 call                  = call,
                 formula               = formula,
                 lambda                = lambda,
                 options               = options,
                 terms                 = check$mt,
                 example_cost_matrix   = example_cost_matrix)
  class(output) <- "cslogit"
  return(output)
}
