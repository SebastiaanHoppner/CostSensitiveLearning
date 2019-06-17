computeObjectiveGradient <- function (betas, X, tX, cost_matrix, diff_cost_matrix, nrowX, lambda) {
  # objective value = average expected cost + lambda * L1 norm of betas (without intercept)
  expterm <- as.numeric(exp(-betas %*% tX))
  if (any(is.infinite(expterm))) {
    expterm[is.infinite(expterm)] <- .Machine$double.xmax
  }
  scores1 <- 1 / (1 + expterm)
  scores0 <- 1 - scores1

  return(list(objective = sum(cost_matrix * matrix(c(scores1, scores0), ncol = 2)) / nrowX +
                lambda * sum(abs(betas[-1])),
              gradient = (diff_cost_matrix * scores1 * scores0) %*% X +
                lambda * c(0, sign(betas[-1])),
              betas = betas))
}
