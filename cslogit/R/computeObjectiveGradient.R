computeObjectiveGradient <- function (betas, X, tX, cost_matrix, diff_costs, nrowX, lambda) {
  # objective value = average expected cost + lambda * L1 norm of betas (without intercept)
  scores1 <- 1 / (1 + exp(-as.numeric(betas %*% tX)))
  scores0 <- 1 - scores1
  return(list(objective = sum(cost_matrix * matrix(c(scores1, scores0), ncol = 2)) / nrowX +
                lambda * sum(abs(betas[-1])),
              gradient = (diff_costs * scores1 * scores0) %*% X +
                lambda * c(0, sign(betas[-1])),
              betas = betas))
}
