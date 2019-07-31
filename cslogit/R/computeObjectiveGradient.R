computeObjectiveGradient <- function (betas, lambda, X, tX, cost_matrix_col2, diff_costs) {
  # objective value = average expected cost + lambda * L1 norm of betas (without intercept)
  scores1 <- 1 / (1 + exp(-as.numeric(betas %*% tX)))
  return(list(objective = sum(cost_matrix_col2 + scores1 * diff_costs) +
                lambda * sum(abs(betas[-1])),
              gradient = (diff_costs * (scores1 * (1 - scores1))) %*% X +
                lambda * c(0, sign(betas[-1])),
              betas = betas))
}
