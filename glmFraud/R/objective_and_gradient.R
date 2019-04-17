objectivefun <- function (betas, intercept_flag, Y, X, amounts, fixed_cost, lambda)
{
  # objective function = average expected cost with L1 penalty
  L1penalty <- ifelse(intercept_flag == 0, sum(abs(betas)), sum(abs(betas[-1])))
  scores <- 1 / (1 + exp(-betas %*% t(X)))
  average_expected_cost <- mean(Y * (1 - scores) * amounts + scores * fixed_cost) + lambda * L1penalty
  return(average_expected_cost)
}



gradientfun <- function (betas, intercept_flag, Y, X, amounts, fixed_cost, lambda)
{
  # gradient of the objective function
  if (intercept_flag == 0)
    sign_vector <- sign(betas)
  else
    sign_vector <- c(0, sign(betas[-1]))
  expterm <- as.numeric(exp(-betas %*% t(X)))
  gradient <- 1/length(Y) * matrix(-Y * amounts + fixed_cost, nrow = 1) %*% (expterm / (1 + expterm)^2 * X) + lambda * sign_vector
  return(gradient)
}
