objective_gradient_fun <- function (betas, X, tX, cost_matrix, diff_cost_matrix, lambda)
{
  # Objective function and gradient used by cslogit
  # -----------------------------------------------------------------------------------------------
  # Arguments:
  #   betas              vector with coefficients for the logistic regression model
  #   X                  model matrix
  #   tX                 tranpose of model matrix
  #   cost_matrix        a matrix of dimension nrow(data) x 2:
  #                      for each instance, the first/second column contains the cost of
  #                      predicting the binary class of the instance as positive/negative
  #   diff_cost_matrix   matrix used for calculting the gradient
  #   lambda             value that controls the L1-penalty of the regression coefficients
  # -----------------------------------------------------------------------------------------------
  # Value:
  #   objective_gradient_fun is called by cslogit and returns a list containing the following:
  #   objective   objective value = average expected cost + lambda * L1-penalty evaluated at betas
  #   gradient    gradient vector evaluated at betas
  #   betas       the betas vector supplied
  # -----------------------------------------------------------------------------------------------
  # Written by Sebastiaan Höppner, 2019
  # -----------------------------------------------------------------------------------------------

  expterm <- as.numeric(exp(-betas %*% tX))
  if (any(is.infinite(expterm))) {
    expterm[is.infinite(expterm)] <- .Machine$double.xmax
  }
  scores <- 1 / (1 + expterm)

  return(list(objective = 2 * mean(cost_matrix * matrix(c(scores, 1 - scores), ncol = 2)) +
                lambda * sum(abs(betas[-1])),
              gradient =  diff_cost_matrix %*%
                (expterm / (1 + expterm)^2 * X) + lambda * c(0, sign(betas[-1])),
              betas = betas))
}
