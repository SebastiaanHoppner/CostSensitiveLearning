\name{computeObjectiveGradient}

\alias{computeObjectiveGradient}

\title{Objective function and gradient used by cslogit}

\description{Returns the value of the objective function and its gradient, used by \code{\link{cslogit}}. The objection function of the \code{cslogit} algorithm is the lasso penalized average expected cost.}

\usage{computeObjectiveGradient(betas, X, tX, cost_matrix, diff_costs, nrowX, lambda)}

\arguments{
\item{betas}{vector with coefficients for the logistic regression model.}
\item{X}{model matrix.}
\item{tX}{transpose of model matrix.}
\item{cost_matrix}{a matrix of dimension \code{nrow(data)} x 2. For each instance, the first/second column contains the cost of predicting the binary class of the instance as positive/negative}
\item{diff_costs}{vector used for calculting the gradient: \cr
\code{(cost_matrix[, 1] - cost_matrix[, 2]) / NROW(X)}.}
\item{nrowX}{number of rows of the model matrix.}
\item{lambda}{value that controls the lasso penalty of the regression coefficients.}
}

\details{...}

\value{
\code{computeObjectiveGradient} is called by \code{cslogit} and returns a list containing the following components:
\item{objective}{objective value = average expected cost + lambda * lasso penalty, evaluated at betas.}
\item{gradient}{gradient vector evaluated at betas.}
\item{betas}{the betas vector supplied.}
}

\references{...}

\author{Sebastiaan Höppner}


\seealso{\code{\link{cslogit}}}

\examples{
# Provide an example (TODO).
}
