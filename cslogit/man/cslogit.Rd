\name{cslogit}

\alias{cslogit}

\title{Instance-dependent cost-sensitive logistic regression with lasso regularization}

\description{Fit an instance-dependent cost-sensitive logistic regression model by minimizing the lasso regularized average expected cost.}

\usage{cslogit(formula, data, cost_matrix, lambda, options = list())}

\arguments{
\item{formula}{an object of class "\code{\link{formula}}": a symbolic description of the model to be fitted. An intercept must be included.}

\item{data}{a data frame containing the variables in the model.}

\item{cost_matrix}{a matrix of dimension \code{nrow(data)} x 2. For each instance, the first/second column contains the cost of correctly/incorrectly predicting the binary class of the instance.}

\item{lambda}{value that controls the lasso regularization of the regression coefficients.}

\item{options}{a list of options that control details of the \code{cslogit} algorithm. The following options are available:
\itemize{
\item{\code{algorithm} \cr
algorithm used by the \code{\link{nloptr}} function. Available algorithms are \code{"SLSQP"} (Sequential Least-Squares Quadratic Programming) (default) and \code{"MMA"} (Method of Moving Asymptotes). See the \href{http://ab-initio.mit.edu/wiki/index.php/NLopt_Algorithms}{NLopt website} for more details.}

\item{\code{maxeval} \cr
maximum number of function evaluations (default is 10000).}

\item{\code{ftol_rel} \cr
obtain the minimum of the objective function to within a relative tolerance (default is 1e-8).}

\item{\code{xtol_rel} \cr
obtain optimal regression coefficients to within a relative tolerance (default is 1e-5).}

\item{\code{start} \cr
starting values for the coefficients in the linear predictor. By default a logistic regression model is fitted in order to use the coefficients as starting values.}

\item{\code{lb} \cr
vector with lower bounds of the coefficients (use \code{-Inf} for coefficients without lower bound). By default the intercept is unbounded and the other coefficients have a lower bound of \code{-max(50, abs(options$start[-1]))}. If a single number is provided, the vector is initialized as \code{c(-Inf, rep(options$lb, length(options$start) - 1))}.}

\item{\code{ub} \cr
vector with upper bounds of the coefficients (use \code{Inf} for coefficients without upper bound). By default the intercept is unbounded and the other coefficients have an upper bound of \code{max(50, abs(options$start[-1]))}. If a single number is provided, the vector is initialized as \code{c(Inf, rep(options$ub, length(options$start) - 1))}.}

\item{\code{check_data} \cr
should the data and variables be checked for \code{NA}, \code{Inf}, \code{-Inf} values, etc. (default is \code{TRUE}) or not (\code{FALSE}).}

\item{\code{print_level} \cr
controls how much output is shown during the optimization process. Possible values:
\tabular{ll}{
0 (default) \tab no output \cr
1 \tab show iteration number and value of objective function \cr
2 \tab 1 + show value of of coefficients}}
}
}
}

\details{This method introduces instance-dependent costs into lasso-regularized logistic regression by changing the objective function of the model to one that is cost-sensitive.}

\value{
\code{cslogit} returns an object of class "\code{cslogit}" which is a list containing the following components:
\item{coefficients}{the vector of fitted coefficients.}
\item{objective}{the value of the objective function in the solution.}
\item{average_expected_cost}{the average expected cost of the solution fitted on the supplied data.}
\item{fitted_values}{the fitted probabilities.}
\item{objective_path}{the vector containing the objective value for each iteration.}
\item{betas_path}{the matrix containing the coefficients for each iteration.}
\item{status}{integer value with the status of the optimization.}
\item{message}{character string produced by NLopt and giving additional information.}
\item{iterations}{the number of iterations that were executed.}
\item{time}{the number of seconds passed to execute the cslogit algorithm.}
\item{call}{the matched call.}
\item{formula}{the formula supplied.}
\item{lambda}{the lambda value supplied.}
\item{options}{the list of arguments supplied, with defaults filled in.}
\item{terms}{the terms object used.}
\item{example_cost_matrix}{a 2x2 cost matrix based on the first negative and positive instance in the data (used by the \code{\link{summary.cslogit}} function).}
}

\references{...}

\author{Sebastiaan Höppner}


\seealso{\code{\link{summary.cslogit}}, \code{\link{plot.cslogit}}, \code{\link{predict.cslogit}}}

\examples{
# Provide an example (TODO).
}
