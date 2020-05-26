\name{cv.cslogit}

\alias{cv.cslogit}

\title{Cross-validation for cslogit}

\description{Cross-validation procedure for cslogit's lambda. The optimal value for lambda is the one with the highest average expected cost over the folds.}

\usage{cv.cslogit(formula, data, cost_matrix, options = list(),
           nfolds = 10L, lambda_path = NULL, seed = NULL)}

\arguments{
\item{formula}{an object of class "\code{\link{formula}}": a symbolic description of the model to be fitted. An intercept must be included.}

\item{data}{a data frame containing the variables in the model.}

\item{cost_matrix}{a matrix of dimension \code{nrow(data)} x 2. For each instance, the first/second column contains the cost of correctly/incorrectly predicting the binary class of the instance.}

\item{nfolds}{number of folds (default is 10).}

\item{lambda_path}{optional user-supplied lambda sequence. Default is \code{NULL} and \code{cslogit} chooses its own sequence.}

\item{seed}{optional positive integer (default is \code{NULL}). Use \code{seed} if you want to use a 'deterministic' sequence of pseudorandom numbers}

\item{options}{a list of options that control details of the \code{cslogit} algorithm. The following options are available (all options are available except \code{print_level}):
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
vector with lower bounds of the coefficients (use \code{-Inf} for coefficients without lower bound). By default the intercept is unbounded and the other coefficients have a lower bound of \code{-max(50, abs(options$start[-1]))}. If a single number is provided, the vector is initialized as \code{c(Inf, rep(options$lb, length(options$start) - 1))}.}

\item{\code{ub} \cr
vector with upper bounds of the coefficients (use \code{Inf} for coefficients without upper bound). By default the intercept is unbounded and the other coefficients have an upper bound of \code{max(50, abs(options$start[-1]))}. If a single number is provided, the vector is initialized as \code{c(-Inf, rep(options$ub, length(options$start) - 1))}.}

\item{\code{check_data} \cr
should the data and variables be checked for \code{NA}, \code{Inf}, \code{-Inf} values, etc. (default is \code{TRUE}) or not (\code{FALSE}).}
}
}
}

\value{
\code{cv.cslogit} returns an object of class "\code{cv.cslogit}" which is a list containing the following components:
\item{optimal_lambda}{the optimal value for \code{lambda} with the highest average expected cost (averaged over the folds).}
\item{cv_results}{the matrix containing the average expected for each fold and each lambda.}
\item{lambda_path}{the used lambda sequence.}
\item{time}{the number of seconds passed to execute the cross-validation.}
\item{call}{the matched call.}
\item{formula}{the formula supplied.}
\item{nfolds}{the number of folds supplied.}
\item{seed}{the seed supplied.}
\item{options}{the list of arguments supplied, with defaults filled in.}
}

\references{Hoppner, S., Baesens, B., Verbeke, W., and Verdonck, T. (2020). Instance- dependent cost-sensitive learning for detecting transfer fraud. \emph{arXiv:2005.02488}}

\author{Sebastiaan Hoppner}


\seealso{\code{\link{plot.cv.cslogit}}}

\examples{
library(cslogit)
data(creditcard)

fixed_cost <- 50
cost_matrix <- matrix(nrow = nrow(creditcard), ncol = 2)
cost_matrix[, 1] <- ifelse(creditcard$Class == 1, fixed_cost, 0)
cost_matrix[, 2] <- ifelse(creditcard$Class == 1, creditcard$Amount, fixed_cost)

# Example 1 - user-speficied lambda_path
cv_results <- cv.cslogit(formula     = Class ~ V1 + V2 + V3 + V4 + V5 + V6 + V7,
                         data        = creditcard,
                         cost_matrix = cost_matrix,
                         nfolds      = 10,
                         lambda_path = c(1e-8, 0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1),
                         seed        = 2020,
                         options     = list(algorithm   = "SLSQP",
                                            maxeval     = 10000,
                                            ftol_rel    = 1e-8,
                                            xtol_rel    = 1e-5,
                                            check_data  = TRUE))
plot(cv_results)

# Example 2 - lambda_path = NULL
# cv_results2 <- cv.cslogit(formula     = Class ~ V1 + V2 + V3 + V4 + V5 + V6 + V7,
#                          data        = creditcard,
#                          cost_matrix = cost_matrix,
#                          nfolds      = 10,
#                          lambda_path = NULL,
#                          seed        = 2020)
# plot(cv_results2)
}
