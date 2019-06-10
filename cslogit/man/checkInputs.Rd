\name{checkInputs}

\alias{checkInputs}

\title{Check input arguments for cslogit}

\description{This function checks the input arguments passed to \code{\link{cslogit}}. If requested, it also checks the provided dataset for \code{NA}, \code{Inf}, \code{-Inf} values, etc.}

\usage{checkInputs(formula, data, cost_matrix, lambda, options = list())}

\arguments{
\item{formula}{an object of class "\code{\link{formula}}": a symbolic description of the model to be fitted. An intercept must be included.}

\item{data}{a data frame containing the variables in the model.}

\item{cost_matrix}{a matrix of dimension \code{nrow(data)} x 2. For each instance, the first/second column contains the cost of correctly/incorrectly predicting the binary class of the instance.}

\item{lambda}{value that controls the lasso penalty of the regression coefficients.}

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
vector with lower bounds of the coefficients (use \code{-Inf} for coefficients without lower bound). By default the intercept is unbounded and the other coefficients have a lower bound of \code{-max(50, abs(options$start[-1]))}. If a single number is provided, the vector is initialized as \code{c(Inf, rep(options$lb, length(options$start) - 1))}.}

\item{\code{ub} \cr
vector with upper bounds of the coefficients (use \code{Inf} for coefficients without upper bound). By default the intercept is unbounded and the other coefficients have an upper bound of \code{max(50, abs(options$start[-1]))}. If a single number is provided, the vector is initialized as \code{c(-Inf, rep(options$ub, length(options$start) - 1))}.}

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

\details{...}

\value{
\code{checkInputs} is called by the \code{\link{cslogit}} function and returns a list containing the following components:
\item{options}{a list of arguments supplied, with defaults filled in.}
\item{mf}{the model frame used by \code{cslogit}.}
\item{mt}{the terms object used by \code{cslogit}.}
}

\references{...}

\author{Sebastiaan Höppner}


\seealso{\code{\link{cslogit}}}

\examples{
# Provide an example (TODO).
}
