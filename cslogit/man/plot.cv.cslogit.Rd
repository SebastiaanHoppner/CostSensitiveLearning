\name{plot.cv.cslogit}

\alias{plot.cv.cslogit}

\title{Plot the cross-validation results produced by cv.cslogit.}

\description{Shows the boxplot of cross-validation results for each of the lambda values used as produced by \code{\link{cv.cslogit}}. The results for the optimal \code{lambda} value are highlighted.}

\usage{
\method{plot}{cv.cslogit}(x, \dots)
}

\arguments{
\item{x}{an object of class "\code{cv.cslogit}". A result of a call to the \code{\link{cv.cslogit}} function.}
\item{...}{further arguments passed to or from other methods.}
}

\details{\code{plot.cv.cslogit} shows the boxplot of cross-validation results as a function of the lambda values used.}

\references{Hoppner, S., Baesens, B., Verbeke, W., and Verdonck, T. (2020). Instance- dependent cost-sensitive learning for detecting transfer fraud. \emph{arXiv:2005.02488}}

\author{Sebastiaan Hoppner}


\seealso{\code{\link{cslogit}}, \code{\link{cv.cslogit}}, \code{\link{plot}}}

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
