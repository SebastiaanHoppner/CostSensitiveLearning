\name{predict.cslogit}

\alias{predict.cslogit}

\title{Predict method for cslogit fits}

\description{Obtains predictions from a fitted \code{cslogit} object.}

\usage{
\method{predict}{cslogit}(object, newdata = NULL, \dots)
}

\arguments{
\item{object}{a fitted object of class "\code{cslogit}".}
\item{newdata}{optionally, a data frame in which to look for variables with which to predict. If omitted, the fitted coefficients are used.}
\item{...}{further arguments passed to or from other methods.}
}

\value{
\code{predict.cslogit} returns a vector of predicted probabilities.
}

\references{Hoppner, S., Baesens, B., Verbeke, W., and Verdonck, T. (2020). Instance- dependent cost-sensitive learning for detecting transfer fraud. \emph{arXiv:2005.02488}}

\author{Sebastiaan Hoppner}


\seealso{\code{\link{cslogit}}, \code{\link{predict}}}

\examples{
library(cslogit)
data(creditcard)

fixed_cost <- 50
cost_matrix <- matrix(nrow = nrow(creditcard), ncol = 2)
cost_matrix[, 1] <- ifelse(creditcard$Class == 1, fixed_cost, 0)
cost_matrix[, 2] <- ifelse(creditcard$Class == 1, creditcard$Amount, fixed_cost)

model <- cslogit(formula     = Class ~ V1 + V2 + V3 + V4 + V5 + V6 + V7,
                 data        = creditcard,
                 cost_matrix = cost_matrix,
                 lambda      = 0.01,
                 options     = list(algorithm   = "SLSQP",
                                    maxeval     = 10000,
                                    ftol_rel    = 1e-8,
                                    xtol_rel    = 1e-5,
                                    check_data  = TRUE,
                                    print_level = 1))
summary(model)
plot(model, show_legend = TRUE, legend_position = "topleft")
plot(model, which = "objective")
plot(model, which = "coefficients")
predict(model, newdata = creditcard)
}
