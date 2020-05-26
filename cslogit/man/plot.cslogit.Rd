\name{plot.cslogit}

\alias{plot.cslogit}

\title{Plot method for cslogit fits}

\description{Two plots (selectable by \code{which}) are available: a plot of objective values against iteration index, and a plot of estimated regression parameters against iteration index.}

\usage{
\method{plot}{cslogit}(x, which = "all", show_legend = FALSE, legend_position = NULL, \dots)
}

\arguments{
\item{x}{a fitted object of class "\code{cslogit}".}
\item{which}{string indicating which plot to show. Available options are
\tabular{ll}{
\code{"objective"} \tab plot objective value versus iteration \cr
\code{"coefficients"} \tab plot estimated regression parameters versus iteration \cr
\code{"all"} (default) \tab show both plots.}
}
\item{show_legend}{should a legend with the variable names be added to the \code{"coefficients"} plot (default is \code{FALSE}).}
\item{legend_position}{string indicating the position of the legend (if any). By default it is verified whether \code{"bottomleft"} or \code{"topleft"} is the best position.}
\item{...}{further arguments passed to or from other methods.}
}

\details{\code{plot.cslogit} plots the evolution of the objective value and regression parameters.}

\references{Hoppner, S., Baesens, B., Verbeke, W., and Verdonck, T. (2020). Instance- dependent cost-sensitive learning for detecting transfer fraud. \emph{arXiv:2005.02488}}

\author{Sebastiaan Hoppner}


\seealso{\code{\link{cslogit}}, \code{\link{plot}}}

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
