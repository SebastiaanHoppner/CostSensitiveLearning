\name{print.performance}

\alias{print.performance}

\title{Prints computed binary classification performance metrics}

\description{Prints several computed binary classification performance metrics as a result of the \code{\link{performance}} function.}

\usage{
\method{print}{performance}(x, \dots)
}

\arguments{
\item{x}{an object of class "\code{performance}". A result of a call to the \code{\link{performance}} function.}
\item{...}{further arguments passed to or from other methods.}
}

\details{
\code{print.performance} prints the result of the \code{\link{performance}} function.
}

\author{Sebastiaan Hoppner}

\seealso{\code{\link{performance}}, \code{\link{plot.performance}}, \code{\link{findOptimalThreshold}}}

\examples{
# library(PerformanceMetrics)
# data(creditcard)
#
# logit <- glm(Class ~ V1 + V2, data = creditcard, family = "binomial")
# probs <- predict(logit, newdata = creditcard, type = "response")
# my_threshold <- 0.5
# preds <- ifelse(probs > my_threshold, 1, 0)
#
# table(creditcard$Class)
# table(preds)
#
# # Example 1 - without cost matrix:
# perf <- performance(scores = probs,
#                     predicted_classes = preds,
#                     true_classes = creditcard$Class,
#                     plot = FALSE)
# print(perf)
# plot(perf)
#
# # Example 2 - with cost matrix:
# fixed_cost <- 50
# cost_matrix <- matrix(nrow = nrow(creditcard), ncol = 2)
# cost_matrix[, 1] <- ifelse(creditcard$Class == 1, fixed_cost, 0)
# cost_matrix[, 2] <- ifelse(creditcard$Class == 1, creditcard$Amount, fixed_cost)
#
# performance(scores = probs,
#             predicted_classes = preds,
#             true_classes = creditcard$Class,
#             cost_matrix = cost_matrix)
}

