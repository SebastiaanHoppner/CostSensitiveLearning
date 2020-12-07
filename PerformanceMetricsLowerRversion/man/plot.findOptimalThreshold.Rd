\name{plot.findOptimalThreshold}

\alias{plot.findOptimalThreshold}

\title{Plots the computed metric values versus the thresholds and shows optimal threshold}

\description{Plots the results of the \code{\link{findOptimalThreshold}} function: the computed metric values versus the thresholds and shows optimal threshold.}

\usage{
\method{plot}{findOptimalThreshold}(x, \dots)
}

\arguments{
\item{x}{an object of class "\code{\link{findOptimalThreshold}}".}
\item{...}{further arguments passed to or from other methods.}
}

\details{\code{plot.findOptimalThreshold} plots ROC curve, precision-recall curve, densities and confusion matrix for binary classification.}

\author{Sebastiaan Hoppner}


\seealso{\code{\link{findOptimalThreshold}}, \code{\link{performance}}}

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

