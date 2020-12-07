\name{findOptimalThreshold}

\alias{findOptimalThreshold}

\title{Finds optimal decision threshold}

\description{Finds the optimal decision threshold as the threshold that optimizes a chosen metric (see \code{\link{performance}}).}

\usage{findOptimalThreshold(scores, true_classes, thresholds = NULL, metric,
                     cost_matrix = NULL, plot = TRUE)}

\arguments{
\item{scores}{a vector of predicted probabilities.}

\item{true_classes}{a vector of true labels. This can be either a factor or in numeric form.}

\item{metric}{classification metric for which to find the optimal threshold. Options are:\cr
\code{"F1"}, \code{"ER"}, \code{"Accuracy"}, \code{"Youden"}, \code{"TPR"}, \code{"FPR"}, \code{"HitRate"}, \code{"Precision"}, \code{"Recall"}, \code{"Sensitivity"}, \code{"Specificity"}, \code{"TP"}, \code{"FP"}, \code{"TN"}, \code{"FN"}, \code{"Alerts"} and \code{"AlertRate"}.\cr
If \code{cost_matrix} is supplied, then options for cost-related metrics are:\cr
\code{"Savings"}, \code{"Cost"} and \code{"AverageCost"}.}

\item{thresholds}{sequence of threshold values inside the unit interval or a positive integer. If \code{NULL} (default) than all unique, provided scores are used as thresholds. If a positive integer is provided, than some of the scores are selected as thresholds respecting the distribution of scores.}

\item{cost_matrix}{a matrix of dimension \code{nrow(data)} x 2 (if provided). For each instance, the first/second column contains the cost of correctly/incorrectly predicting the binary class of the instance. Only needed if argument \code{metric} is a cost-related metric (default is NULL).}

\item{plot}{should the computed metric values be plotted against their corresponding thresholds (default is \code{TRUE}) or not (\code{FALSE}).}
}

\value{
The \code{findOptimalThreshold} function returns a list containing the following components:
\item{call}{the matched call.}
\item{thresholds}{the sequence of thresholds supplied.}
\item{measures}{the metric values matching the supplied thresholds.}
\item{optimal_threshold}{the optimal threshold.}
\item{optimal_measure}{the optimal metric value.}
}

\references{Hoppner, S., Baesens, B., Verbeke, W., and Verdonck, T. (2020). Instance- dependent cost-sensitive learning for detecting transfer fraud. \emph{arXiv:2005.02488}

Bahnsen, A. C., Aouada, D., Stojanovic, A., and Ottersten, B. (2016). Feature engineering strategies for credit card fraud detection. \emph{Expert Systems with Applications}, 51:134–142.

Hand, D.J. (2010). Evaluating diagnostic tests: the area under the ROC curve and the balance of errors. \emph{Statistics in Medicine}, 29, 1502–1510.
}

\author{Sebastiaan Hoppner}

\seealso{\code{\link{plot.findOptimalThreshold}}, \code{\link{performance}}}

\examples{
# library(PerformanceMetrics)
# data(creditcard)
#
# logit <- glm(Class ~ V1 + V2, data = creditcard, family = "binomial")
# probs <- predict(logit, newdata = creditcard, type = "response")
#
# # Example 1 - Error Rate - no thresholds specified:
# optimum <- findOptimalThreshold(scores = probs,
#                                 true_classes = creditcard$Class,
#                                 thresholds = NULL,
#                                 metric = "ER",
#                                 plot = FALSE)
# print(optimum)
# plot(optimum)
# print(head(data.frame(thresholds = optimum$thresholds,
#                       measures   = optimum$thresholds)))
#
# # Example 2 - F1 measure - no thresholds specified:
# findOptimalThreshold(scores = probs,
#                      true_classes = creditcard$Class,
#                      thresholds = NULL,
#                      metric = "F1",
#                      plot = TRUE)
#
# # Example 3 - Savings - thresholds specified:
# fixed_cost <- 50
# cost_matrix <- matrix(nrow = nrow(creditcard), ncol = 2)
# cost_matrix[, 1] <- ifelse(creditcard$Class == 1, fixed_cost, 0)
# cost_matrix[, 2] <- ifelse(creditcard$Class == 1, creditcard$Amount, fixed_cost)
#
# findOptimalThreshold(scores = probs,
#                      true_classes = creditcard$Class,
#                      thresholds = c(0.01, 0.02, seq(0.05, 0.95, 0.05), 0.99),
#                      metric = "Savings",
#                      cost_matrix = cost_matrix)
#
# # Example 4 - Cost - number of thresholds specified:
# findOptimalThreshold(scores = probs,
#                     true_classes = creditcard$Class,
#                     thresholds = 50,
#                     metric = "Cost",
#                     cost_matrix = cost_matrix)
}
