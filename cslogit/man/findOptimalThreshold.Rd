\name{findOptimalThreshold}

\alias{findOptimalThreshold}

\title{Find optimal decision threshold}

\description{Find the optimal decision threshold as the threshold that optimizes a chosen metric (see \code{\link{performance}} and \code{\link{costPerformance}}).}

\usage{findOptimalThreshold(scores, true_classes, metric, maximize, thresholds,
                     cost_matrix = NULL, show = TRUE)}

\arguments{
\item{scores}{a vector of predicted probabilities.}

\item{true_classes}{a vector of true labels. This can be either a factor or in numeric form.}

\item{metric}{classification metric for which to find the optimal threshold. Options are:\cr
\code{"alerts"},
\code{"alert_rate"}, \code{"TP"}, \code{"FP"}, \code{"TN"}, \code{"FN"}, \code{"TPR"}, \code{"FPR"},
\code{"ER"}, \code{"Precision"}, \code{"Recall"} and \code{"F1"}.\cr
If \code{cost_matrix} is supplied, then options for cost-related metrics are:\cr
\code{"detected_fraud_amount"}, \code{"detected_fraud_amount_ratio"}, \code{"cost"},
\code{"average_cost"} and \code{"savings".}}

\item{maximize}{should the optimal threshold correspond with the maximum (\code{TRUE}) or minimum (\code{FALSE}) value of the metric.}

\item{thresholds}{sequence of threshold values.}

\item{cost_matrix}{a matrix of dimension \code{nrow(data)} x 2 (if provided). For each instance, the first/second column contains the cost of correctly/incorrectly predicting the binary class of the instance. Only needed if argument \code{metric} is a cost-related metric (default is NULL).}

\item{show}{should the computed savings values be plotted against their corresponding thresholds (default is \code{TRUE}) or not (\code{FALSE}).}
}

\details{...}

\value{
The \code{findOptimalThreshold} function returns a list containing the following components:
\item{optimal_threshold}{the optimal threshold with the highest savings.}
\item{savings}{the savings values matching the supplied thresholds.}
\item{call}{the matched call.}
\item{thresholds}{the sequence of thresholds supplied.}
}

\references{...}

\author{Sebastiaan Höppner}


\seealso{\code{\link{performance}}, \code{\link{costPerformance}}}

\examples{
# Provide an example (TODO).
}
