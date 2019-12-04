\name{findOptimalThreshold}

\alias{findOptimalThreshold}

\title{Find optimal decision threshold}

\description{Find the optimal decision threshold as the threshold that optimizes a chosen metric (see \code{\link{performance}} and \code{\link{costPerformance}}).}

\usage{findOptimalThreshold(scores, true_classes, thresholds, metric,
                     cost_matrix = NULL, show = TRUE)}

\arguments{
\item{scores}{a vector of predicted probabilities.}

\item{true_classes}{a vector of true labels. This can be either a factor or in numeric form.}

\item{metric}{classification metric for which to find the optimal threshold. Options are:\cr
\code{"F1"}, \code{"ER"}, \code{"TPR"}, \code{"FPR"}, \code{"Precision"} and \code{"Recall"}.\cr
If \code{cost_matrix} is supplied, then options for cost-related metrics are:\cr
\code{"savings"}, \code{"cost"}, \code{"average_cost"}\code{"detected_fraud_amount"} and \code{"detected_fraud_amount_ratio"}.}

\item{thresholds}{sequence of threshold values.}

\item{cost_matrix}{a matrix of dimension \code{nrow(data)} x 2 (if provided). For each instance, the first/second column contains the cost of correctly/incorrectly predicting the binary class of the instance. Only needed if argument \code{metric} is a cost-related metric (default is NULL).}

\item{show}{should the computed savings values be plotted against their corresponding thresholds (default is \code{TRUE}) or not (\code{FALSE}).}
}

\details{...}

\value{
The \code{findOptimalThreshold} function returns a list containing the following components:
\item{call}{the matched call.}
\item{thresholds}{the sequence of thresholds supplied.}
\item{measures}{the metric values matching the supplied thresholds.}
\item{optimal_threshold}{the optimal threshold.}
\item{optimal_measure}{the optimal metric value.}
}

\references{...}

\author{Sebastiaan Höppner}


\seealso{\code{\link{performance}}, \code{\link{costPerformance}}}

\examples{
# Provide an example (TODO).
}
