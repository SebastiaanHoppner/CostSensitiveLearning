\name{findOptimalThreshold}

\alias{findOptimalThreshold}

\title{Find optimal decision threshold}

\description{Find the optimal decision threshold as the threshold that maximizes the savings metrics (see \code{\link{costPerformance}}).}

\usage{findOptimalThreshold(scores, true_classes, cost_matrix, thresholds, show = TRUE)}

\arguments{
\item{scores}{a vector of predicted probabilities.}

\item{true_classes}{a vector of true labels. This can be either a factor or in numeric form.}

\item{cost_matrix}{a matrix of dimension \code{nrow(data)} x 2. For each instance, the first/second column contains the cost of correctly/incorrectly predicting the binary class of the instance.}

\item{thresholds}{sequence of threshold values.}

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


\seealso{\code{\link{costPerformance}}}

\examples{
# Provide an example (TODO).
}
