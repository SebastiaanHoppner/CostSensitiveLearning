\name{costPerformance}

\alias{costPerformance}

\title{Compute cost-related performance metrics}

\description{Computes several cost-related performance metrics.}

\usage{costPerformance(scores, predicted_classes, true_classes, cost_matrix)}

\arguments{
\item{scores}{a vector of predicted probabilities.}

\item{predicted_classes}{a vector of predicted labels. This can be either a factor or in numeric form.}

\item{true_classes}{a vector of true labels. This can be either a factor or in numeric form.}

\item{cost_matrix}{a matrix of dimension \code{nrow(data)} x 2. For each instance, the first/second column contains the cost of correctly/incorrectly predicting the binary class of the instance.}
}

\details{...}

\value{
The \code{costPerformance} function returns a data frame containing the following components:
\tabular{l}{
total fraud amount \cr
detected fraud amount \cr
detected fraud amount ratio \cr
cost no model \cr
minimum cost \cr
maximum savings \cr
total cost \cr
average cost \cr
expected total cost \cr
average expected cost \cr
savings \cr
expected savings
}
}

\references{...}

\author{Sebastiaan Höppner}


\seealso{\code{\link{performance}}}

\examples{
# Provide an example (TODO).
}
