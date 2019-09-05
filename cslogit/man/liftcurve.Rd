\name{liftcurve}

\alias{liftcurve}

\title{Plot gains, lift and costs curves}

\description{Three plots (selectable by \code{which}) are available: gains curve, lift curve and costs curve}

\usage{liftcurve(scores, true_classes, cost_matrix = NULL, show = TRUE, which = "all")}

\arguments{
\item{scores}{a vector of predicted probabilities.}
\item{true_classes}{a vector of true labels. This can be either a factor or in numeric form.}
\item{cost_matrix}{a matrix of dimension \code{nrow(data)} x 2. For each instance, the first/second column contains the cost of correctly/incorrectly predicting the binary class of the instance.}
\item{show}{should the curves be plotted (default is \code{TRUE}) or not (\code{FALSE}).}
\item{which}{string indicating which plot to show. Available options are
\tabular{ll}{
\code{"gains"} \tab plot gains curve \cr
\code{"lift"} \tab plot lift curve \cr
\code{"costs"} \tab plot costs curve \cr
\code{"all"} (default) \tab show all three plots.}
}
}

\details{\code{liftcurve} plots gains, lift and costs curves.}

\value{
\code{liftcurve} returns a list containing the following components:
\item{auc_gains}{the area under the gains curve relative to the area under the "perfect" gains curve.}
\item{auc_lift}{the area under the lift curve relative to the area under the "perfect" lift curve.}
\item{auc_costs}{the area under the costs curve.}
}

\references{...}

\author{Sebastiaan Höppner}

\examples{
# Provide an example (TODO).
}
