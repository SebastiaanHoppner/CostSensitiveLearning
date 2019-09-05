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

\references{...}

\author{Sebastiaan Höppner}


\seealso{\code{\link{cslogit}}, \code{\link{plot}}}

\examples{
# Provide an example (TODO).
}
