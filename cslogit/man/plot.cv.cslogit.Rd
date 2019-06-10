\name{plot.cv.cslogit}

\alias{plot.cv.cslogit}

\title{Plot the cross-validation results produced by cv.cslogit.}

\description{Shows the boxplot of cross-validation results for each of the lambda values used as produced by \code{\link{cv.cslogit}}. The results for the optimal \code{lambda} value are highlighted.}

\usage{
\method{plot}{cv.cslogit}(x, \dots)
}

\arguments{
\item{x}{an object of class "\code{cv.cslogit}". A result of a call to the \code{\link{cv.cslogit}} function.}
\item{...}{further arguments passed to or from other methods.}
}

\details{\code{plot.cv.cslogit} shows the boxplot of cross-validation results as a function of the lambda values used.}

\references{...}

\author{Sebastiaan Höppner}


\seealso{\code{\link{cslogit}}, \code{\link{predict}}}

\examples{
# Provide an example (TODO).
}
