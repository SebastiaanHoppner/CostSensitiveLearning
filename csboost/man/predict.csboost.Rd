\name{predict.csboost}

\alias{predict.csboost}

\title{Predict method for csboost fits}

\description{Obtains predictions from a fitted \code{csboost} object.}

\usage{\method{predict}{csboost}(object, newdata, ntreelimit = NULL, \dots)}

\arguments{
\item{object}{a fitted object of class "\code{csboost}".}
\item{newdata}{a data frame in which to look for variables with which to predict.}
\item{ntreelimit}{limit the number of model's trees or boosting iterations used in prediction. It will use all the trees by default (\code{NULL} value).}
\item{...}{further arguments passed to \code{predict.xgb.Booster}.}
}

\details{...}

\value{
\code{predict.csboost} returns a vector of predicted probabilities.
}

\references{...}

\author{Sebastiaan Höppner}


\seealso{\code{\link{csboost}}, \code{\link{predict}}}

\examples{
# Provide an example (TODO).
}
