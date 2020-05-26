\name{summary.csboost}

\alias{summary.csboost}

\title{Summarizing csboost fits}

\description{The function is a method for class \code{csboost} objects that summarizes their fit.}

\usage{
\method{summary}{csboost}(object, \dots)
}

\arguments{
\item{object}{an object of class "\code{csboost}". A result of a call to the \code{csboost} function.}
\item{...}{further arguments passed to or from other methods.}
}

\details{\code{summary.csboost} prints a summary of the "\code{csboost}" object.}

\references{Hoppner, S., Baesens, B., Verbeke, W., and Verdonck, T. (2020). Instance- dependent cost-sensitive learning for detecting transfer fraud. \emph{arXiv:2005.02488}}

\author{Sebastiaan Hoppner}


\seealso{\code{\link{csboost}}, \code{\link{summary}}}

\examples{
library(csboost)
data(creditcard)

fixed_cost <- 50
cost_matrix <- matrix(nrow = nrow(creditcard), ncol = 2)
cost_matrix[, 1] <- ifelse(creditcard$Class == 1, fixed_cost, 0)
cost_matrix[, 2] <- ifelse(creditcard$Class == 1, creditcard$Amount, fixed_cost)

i0 <- which(creditcard$Class == 0)
i1 <- which(creditcard$Class == 1)

set.seed(2020)
i0_train <- sample(i0, size = 0.7 * length(i0))
i1_train <- sample(i1, size = 0.7 * length(i1))

train <- creditcard[ c(i0_train, i1_train), ]
test  <- creditcard[-c(i0_train, i1_train), ]

cost_matrix_train <- cost_matrix[ c(i0_train, i1_train), ]
cost_matrix_test  <- cost_matrix[-c(i0_train, i1_train), ]

csbtree <- csboost(formula               = Class ~ . - 1,
                   train                 = train,
                   test                  = test,
                   cost_matrix_train     = cost_matrix_train,
                   cost_matrix_test      = cost_matrix_test,
                   nrounds               = 300,
                   early_stopping_rounds = 20,
                   verbose               = 1,
                   print_every_n         = 1)

summary(csbtree)
plot(csbtree)
predict(csbtree, newdata = test)
}
