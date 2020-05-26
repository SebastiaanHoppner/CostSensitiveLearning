\name{print.findOptimalThreshold}

\alias{print.findOptimalThreshold}

\title{Prints optimal threshold and corresponding classification performance}

\description{Prints optimal threshold and corresponding classification performance as a result of the \code{\link{findOptimalThreshold}}function.}

\usage{
\method{print}{findOptimalThreshold}(x, \dots)
}

\arguments{
\item{x}{an object of class "\code{\link{findOptimalThreshold}}". A result of a call to the \code{\link{findOptimalThreshold}} function.}
\item{...}{further arguments passed to or from other methods.}
}

\details{
\code{print.findOptimalThreshold} prints the result of the \code{\link{findOptimalThreshold}} function.
}

\author{Sebastiaan Hoppner}

\seealso{\code{\link{findOptimalThreshold}}, \code{\link{plot.findOptimalThreshold}}, \code{\link{performance}}}

\examples{
library(PerformanceMetrics)
data(creditcard)

logit <- glm(Class ~ V1 + V2, data = creditcard, family = "binomial")
probs <- predict(logit, newdata = creditcard, type = "response")
my_threshold <- 0.5
preds <- ifelse(probs > my_threshold, 1, 0)

table(creditcard$Class)
table(preds)

# Example 1 - without cost matrix:
perf <- performance(scores = probs,
                    predicted_classes = preds,
                    true_classes = creditcard$Class,
                    plot = FALSE)
print(perf)
plot(perf)

# Example 2 - with cost matrix:
fixed_cost <- 50
cost_matrix <- matrix(nrow = nrow(creditcard), ncol = 2)
cost_matrix[, 1] <- ifelse(creditcard$Class == 1, fixed_cost, 0)
cost_matrix[, 2] <- ifelse(creditcard$Class == 1, creditcard$Amount, fixed_cost)

performance(scores = probs,
            predicted_classes = preds,
            true_classes = creditcard$Class,
            cost_matrix = cost_matrix)
}

