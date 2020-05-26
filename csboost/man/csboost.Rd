\name{csboost}

\alias{csboost}

\title{Instance-dependent cost-sensitive extreme gradient boosting}

\description{Instance-dependent cost-sensitive extreme gradient boosting.}

\usage{csboost(formula, train, test = NULL,
        cost_matrix_train, cost_matrix_test = NULL,
        nrounds, params = list(),
        verbose = 1, print_every_n = 1L, early_stopping_rounds = NULL,
        save_period = NULL, save_name = "xgboost.model",
        xgb_model = NULL, ...)}

\arguments{
\item{formula}{an object of class "\code{\link{formula}}": a symbolic description of the model to be fitted. An intercept can not be included.}

\item{train}{a training set as a data frame containing the variables in the model.}

\item{test}{a test set (if provided) as a data frame containing the variables in the model (default is \code{NULL}).}

\item{cost_matrix_train}{a matrix of dimension \code{nrow(train)} x 2. For each instance, the first/second column contains the cost of correctly/incorrectly predicting the binary class of the instance.}

\item{cost_matrix_test}{a matrix of dimension \code{nrow(test)} x 2 (if provided). For each instance, the first/second column contains the cost of correctly/incorrectly predicting the binary class of the instance (default is \code{NULL}).}

\item{nrounds}{max number of boosting iterations.}

\item{params}{the list of parameters. The complete list of parameters is available at \href{https://xgboost.readthedocs.io/en/latest/parameter.html}{https://xgboost.readthedocs.io/en/latest/parameter.html} A short summary is available in the documentation \code{help(xgb.train)}.}

\item{verbose}{If 0, xgboost will stay silent. If 1, it will print information about performance. If 2, some additional information will be printed out.}

\item{print_every_n}{Print each n-th iteration evaluation messages when \code{verbose > 0}. Default is 1 which means all messages are printed.}

\item{early_stopping_rounds}{If NULL, the early stopping function is not triggered. If set to an integer k, training with a validation set will stop if the performance doesn't improve for k rounds.}

\item{save_period}{when it is non-NULL, model is saved to disk after every save_period rounds, 0 means save at the end.}

\item{save_name}{the name or path for periodically saved model file.}

\item{xgb_model}{a previously built model to continue the training from. Could be either an object of class \code{xgb.Booster}, or its raw data, or the name of a file with a previously saved model.}

\item{...}{other parameters to pass to \code{params}.}

}

\details{This method introduces instance-dependent costs into extreme gradient boosting by changing the objective function of the model to one that is cost-sensitive.}

\value{
\code{csboost} returns an object of class "\code{csboost}" which is a list containing the following components:
\item{call}{the matched call.}
\item{time}{the number of seconds passed to execute the csboost algorithm.}
\item{xgbmodel}{an object of class \code{xgb.Booster} with the following elements:}
\itemize{
  \item \code{handle} \cr a handle (pointer) to the xgboost model in memory.
  \item \code{raw} \cr a cached memory dump of the xgboost model saved as R's \code{raw} type.
  \item \code{niter} \cr number of boosting iterations.
  \item \code{evaluation_log} \cr evaluation history stored as a \code{data.table} with the
        first column corresponding to iteration number and the rest corresponding to evaluation
        metrics' values.
  \item \code{call} \cr a function call.
  \item \code{params} \cr parameters that were passed to the xgboost library.
  \item \code{callbacks} \cr callback functions that were either automatically assigned or
        explicitly passed.
  \item \code{best_iteration} \cr iteration number with the best evaluation metric value
        (only available with early stopping).
  \item \code{best_ntreelimit} the \code{ntreelimit} value corresponding to the best iteration,
        which could further be used in \code{predict} method
        (only available with early stopping).
  \item \code{best_score} \cr the best evaluation metric value during early stopping.
        (only available with early stopping).
  \item \code{feature_names} \cr names of the training dataset features
        (only when column names were defined in training data).
  \item \code{nfeatures} \cr number of features in training data.
}
}

\references{Hoppner, S., Baesens, B., Verbeke, W., and Verdonck, T. (2020). Instance- dependent cost-sensitive learning for detecting transfer fraud. \emph{arXiv:2005.02488}}

\author{Sebastiaan Hoppner}

\seealso{\code{\link{summary.csboost}}, \code{\link{plot.csboost}}, \code{\link{predict.csboost}}}


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
