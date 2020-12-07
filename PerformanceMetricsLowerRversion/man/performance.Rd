\name{performance}

\alias{performance}

\title{Computes binary classification performance metrics}

\description{Computes several binary classification performance metrics.}

\usage{performance(scores, predicted_classes, true_classes, cost_matrix = NULL, plot = TRUE)}

\arguments{
\item{scores}{a vector of predicted probabilities.}
\item{predicted_classes}{a vector of predicted labels. This can be either a factor or in numeric form. It is converted to the default choice of numeric 0s and 1s according to a specific set of conventions. Must contain at most two classes and no missing values.}
\item{true_classes}{a vector of true labels. This can be either a factor or in numeric form. It is converted to the default choice of numeric 0s and 1s according to a specific set of conventions. Must contain at most two classes, at least one instance of each class, and no missing values.}
\item{cost_matrix}{an (optional) matrix of dimension \code{length(scores)} x 2 (default is \code{NULL}).
For each instance, the first/second column contains the cost of correctly/incorrectly predicting the binary class of the instance.}
\item{plot}{should the ROC, ROCCH, precision-recall curve, densities and confusion matrix be plotted (default is \code{TRUE}) or not (\code{FALSE}).}}


\details{
The functions \code{\link{performance}} and \code{\link{findOptimalThreshold}} are the two main functions of the performance package. The \code{performance} function takes as input the scores obtained from deploying a binary classifier to a given data set, the vector of predicted class labels, the vector of true class labels, as well as an optional cost matrix which  in thecontains the cost of correctly/incorrectly predicting the binary class of each instance in the data set.

To avoid confusion, class labels are switched to 0s (representing "negatives") and 1s (representing "positives"). It is generally understood that scores are such that class 0 objects tend to receive lower scores than class 1 objects, and, whenever AUC < 0.5, the signs of the scores of that classifier are reversed, as is customary in the literature. Any such switches produce a warning.

The \code{performance} function outputs a list of class "\code{performance}", with one field named "\code{metrics}" that reports several performance metrics in the form of a data frame. The classification metrics are mentioned below.
}

\value{
\code{performance} returns a list containing the following components:
\item{ROCcurve}{Receiver Operating Characteristic (ROC) curve. The ROC and ROCCH curve can be plotted with the \code{\link{plot.performance}} function.}
\item{ROCCHcurve}{convex hull of the ROC curve.}
\item{PRcurve}{precision-recall curve. The precision-recall curve can be plotted with the \code{\link{plot.performance}} function.}
\item{Densities}{list with two components: the first, resp second, element contains the estimated density of the negative, resp. positive instances. The density curves can be plotted with the \code{plot.performance} function.}
\item{ylevels}{the two class labels as provided by \code{true_classes}.}
\item{confusionmatrix}{confusion matrix based on predicted labels and true labels.}
\item{metrics}{a data frame containing the following classification metrics:
\tabular{ll}{
\code{Alerts} \tab number of instances predicted as positive \cr
\code{AlertRate} \tab fraction of instances predicted as positive \cr
\code{TP} \tab number of true positives  \cr
\code{FP} \tab number of false positives  \cr
\code{TN} \tab number of true negatives  \cr
\code{FN} \tab number of false negatives  \cr
\code{TPR} \tab true positive rate (a.k.a. sensitivity, recall, hit rate)  \cr
\code{FPR} \tab false positive rate  \cr
\code{ER} \tab error rate  \cr
\code{Accuracy} \tab accuracy (1 - ER) \cr
\code{HitRate} \tab hit rate \cr
\code{Sensitivity} \tab sensitivity \cr
\code{Specificty} \tab specificity (1 - FPR) \cr
\code{Youden} \tab sensitivity + specificity - 1 \cr
\code{Precision} \tab precision  \cr
\code{Recall} \tab recall  \cr
\code{F1} \tab F1 score  \cr
\code{MER} \tab minimum error rate \cr
\code{AUC} \tab area under ROC curve \cr
\code{AUCCH} \tab area under convex hull of ROC curve \cr
\code{AUPRC} \tab area under the precision-recall curve \cr
\code{Logloss} \tab logloss, i.e. negative binomial log-likelihood \cr
\code{Gini} \tab Gini coefficient  \cr
\code{KS} \tab Kolmogorov-Smirnoff statistic
}
If a cost matrix is specified, the following cost-related metrics are included:
\tabular{ll}{
\code{Savings} \tab cost savings of using classification model as compared with using no model at all  \cr
\code{ExpectedSavings} \tab expected cost savings \cr
\code{Cost} \tab total cost of using classification model \cr
\code{AverageCost} \tab average cost  \cr
\code{ExpectedCost} \tab expected cost  \cr
\code{AverageExpectedCost} \tab average expected cost (AEC)  \cr
\code{CostNoModel} \tab cost of using no model, i.e. min(CostAllPositive, CostAllNegative)
}}
}

\references{Hoppner, S., Baesens, B., Verbeke, W., and Verdonck, T. (2020). Instance- dependent cost-sensitive learning for detecting transfer fraud. \emph{arXiv:2005.02488}

Bahnsen, A. C., Aouada, D., Stojanovic, A., and Ottersten, B. (2016). Feature engineering strategies for credit card fraud detection. \emph{Expert Systems with Applications}, 51:134–142.

Hand, D.J. (2010). Evaluating diagnostic tests: the area under the ROC curve and the balance of errors. \emph{Statistics in Medicine}, 29, 1502–1510.
}

\author{Sebastiaan Hoppner}

\seealso{\code{\link{plot.performance}}, \code{\link{findOptimalThreshold}}}

\examples{
# library(PerformanceMetrics)
# data(creditcard)
#
# logit <- glm(Class ~ V1 + V2, data = creditcard, family = "binomial")
# probs <- predict(logit, newdata = creditcard, type = "response")
# my_threshold <- 0.5
# preds <- ifelse(probs > my_threshold, 1, 0)
#
# table(creditcard$Class)
# table(preds)
#
# # Example 1 - without cost matrix:
# perf <- performance(scores = probs,
#                     predicted_classes = preds,
#                     true_classes = creditcard$Class,
#                     plot = FALSE)
# print(perf)
# plot(perf)
#
# # Example 2 - with cost matrix:
# fixed_cost <- 50
# cost_matrix <- matrix(nrow = nrow(creditcard), ncol = 2)
# cost_matrix[, 1] <- ifelse(creditcard$Class == 1, fixed_cost, 0)
# cost_matrix[, 2] <- ifelse(creditcard$Class == 1, creditcard$Amount, fixed_cost)
#
# performance(scores = probs,
#             predicted_classes = preds,
#             true_classes = creditcard$Class,
#             cost_matrix = cost_matrix)
}
