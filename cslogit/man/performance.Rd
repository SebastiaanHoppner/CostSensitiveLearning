\name{performance}

\alias{performance}

\title{Compute binary classification performance metrics}

\description{Computes several binary classification performance metrics.}

\usage{performance(scores, predicted_classes, true_classes)}

\arguments{
\item{scores}{a vector of predicted probabilities.}

\item{predicted_classes}{a vector of predicted labels. This can be either a factor or in numeric form.}

\item{true_classes}{a vector of true labels. This can be either a factor or in numeric form.}
}

\details{...}

\value{
\code{performance} returns a list containing the following components:
\item{pr_curve}{precision-recall curve. This can be \code{NA} if the \code{\link{pr.curve}} function gives an error.}
\item{confusionmatrix}{confusion matrix based on predicted labels and true labels.}
\item{metrics}{a data frame containing the following classification metrics:
\tabular{ll}{
alerts \tab number of instances predicted as positive \cr
alert_rate \tab fraction of instances predicted as positive \cr
TP \tab number of true positives  \cr
FP \tab number of false positives  \cr
TN \tab number of true negatives  \cr
FN \tab number of false negatives  \cr
TPR \tab true positive rate  \cr
FPR \tab false positive rate  \cr
ER \tab error rate  \cr
MER \tab minimum error rate. This can be \code{NA} if the \code{\link{HMeasure}} function gives an error  \cr
Precision \tab precision  \cr
Recall \tab recall  \cr
F1 \tab F1 score  \cr
AUC \tab Area under ROC curve. This can be \code{NA} if the \code{\link{HMeasure}} function gives an error  \cr
AUC_pr \tab area under the precision-recall curve. This can be \code{NA} if the \code{\link{pr.curve}} function gives an error.
}}
}

\references{...}

\author{Sebastiaan Höppner}


\seealso{\code{\link{costPerformance}}}

\examples{
# Provide an example (TODO).
}
