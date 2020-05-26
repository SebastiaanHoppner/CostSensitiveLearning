\name{creditcard}
\docType{data}
\alias{creditcard}
\title{Creditcard Transaction Data}
\description{
Transactions made by credit cards in September 2013 by European cardholders. This data set presents a small subset of transactions that occurred in two days, where we have 465 frauds out of 1409 transactions. It contains only numerical input variables which are the result of a PCA transformation. Due to confidentiality issues, the original features and more background information about the data cannot be provided. Features V1, V2, ..., V28 are the principal components obtained with PCA. The only feature which has not been transformed with PCA is ‘Amount’ which is the transaction amount. Feature ‘Class’ is the response variable which takes value 1 in case of fraud and 0 otherwise.
}
\usage{data(creditcard)}
\format{A data frame containing 1409 observations and 30 variables.}
\source{kaggle.com, made available by Andrea Dal Pozzolo et al., Calibrating Probability with Undersampling for Unbalanced Classification. In Symposium on Computational Intelligence and Data Mining (CIDM), IEEE, 2015.}
\keyword{datasets}
\examples{
data(creditcard)
str(creditcard)
head(creditcard)
summary(creditcard)
}
