# CostSensitiveLearning
The repository contains the R packages `cslogit`, `csboost` and `PerformanceMetrics` and the corresponding article:  
Höppner, S., Baesens, B., Verbeke, W., and Verdonck, T. (2020). Instance- dependent cost-sensitive learning for detecting transfer fraud. arXiv:2005.02488  
https://arxiv.org/abs/2005.02488

The R packages `cslogit` and `csboost` respectively contain the implementation of the cslogit and csboost algorithm for instance-dependent cost-sensitive learning using lasso-regularized logistic regression and gradient boosted decision trees, respectively. The R package `PerformanceMetrics` contains functions for performance measurement and threshold tuning for binary classification.

The R packages can be installed through the `devtools` package:  
`devtools::install_github("SebastiaanHoppner/CostSensitiveLearning/cslogit")`  
`devtools::install_github("SebastiaanHoppner/CostSensitiveLearning/csboost")`  
`devtools::install_github("SebastiaanHoppner/CostSensitiveLearning/PerformanceMetrics")`
