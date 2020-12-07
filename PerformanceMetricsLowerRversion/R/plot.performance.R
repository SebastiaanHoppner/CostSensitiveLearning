plot.performance <- function (x, which = "all", ...) {
  # check inputs
  if (missing(x)) {
    stop("Argument 'x' is missing, with no default.")
  }
  if (class(x) != "performance") {
    stop("Argument 'x' must be of class 'performance'.")
  }
  if (class(which) != "character") {
    stop("Argument 'which' should be one of \"all\", \"roc\", \"prcurve\", \"density\", \"cm\" or ",
         "\"confusionmatrix\".")
  }
  which <- tolower(which)
  if (!which %in% c("all", "roc", "prcurve", "density", "cm", "confusion matrix")) {
    stop("Argument 'which' should be one of \"all\", \"roc\", \"prcurve\", \"density\", \"cm\" or ",
         "\"confusionmatrix\".")
  }

  if (which == "all") {
    par(mfrow = c(2, 2))
  }

  if (which == "roc" | which == "all") {
    # plot ROC curve
    plot(x$ROCcurve, type = "l", las = 1, xlim = c(0, 1), ylim = c(0, 1),
         xlab = "FPR (1 - Specificity)", ylab = "TPR (Sensitivity)",
         main = paste0("ROC (continuous) and ROCCH (dotted)\n",
                       "AUC = ", round(x$metrics$AUC, 4), ", AUCCH = ", round(x$metrics$AUCCH, 4)))
    grid(col = "gray90")
    segments(x0 = 0, y0 = 0, x1 = 1, y1 = 1, lty = 4, col = "darkgray")
    lines(x$ROCcurve, lwd = 2, col = "olivedrab4")
    lines(x$ROCCHcurve, lty = 2)
  }

  if (which == "prcurve" | which == "all") {
    # plot precision-recall curve
    plot(x$PRcurve, type = "l", las = 1, xlim = c(0, 1), ylim = c(0, 1),
         xlab = "Recall", ylab = "Precision",
         main = paste("PR curve\n", "AUPRC =", round(x$metrics$AUPRC, 4)))
    grid(col = "gray90")
    percentage_of_positives <- sum(x$confusionmatrix[, 2]) / sum(x$confusionmatrix)
    segments(x0 = 0, y0 = percentage_of_positives, x1 = 1, y1 = percentage_of_positives,
             lty = 4, col = "darkgray")
    lines(x$PRcurve, lwd = 2, col = "olivedrab4")
  }

  if (which == "density" | which == "all") {
    # plot density curves
    plot(x$Densities$d0, xlim = c(0, 1), ylim = c(0, max(c(x$Densities$d0$y, x$Densities$d1$y))),
         lty = 2, lwd = 2, las = 1, col = "dodgerblue", xlab = "Score", ylab = "Density",
         main = paste0("Score densities\n",
                       "(class ", x$ylevels[1], ": blue, class ", x$ylevels[2], ": red)"))
    lines(x$Densities$d1, lty = 4, lwd = 2, col = "red")
  }

  if (which == "cm" | which == "confusionmatrix" | which == "all") {
    # display confusion matrix
    colorvec <- c("red", "olivedrab4")
    if (x$metrics$TP + x$metrics$TN < x$metrics$FP + x$metrics$FN) {
      colorvec <- colorvec[2:1]
    }
    fourfoldplot(x$confusionmatrix, color = colorvec, conf.level = 0, main = "Confusion Matrix")
    text(x = -0.4, y =  0.4, "TN", cex = 2)
    text(x =  0.4, y = -0.4, "TP", cex = 2)
    text(x =  0.4, y =  0.4, "FN", cex = 2)
    text(x = -0.4, y = -0.4, "FP", cex = 2)
  }

  if (which == "all") {
    par(mfrow = c(1, 1))
  }
}
