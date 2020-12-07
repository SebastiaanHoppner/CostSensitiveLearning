performance <- function (scores, predicted_classes, true_classes, cost_matrix = NULL, plot = TRUE) {
  # check inputs
  checkInputs(scores, predicted_classes, true_classes, cost_matrix, plot,
              thresholds = NULL, metric = NULL)

  # names can confuse and are otherwise useless - remove them
  names(scores) <- NULL
  names(true_classes) <- NULL
  names(predicted_classes) <- NULL
  names(cost_matrix) <- NULL

  # process inputs
  s <- scores
  y <- factor(as.character(true_classes))
  yhat <- factor(as.character(predicted_classes), levels = levels(y))
  ylevels <- levels(y)

  # relabel classes to numeric version
  y <- relabel(y, "true_classes")
  yhat <- relabel(yhat, "predicted_classes")

  # confusion matrix related metrics
  Alerts <- sum(yhat)
  AlertRate <- mean(yhat)

  TP <- sum(y == 1 & yhat == 1)
  FP <- sum(y == 0 & yhat == 1)
  TN <- sum(y == 0 & yhat == 0)
  FN <- sum(y == 1 & yhat == 0)

  TPR <- TP / (TP + FN)
  FPR <- FP / (FP + TN)
  ER <- (FP + FN) / (TP + FP + TN + FN)

  Accuracy <- 1 - ER
  HitRate <- TPR
  Sensitivity <- TPR
  Specificity <- 1 - FPR
  Youden <- Sensitivity + Specificity - 1

  Precision <- TP / (TP + FP)
  Recall <- TPR
  F1metric <- 2 * (Precision * Recall) / (Precision + Recall)

  # using the numeric version of the class labels
  n <- length(y)
  n1 <- sum(y)
  n0 <- n - n1
  pi0 <- n0 / n
  pi1 <- n1 / n

  # calculate raw ROC, replacing any tied sequences by a diagonal
  # raw ROC starts at (F0[1] = 0, F1[1] = 0) and ends at (F0[K1] = 1, F1[K1] = 1)
  GetScoreDistributions <- function(y, s, n1, n0) {
    s1 <- tapply(y, s, sum) / n1 # counts the instances of each unique score, and ranks them by score
    s0 <- tapply(1 - y, s, sum) / n0
    s1 <- c(0, s1, 1 - sum(s1)) # make sure to add the points (0, 0) and (1, 1)
    s0 <- c(0, s0, 1 - sum(s0)) # make sure to add the points (0, 0) and (1, 1)
    F1 <- cumsum(s1) # empirical cdfs
    F0 <- cumsum(s0) # empirical cdfs
    return(list(F1 = F1, F0 = F0, s1 = s1, s0 = s0))
  }

  out <- GetScoreDistributions(y, s, n1, n0)
  AUC <- 1 - sum(out$s0 * (out$F1 - 0.5 * out$s1))
  if (AUC < 0.5) { # if the AUC < 0.5, switch scores and repeat
    warning("ROC curve of scores mostly lying under the diagonal! Switching scores.", call. = FALSE)
    s <- 1 - s  # this assumes scores are inside the unit interval
    out <- GetScoreDistributions(y, s, n1, n0)
    AUC <- 1 - sum(out$s0 * (out$F1 - 0.5 * out$s1))
  }
  F1 <- out$F1
  F0 <- out$F0
  s1 <- out$s1
  s0 <- out$s0

  ROCcurve <- cbind(1 - F0, 1 - F1)
  colnames(ROCcurve) <- c("FPR", "TPR")
  rownames(ROCcurve) <- NULL

  # get aggregate statistics
  Logloss <- -mean(y * log(s) + (1 - y) * log(1 - s))
  Gini <- 2 * AUC - 1
  KS <- max(abs(F0 - F1))
  MER <- min(pi0 * (1 - F0) + pi1 * F1)

  # restrict to upper convex hull by considering ROC above diagonal only
  chullPoins <- chull(1 - F0, pmax(1 - F1, 1 - F0))
  G0 <- 1 - F0[chullPoins]
  G1 <- 1 - F1[chullPoins]
  hc <- length(chullPoins)
  sG0 <- c(0, G0[c(2:length(G0))] - G0[c(1:(length(G0) - 1))])
  sG1 <- c(0, G1[c(2:length(G1))] - G1[c(1:(length(G1) - 1))])
  AUCCH <- sum(sG0 * (G1 - 0.5 * sG1))
  ROCCHcurve <- cbind(G0, G1)
  colnames(ROCCHcurve) <- c("FPR", "TPR")
  rownames(ROCCHcurve) <- NULL

  # AUPRC
  s_1 <- s[which(y == 1)]
  s_0 <- s[which(y == 0)]

  all_s <- c(s_1, s_0)
  all_weights_pos <- c(rep(1, length(s_1)), rep(0, length(s_0)))
  all_weights_neg <- 1 - all_weights_pos

  order_all_s <- order(all_s, decreasing = TRUE)
  all_s <- all_s[order_all_s]
  all_weights_pos <- all_weights_pos[order_all_s]
  all_weights_neg <- all_weights_neg[order_all_s]

  cum_weights_pos <- cumsum(all_weights_pos)
  cum_weights_neg <- cumsum(all_weights_neg)
  cum_use <- c(all_s[-length(all_s)] != all_s[-1], TRUE)

  cum_weights_pos <- cum_weights_pos[cum_use]
  cum_weights_neg <- cum_weights_neg[cum_use]

  r_fg <- sum(all_weights_pos)
  tp <- cum_weights_pos
  fp <- cum_weights_neg
  tp_prev <- c(0, cum_weights_pos[-length(cum_weights_pos)])
  fp_prev <- c(0, cum_weights_neg[-length(cum_weights_neg)])

  h <- (fp - fp_prev) / (tp - tp_prev)
  a <- 1 + h
  b <- (fp_prev - h * tp_prev) / r_fg
  h[tp == tp_prev] <- 1
  a[tp == tp_prev] <- 1
  b[tp == tp_prev] <- 0

  v <- (tp / r_fg - tp_prev / r_fg - b / a *
          (log(a * tp / r_fg + b) - log(a * tp_prev / r_fg + b))) / a
  v2 <- (tp / r_fg - tp_prev / r_fg) / a
  v[b == 0] <- v2[b == 0]

  AUPRC <- sum(v)

  # precision-recall curve
  minStepSize <- 1 / r_fg
  minCurve <- cbind(tp / r_fg, tp / (tp + fp))
  idxs <- which((tp - tp_prev)/r_fg > minStepSize & tp/(tp + fp) != tp_prev / (tp_prev + fp_prev))
  idxs <- idxs[idxs > 1]
  if (length(idxs) > 0) {
    PRcurve <- sapply(idxs, function (i) {
      x <- seq(0, minCurve[i, 1] - minCurve[i - 1, 1], by = minStepSize)
      sns <- minCurve[i - 1, 1] + x
      prcs <- (minCurve[i - 1, 1] + x) / (minCurve[i-1, 1] + x + fp[i - 1] / r_fg + h[i] * x)
      temp <- rbind(sns, prcs)
      return(temp)
    })
    PRcurve <- matrix(unlist(PRcurve), ncol = 2, byrow = TRUE)
    PRcurve <- rbind(minCurve, PRcurve)
  } else {
    PRcurve <- minCurve
  }
  PRcurve <- PRcurve[order(PRcurve[, 1], decreasing = TRUE), ]
  PRcurve <- rbind(c(1, PRcurve[1, 2]), PRcurve, c(0, PRcurve[nrow(PRcurve), 2]))
  colnames(PRcurve) <- c("Recall", "Precision")
  rownames(PRcurve) <- NULL

  # confusion matrix
  CM <- matrix(c(TN, FP, FN, TP), ncol = 2, nrow = 2)
  colnames(CM) <- rownames(CM) <- ylevels
  names(dimnames(CM)) <- c("Prediction", "Reference")
  CM <- as.table(CM)

  if (is.null(cost_matrix)) {
    # collect computed metrics
    metrics <- cbind.data.frame(Alerts, AlertRate, TP, FP, TN, FN, TPR, FPR, ER, Accuracy, HitRate,
                                Sensitivity, Specificity, Youden, Precision, Recall, F1 = F1metric,
                                MER, AUC, AUCCH, AUPRC, Logloss, Gini, KS)
  } else {
    # rearrange cost matrix
    cost_matrix[y == 0, ] <- cost_matrix[y == 0, c(2, 1)]

    Cost <- sum(cost_matrix * matrix(c(yhat, 1 - yhat), ncol = 2))
    AverageCost <- Cost / n

    ExpectedCost <- sum(cost_matrix * matrix(c(scores, 1 - scores), ncol = 2))
    AverageExpectedCost <- ExpectedCost / n

    CostNoModel <- min(sum(cost_matrix[, 1]), sum(cost_matrix[, 2]))
    Savings <- (CostNoModel - Cost) / CostNoModel
    ExpectedSavings <- (CostNoModel - ExpectedCost) / CostNoModel

    # collect computed metrics
    metrics <- cbind.data.frame(Alerts, AlertRate, TP, FP, TN, FN, TPR, FPR, ER, Accuracy, HitRate,
                                Sensitivity, Specificity, Youden, Precision, Recall, F1 = F1metric,
                                MER, AUC, AUCCH, AUPRC, Logloss, Gini, KS,
                                Savings, ExpectedSavings, Cost, AverageCost,
                                ExpectedCost, AverageExpectedCost, CostNoModel)
  }

  # score densities
  d0 <- density(s_0, from = min(s_0), to = max(s_0))
  d1 <- density(s_1, from = min(s_1), to = max(s_1))

  if (plot) {
    par(mfrow = c(2, 2))
    # plot ROC curve
    plot(ROCcurve, type = "l", las = 1, xlim = c(0, 1), ylim = c(0, 1),
         xlab = "FPR (1 - Specificity)", ylab = "TPR (Sensitivity)",
         main = paste0("ROC (continuous) and ROCCH (dotted)\n",
                       "AUC = ", round(AUC, 4), ", AUCCH = ", round(AUCCH, 4)))
    grid(col = "gray90")
    segments(x0 = 0, y0 = 0, x1 = 1, y1 = 1, lty = 4, col = "darkgray")
    lines(ROCcurve, lwd = 2, col = "olivedrab4")
    lines(ROCCHcurve, lty = 2)

    # plot precision-recall curve
    plot(PRcurve, type = "l", las = 1, xlim = c(0, 1), ylim = c(0, 1),
         xlab = "Recall", ylab = "Precision",
         main = paste("PR curve\n", "AUPRC =", round(AUPRC, 4)))
    grid(col = "gray90")
    segments(x0 = 0, y0 = pi1, x1 = 1, y1 = pi1, lty = 4, col = "darkgray")
    lines(PRcurve, lwd = 2, col = "olivedrab4")

    # plot density curves
    plot(d0, xlim = c(0, 1), ylim = c(0, max(c(d0$y, d1$y))),
         lty = 2, lwd = 2, las = 1, col = "dodgerblue", xlab = "Score", ylab = "Density",
         main = paste0("Score densities\n",
                       "(class ", ylevels[1], ": blue, class ", ylevels[2], ": red)"))
    lines(d1, lty = 4, lwd = 2, col = "red")

    # display confusion matrix
    colorvec <- c("red", "olivedrab4")
    if (TP + TN < FP + FN) {
      colorvec <- colorvec[2:1]
    }
    fourfoldplot(CM, color = colorvec, conf.level = 0, main = "Confusion Matrix")
    text(x = -0.4, y =  0.4, "TN", cex = 2)
    text(x =  0.4, y = -0.4, "TP", cex = 2)
    text(x =  0.4, y =  0.4, "FN", cex = 2)
    text(x = -0.4, y = -0.4, "FP", cex = 2)
    par(mfrow = c(1, 1))
  }

  # output
  output <- list(ROCcurve        = ROCcurve,
                 ROCCHcurve      = ROCCHcurve,
                 PRcurve         = PRcurve,
                 Densities       = list(d0 = d0, d1 = d1),
                 ylevels         = ylevels,
                 confusionmatrix = CM,
                 metrics         = metrics)
  class(output) <- "performance"
  return(output)
}
