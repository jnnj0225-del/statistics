suppressMessages({library(survival)})

timeROC_KM <- function(time, status, marker, t) {
  cutoffs <- sort(unique(marker))
  overall <- survfit(Surv(time, status) ~ 1)
  St <- summary(overall, times = t, extend = TRUE)$surv
  Ft <- 1 - St
  sens <- numeric(length(cutoffs)); spec <- numeric(length(cutoffs))
  for (i in seq_along(cutoffs)) {
    c <- cutoffs[i]
    hi <- marker > c
    p_hi <- mean(hi); p_lo <- 1 - p_hi
    if (p_hi > 0 && p_hi < 1) {
      S_hi_t <- summary(survfit(Surv(time[hi], status[hi]) ~ 1), times = t, extend = TRUE)$surv
      S_lo_t <- summary(survfit(Surv(time[!hi], status[!hi]) ~ 1), times = t, extend = TRUE)$surv
    } else if (p_hi == 0) { S_hi_t <- 1; S_lo_t <- St } else { S_hi_t <- St; S_lo_t <- 1 }
    p_hi_and_gt <- p_hi * S_hi_t
    p_hi_and_le <- p_hi - p_hi_and_gt
    p_lo_and_gt <- p_lo * S_lo_t
    sens[i] <- if (Ft > 0) p_hi_and_le / Ft else NA
    spec[i] <- if (St > 0) p_lo_and_gt / St else NA
  }
  data.frame(cutoff = cutoffs, sens = sens, spec = spec)
}

auc_trapz <- function(sens, spec) {
  fpr <- 1 - spec; o <- order(fpr); fpr <- fpr[o]; tpr <- sens[o]
  fpr <- c(0, fpr, 1); tpr <- c(0, tpr, 1)
  sum(diff(fpr) * (head(tpr, -1) + tail(tpr, -1)) / 2)
}

best_cutoff <- function(time, status, marker, t) {
  roc <- timeROC_KM(time, status, marker, t)
  roc$youden <- roc$sens + roc$spec - 1
  best <- roc[which.max(roc$youden), ]
  list(cutoff = best$cutoff, sens = best$sens, spec = best$spec,
       auc = auc_trapz(roc$sens, roc$spec), roc = roc)
}
