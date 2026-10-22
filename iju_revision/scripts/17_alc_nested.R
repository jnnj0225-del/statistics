source("helpers.R")
suppressMessages({library(survival)})
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
d <- readRDS(file.path(wd, "d.rds"))
d$IMDC_f <- factor(d$IMDC, levels=c(1,2))

get_c <- function(lp, time, status) as.numeric(concordance(Surv(time, status) ~ lp, reverse=TRUE)$concordance)

build_grp_alc <- function(dat2, cutoff) {
  hi <- as.numeric(dat2$ALC <= cutoff)  # ALC-low = "high risk" direction, consistent with 02b_ALC_median.R
  interaction(factor(dat2$IMDC, levels=c(1,2)), factor(hi, levels=c(0,1)), drop=FALSE)
}

nested_boot_alc <- function(data, B=1000, seed=1) {
  set.seed(seed)
  n <- nrow(data)
  cutoff_full <- median(data$ALC)
  grp_full <- build_grp_alc(data, cutoff_full)
  fit_full <- coxph(Surv(OS_time, death) ~ grp_full, data=data)
  lp_full <- predict(fit_full, type="lp")
  c_apparent <- get_c(lp_full, data$OS_time, data$death)

  optimism <- rep(NA_real_, B)
  for (b in 1:B) {
    idx <- sample.int(n, n, replace=TRUE)
    db <- data[idx, ]
    cutoff_b <- median(db$ALC)
    grp_b <- build_grp_alc(db, cutoff_b)
    if (length(unique(grp_b)) < 2) next
    fit_b <- tryCatch(coxph(Surv(OS_time, death) ~ grp_b, data=db), error=function(e) NULL)
    if (is.null(fit_b)) next
    lp_b <- tryCatch(predict(fit_b, type="lp"), error=function(e) NULL)
    if (is.null(lp_b)) next
    ctr <- tryCatch(get_c(lp_b, db$OS_time, db$death), error=function(e) NA)
    if (is.na(ctr)) next

    grp_eval <- build_grp_alc(data, cutoff_b)
    newdat <- data.frame(grp_b = grp_eval)
    lp_eval <- tryCatch(predict(fit_b, newdata=newdat, type="lp"), error=function(e) NULL)
    if (is.null(lp_eval) || all(is.na(lp_eval))) next
    keep <- !is.na(lp_eval)
    cte <- tryCatch(get_c(lp_eval[keep], data$OS_time[keep], data$death[keep]), error=function(e) NA)
    if (is.na(cte)) next
    optimism[b] <- ctr - cte
  }
  mean_opt <- mean(optimism, na.rm=TRUE)
  c_corrected <- c_apparent - mean_opt
  corrected_b <- c_apparent - optimism
  ci <- quantile(corrected_b, c(.025,.975), na.rm=TRUE)
  list(c_apparent=c_apparent, c_corrected=c_corrected, ci_lo=ci[1], ci_hi=ci[2], n_ok=sum(!is.na(optimism)), B=B)
}

cat("=========== Nested bootstrap C-index, IMDC-ALC (median cutoff re-derived per resample, B=1000) ===========\n\n")
res <- nested_boot_alc(d, B=1000, seed=1)
cat(sprintf("IMDC-ALC    apparent C=%.4f  nested-corrected C=%.4f  95%%CI [%.4f, %.4f]  (ok=%d/%d)\n",
            res$c_apparent, res$c_corrected, res$ci_lo, res$ci_hi, res$n_ok, res$B))
saveRDS(res, file.path(wd, "alc_nested_result.rds"))
cat("\nsaved alc_nested_result.rds\n")
