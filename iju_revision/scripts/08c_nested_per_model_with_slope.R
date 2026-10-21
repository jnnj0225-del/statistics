source("helpers.R")
suppressMessages({library(survival)})
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
d <- readRDS(file.path(wd, "d.rds"))
d$IMDC_f <- factor(d$IMDC, levels=c(1,2))

get_c <- function(lp, time, status) as.numeric(concordance(Surv(time, status) ~ lp, reverse=TRUE)$concordance)

build_grp_fixed <- function(dat2, spec, cutoffs) {
  if (is.null(spec)) return(factor(dat2$IMDC, levels=c(1,2)))
  hiflags <- sapply(spec, function(m) as.numeric(dat2[[m]] > cutoffs[[m]]))
  if (is.null(dim(hiflags))) hiflags <- matrix(hiflags, ncol=length(spec))
  mhigh <- as.numeric(rowSums(hiflags, na.rm=TRUE) >= 1)
  interaction(factor(dat2$IMDC, levels=c(1,2)), factor(mhigh, levels=c(0,1)), drop=FALSE)
}

# Calibration slope definition (matches the logic behind rms::validate's "Slope"):
#   training slope is always exactly 1 (a Cox fit's own linear predictor regressed on itself
#   reproduces coefficient 1), so optimism_slope_b = 1 - test_slope_b, and the
#   optimism-corrected slope = 1 - mean(optimism_slope_b) = mean(test_slope_b).
# test_slope_b is obtained by fitting coxph(Surv(time,event) ~ lp_eval) on the ORIGINAL data,
# where lp_eval is the resample-b model's linear predictor applied to the original data.

nested_boot_one <- function(spec, data, B=1000, seed=1) {
  set.seed(seed)
  n <- nrow(data)

  cutoffs_full <- list()
  if (!is.null(spec)) for (m in spec) cutoffs_full[[m]] <- best_cutoff(data$OS_time, data$death, data[[m]], 24)$cutoff
  grp_full <- build_grp_fixed(data, spec, cutoffs_full)
  fit_full <- coxph(Surv(OS_time, death) ~ grp_full, data=data)
  lp_full <- predict(fit_full, type="lp")
  c_apparent <- get_c(lp_full, data$OS_time, data$death)

  train_c <- rep(NA_real_, B); test_c <- rep(NA_real_, B); optimism <- rep(NA_real_, B)
  test_slope <- rep(NA_real_, B)
  for (b in 1:B) {
    idx <- sample.int(n, n, replace=TRUE)
    db <- data[idx, ]
    cutoffs_b <- list()
    ok <- TRUE
    if (!is.null(spec)) {
      for (m in spec) {
        bc <- tryCatch(best_cutoff(db$OS_time, db$death, db[[m]], 24), error=function(e) NULL)
        if (is.null(bc) || !is.finite(bc$cutoff)) { ok <- FALSE; break }
        cutoffs_b[[m]] <- bc$cutoff
      }
    }
    if (!ok) next
    grp_b <- build_grp_fixed(db, spec, cutoffs_b)
    if (length(unique(grp_b)) < 2) next
    fit_b <- tryCatch(coxph(Surv(OS_time, death) ~ grp_b, data=db), error=function(e) NULL)
    if (is.null(fit_b)) next
    lp_b <- tryCatch(predict(fit_b, type="lp"), error=function(e) NULL)
    if (is.null(lp_b)) next
    ctr <- tryCatch(get_c(lp_b, db$OS_time, db$death), error=function(e) NA)
    if (is.na(ctr)) next
    train_c[b] <- ctr

    grp_eval <- build_grp_fixed(data, spec, cutoffs_b)
    newdat <- data.frame(grp_b = grp_eval)
    lp_eval <- tryCatch(predict(fit_b, newdata=newdat, type="lp"), error=function(e) NULL)
    if (is.null(lp_eval) || all(is.na(lp_eval))) next
    keep <- !is.na(lp_eval)
    cte <- tryCatch(get_c(lp_eval[keep], data$OS_time[keep], data$death[keep]), error=function(e) NA)
    if (is.na(cte)) next
    test_c[b] <- cte
    optimism[b] <- train_c[b] - test_c[b]

    # calibration slope: regress original data's outcome on this resample's linear predictor
    if (sum(keep) >= 3 && length(unique(lp_eval[keep])) > 1) {
      sfit <- tryCatch(coxph(Surv(data$OS_time[keep], data$death[keep]) ~ lp_eval[keep]), error=function(e) NULL)
      if (!is.null(sfit)) test_slope[b] <- unname(coef(sfit)[1])
    }
  }
  mean_opt <- mean(optimism, na.rm=TRUE)
  c_corrected <- c_apparent - mean_opt
  corrected_b <- c_apparent - optimism
  ci <- quantile(corrected_b, c(.025,.975), na.rm=TRUE)

  slope_corrected <- mean(test_slope, na.rm=TRUE)
  slope_ci <- quantile(test_slope, c(.025,.975), na.rm=TRUE)

  list(label=if(is.null(spec)) "IMDC alone" else paste(spec, collapse="+"),
       cutoffs_full=cutoffs_full, c_apparent=c_apparent, c_corrected=c_corrected,
       ci_lo=ci[1], ci_hi=ci[2],
       slope_corrected=slope_corrected, slope_ci_lo=slope_ci[1], slope_ci_hi=slope_ci[2],
       n_ok=sum(!is.na(optimism)), n_slope_ok=sum(!is.na(test_slope)), B=B)
}

# 8 models matching the actual published Supplementary Table 4 (PIV excluded: borderline NS for OS in sTable3)
specs <- list(NULL, "NLR", "dNLR", "MLR", "SIRI", "CRP", c("NLR","MLR"), c("dNLR","MLR"))

cat("=========== Nested bootstrap C-index + calibration slope, per model ===========\n\n")
results <- list()
t0 <- Sys.time()
for (spec in specs) {
  res <- nested_boot_one(spec, d, B=1000, seed=1)
  results[[res$label]] <- res
  cat(sprintf("%-14s apparent C=%.4f  corrected C=%.4f  95%%CI [%.4f,%.4f]  |  slope=%.3f  95%%CI [%.3f,%.3f]  (elapsed=%.0fs)\n",
              res$label, res$c_apparent, res$c_corrected, res$ci_lo, res$ci_hi,
              res$slope_corrected, res$slope_ci_lo, res$slope_ci_hi,
              as.numeric(difftime(Sys.time(), t0, units="secs"))))
}

saveRDS(results, file.path(wd, "nested_per_model_with_slope_result.rds"))
cat("\nsaved nested_per_model_with_slope_result.rds\n")
