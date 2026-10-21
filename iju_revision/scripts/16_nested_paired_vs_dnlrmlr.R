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

fit_eval_one <- function(spec, data, db) {
  # derive cutoffs from resample db, fit on db, evaluate on original data
  cutoffs_b <- list(); ok <- TRUE
  if (!is.null(spec)) {
    for (m in spec) {
      bc <- tryCatch(best_cutoff(db$OS_time, db$death, db[[m]], 24), error=function(e) NULL)
      if (is.null(bc) || !is.finite(bc$cutoff)) { ok <- FALSE; break }
      cutoffs_b[[m]] <- bc$cutoff
    }
  }
  if (!ok) return(NULL)
  grp_b <- build_grp_fixed(db, spec, cutoffs_b)
  if (length(unique(grp_b)) < 2) return(NULL)
  fit_b <- tryCatch(coxph(Surv(OS_time, death) ~ grp_b, data=db), error=function(e) NULL)
  if (is.null(fit_b)) return(NULL)
  grp_eval <- build_grp_fixed(data, spec, cutoffs_b)
  newdat <- data.frame(grp_b = grp_eval)
  lp_eval <- tryCatch(predict(fit_b, newdata=newdat, type="lp"), error=function(e) NULL)
  if (is.null(lp_eval) || all(is.na(lp_eval))) return(NULL)
  keep <- !is.na(lp_eval)
  cte <- tryCatch(get_c(lp_eval[keep], data$OS_time[keep], data$death[keep]), error=function(e) NA)
  if (is.na(cte)) return(NULL)
  cte
}

# nested + paired comparison: in each resample, BOTH models' cutoffs are re-derived (nested),
# fit on the same resample, evaluated out-of-resample on the original data, then differenced.
nested_paired_boot <- function(specA, specB, labelA, labelB, data, B=500, seed=2024) {
  set.seed(seed)
  n <- nrow(data)
  diffs <- rep(NA_real_, B)
  cA_vec <- rep(NA_real_, B); cB_vec <- rep(NA_real_, B)
  t0 <- Sys.time()
  for (b in 1:B) {
    idx <- sample.int(n, n, replace=TRUE)
    db <- data[idx, ]
    cA <- fit_eval_one(specA, data, db)
    cB <- fit_eval_one(specB, data, db)
    if (is.null(cA) || is.null(cB)) next
    cA_vec[b] <- cA; cB_vec[b] <- cB
    diffs[b] <- cA - cB
  }
  ci <- quantile(diffs, c(.025,.975), na.rm=TRUE)
  cat(sprintf("%-16s vs %-14s : mean diff=%+.4f  95%%CI [%+.4f, %+.4f]  (ok=%d/%d)  elapsed=%.0fs  %s\n",
              labelA, labelB, mean(diffs, na.rm=TRUE), ci[1], ci[2], sum(!is.na(diffs)), B,
              as.numeric(difftime(Sys.time(), t0, units="secs")),
              ifelse(ci[1]>0 || ci[2]<0, "*", "")))
  list(labelA=labelA, labelB=labelB, diffs=diffs, mean_diff=mean(diffs,na.rm=TRUE), ci_lo=ci[1], ci_hi=ci[2])
}

specA <- c("dNLR","MLR")
others <- list(
  list(spec=NULL,          label="IMDC alone"),
  list(spec="NLR",         label="IMDC-NLR"),
  list(spec="dNLR",        label="IMDC-dNLR"),
  list(spec="MLR",         label="IMDC-MLR"),
  list(spec="SIRI",        label="IMDC-SIRI"),
  list(spec="PIV",         label="IMDC-PIV"),
  list(spec="CRP",         label="IMDC-CRP"),
  list(spec=c("NLR","MLR"),label="IMDC-NLR-MLR")
)

cat("=========== Nested + paired bootstrap: IMDC-dNLR-MLR vs each of 8 other candidates (B=500) ===========\n\n")
results <- list()
for (o in others) {
  res <- nested_paired_boot(specA, o$spec, "IMDC-dNLR-MLR", o$label, d, B=500, seed=2024)
  results[[o$label]] <- res
}

saveRDS(results, file.path(wd, "nested_paired_vs_dnlrmlr_result.rds"))
cat("\nsaved nested_paired_vs_dnlrmlr_result.rds\n")
