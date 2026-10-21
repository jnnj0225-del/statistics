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

# Identical pipeline to 08_nested_per_model.R's nested_boot_one(), unchanged.
# Called here with `data` already restricted to one regimen subgroup, so both the
# resampling AND the "apparent/original" anchor are scoped to that subgroup only
# (Option A: cutoffs and coefficients are both re-derived from within-regimen resamples).
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
  }
  mean_opt <- mean(optimism, na.rm=TRUE)
  c_corrected <- c_apparent - mean_opt
  corrected_b <- c_apparent - optimism
  ci <- quantile(corrected_b, c(.025,.975), na.rm=TRUE)
  list(label=if(is.null(spec)) "IMDC alone" else paste(spec, collapse="+"),
       cutoffs_full=cutoffs_full, c_apparent=c_apparent, c_corrected=c_corrected,
       ci_lo=ci[1], ci_hi=ci[2], n_ok=sum(!is.na(optimism)), B=B, n=n)
}

specs <- list(NULL, "NLR", "dNLR", "MLR", "SIRI", "PIV", "CRP", c("NLR","MLR"), c("dNLR","MLR"))

run_regimen <- function(label, dat) {
  cat(sprintf("\n=========== Regimen-stratified nested bootstrap: %s (n=%d) ===========\n\n", label, nrow(dat)))
  results <- list()
  t0 <- Sys.time()
  for (spec in specs) {
    res <- nested_boot_one(spec, dat, B=1000, seed=1)
    results[[res$label]] <- res
    cat(sprintf("%-14s apparent C=%.4f  nested-corrected C=%.4f  95%%CI [%.4f, %.4f]  (ok=%d/%d)  elapsed=%.0fs\n",
                res$label, res$c_apparent, res$c_corrected, res$ci_lo, res$ci_hi, res$n_ok, res$B,
                as.numeric(difftime(Sys.time(), t0, units="secs"))))
  }
  results
}

d_ici <- d[d$IOIO == 1, ]  # ICI-ICI
d_tki <- d[d$IOIO == 0, ]  # ICI-TKI
cat("ICI-ICI n =", nrow(d_ici), "   ICI-TKI n =", nrow(d_tki), "\n")

res_ici <- run_regimen("ICI-ICI", d_ici)
res_tki <- run_regimen("ICI-TKI", d_tki)

saveRDS(list(ICI_ICI=res_ici, ICI_TKI=res_tki), file.path(wd, "nested_per_model_regimen_result.rds"))
cat("\nsaved nested_per_model_regimen_result.rds\n")
