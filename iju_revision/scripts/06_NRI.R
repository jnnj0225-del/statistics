suppressMessages({library(survival)})
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
d <- readRDS(file.path(wd, "d.rds"))
d$IMDC_f <- factor(d$IMDC, levels=c(1,2))

# predicted 24-month death probability from a fitted coxph model
pred_risk24 <- function(fit, data, t=24) {
  sf <- survfit(fit, newdata=data)
  # individual survival curves; extract S(t) per subject
  idx <- findInterval(t, sf$time)
  if (idx < 1) return(rep(1 - sf$surv[1, ], nrow(data))[1:nrow(data)]*0 + (1 - 1)) # not expected
  Smat <- sf$surv
  if (is.null(dim(Smat))) Smat <- matrix(Smat, ncol=1)
  St <- Smat[idx, ]
  1 - St
}

cont_nri <- function(risk_old, risk_new, event, time, horizon=24) {
  # restrict to subjects with known status at horizon: event before horizon (event=1), or followed >= horizon (censored counts as non-event at horizon)
  is_event <- event==1 & time<=horizon
  is_nonevent <- time>=horizon  # event-free survivors past horizon
  up <- risk_new > risk_old
  down <- risk_new < risk_old
  e_up <- mean(up[is_event]); e_down <- mean(down[is_event])
  ne_up <- mean(up[is_nonevent]); ne_down <- mean(down[is_nonevent])
  nri <- (e_up - e_down) + (ne_down - ne_up)
  list(nri=nri, n_event=sum(is_event), n_nonevent=sum(is_nonevent), e_up=e_up, e_down=e_down, ne_up=ne_up, ne_down=ne_down)
}

boot_nri_ci <- function(fit_old_formula, fit_new_formula, data, B=500, horizon=24) {
  environment(fit_old_formula) <- environment()
  environment(fit_new_formula) <- environment()
  n <- nrow(data)
  vals <- numeric(B)
  for (b in 1:B) {
    idx <- sample.int(n, n, replace=TRUE)
    db <- data[idx, ]
    fo <- tryCatch(coxph(fit_old_formula, data=db), error=function(e) NULL)
    fn <- tryCatch(coxph(fit_new_formula, data=db), error=function(e) NULL)
    if (is.null(fo) || is.null(fn)) { vals[b] <- NA; next }
    ro <- tryCatch(pred_risk24(fo, db, horizon), error=function(e) NULL)
    rn <- tryCatch(pred_risk24(fn, db, horizon), error=function(e) NULL)
    if (is.null(ro) || is.null(rn)) { vals[b] <- NA; next }
    res <- tryCatch(cont_nri(ro, rn, db$death, db$OS_time, horizon), error=function(e) NULL)
    vals[b] <- if (!is.null(res)) res$nri else NA
  }
  quantile(vals, c(.025,.5,.975), na.rm=TRUE)
}

cat("=========== R1-1: continuous NRI, 24-month OS ===========\n\n")

# apparent NRI point estimates
fit_dnlr   <- coxph(Surv(OS_time, death) ~ IMDC_f + factor(dNLR_ge), data=d, x=TRUE)
fit_mlr    <- coxph(Surv(OS_time, death) ~ IMDC_f + factor(MLR_ge), data=d, x=TRUE)
fit_both   <- coxph(Surv(OS_time, death) ~ IMDC_f + factor(dNLR_ge) + factor(MLR_ge), data=d, x=TRUE)

r_dnlr <- pred_risk24(fit_dnlr, d)
r_mlr  <- pred_risk24(fit_mlr, d)
r_both <- pred_risk24(fit_both, d)

cat("--- Adding MLR to IMDC+dNLR (does MLR add value beyond dNLR?) ---\n")
res1 <- cont_nri(r_dnlr, r_both, d$death, d$OS_time)
cat(sprintf("NRI=%.3f (n_event=%d, n_nonevent=%d)\n", res1$nri, res1$n_event, res1$n_nonevent))
ci1 <- boot_nri_ci(Surv(OS_time,death) ~ IMDC_f + factor(dNLR_ge), Surv(OS_time,death) ~ IMDC_f + factor(dNLR_ge) + factor(MLR_ge), d, B=500)
cat("bootstrap median/95%CI:", ci1, "\n")

cat("\n--- Adding dNLR to IMDC+MLR (does dNLR add value beyond MLR?) ---\n")
res2 <- cont_nri(r_mlr, r_both, d$death, d$OS_time)
cat(sprintf("NRI=%.3f (n_event=%d, n_nonevent=%d)\n", res2$nri, res2$n_event, res2$n_nonevent))
ci2 <- boot_nri_ci(Surv(OS_time,death) ~ IMDC_f + factor(MLR_ge), Surv(OS_time,death) ~ IMDC_f + factor(dNLR_ge) + factor(MLR_ge), d, B=500)
cat("bootstrap median/95%CI:", ci2, "\n")

cat("\n--- IDI (integrated discrimination improvement), same comparisons ---\n")
idi <- function(risk_old, risk_new, event, time, horizon=24) {
  is_event <- event==1 & time<=horizon
  is_nonevent <- time>=horizon
  mean(risk_new[is_event]) - mean(risk_old[is_event]) - (mean(risk_new[is_nonevent]) - mean(risk_old[is_nonevent]))
}
cat("IDI (MLR added to dNLR):", idi(r_dnlr, r_both, d$death, d$OS_time), "\n")
cat("IDI (dNLR added to MLR):", idi(r_mlr, r_both, d$death, d$OS_time), "\n")
