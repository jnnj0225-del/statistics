suppressMessages({library(survival)})
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
d <- readRDS(file.path(wd, "d.rds"))
d$IMDC_f <- factor(d$IMDC, levels=c(1,2))
d$grp4_dnlrmlr <- factor(d$dNLR_MLR_4grp, levels=c(0,1,2,3), labels=c("IL","IH","PL","PH"))
d$grp4_crp <- interaction(d$IMDC_f, factor(d$CRP_ge), drop=TRUE)

cat("=========== R2-4 / R3-6: IMDC-dNLR-MLR vs IMDC-CRP, paired bootstrap C-index difference ===========\n\n")
cat("grp4_crp table:\n"); print(table(d$grp4_crp))

get_c <- function(fit, newdata) {
  lp <- predict(fit, newdata=newdata, type="lp")
  cc <- concordance(Surv(newdata$OS_time, newdata$death) ~ lp, reverse=TRUE)
  cc$concordance
}

fitA <- coxph(Surv(OS_time, death) ~ grp4_dnlrmlr, data=d, x=TRUE)
fitB <- coxph(Surv(OS_time, death) ~ grp4_crp, data=d, x=TRUE)
appA <- get_c(fitA, d); appB <- get_c(fitB, d)
cat(sprintf("Apparent C: IMDC-dNLR-MLR=%.4f  IMDC-CRP=%.4f  diff=%.4f\n", appA, appB, appA-appB))

n <- nrow(d)
B <- 1000
set.seed(2024)
diffs <- numeric(B); cA_boot <- numeric(B); cB_boot <- numeric(B)
optA <- numeric(B); optB <- numeric(B)
for (b in 1:B) {
  idx <- sample.int(n, n, replace=TRUE)
  db <- d[idx, ]
  fA <- tryCatch(coxph(Surv(OS_time, death) ~ grp4_dnlrmlr, data=db, x=TRUE), error=function(e) NULL)
  fB <- tryCatch(coxph(Surv(OS_time, death) ~ grp4_crp, data=db, x=TRUE), error=function(e) NULL)
  if (is.null(fA) || is.null(fB)) next
  cA_train <- tryCatch(get_c(fA, db), error=function(e) NA)
  cA_test  <- tryCatch(get_c(fA, d), error=function(e) NA)
  cB_train <- tryCatch(get_c(fB, db), error=function(e) NA)
  cB_test  <- tryCatch(get_c(fB, d), error=function(e) NA)
  optA[b] <- cA_train - cA_test
  optB[b] <- cB_train - cB_test
  cA_boot[b] <- cA_test; cB_boot[b] <- cB_test
  diffs[b] <- cA_test - cB_test
}
corrA <- appA - mean(optA, na.rm=TRUE)
corrB <- appB - mean(optB, na.rm=TRUE)
cat(sprintf("\nOptimism-corrected C: IMDC-dNLR-MLR=%.4f  IMDC-CRP=%.4f\n", corrA, corrB))
cat(sprintf("Bootstrap 95%% CI, apparent C IMDC-dNLR-MLR: %.4f - %.4f\n", quantile(appA - optA, .025, na.rm=TRUE), quantile(appA - optA, .975, na.rm=TRUE)))
cat(sprintf("Bootstrap 95%% CI, apparent C IMDC-CRP: %.4f - %.4f\n", quantile(appB - optB, .025, na.rm=TRUE), quantile(appB - optB, .975, na.rm=TRUE)))
ci <- quantile(diffs, c(.025,.975), na.rm=TRUE)
cat(sprintf("\nPaired difference (dNLR-MLR minus CRP), out-of-resample evaluated each time:\n  mean diff=%.4f  95%% CI: %.4f to %.4f\n", mean(diffs, na.rm=TRUE), ci[1], ci[2]))
cat("failed resamples:", sum(is.na(diffs)), "of", B, "\n")
