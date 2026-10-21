suppressMessages({library(survival)})
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
d <- readRDS(file.path(wd, "d.rds"))
d$IMDC_f <- factor(d$IMDC, levels=c(1,2))
d$regimen <- factor(d$IOIO, levels=c(1,0), labels=c("ICI-ICI","ICI-TKI"))
d$grp4_dnlrmlr <- factor(d$dNLR_MLR_4grp, levels=c(0,1,2,3), labels=c("IL","IH","PL","PH"))
d$grp4_crp <- interaction(d$IMDC_f, factor(d$CRP_ge), drop=TRUE)

cat("=========== Classification x regimen interaction tests (likelihood ratio test), OS ===========\n\n")

test_interaction <- function(grpvar, label) {
  f_main <- as.formula(paste("Surv(OS_time, death) ~", grpvar, "+ regimen"))
  f_int  <- as.formula(paste("Surv(OS_time, death) ~", grpvar, "* regimen"))
  m_main <- coxph(f_main, data=d)
  m_int  <- coxph(f_int, data=d)
  a <- anova(m_main, m_int)
  cat(sprintf("--- %s x regimen ---\n", label))
  print(a)
  cat("\n")
  invisible(a)
}

cat(">>> Reference (already in manuscript): dNLR-MLR classification x regimen\n")
test_interaction("grp4_dnlrmlr", "IMDC-dNLR-MLR")

cat(">>> New: CRP classification x regimen\n")
test_interaction("grp4_crp", "IMDC-CRP")

cat("\n=========== For context: CRP vs dNLR-MLR, apparent C-index within ICI-TKI only (n=88) ===========\n")
d_tki <- d[d$regimen == "ICI-TKI", ]
get_c <- function(fit, newdata) {
  lp <- predict(fit, newdata=newdata, type="lp")
  cc <- concordance(Surv(newdata$OS_time, newdata$death) ~ lp, reverse=TRUE)
  cc$concordance
}
fitA <- coxph(Surv(OS_time, death) ~ grp4_dnlrmlr, data=d_tki, x=TRUE)
fitB <- coxph(Surv(OS_time, death) ~ grp4_crp, data=d_tki, x=TRUE)
cat("apparent C, dNLR-MLR (ICI-TKI only):", round(get_c(fitA, d_tki), 4), "\n")
cat("apparent C, CRP (ICI-TKI only):", round(get_c(fitB, d_tki), 4), "\n")

# paired bootstrap difference, within ICI-TKI only
set.seed(99)
n <- nrow(d_tki)
B <- 1000
diffs <- numeric(B)
for (b in 1:B) {
  idx <- sample.int(n, n, replace=TRUE)
  db <- d_tki[idx, ]
  fA <- tryCatch(coxph(Surv(OS_time, death) ~ grp4_dnlrmlr, data=db, x=TRUE), error=function(e) NULL)
  fB <- tryCatch(coxph(Surv(OS_time, death) ~ grp4_crp, data=db, x=TRUE), error=function(e) NULL)
  if (is.null(fA) || is.null(fB)) { diffs[b] <- NA; next }
  cA <- tryCatch(get_c(fA, d_tki), error=function(e) NA)
  cB <- tryCatch(get_c(fB, d_tki), error=function(e) NA)
  diffs[b] <- cA - cB
}
ci <- quantile(diffs, c(.025,.975), na.rm=TRUE)
cat(sprintf("\nPaired bootstrap (within ICI-TKI only, B=1000): dNLR-MLR minus CRP\n  mean diff=%.4f  95%% CI: %.4f to %.4f\n",
            mean(diffs, na.rm=TRUE), ci[1], ci[2]))
