suppressMessages({library(survival)})
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
d <- readRDS(file.path(wd, "d.rds"))
d$IMDC_f <- factor(d$IMDC, levels=c(1,2))
d$grp_dnlrmlr <- factor(d$dNLR_MLR_4grp, levels=c(0,1,2,3), labels=c("IL","IH","PL","PH"))
d$ALC_low <- as.numeric(d$ALC <= median(d$ALC))
d$grp_alc <- interaction(d$IMDC_f, factor(d$ALC_low), drop=TRUE)

get_c <- function(fit, newdata) {
  lp <- predict(fit, newdata=newdata, type="lp")
  cc <- concordance(Surv(newdata$OS_time, newdata$death) ~ lp, reverse=TRUE)
  cc$concordance
}

cat("=========== IMDC-dNLR-MLR vs IMDC-ALC, apparent-level paired bootstrap (B=1000) ===========\n\n")
fitA <- coxph(Surv(OS_time, death) ~ grp_dnlrmlr, data=d, x=TRUE)
fitB <- coxph(Surv(OS_time, death) ~ grp_alc, data=d, x=TRUE)
appA <- get_c(fitA, d); appB <- get_c(fitB, d)
cat(sprintf("apparent C: dNLR-MLR=%.4f  ALC=%.4f  diff=%.4f\n", appA, appB, appA-appB))

set.seed(2024)
n <- nrow(d)
B <- 1000
diffs <- numeric(B)
for (b in 1:B) {
  idx <- sample.int(n, n, replace=TRUE)
  db <- d[idx, ]
  fA <- tryCatch(coxph(Surv(OS_time, death) ~ grp_dnlrmlr, data=db, x=TRUE), error=function(e) NULL)
  fB <- tryCatch(coxph(Surv(OS_time, death) ~ grp_alc, data=db, x=TRUE), error=function(e) NULL)
  if (is.null(fA) || is.null(fB)) { diffs[b] <- NA; next }
  cA <- tryCatch(get_c(fA, d), error=function(e) NA)
  cB <- tryCatch(get_c(fB, d), error=function(e) NA)
  diffs[b] <- cA - cB
}
ci <- quantile(diffs, c(.025,.975), na.rm=TRUE)
cat(sprintf("\nPaired difference (dNLR-MLR minus ALC): mean=%.4f  95%% CI: %.4f to %.4f  failed=%d/%d\n",
            mean(diffs, na.rm=TRUE), ci[1], ci[2], sum(is.na(diffs)), B))
