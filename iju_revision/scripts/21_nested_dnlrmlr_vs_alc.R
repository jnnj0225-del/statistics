source("helpers.R")
suppressMessages({library(survival)})
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
d <- readRDS(file.path(wd, "d.rds"))
d$IMDC_f <- factor(d$IMDC, levels=c(1,2))

get_c <- function(lp, time, status) as.numeric(concordance(Surv(time, status) ~ lp, reverse=TRUE)$concordance)

# dNLR-MLR: cutoffs re-derived via ROC per resample
build_grp_dnlrmlr <- function(dat2, cutoffs) {
  hiflags <- sapply(c("dNLR","MLR"), function(m) as.numeric(dat2[[m]] > cutoffs[[m]]))
  mhigh <- as.numeric(rowSums(hiflags, na.rm=TRUE) >= 1)
  interaction(factor(dat2$IMDC, levels=c(1,2)), factor(mhigh, levels=c(0,1)), drop=FALSE)
}
# ALC: median cutoff re-derived per resample, low=high risk
build_grp_alc <- function(dat2, cutoff) {
  hi <- as.numeric(dat2$ALC <= cutoff)
  interaction(factor(dat2$IMDC, levels=c(1,2)), factor(hi, levels=c(0,1)), drop=FALSE)
}

fit_eval_dnlrmlr <- function(data, db) {
  cutoffs_b <- list(); ok <- TRUE
  for (m in c("dNLR","MLR")) {
    bc <- tryCatch(best_cutoff(db$OS_time, db$death, db[[m]], 24), error=function(e) NULL)
    if (is.null(bc) || !is.finite(bc$cutoff)) { ok <- FALSE; break }
    cutoffs_b[[m]] <- bc$cutoff
  }
  if (!ok) return(NULL)
  grp_b <- build_grp_dnlrmlr(db, cutoffs_b)
  if (length(unique(grp_b)) < 2) return(NULL)
  fit_b <- tryCatch(coxph(Surv(OS_time, death) ~ grp_b, data=db), error=function(e) NULL)
  if (is.null(fit_b)) return(NULL)
  grp_eval <- build_grp_dnlrmlr(data, cutoffs_b)
  newdat <- data.frame(grp_b = grp_eval)
  lp_eval <- tryCatch(predict(fit_b, newdata=newdat, type="lp"), error=function(e) NULL)
  if (is.null(lp_eval) || all(is.na(lp_eval))) return(NULL)
  keep <- !is.na(lp_eval)
  tryCatch(get_c(lp_eval[keep], data$OS_time[keep], data$death[keep]), error=function(e) NULL)
}

fit_eval_alc <- function(data, db) {
  cutoff_b <- median(db$ALC)
  grp_b <- build_grp_alc(db, cutoff_b)
  if (length(unique(grp_b)) < 2) return(NULL)
  fit_b <- tryCatch(coxph(Surv(OS_time, death) ~ grp_b, data=db), error=function(e) NULL)
  if (is.null(fit_b)) return(NULL)
  grp_eval <- build_grp_alc(data, cutoff_b)
  newdat <- data.frame(grp_b = grp_eval)
  lp_eval <- tryCatch(predict(fit_b, newdata=newdat, type="lp"), error=function(e) NULL)
  if (is.null(lp_eval) || all(is.na(lp_eval))) return(NULL)
  keep <- !is.na(lp_eval)
  tryCatch(get_c(lp_eval[keep], data$OS_time[keep], data$death[keep]), error=function(e) NULL)
}

cat("=========== Nested paired bootstrap: IMDC-dNLR-MLR vs IMDC-ALC (B=1000) ===========\n\n")
set.seed(2024)
n <- nrow(d)
B <- 1000
diffs <- rep(NA_real_, B)
t0 <- Sys.time()
for (b in 1:B) {
  idx <- sample.int(n, n, replace=TRUE)
  db <- d[idx, ]
  cA <- fit_eval_dnlrmlr(d, db)
  cB <- fit_eval_alc(d, db)
  if (is.null(cA) || is.null(cB)) next
  diffs[b] <- cA - cB
  if (b %% 200 == 0) cat("  ...", b, "/", B, " elapsed:", round(difftime(Sys.time(),t0,units="secs"),1), "s\n")
}
ci <- quantile(diffs, c(.025,.975), na.rm=TRUE)
cat(sprintf("\nPaired difference (dNLR-MLR minus ALC), nested: mean=%.4f  95%% CI: %.4f to %.4f  (ok=%d/%d)\n",
            mean(diffs, na.rm=TRUE), ci[1], ci[2], sum(!is.na(diffs)), B))
