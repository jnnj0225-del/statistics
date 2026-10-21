source("helpers.R")
suppressMessages({library(survival)})
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
d <- readRDS(file.path(wd, "d.rds"))

markers <- c("NLR","dNLR","MLR","PLR","SII","SIRI","PIV","CRP")

cat("=========== Bootstrap 95% CI for cutoff values (24-month OS, time-dependent ROC, B=1000) ===========\n\n")

set.seed(42)
B <- 500
n <- nrow(d)
cutoff_mat <- matrix(NA_real_, nrow=B, ncol=length(markers), dimnames=list(NULL, markers))

t0 <- Sys.time()
for (b in 1:B) {
  idx <- sample.int(n, n, replace=TRUE)
  db <- d[idx, ]
  for (m in markers) {
    bc <- tryCatch(best_cutoff(db$OS_time, db$death, db[[m]], 24), error=function(e) NULL)
    if (!is.null(bc) && is.finite(bc$cutoff)) cutoff_mat[b, m] <- bc$cutoff
  }
  if (b %% 200 == 0) cat("  ...", b, "/", B, " elapsed:", round(difftime(Sys.time(),t0,units="secs"),1), "s\n")
}

orig_cutoffs <- sapply(markers, function(m) best_cutoff(d$OS_time, d$death, d[[m]], 24)$cutoff)

cat("\nResults:\n")
for (m in markers) {
  v <- cutoff_mat[, m]
  ci <- quantile(v, c(.025,.975), na.rm=TRUE)
  med <- median(v, na.rm=TRUE)
  cat(sprintf("%-5s original cutoff=%.4f  bootstrap median=%.4f  95%%CI [%.4f, %.4f]  (n_ok=%d/%d)\n",
              m, orig_cutoffs[m], med, ci[1], ci[2], sum(!is.na(v)), B))
}

saveRDS(list(cutoff_mat=cutoff_mat, orig_cutoffs=orig_cutoffs), file.path(wd, "cutoff_bootstrap_ci.rds"))
cat("\nsaved cutoff_bootstrap_ci.rds\n")
