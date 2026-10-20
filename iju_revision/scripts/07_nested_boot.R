source("helpers.R")
suppressMessages({library(survival)})
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
d <- readRDS(file.path(wd, "d.rds"))
d$IMDC_f <- factor(d$IMDC, levels=c(1,2))

markers <- c("NLR","dNLR","MLR","PLR","SII","SIRI","PIV","CRP")
combo_pairs <- list(c("NLR","MLR"), c("dNLR","MLR"))

get_c <- function(lp, time, status) {
  as.numeric(concordance(Surv(time, status) ~ lp, reverse=TRUE)$concordance)
}

# fit full pipeline on a given data frame `dat`; returns list(selected_name, fit, cutoffs, lp_fn, c_apparent)
run_pipeline <- function(dat) {
  dat$IMDC_f <- factor(dat$IMDC, levels=c(1,2))
  cutoffs <- list()
  sig <- character(0)
  hi <- list()
  for (m in markers) {
    bc <- tryCatch(best_cutoff(dat$OS_time, dat$death, dat[[m]], 24), error=function(e) NULL)
    if (is.null(bc) || !is.finite(bc$cutoff)) next
    cutoffs[[m]] <- bc$cutoff
    hi[[m]] <- as.numeric(dat[[m]] > bc$cutoff)
    if (length(unique(hi[[m]])) < 2) next
    mv <- tryCatch(coxph(Surv(OS_time, death) ~ IMDC_f + hi[[m]], data=dat), error=function(e) NULL)
    if (is.null(mv)) next
    p <- tryCatch(summary(mv)$coefficients["hi[[m]]", "Pr(>|z|)"], error=function(e) NA)
    if (!is.na(p) && p < 0.05) sig <- c(sig, m)
  }

  candidates <- list()
  for (m in sig) {
    candidates[[m]] <- list(type="single", markers=m)
  }
  for (pr in combo_pairs) {
    if (all(pr %in% sig)) {
      nm <- paste(pr, collapse="_")
      candidates[[nm]] <- list(type="combo", markers=pr)
    }
  }
  candidates[["IMDC_only"]] <- list(type="imdc", markers=character(0))
  if (length(candidates) == 0) return(NULL)

  build_grp <- function(dat2, spec) {
    if (spec$type == "imdc") return(factor(dat2$IMDC, levels=c(1,2)))
    hiflags <- sapply(spec$markers, function(m) as.numeric(dat2[[m]] > cutoffs[[m]]))
    if (is.null(dim(hiflags))) hiflags <- matrix(hiflags, ncol=length(spec$markers))
    mhigh <- as.numeric(rowSums(hiflags, na.rm=TRUE) >= 1)
    interaction(factor(dat2$IMDC, levels=c(1,2)), factor(mhigh, levels=c(0,1)), drop=FALSE)
  }

  best_name <- NULL; best_c <- -Inf; best_fit <- NULL; best_grp_dat <- NULL
  for (nm in names(candidates)) {
    spec <- candidates[[nm]]
    grp <- build_grp(dat, spec)
    if (length(unique(grp)) < 2) next
    fit <- tryCatch(coxph(Surv(OS_time, death) ~ grp, data=dat), error=function(e) NULL)
    if (is.null(fit)) next
    lp <- tryCatch(predict(fit, type="lp"), error=function(e) NULL)
    if (is.null(lp)) next
    cst <- tryCatch(get_c(lp, dat$OS_time, dat$death), error=function(e) NA)
    if (is.na(cst)) next
    if (cst > best_c) { best_c <- cst; best_name <- nm; best_fit <- fit; best_spec <- spec }
  }
  if (is.null(best_name)) return(NULL)
  list(selected=best_name, spec=best_spec, fit=best_fit, cutoffs=cutoffs, c_apparent=best_c, build_grp=build_grp)
}

cat("=========== R1-2 / R3-3: fully nested bootstrap (cutoffs + marker selection + model selection re-done in each resample) ===========\n\n")

# reference: pipeline on original data
orig <- run_pipeline(d)
cat("Original-data pipeline selected:", orig$selected, " apparent C=", round(orig$c_apparent,4), "\n")
cat("Original cutoffs:\n"); print(orig$cutoffs)

set.seed(2024)
B <- 1000
n <- nrow(d)
optimism <- rep(NA_real_, B)
test_c   <- rep(NA_real_, B)
train_c  <- rep(NA_real_, B)
selected <- rep(NA_character_, B)
fail <- 0

t0 <- Sys.time()
for (b in 1:B) {
  idx <- sample.int(n, n, replace=TRUE)
  db <- d[idx, ]
  res <- tryCatch(run_pipeline(db), error=function(e) NULL)
  if (is.null(res)) { fail <- fail + 1; next }
  selected[b] <- res$selected
  train_c[b] <- res$c_apparent

  # evaluate the resample-derived model (its own cutoffs + its own coefficients) on the ORIGINAL full dataset
  grp_full <- tryCatch(res$build_grp(d, res$spec), error=function(e) NULL)
  if (is.null(grp_full)) { fail <- fail + 1; next }
  newdat <- data.frame(grp = grp_full)
  lp_full <- tryCatch(predict(res$fit, newdata=newdat, type="lp"), error=function(e) NULL)
  if (is.null(lp_full) || all(is.na(lp_full))) { fail <- fail + 1; next }
  keep <- !is.na(lp_full)
  cte <- tryCatch(get_c(lp_full[keep], d$OS_time[keep], d$death[keep]), error=function(e) NA)
  if (is.na(cte)) { fail <- fail + 1; next }
  test_c[b] <- cte
  optimism[b] <- train_c[b] - test_c[b]

  if (b %% 100 == 0) cat("  ...", b, "/", B, " elapsed:", round(difftime(Sys.time(),t0,units="secs"),1), "s\n")
}

cat("\nfailed resamples:", fail, "of", B, "\n")
cat("\nSelection frequency across resamples:\n")
print(table(selected, useNA="ifany"))

mean_optimism <- mean(optimism, na.rm=TRUE)
cat("\nmean optimism (train - test):", round(mean_optimism,4), " (n=", sum(!is.na(optimism)), ")\n")
cat("Apparent C-index of originally-selected model (dNLR-MLR) on full data:", round(orig$c_apparent,4), "\n")
cat("NESTED optimism-corrected C-index:", round(orig$c_apparent - mean_optimism,4), "\n")
cat("\n(for comparison, naive single-model bootstrap optimism-corrected C reported earlier: 0.705-0.711)\n")

cat("\n95% CI of test_c (out-of-resample performance of resample-selected pipeline, evaluated on full data):\n")
print(quantile(test_c, c(.025,.5,.975), na.rm=TRUE))

saveRDS(list(optimism=optimism, test_c=test_c, train_c=train_c, selected=selected, orig=orig),
        file.path(wd, "nested_boot_result.rds"))
cat("\nsaved nested_boot_result.rds\n")
