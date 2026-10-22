source("helpers.R")
suppressMessages({library(survival)})
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
d <- readRDS(file.path(wd, "d.rds"))
d$IMDC_f <- factor(d$IMDC, levels=c(1,2))

# ALC is added as a 9th single-marker candidate. Unlike the other 8 markers (ROC-derived cutoff,
# "high value = high risk"), ALC uses a median cutoff and "low value = high risk" (lymphopenia),
# consistent with 02b_ALC_median.R. ALC is NOT added to the pre-specified combo pairs (NLR+MLR,
# dNLR+MLR are the only combinations the manuscript's methodology considers) -- it is evaluated
# only as a standalone single-marker candidate, same footing as NLR, dNLR, MLR, SIRI, CRP, etc.

markers <- c("NLR","dNLR","MLR","PLR","SII","SIRI","PIV","CRP")
combo_pairs <- list(c("NLR","MLR"), c("dNLR","MLR"))

get_c <- function(lp, time, status) {
  as.numeric(concordance(Surv(time, status) ~ lp, reverse=TRUE)$concordance)
}

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

  # ALC: special-cased cutoff rule (median, low=high risk)
  cutoffs[["ALC"]] <- median(dat$ALC)
  hi_alc <- as.numeric(dat$ALC <= cutoffs[["ALC"]])
  if (length(unique(hi_alc)) >= 2) {
    mv_alc <- tryCatch(coxph(Surv(OS_time, death) ~ IMDC_f + hi_alc, data=dat), error=function(e) NULL)
    if (!is.null(mv_alc)) {
      p_alc <- tryCatch(summary(mv_alc)$coefficients["hi_alc", "Pr(>|z|)"], error=function(e) NA)
      if (!is.na(p_alc) && p_alc < 0.05) sig <- c(sig, "ALC")
    }
  }

  candidates <- list()
  for (m in sig) candidates[[m]] <- list(type="single", markers=m)
  for (pr in combo_pairs) {
    if (all(pr %in% sig)) { nm <- paste(pr, collapse="_"); candidates[[nm]] <- list(type="combo", markers=pr) }
  }
  candidates[["IMDC_only"]] <- list(type="imdc", markers=character(0))
  if (length(candidates) == 0) return(NULL)

  build_grp <- function(dat2, spec) {
    if (spec$type == "imdc") return(factor(dat2$IMDC, levels=c(1,2)))
    hiflags <- sapply(spec$markers, function(m) {
      if (m == "ALC") as.numeric(dat2$ALC <= cutoffs[["ALC"]]) else as.numeric(dat2[[m]] > cutoffs[[m]])
    })
    if (is.null(dim(hiflags))) hiflags <- matrix(hiflags, ncol=length(spec$markers))
    mhigh <- as.numeric(rowSums(hiflags, na.rm=TRUE) >= 1)
    interaction(factor(dat2$IMDC, levels=c(1,2)), factor(mhigh, levels=c(0,1)), drop=FALSE)
  }

  best_name <- NULL; best_c <- -Inf; best_fit <- NULL; best_spec <- NULL
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

cat("=========== Full-pipeline nested bootstrap WITH ALC as a 9th candidate marker (B=1000) ===========\n\n")

orig <- run_pipeline(d)
cat("Original-data pipeline selected:", orig$selected, " apparent C=", round(orig$c_apparent,4), "\n")

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
cat("\nSelection frequency across resamples (with ALC included):\n")
print(sort(table(selected, useNA="ifany"), decreasing=TRUE))

mean_optimism <- mean(optimism, na.rm=TRUE)
corrected_b <- orig$c_apparent - optimism
ci <- quantile(corrected_b, c(.025,.975), na.rm=TRUE)
cat("\nmean optimism (train - test):", round(mean_optimism,4), " (n=", sum(!is.na(optimism)), ")\n")
cat("Apparent C-index of originally-selected model on full data:", round(orig$c_apparent,4), "\n")
cat("NESTED optimism-corrected C-index:", round(orig$c_apparent - mean_optimism,4),
    " 95%CI [", round(ci[1],4), ",", round(ci[2],4), "]\n")

saveRDS(list(optimism=optimism, test_c=test_c, train_c=train_c, selected=selected, orig=orig),
        file.path(wd, "full_pipeline_with_alc_result.rds"))
cat("\nsaved full_pipeline_with_alc_result.rds\n")
