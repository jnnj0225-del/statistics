source("helpers.R")
suppressMessages({library(survival)})
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
d <- readRDS(file.path(wd, "d.rds"))
d$IMDC_f <- factor(d$IMDC, levels=c(1,2))

markers <- c("NLR","dNLR","MLR","PLR","SII","SIRI","PIV","CRP")
combo_pairs <- list(c("NLR","MLR"), c("dNLR","MLR"))

get_c <- function(lp, time, status) as.numeric(concordance(Surv(time, status) ~ lp, reverse=TRUE)$concordance)

run_pipeline <- function(dat) {
  dat$IMDC_f <- factor(dat$IMDC, levels=c(1,2))
  cutoffs <- list(); sig <- character(0); hi <- list()
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
  for (m in sig) candidates[[m]] <- list(type="single", markers=m)
  for (pr in combo_pairs) if (all(pr %in% sig)) { nm <- paste(pr, collapse="_"); candidates[[nm]] <- list(type="combo", markers=pr) }
  candidates[["IMDC_only"]] <- list(type="imdc", markers=character(0))
  if (length(candidates) == 0) return(NULL)

  build_grp <- function(dat2, spec) {
    if (spec$type == "imdc") return(factor(dat2$IMDC, levels=c(1,2)))
    hiflags <- sapply(spec$markers, function(m) as.numeric(dat2[[m]] > cutoffs[[m]]))
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

run_full_pipeline_boot <- function(dat, label, B=1000, seed=2024) {
  cat("\n=========== Full-pipeline nested bootstrap:", label, " (n=", nrow(dat), ") ===========\n\n")
  orig <- run_pipeline(dat)
  cat("Pipeline on this group's own data selected:", orig$selected, " apparent C=", round(orig$c_apparent,4), "\n")

  set.seed(seed)
  n <- nrow(dat)
  optimism <- rep(NA_real_, B); test_c <- rep(NA_real_, B); train_c <- rep(NA_real_, B)
  selected <- rep(NA_character_, B); fail <- 0
  t0 <- Sys.time()
  for (b in 1:B) {
    idx <- sample.int(n, n, replace=TRUE)
    db <- dat[idx, ]
    res <- tryCatch(run_pipeline(db), error=function(e) NULL)
    if (is.null(res)) { fail <- fail + 1; next }
    selected[b] <- res$selected
    train_c[b] <- res$c_apparent

    grp_full <- tryCatch(res$build_grp(dat, res$spec), error=function(e) NULL)
    if (is.null(grp_full)) { fail <- fail + 1; next }
    newdat <- data.frame(grp = grp_full)
    lp_full <- tryCatch(predict(res$fit, newdata=newdat, type="lp"), error=function(e) NULL)
    if (is.null(lp_full) || all(is.na(lp_full))) { fail <- fail + 1; next }
    keep <- !is.na(lp_full)
    cte <- tryCatch(get_c(lp_full[keep], dat$OS_time[keep], dat$death[keep]), error=function(e) NA)
    if (is.na(cte)) { fail <- fail + 1; next }
    test_c[b] <- cte
    optimism[b] <- train_c[b] - test_c[b]
    if (b %% 200 == 0) cat("  ...", b, "/", B, " elapsed:", round(difftime(Sys.time(),t0,units="secs"),1), "s\n")
  }
  cat("failed resamples:", fail, "of", B, "\n")
  cat("Selection frequency:\n"); print(sort(table(selected, useNA="ifany"), decreasing=TRUE))
  mean_opt <- mean(optimism, na.rm=TRUE)
  corrected_b <- orig$c_apparent - optimism
  ci <- quantile(corrected_b, c(.025,.975), na.rm=TRUE)
  cat(sprintf("Nested optimism-corrected C-index: %.4f  95%%CI [%.4f, %.4f]\n",
              orig$c_apparent - mean_opt, ci[1], ci[2]))
  list(label=label, orig=orig, optimism=optimism, test_c=test_c, train_c=train_c, selected=selected,
       c_corrected=orig$c_apparent - mean_opt, ci_lo=ci[1], ci_hi=ci[2], fail=fail, B=B)
}

d_ici <- d[d$IOIO == 1, ]
d_tki <- d[d$IOIO == 0, ]

res_overall <- list(label="Overall (n=192)",
                     selected_freq=c(dNLR_MLR=413, CRP=339, dNLR=86, MLR=83, NLR_MLR=33, NLR=23, SIRI=12, PIV=6, PLR=3, IMDC_only=2),
                     c_corrected=0.6797, ci_lo=0.6147, ci_hi=0.7383)  # from the earlier completed run (07_nested_boot.R), B=1000

res_ici <- run_full_pipeline_boot(d_ici, "ICI-ICI", B=1000, seed=2024)
res_tki <- run_full_pipeline_boot(d_tki, "ICI-TKI", B=1000, seed=2024)

saveRDS(list(overall=res_overall, ici=res_ici, tki=res_tki), file.path(wd, "full_pipeline_regimen_result.rds"))
cat("\nsaved full_pipeline_regimen_result.rds\n")
