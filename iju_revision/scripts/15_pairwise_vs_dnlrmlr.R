suppressMessages({library(survival)})
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
d <- readRDS(file.path(wd, "d.rds"))
d$IMDC_f <- factor(d$IMDC, levels=c(1,2))

# fixed classifications, using the full-data-derived cutoffs already established (same convention as 03_crp_vs_dnlrmlr.R)
d$grp_imdc    <- d$IMDC_f
d$grp_nlr     <- interaction(d$IMDC_f, factor(as.numeric(d$NLR > 2.5)),   drop=TRUE)
d$grp_dnlr    <- interaction(d$IMDC_f, factor(d$dNLR_ge), drop=TRUE)
d$grp_mlr     <- interaction(d$IMDC_f, factor(d$MLR_ge),  drop=TRUE)
d$grp_siri    <- interaction(d$IMDC_f, factor(as.numeric(d$SIRI > 1.576328)), drop=TRUE)
d$grp_crp     <- interaction(d$IMDC_f, factor(d$CRP_ge),  drop=TRUE)
d$grp_nlrmlr  <- interaction(d$IMDC_f, factor(as.numeric(d$NLR > 2.5 | d$MLR_ge==1)), drop=TRUE)
d$grp_dnlrmlr <- factor(d$dNLR_MLR_4grp, levels=c(0,1,2,3), labels=c("IL","IH","PL","PH"))

get_c <- function(fit, newdata) {
  lp <- predict(fit, newdata=newdata, type="lp")
  cc <- concordance(Surv(newdata$OS_time, newdata$death) ~ lp, reverse=TRUE)
  cc$concordance
}

pairwise_boot <- function(grpA, grpB, labelA, labelB, data, B=1000, seed=2024) {
  fA <- as.formula(paste("Surv(OS_time, death) ~", grpA))
  fB <- as.formula(paste("Surv(OS_time, death) ~", grpB))
  fitA <- coxph(fA, data=data, x=TRUE)
  fitB <- coxph(fB, data=data, x=TRUE)
  appA <- get_c(fitA, data); appB <- get_c(fitB, data)

  set.seed(seed)
  n <- nrow(data)
  diffs <- numeric(B)
  for (b in 1:B) {
    idx <- sample.int(n, n, replace=TRUE)
    db <- data[idx, ]
    fAb <- tryCatch(coxph(fA, data=db, x=TRUE), error=function(e) NULL)
    fBb <- tryCatch(coxph(fB, data=db, x=TRUE), error=function(e) NULL)
    if (is.null(fAb) || is.null(fBb)) { diffs[b] <- NA; next }
    cA <- tryCatch(get_c(fAb, data), error=function(e) NA)
    cB <- tryCatch(get_c(fBb, data), error=function(e) NA)
    diffs[b] <- cA - cB
  }
  ci <- quantile(diffs, c(.025,.975), na.rm=TRUE)
  cat(sprintf("%s (%.4f) vs %-14s (%.4f): mean diff=%+.4f  95%%CI [%+.4f, %+.4f]  %s\n",
              labelA, appA, labelB, appB, mean(diffs, na.rm=TRUE), ci[1], ci[2],
              ifelse(ci[1]>0 || ci[2]<0, "*", "")))
}

cat("=========== Pairwise bootstrap: IMDC-dNLR-MLR vs each other candidate model (B=1000) ===========\n\n")

comparisons <- list(
  c("grp_imdc",   "IMDC alone"),
  c("grp_nlr",    "IMDC-NLR"),
  c("grp_dnlr",   "IMDC-dNLR"),
  c("grp_mlr",    "IMDC-MLR"),
  c("grp_siri",   "IMDC-SIRI"),
  c("grp_crp",    "IMDC-CRP"),
  c("grp_nlrmlr", "IMDC-NLR-MLR")
)

for (comp in comparisons) {
  pairwise_boot("grp_dnlrmlr", comp[1], "IMDC-dNLR-MLR", comp[2], d)
}
