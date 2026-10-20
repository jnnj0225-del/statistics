source("helpers.R")
suppressMessages({library(survival)})
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
d <- readRDS(file.path(wd, "d.rds"))

markers <- list(NLR=d$NLR, dNLR=d$dNLR, MLR=d$MLR, CRP=d$CRP)
# also need PLR, SII, SIRI, PIV but not in d.rds (not loaded) -- add dNLR/MLR/NLR/CRP which are the ones in the final model + comparator

cat("=========== R3-4: cutoff sensitivity, 18-month vs 24-month OS ===========\n\n")
reported24 <- list(NLR=c(2.50,0.629), dNLR=c(2.29,0.680), MLR=c(0.38,0.646), CRP=c(4.00,0.730))

for (nm in names(markers)) {
  x <- markers[[nm]]
  b24 <- best_cutoff(d$OS_time, d$death, x, 24)
  b18 <- best_cutoff(d$OS_time, d$death, x, 18)
  cat(sprintf("%-5s  24mo: cutoff=%.3f AUC=%.3f (reported %.2f/%.3f)   |   18mo: cutoff=%.3f AUC=%.3f\n",
              nm, b24$cutoff, b24$auc, reported24[[nm]][1], reported24[[nm]][2], b18$cutoff, b18$auc))
}

cat("\n--- classification agreement: dNLR+MLR 4-group using 18mo-derived cutoffs vs original (24mo) 4-group ---\n")
b18_dnlr <- best_cutoff(d$OS_time, d$death, d$dNLR, 18)
b18_mlr  <- best_cutoff(d$OS_time, d$death, d$MLR, 18)
cat("18mo cutoffs: dNLR=", b18_dnlr$cutoff, " MLR=", b18_mlr$cutoff, "\n")
d$dNLR_ge_18 <- as.numeric(d$dNLR > b18_dnlr$cutoff)
d$MLR_ge_18  <- as.numeric(d$MLR > b18_mlr$cutoff)
d$marker_high_18 <- as.numeric(d$dNLR_ge_18==1 | d$MLR_ge_18==1)
d$marker_high_24 <- as.numeric(d$dNLR_ge==1 | d$MLR_ge==1)
cat("agreement table (18mo-based marker-high vs 24mo-based marker-high):\n")
print(table(d$marker_high_18, d$marker_high_24))
cat("simple agreement:", mean(d$marker_high_18==d$marker_high_24), "\n")

d$IMDC_f <- factor(d$IMDC, levels=c(1,2))
d$grp4_18 <- interaction(d$IMDC_f, d$marker_high_18, drop=TRUE)
cat("\n4-group table using 18mo cutoffs:\n"); print(table(d$grp4_18))
cx18 <- coxph(Surv(OS_time, death) ~ grp4_18, data=d)
cat("HRs, 18mo-cutoff classification (OS):\n")
print(summary(cx18)$conf.int)
