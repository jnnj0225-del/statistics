source("helpers.R")
suppressMessages({library(survival); library(rms)})
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
d <- readRDS(file.path(wd, "d.rds"))

cat("=========== R2-2: ALC analysis ===========\n\n")

cat("--- (1) ALC cutoff via time-dependent ROC (24mo OS), Youden ---\n")
bc <- best_cutoff(d$OS_time, d$death, d$ALC, 24)
cat(sprintf("Derived cutoff=%.3f  sens=%.3f spec=%.3f AUC=%.3f\n", bc$cutoff, bc$sens, bc$spec, bc$auc))
cat("median ALC (literature-style cutoff):", median(d$ALC), "\n")
cat("n below/above derived cutoff:", sum(d$ALC<=bc$cutoff), sum(d$ALC>bc$cutoff), "\n")
cat("n below/above 1.0 (Takemura-style, x10^3/uL):", sum(d$ALC<=1.0), sum(d$ALC>1.0), "\n")
cat("n below/above median (Ueda-style):", sum(d$ALC<=median(d$ALC)), sum(d$ALC>median(d$ALC)), "\n")

d$ALC_low <- as.numeric(d$ALC <= bc$cutoff)  # ALC LOW is the adverse direction (lymphopenia)

cat("\n--- (2) Correlation: ALC vs dNLR, ALC vs MLR (Spearman) ---\n")
print(cor.test(d$ALC, d$dNLR, method="spearman"))
print(cor.test(d$ALC, d$MLR, method="spearman"))

cat("\n--- (3) Univariate Cox: ALC-low vs ALC-high, OS ---\n")
cx <- coxph(Surv(OS_time, death) ~ factor(ALC_low), data=d)
print(summary(cx)$conf.int); print(summary(cx)$coefficients)

cat("\n--- (4) Multivariable: IMDC + ALC, OS ---\n")
d$IMDC_f <- factor(d$IMDC, levels=c(1,2))
cx2 <- coxph(Surv(OS_time, death) ~ IMDC_f + factor(ALC_low), data=d)
print(summary(cx2)$conf.int); print(summary(cx2)$coefficients)

cat("\n--- (5) Decisive test: does dNLR/MLR remain significant adjusting for ALC? ---\n")
d$grp4 <- factor(d$dNLR_MLR_4grp, levels=c(0,1,2,3), labels=c("IL","IH","PL","PH"))
cx3 <- coxph(Surv(OS_time, death) ~ grp4 + factor(ALC_low), data=d)
print(summary(cx3)$conf.int); print(summary(cx3)$coefficients)

cat("also: IMDC + dNLR_ge + MLR_ge + ALC_low jointly (not the 4-group combo, individual terms)\n")
cx4 <- coxph(Surv(OS_time, death) ~ IMDC_f + factor(dNLR_ge) + factor(MLR_ge) + factor(ALC_low), data=d)
print(summary(cx4)$conf.int); print(summary(cx4)$coefficients)

cat("\n--- (6) sensitivity: alternative ALC cutoffs (median, 1.0) in the same multivariable model ---\n")
d$ALC_low_median <- as.numeric(d$ALC <= median(d$ALC))
d$ALC_low_1000   <- as.numeric(d$ALC <= 1.0)
for (v in c("ALC_low_median","ALC_low_1000")) {
  cat("--", v, "--\n")
  f <- as.formula(paste("Surv(OS_time, death) ~ IMDC_f + factor(", v, ")"))
  cxs <- coxph(f, data=d)
  print(summary(cxs)$conf.int)
}

cat("\n--- (7) C-index: IMDC+ALC, bootstrap B=1000 ---\n")
dd <- datadist(d); options(datadist="dd")
set.seed(1234)
fit <- cph(Surv(OS_time, death) ~ factor(ALC_low) + IMDC_f, data=d, x=TRUE, y=TRUE, surv=TRUE)
v <- validate(fit, method="boot", B=1000)
app_c <- 0.5*(v["Dxy","index.orig"]+1); opt_c <- 0.5*(v["Dxy","index.corrected"]+1)
cat(sprintf("IMDC-ALC: apparent C=%.3f corrected C=%.3f corrected slope=%.3f\n", app_c, opt_c, v["Slope","index.corrected"]))

cat("\n--- (8) 4-group IMDC x ALC classification (parallel to IMDC-dNLR-MLR) ---\n")
d$grp4_alc <- interaction(d$IMDC_f, d$ALC_low, drop=TRUE)
print(table(d$grp4_alc))
d$grp4_alc <- relevel(d$grp4_alc, ref=levels(d$grp4_alc)[1])
fit_km <- survfit(Surv(OS_time, death) ~ grp4_alc, data=d)
print(fit_km)
cx5 <- coxph(Surv(OS_time, death) ~ grp4_alc, data=d)
print(summary(cx5)$conf.int); print(summary(cx5)$coefficients)
