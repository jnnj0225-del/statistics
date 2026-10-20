suppressMessages({library(survival); library(rms)})
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
d <- readRDS(file.path(wd, "d.rds"))
d$IMDC_f <- factor(d$IMDC, levels=c(1,2))
d$grp4 <- factor(d$dNLR_MLR_4grp, levels=c(0,1,2,3), labels=c("IL","IH","PL","PH"))
d$ALC_low <- as.numeric(d$ALC <= median(d$ALC))  # median cutoff, balanced 96/96
d$ALC_low_f <- factor(d$ALC_low)

cat("=========== R2-2 (b): ALC with median cutoff (primary) ===========\n\n")

cat("--- univariate, ALC-low(<=median) vs high, OS ---\n")
cx <- coxph(Surv(OS_time, death) ~ factor(ALC_low), data=d)
print(summary(cx)$conf.int); print(summary(cx)$coefficients)
cat("median OS by ALC group:\n")
print(survfit(Surv(OS_time, death) ~ factor(ALC_low), data=d))

cat("\n--- multivariable IMDC + ALC(median) ---\n")
cx2 <- coxph(Surv(OS_time, death) ~ IMDC_f + factor(ALC_low), data=d)
print(summary(cx2)$conf.int); print(summary(cx2)$coefficients)

cat("\n--- decisive test: dNLR-MLR grp4 adjusted for ALC(median) ---\n")
cx3 <- coxph(Surv(OS_time, death) ~ grp4 + factor(ALC_low), data=d)
print(summary(cx3)$conf.int); print(summary(cx3)$coefficients)

cat("\n--- joint individual terms: IMDC + dNLR_ge + MLR_ge + ALC(median) ---\n")
cx4 <- coxph(Surv(OS_time, death) ~ IMDC_f + factor(dNLR_ge) + factor(MLR_ge) + factor(ALC_low), data=d)
print(summary(cx4)$conf.int); print(summary(cx4)$coefficients)

cat("\n--- PFS versions ---\n")
cxp <- coxph(Surv(PFS_time, progression) ~ factor(ALC_low), data=d)
print(summary(cxp)$conf.int)
cxp2 <- coxph(Surv(PFS_time, progression) ~ grp4 + factor(ALC_low), data=d)
print(summary(cxp2)$conf.int)

cat("\n--- C-index: IMDC alone / IMDC+ALC(median) / IMDC+dNLR+MLR, bootstrap B=1000 ---\n")
dd <- datadist(d); options(datadist="dd")
run_model <- function(formula_rhs, label) {
  set.seed(1234)
  f <- as.formula(paste("Surv(OS_time, death) ~", formula_rhs))
  fit <- cph(f, data=d, x=TRUE, y=TRUE, surv=TRUE)
  v <- validate(fit, method="boot", B=1000)
  app_c <- 0.5*(v["Dxy","index.orig"]+1); opt_c <- 0.5*(v["Dxy","index.corrected"]+1)
  cat(sprintf("%-22s apparent C=%.3f  corrected C=%.3f  corrected slope=%.3f\n",
              label, app_c, opt_c, v["Slope","index.corrected"]))
}
run_model("IMDC_f", "IMDC alone")
run_model("IMDC_f + ALC_low_f", "IMDC+ALC(median)")
run_model("grp4", "IMDC-dNLR-MLR")
run_model("grp4 + ALC_low_f", "IMDC-dNLR-MLR+ALC")

cat("\n--- 4-group IMDC x ALC(median) classification ---\n")
d$grp4_alc <- interaction(d$IMDC_f, d$ALC_low, drop=TRUE)
print(table(d$grp4_alc))
d$grp4_alc <- relevel(d$grp4_alc, ref=levels(d$grp4_alc)[1])
print(survfit(Surv(OS_time, death) ~ grp4_alc, data=d))
cx5 <- coxph(Surv(OS_time, death) ~ grp4_alc, data=d)
print(summary(cx5)$conf.int); print(summary(cx5)$coefficients)
