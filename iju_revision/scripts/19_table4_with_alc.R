suppressMessages({library(survival)})
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
d <- readRDS(file.path(wd, "d.rds"))
d$IMDC_f <- factor(d$IMDC, levels=c(1,2))
d$grp4 <- factor(d$dNLR_MLR_4grp, levels=c(0,1,2,3), labels=c("IL","IH","PL","PH"))
d$ALC_low <- as.numeric(d$ALC <= median(d$ALC))
d$ALC_low_f <- factor(d$ALC_low)
d$regimen <- factor(d$IOIO, levels=c(1,0), labels=c("ICI-ICI","ICI-TKI"))

cat("=========== Table 4 - style multivariable analysis, WITH ALC added ===========\n")
cat("(covariates: grp4 + regimen + bone_mets + nephrectomy + age, same as published Table 4)\n\n")

cat("--- OS, without ALC (reference / matches published Table 4) ---\n")
os_ref <- coxph(Surv(OS_time, death) ~ grp4 + regimen + bone_mets + nephrectomy + age, data=d)
print(summary(os_ref)$conf.int)
print(summary(os_ref)$coefficients[,c("coef","Pr(>|z|)")])

cat("\n--- OS, WITH ALC added ---\n")
os_alc <- coxph(Surv(OS_time, death) ~ grp4 + ALC_low_f + regimen + bone_mets + nephrectomy + age, data=d)
print(summary(os_alc)$conf.int)
print(summary(os_alc)$coefficients[,c("coef","Pr(>|z|)")])

cat("\n\n--- PFS, without ALC (reference / matches published Table 4) ---\n")
pfs_ref <- coxph(Surv(PFS_time, progression) ~ grp4 + regimen + bone_mets + nephrectomy + age, data=d)
print(summary(pfs_ref)$conf.int)
print(summary(pfs_ref)$coefficients[,c("coef","Pr(>|z|)")])

cat("\n--- PFS, WITH ALC added ---\n")
pfs_alc <- coxph(Surv(PFS_time, progression) ~ grp4 + ALC_low_f + regimen + bone_mets + nephrectomy + age, data=d)
print(summary(pfs_alc)$conf.int)
print(summary(pfs_alc)$coefficients[,c("coef","Pr(>|z|)")])
