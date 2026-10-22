suppressMessages({library(survival)})
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
d <- readRDS(file.path(wd, "d.rds"))
d$IMDC_f <- factor(d$IMDC, levels=c(1,2))
d$grp4 <- factor(d$dNLR_MLR_4grp, levels=c(0,1,2,3), labels=c("IL","IH","PL","PH"))
d$ALC_low <- as.numeric(d$ALC <= median(d$ALC))
d$ALC_low_f <- factor(d$ALC_low)
d$regimen <- factor(d$IOIO, levels=c(1,0), labels=c("ICI-ICI","ICI-TKI"))

fmt_table <- function(fit, label) {
  s <- summary(fit)
  ci <- s$conf.int
  co <- s$coefficients
  out <- data.frame(
    variable = rownames(ci),
    HR = sprintf("%.2f", ci[,"exp(coef)"]),
    CI95 = sprintf("%.2f–%.2f", ci[,"lower .95"], ci[,"upper .95"]),
    p = sprintf("%.4f", co[,"Pr(>|z|)"])
  )
  cat("\n===", label, "===\n")
  print(out, row.names=FALSE)
  invisible(out)
}

os_ref <- coxph(Surv(OS_time, death) ~ grp4 + regimen + bone_mets + nephrectomy + age, data=d)
os_alc <- coxph(Surv(OS_time, death) ~ grp4 + ALC_low_f + regimen + bone_mets + nephrectomy + age, data=d)
pfs_ref <- coxph(Surv(PFS_time, progression) ~ grp4 + regimen + bone_mets + nephrectomy + age, data=d)
pfs_alc <- coxph(Surv(PFS_time, progression) ~ grp4 + ALC_low_f + regimen + bone_mets + nephrectomy + age, data=d)

fmt_table(os_ref,  "OS - without ALC (matches published Table 4)")
fmt_table(os_alc,  "OS - WITH ALC added")
fmt_table(pfs_ref, "PFS - without ALC (matches published Table 4)")
fmt_table(pfs_alc, "PFS - WITH ALC added")
