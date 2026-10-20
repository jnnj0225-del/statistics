suppressMessages({library(survival)})
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
d <- readRDS(file.path(wd, "d.rds"))

cat("=========== R1-1: dNLR-high vs MLR-high overlap ===========\n")
tab <- table(dNLR_high=d$dNLR_ge, MLR_high=d$MLR_ge)
print(tab)
neither <- sum(d$dNLR_ge==0 & d$MLR_ge==0)
dnlr_only <- sum(d$dNLR_ge==1 & d$MLR_ge==0)
mlr_only <- sum(d$dNLR_ge==0 & d$MLR_ge==1)
both <- sum(d$dNLR_ge==1 & d$MLR_ge==1)
cat(sprintf("\nneither=%d (%.1f%%)  dNLR-only=%d (%.1f%%)  MLR-only=%d (%.1f%%)  both=%d (%.1f%%)\n",
            neither, 100*neither/192, dnlr_only, 100*dnlr_only/192, mlr_only, 100*mlr_only/192, both, 100*both/192))
cat("Jaccard overlap (both / union):", both / (dnlr_only+mlr_only+both), "\n")
cat("Cohen's kappa:\n")
po <- (neither+both)/192
pe <- (mean(d$dNLR_ge)*mean(d$MLR_ge)) + ((1-mean(d$dNLR_ge))*(1-mean(d$MLR_ge)))
cat("kappa =", (po-pe)/(1-pe), "\n")

cat("\n\n=========== R3-5: dNLR-high vs IMDC neutrophilia criterion overlap ===========\n")
tab2 <- table(dNLR_high=d$dNLR_ge, IMDC_neutrophilia=d$ANC_group)
print(tab2)
cat("\n% of dNLR-high patients who ALSO meet IMDC neutrophilia criterion:\n")
dnlr_hi <- d[d$dNLR_ge==1,]
cat(sprintf("%d / %d = %.1f%%\n", sum(dnlr_hi$ANC_group==1), nrow(dnlr_hi), 100*mean(dnlr_hi$ANC_group==1)))
cat("\n% of IMDC-neutrophilia patients who are ALSO dNLR-high:\n")
neut <- d[d$ANC_group==1,]
cat(sprintf("%d / %d = %.1f%%\n", sum(neut$dNLR_ge==1), nrow(neut), 100*mean(neut$dNLR_ge==1)))
po2 <- mean(d$dNLR_ge==d$ANC_group)
pe2 <- mean(d$dNLR_ge)*mean(d$ANC_group) + (1-mean(d$dNLR_ge))*(1-mean(d$ANC_group))
cat("simple agreement:", po2, " Cohen kappa:", (po2-pe2)/(1-pe2), "\n")
cat("Spearman ANC (continuous) vs dNLR (continuous):\n")
print(cor.test(d$ANC, d$dNLR, method="spearman"))

cat("\n\n=========== R2-3: regimen-stratified C-index refresh ===========\n")
d$IMDC_f <- factor(d$IMDC, levels=c(1,2))
d$grp4 <- factor(d$dNLR_MLR_4grp, levels=c(0,1,2,3), labels=c("IL","IH","PL","PH"))
suppressMessages(library(rms))
dd <- datadist(d); options(datadist="dd")
run_model <- function(sub, label) {
  set.seed(1234)
  fit <- cph(Surv(OS_time, death) ~ grp4, data=sub, x=TRUE, y=TRUE, surv=TRUE)
  v <- validate(fit, method="boot", B=1000)
  app_c <- 0.5*(v["Dxy","index.orig"]+1); opt_c <- 0.5*(v["Dxy","index.corrected"]+1)
  cat(sprintf("%-10s n=%3d events=%3d | apparent C=%.3f  corrected C=%.3f  corrected slope=%.3f\n",
              label, nrow(sub), sum(sub$death), app_c, opt_c, v["Slope","index.corrected"]))
}
run_model(subset(d, IOIO==1), "ICI-ICI")
run_model(subset(d, IOIO==0), "ICI-TKI")
