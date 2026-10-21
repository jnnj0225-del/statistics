suppressMessages({library(survival)})
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
d <- readRDS(file.path(wd, "d.rds"))
d$IMDC_f <- factor(d$IMDC, levels=c(1,2))
d$regimen <- factor(d$IOIO, levels=c(1,0), labels=c("ICI-ICI","ICI-TKI"))
d$grp4_dnlr <- interaction(d$IMDC_f, factor(d$dNLR_ge), drop=TRUE)
d$grp4_mlr  <- interaction(d$IMDC_f, factor(d$MLR_ge),  drop=TRUE)

cat("=========== Classification x regimen interaction tests (LRT), OS ===========\n\n")

test_interaction <- function(grpvar, label) {
  f_main <- as.formula(paste("Surv(OS_time, death) ~", grpvar, "+ regimen"))
  f_int  <- as.formula(paste("Surv(OS_time, death) ~", grpvar, "* regimen"))
  m_main <- coxph(f_main, data=d)
  m_int  <- coxph(f_int, data=d)
  a <- anova(m_main, m_int)
  cat(sprintf("--- %s x regimen ---\n", label))
  print(a)
  cat("\n")
}

test_interaction("grp4_dnlr", "IMDC-dNLR")
test_interaction("grp4_mlr",  "IMDC-MLR")
