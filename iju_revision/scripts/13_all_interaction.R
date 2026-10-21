suppressMessages({library(survival)})
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
d <- readRDS(file.path(wd, "d.rds"))
d$IMDC_f <- factor(d$IMDC, levels=c(1,2))
d$regimen <- factor(d$IOIO, levels=c(1,0), labels=c("ICI-ICI","ICI-TKI"))

d$NLR_ge_bin <- as.numeric(d$NLR > 2.5)

d$grp4_imdc  <- d$IMDC_f
d$grp4_nlr   <- interaction(d$IMDC_f, factor(d$NLR_ge_bin), drop=TRUE)
d$grp4_dnlr  <- interaction(d$IMDC_f, factor(d$dNLR_ge), drop=TRUE)
d$grp4_mlr   <- interaction(d$IMDC_f, factor(d$MLR_ge),  drop=TRUE)
d$grp4_siri  <- interaction(d$IMDC_f, factor(as.numeric(d$SIRI > 1.576328)), drop=TRUE)
d$grp4_crp   <- interaction(d$IMDC_f, factor(d$CRP_ge),  drop=TRUE)
d$grp4_nlrmlr  <- interaction(d$IMDC_f, factor(as.numeric(d$NLR_ge_bin==1 | d$MLR_ge==1)),  drop=TRUE)
d$grp4_dnlrmlr <- factor(d$dNLR_MLR_4grp, levels=c(0,1,2,3), labels=c("IL","IH","PL","PH"))

cat("=========== Classification x regimen interaction tests (LRT), OS — all 8 candidate models ===========\n\n")

test_interaction <- function(grpvar, label) {
  f_main <- as.formula(paste("Surv(OS_time, death) ~", grpvar, "+ regimen"))
  f_int  <- as.formula(paste("Surv(OS_time, death) ~", grpvar, "* regimen"))
  m_main <- tryCatch(coxph(f_main, data=d), error=function(e) NULL)
  m_int  <- tryCatch(coxph(f_int, data=d), error=function(e) NULL)
  if (is.null(m_main) || is.null(m_int)) { cat(label, ": fit failed\n"); return(invisible(NULL)) }
  a <- tryCatch(anova(m_main, m_int), error=function(e) NULL)
  if (is.null(a)) { cat(label, ": anova failed\n"); return(invisible(NULL)) }
  chisq <- a$Chisq[2]; dfv <- a$Df[2]; p <- a$`Pr(>|Chi|)`[2]
  cat(sprintf("%-16s chisq=%.3f  df=%d  p=%.4f  %s\n", label, chisq, dfv, p, ifelse(p<0.05,"*","")))
}

test_interaction("grp4_imdc",   "IMDC alone")
test_interaction("grp4_nlr",    "IMDC-NLR")
test_interaction("grp4_dnlr",   "IMDC-dNLR")
test_interaction("grp4_mlr",    "IMDC-MLR")
test_interaction("grp4_siri",   "IMDC-SIRI")
test_interaction("grp4_crp",    "IMDC-CRP")
test_interaction("grp4_nlrmlr", "IMDC-NLR-MLR")
test_interaction("grp4_dnlrmlr","IMDC-dNLR-MLR")
