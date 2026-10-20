library(readxl)
wd <- "/tmp/claude-0/-home-user-statistics/fdab54e3-f5df-589a-8842-e4a2d9bc21fd/scratchpad/iju_revision/analysis"
raw <- read_excel(file.path(wd, "data_24m.xlsx"), sheet = "全体", col_names = FALSE)
raw <- as.data.frame(raw)
idx <- function(i) raw[[i]][-1]  # 1-based column number; row1 is header

d <- data.frame(
  id            = idx(1),
  sex           = idx(2),
  age           = as.numeric(idx(4)),
  nephrectomy   = as.numeric(idx(14)),
  IOIO          = as.numeric(idx(43)),   # 1=ICI-ICI, 0=ICI-TKI
  bone_mets     = as.numeric(idx(120)),
  IMDC          = as.numeric(idx(125)),  # 1 intermediate, 2 poor
  WBC           = as.numeric(idx(134)),
  ANC_group     = as.numeric(idx(135)),  # IMDC neutrophilia flag candidate
  ANC           = as.numeric(idx(136)),
  ALC           = as.numeric(idx(138)),  # absolute lymphocyte count x10^3/uL
  AMC           = as.numeric(idx(139)),
  NLR           = as.numeric(idx(149)),
  dNLR          = as.numeric(idx(153)),
  dNLR_ge       = as.numeric(idx(154)),
  MLR           = as.numeric(idx(157)),
  MLR_ge        = as.numeric(idx(158)),
  dNLR_MLR_4grp = as.numeric(idx(167)),  # 0 IL,1 IH,2 PL,3 PH
  CRP           = as.numeric(idx(185)),
  CRP_ge        = as.numeric(idx(186)),
  PLR           = as.numeric(idx(161)),
  SII           = as.numeric(idx(173)),
  SIRI          = as.numeric(idx(177)),
  PIV           = as.numeric(idx(181)),
  progression   = as.numeric(idx(196)),
  OS_time       = as.numeric(idx(206)),
  PFS_time      = as.numeric(idx(207)),
  death         = as.numeric(idx(208)),
  stringsAsFactors = FALSE
)

cat("N =", nrow(d), "\n")
cat("Male n(%):", sum(d$sex=="M"), round(100*mean(d$sex=="M"),1), "\n")
cat("IMDC intermediate n(%):", sum(d$IMDC==1), round(100*mean(d$IMDC==1),1), "\n")
cat("median age (range):", median(d$age), range(d$age), "\n")
cat("dNLR_MLR_4grp table:\n"); print(table(d$dNLR_MLR_4grp))
cat("death n(%):", sum(d$death), round(100*mean(d$death),1), "\n")
cat("median followup:", median(d$OS_time), "\n")
cat("\nALC summary:\n"); print(summary(d$ALC))
cat("ANC summary:\n"); print(summary(d$ANC))
cat("ANC_group table:\n"); print(table(d$ANC_group, useNA="always"))
cat("CRP summary:\n"); print(summary(d$CRP))
cat("missing ALC:", sum(is.na(d$ALC)), " missing ANC:", sum(is.na(d$ANC)), " missing CRP:", sum(is.na(d$CRP)), "\n")

saveRDS(d, file.path(wd, "d.rds"))
