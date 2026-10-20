## ============================================================================
## 入れ子型(nested)ブートストラップによる C-index と 95%CI の算出
## ―― マーカー1個ずつ実行するバージョン(EZRでの実行を想定)
## ============================================================================
##
## 【使い方】
## 1. EZRの「データセット」に、以下の列が入った状態にしておく:
##      - OS_time : 全生存期間(観察期間、月)
##      - death   : 死亡イベント (1=死亡, 0=打切り)
##      - マーカーの連続値の列(例: CRP, dNLR, MLR, NLR, SIRI, PIV など)
## 2. 下の「★設定ここから★」のブロックだけを書き換えて実行する。
##      - 単独マーカーの場合:   MARKERS <- "CRP"
##      - 2マーカー併用(dNLR+MLR型)の場合: MARKERS <- c("dNLR", "MLR")
##      - IMDC単独(マーカーなし)の場合:    MARKERS <- NULL
## 3. 1回の実行で1モデル分だけ計算する。全マーカーを1つのループで回すと
##    EZRが固まりやすいため、モデルごとに分けて実行してください
##    (このスクリプト全体を選択してEZRの「スクリプト」ウィンドウで実行→
##     結果を記録→MARKERSを書き換えて再実行、を繰り返す)。
## 4. 計算時間の目安: 単独マーカーで約5分、2マーカー併用で約10〜15分
##    (B=1000、n=192での実測値。PCの性能により変動)。
##    100回ごとに進捗がコンソールに表示されます。
##
## ============================================================================

suppressMessages({library(survival)})

## ★設定ここから★ -------------------------------------------------------

d <- Dataset              # EZRのデータセット名。異なる場合はここを変更
TIME_COL  <- "OS_time"    # 生存時間の列名
EVENT_COL <- "death"      # イベント(1=death)の列名
HORIZON   <- 24           # カットオフ探索・評価する時点(月)

MARKERS <- "CRP"          # ← ここを 1個 または 2個(c("dNLR","MLR")) に変更して実行
                          #    IMDC単独で評価したい場合は NULL にする

B <- 1000                 # ブートストラップ回数

## ★設定ここまで★ -------------------------------------------------------

time_vec  <- d[[TIME_COL]]
event_vec <- d[[EVENT_COL]]
n <- nrow(d)

## ---- 時間依存ROC(KM法)によるYouden最適カットオフの探索 --------------
## 既存の解析(本文・Supplementary Table 4)と同じ方法:
## Heagerty-Zheng の "incident/dynamic" 時間依存ROCをKaplan-Meier法で計算

timeROC_KM <- function(time, status, marker, t) {
  cutoffs <- sort(unique(marker))
  overall <- survfit(Surv(time, status) ~ 1)
  St <- summary(overall, times = t, extend = TRUE)$surv
  Ft <- 1 - St
  sens <- numeric(length(cutoffs)); spec <- numeric(length(cutoffs))
  for (i in seq_along(cutoffs)) {
    cut_i <- cutoffs[i]
    hi <- marker > cut_i
    p_hi <- mean(hi); p_lo <- 1 - p_hi
    if (p_hi > 0 && p_hi < 1) {
      S_hi_t <- summary(survfit(Surv(time[hi], status[hi]) ~ 1), times = t, extend = TRUE)$surv
      S_lo_t <- summary(survfit(Surv(time[!hi], status[!hi]) ~ 1), times = t, extend = TRUE)$surv
    } else if (p_hi == 0) { S_hi_t <- 1; S_lo_t <- St } else { S_hi_t <- St; S_lo_t <- 1 }
    p_hi_and_gt <- p_hi * S_hi_t
    p_hi_and_le <- p_hi - p_hi_and_gt
    p_lo_and_gt <- p_lo * S_lo_t
    sens[i] <- if (Ft > 0) p_hi_and_le / Ft else NA
    spec[i] <- if (St > 0) p_lo_and_gt / St else NA
  }
  data.frame(cutoff = cutoffs, sens = sens, spec = spec)
}

best_cutoff <- function(time, status, marker, t) {
  roc <- timeROC_KM(time, status, marker, t)
  roc$youden <- roc$sens + roc$spec - 1
  best <- roc[which.max(roc$youden), ]
  list(cutoff = best$cutoff)
}

get_c <- function(lp, time, status) {
  as.numeric(concordance(Surv(time, status) ~ lp, reverse = TRUE)$concordance)
}

build_grp <- function(dat2, spec, cutoffs) {
  if (is.null(spec)) return(factor(dat2$IMDC_f, levels = levels(dat2$IMDC_f)))
  hiflags <- sapply(spec, function(m) as.numeric(dat2[[m]] > cutoffs[[m]]))
  if (is.null(dim(hiflags))) hiflags <- matrix(hiflags, ncol = length(spec))
  mhigh <- as.numeric(rowSums(hiflags, na.rm = TRUE) >= 1)
  interaction(dat2$IMDC_f, factor(mhigh, levels = c(0,1)), drop = FALSE)
}

## ---- IMDC列を因子化(列名が違う場合はここを調整) ------------------------
if (!"IMDC_f" %in% names(d)) {
  if ("IMDC" %in% names(d)) {
    d$IMDC_f <- factor(d$IMDC, levels = sort(unique(d$IMDC)))
  } else {
    stop("IMDC列が見つかりません。IMDC_fまたはIMDCという列名でIMDC分類(中間 / 不良)を用意してください。")
  }
}

cat("=========== 入れ子型ブートストラップ: ", if (is.null(MARKERS)) "IMDC単独" else paste(MARKERS, collapse="+"),
    " (B=", B, ") ===========\n\n", sep = "")

## ---- 元データでの apparent C-index -------------------------------------
cutoffs_full <- list()
if (!is.null(MARKERS)) {
  for (m in MARKERS) {
    bc <- best_cutoff(time_vec, event_vec, d[[m]], HORIZON)
    cutoffs_full[[m]] <- bc$cutoff
    cat(sprintf("  %s の元データでのカットオフ: %.4f\n", m, bc$cutoff))
  }
}
grp_full <- build_grp(d, MARKERS, cutoffs_full)
fit_full <- coxph(Surv(time_vec, event_vec) ~ grp_full, data = d)
lp_full  <- predict(fit_full, type = "lp")
c_apparent <- get_c(lp_full, time_vec, event_vec)
cat(sprintf("\napparent C-index (元データ) = %.4f\n\n", c_apparent))

## ---- ブートストラップ本体 ------------------------------------------------
train_c  <- rep(NA_real_, B)
test_c   <- rep(NA_real_, B)
optimism <- rep(NA_real_, B)

t0 <- Sys.time()
for (b in 1:B) {
  idx <- sample.int(n, n, replace = TRUE)
  db  <- d[idx, ]

  cutoffs_b <- list()
  ok <- TRUE
  if (!is.null(MARKERS)) {
    for (m in MARKERS) {
      bc <- tryCatch(best_cutoff(db[[TIME_COL]], db[[EVENT_COL]], db[[m]], HORIZON), error = function(e) NULL)
      if (is.null(bc) || !is.finite(bc$cutoff)) { ok <- FALSE; break }
      cutoffs_b[[m]] <- bc$cutoff
    }
  }
  if (!ok) next

  grp_b <- build_grp(db, MARKERS, cutoffs_b)
  if (length(unique(grp_b)) < 2) next
  fit_b <- tryCatch(coxph(Surv(db[[TIME_COL]], db[[EVENT_COL]]) ~ grp_b, data = db), error = function(e) NULL)
  if (is.null(fit_b)) next
  lp_b <- tryCatch(predict(fit_b, type = "lp"), error = function(e) NULL)
  if (is.null(lp_b)) next
  ctr <- tryCatch(get_c(lp_b, db[[TIME_COL]], db[[EVENT_COL]]), error = function(e) NA)
  if (is.na(ctr)) next
  train_c[b] <- ctr

  ## このリサンプルで作ったモデル(係数・カットオフとも)を元データで評価
  grp_eval <- build_grp(d, MARKERS, cutoffs_b)
  newdat <- data.frame(grp_b = grp_eval)
  lp_eval <- tryCatch(predict(fit_b, newdata = newdat, type = "lp"), error = function(e) NULL)
  if (is.null(lp_eval) || all(is.na(lp_eval))) next
  keep <- !is.na(lp_eval)
  cte <- tryCatch(get_c(lp_eval[keep], time_vec[keep], event_vec[keep]), error = function(e) NA)
  if (is.na(cte)) next
  test_c[b] <- cte
  optimism[b] <- train_c[b] - test_c[b]

  if (b %% 100 == 0) {
    cat(sprintf("  ... %d / %d 完了  (経過 %.0f 秒)\n", b, B, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
}

## ---- 結果のまとめ ---------------------------------------------------------
n_ok <- sum(!is.na(optimism))
mean_opt <- mean(optimism, na.rm = TRUE)
c_corrected <- c_apparent - mean_opt
## CIの定義: 以前の「カットオフ固定」版(03_crp_vs_dnlrmlr.R)と同じ方法に統一。
## 各リサンプルごとに corrected_b = apparent_C(元データ) - optimism_b を計算し、
## その分布の2.5/97.5パーセンタイルをCIとする。
## mean(corrected_b) は定義上、報告する補正C-indexの点推定値と完全に一致する。
corrected_b <- c_apparent - optimism
ci <- quantile(corrected_b, c(0.025, 0.975), na.rm = TRUE)

cat("\n=========== 結果 ===========\n")
cat(sprintf("モデル: %s\n", if (is.null(MARKERS)) "IMDC単独" else paste("IMDC + ", paste(MARKERS, collapse=" + "))))
cat(sprintf("有効リサンプル数: %d / %d\n", n_ok, B))
cat(sprintf("apparent C-index(元データ)        = %.4f\n", c_apparent))
cat(sprintf("mean optimism(train - test)         = %.4f\n", mean_opt))
cat(sprintf("入れ子型 optimism補正 C-index        = %.4f\n", c_corrected))
cat(sprintf("95%% CI(補正C-indexに対する信頼区間) = [%.4f, %.4f]\n", ci[1], ci[2]))

## 次に別のモデルを計算する場合は、上の MARKERS を書き換えて
## このスクリプト全体を再実行してください。
