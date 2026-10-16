# =====================================================================
# V9 — TAREFA 3: nivel do VIX nos quantis condicionais. Semente 20261015 + 3.
#  3a  equacao do VIX do QVAR (p = 2) ajustada em tau = 0,95 (e 0,05) em
#      cada par x subamostra: dias com residuo positivo em 0,95 (acima do
#      quantil condicional) e, em 0,05, dias com residuo negativo (abaixo
#      do quantil condicional de 5%, a cauda analoga); por completude, a
#      leitura literal (residuo positivo em 0,05) tambem e reportada.
#      Estatisticas do NIVEL do VIX e de dVIX (diferenca de nivel).
#  3b  estado por limiar fixo: VIX_{t-1} acima do percentil 80 (alt. 90) da
#      amostra total 2018-2026 do par. Projecoes locais h = 0..5 do retorno
#      acumulado sobre a inovacao do VIX (residuo de AR(2) do nivel, MQO,
#      amostra total do par), com interacoes estado x pos-ETF, controles
#      (2 defasagens do retorno e da inovacao), erros de Newey-West
#      (lag h + 1) e bootstrap em blocos de 20 linhas (B = 500).
# Origem: v9_04_tarefa3.R (inalterado salvo caminhos). Tabelas S12, S18 (B).
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_v9.R"))
suppressPackageStartupMessages({ library(sandwich) })
t0 <- Sys.time(); log_pkgs9("v9_04_tarefa3.R")
K <- 3; SEED <- SEED_V9 + K
D <- carregar_v4()

## ---------------- 3a -----------------------------------------------------
st6 <- function(x) c(n = length(x), mean = mean(x), median = median(x), p10 = unname(quantile(x, 0.10)), p90 = unname(quantile(x, 0.90)), min = min(x), max = max(x))
r3a <- list()
for (cn in c("btc","eth")) for (s in c("pre","post")) {
  Z <- D$amostras[[cn]][[s]]; M <- coredata(Z); n <- nrow(M); tt <- (NLAG + 1):n
  lev <- M[tt, "VIX"]; dv <- M[tt, "VIX"] - M[tt - 1, "VIX"]
  f95 <- qvar_fit(M, 0.95); f05 <- qvar_fit(M, 0.05)
  sets <- list("tau = 0.95, residual > 0 (above conditional quantile)" = f95$Res[, "VIX"] > 0,
               "tau = 0.05, residual < 0 (below conditional quantile)" = f05$Res[, "VIX"] < 0,
               "tau = 0.05, residual > 0 (literal reading)" = f05$Res[, "VIX"] > 0,
               "All days in subsample" = rep(TRUE, length(tt)))
  for (nm in names(sets)) { w <- sets[[nm]]
    r3a[[length(r3a) + 1]] <- rbind(data.frame(pair = toupper(cn), sample = s, day_set = nm, variable = "VIX level", t(st6(lev[w]))),
                                    data.frame(pair = toupper(cn), sample = s, day_set = nm, variable = "Change in VIX (level difference)", t(st6(dv[w])))) }
}
T3a <- do.call(rbind, r3a); rownames(T3a) <- NULL; print(T3a)
write.csv(T3a, file.path(OUT9, "csv", "V9-3a_vix_at_conditional_quantiles_raw.csv"), row.names = FALSE)
progresso("Tarefa 3a concluida.")

## ---------------- 3b -----------------------------------------------------
HMAX <- 5; B_LP <- B_FULL
mk_lp <- function(cn, pct) {
  Z <- D$amostras[[cn]]$total; a <- toupper(cn); dt <- as.Date(index(Z)); v <- as.numeric(Z[, "VIX"]); r <- as.numeric(Z[, a])
  ar <- ar.ols(v, order.max = 2, aic = FALSE, demean = TRUE, intercept = TRUE); sh <- as.numeric(ar$resid)
  thr <- quantile(v, pct, names = FALSE)
  n <- length(v); vlag <- c(NA, v[-n])
  df <- data.frame(date = dt, ret = r, shock = sh, vlag = vlag, ret_lag1 = c(NA, r[-n]), ret_lag2 = c(NA, NA, r[-c(n - 1, n)]),
                   shock_lag1 = c(NA, sh[-n]), shock_lag2 = c(NA, NA, sh[-c(n - 1, n)]))
  df$High <- as.numeric(df$vlag > thr); df$Post <- as.numeric(df$date >= CUTS[[cn]])
  df$HPre <- df$High * (1 - df$Post); df$LPre <- (1 - df$High) * (1 - df$Post); df$HPost <- df$High * df$Post; df$LPost <- (1 - df$High) * df$Post
  df$s_HPre <- df$shock * df$HPre; df$s_LPre <- df$shock * df$LPre; df$s_HPost <- df$shock * df$HPost; df$s_LPost <- df$shock * df$LPost
  for (h in 0:HMAX) df[[paste0("y", h)]] <- sapply(1:n, function(i) if (i + h <= n) sum(r[i:(i + h)]) else NA_real_)
  df <- df[complete.cases(df[, c("shock","vlag","ret_lag2","shock_lag2")]), ]
  attr(df, "thr") <- thr; df
}
XC <- c("s_HPre","s_LPre","s_HPost","s_LPost","HPre","HPost","LPost","ret_lag1","ret_lag2","shock_lag1","shock_lag2")
fit_fast <- function(df) { X <- cbind(1, as.matrix(df[, XC]))
  t(sapply(0:HMAX, function(h) { y <- df[[paste0("y", h)]]; ok <- is.finite(y); f <- lm.fit(X[ok, , drop = FALSE], y[ok])
    if (f$rank < ncol(X)) return(rep(NA_real_, 5)); b <- f$coefficients[2:5]; c(b, b[3] - b[1]) })) }
rows <- list(); thr_tab <- list()
set.seed(SEED)
for (pct in c(0.80, 0.90)) for (cn in c("btc","eth")) {
  df <- mk_lp(cn, pct); n <- nrow(df)
  thr_tab[[length(thr_tab) + 1]] <- data.frame(pair = toupper(cn), percentile = pct, threshold_VIX = attr(df, "thr"),
    days_high_pre = sum(df$HPre), days_low_pre = sum(df$LPre), days_high_post = sum(df$HPost), days_low_post = sum(df$LPost))
  bm <- replicate(B_LP, { ii <- mbb_idx(n); fit_fast(df[ii, ]) }, simplify = FALSE)
  for (h in 0:HMAX) {
    fit <- lm(as.formula(paste0("y", h, " ~ ", paste(XC, collapse = " + "))), data = df, na.action = na.omit)
    nw <- NeweyWest(fit, lag = h + 1, prewhite = FALSE, adjust = TRUE); b <- coef(fit)
    nmz <- c("s_HPre","s_LPre","s_HPost","s_LPost")
    bh <- sapply(bm, function(m) m[h + 1, ]); valid <- apply(is.finite(bh), 2, all)
    for (j in 1:5) {
      if (j <= 4) { est <- b[nmz[j]]; se <- sqrt(nw[nmz[j], nmz[j]]); lab <- c("High state, pre-ETF","Low state, pre-ETF","High state, post-ETF","Low state, post-ETF")[j] }
      else { est <- b["s_HPost"] - b["s_HPre"]; se <- sqrt(nw["s_HPost","s_HPost"] + nw["s_HPre","s_HPre"] - 2 * nw["s_HPost","s_HPre"]); lab <- "High state: post - pre" }
      x <- bh[j, valid]
      rows[[length(rows) + 1]] <- data.frame(pair = toupper(cn), threshold = sprintf("VIX(t-1) > p%d", round(100 * pct)), h = h, coefficient = lab,
        estimate = unname(est), nw_se = unname(se), nw_ci95_lo = unname(est - 1.96 * se), nw_ci95_hi = unname(est + 1.96 * se),
        boot_ci95_lo = quantile(x, 0.025, names = FALSE), boot_ci95_hi = quantile(x, 0.975, names = FALSE), n_valid = sum(valid), B = B_LP, n_obs = nobs(fit),
        nw_excludes_0 = (est - 1.96 * se > 0) | (est + 1.96 * se < 0), boot_excludes_0 = quantile(x, 0.025) > 0 | quantile(x, 0.975) < 0)
    }
  }
}
T3b <- do.call(rbind, rows); rownames(T3b) <- NULL; TH <- do.call(rbind, thr_tab)
print(TH); print(T3b[T3b$coefficient == "High state: post - pre", ])
write.csv(T3b, file.path(OUT9, "csv", "V9-3b_LP_fixed_threshold_raw.csv"), row.names = FALSE)
write.csv(TH, file.path(OUT9, "csv", "V9-3b_thresholds.csv"), row.names = FALSE)
saveRDS(list(T3a = T3a, T3b = T3b, TH = TH, seed = SEED), file.path(OUT9, "rds", "v9_3.rds"))
progresso(sprintf("Tarefa 3 concluida (3a nivel do VIX nos quantis; 3b LP com limiar fixo, B=500). Diferencas pos-pre (estado alto) com IC bootstrap excluindo zero: %d de %d.",
                  sum(T3b$boot_excludes_0[T3b$coefficient == "High state: post - pre"]), sum(T3b$coefficient == "High state: post - pre")))
log_v9(sprintf("24_v9 (Tarefa 3) concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
