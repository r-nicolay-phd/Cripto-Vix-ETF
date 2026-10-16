# =====================================================================
# V9 — TAREFA 6: itens menores. Semente 20261015 + 6.
#  6a  canal de fluxo (estado em t-1; mesmo desenho de v7_13): MDE (80% de
#      poder, 5%) = 2,80 x EP bootstrap do diferencial medio h0-5 e h0-10;
#      Wald chi2 e Wald bootstrap (B = 500, blocos de 20 linhas).
#  6b  premio do VIX e EPU so a partir de 02/01/2018, p = 2, H = 10:
#      NPDC(0.05/0.50/0.95) total, pre, pos e Delta; DVOL em linha separada
#      (volatilidade implicita da propria cripto; serie desde 24/03/2021).
#  6c  p in {1,2,3,8 (AIC)} x H in {5,10,20}: Delta(0.50), Delta(0.90),
#      Delta(0.95), DD(0.95) (pontuais).
#  6d  nova Tabela 1: baseline 24:00 UTC, precos 16:00 ET (Coinbase, mesmos
#      arquivos horarios da V6/V7) e Dlog VIX com p = 2, cada um com IC do
#      bootstrap corrigido.
#  6e  Fig. 4 (contrafactual em tau 0,50 e 0,95, IC corrigidos).
# Origem: v9_07_tarefa6.R. Alteracoes: caminhos; removidos 6d (nova
# Tabela 1 com o bootstrap da V9) e 6e (Fig. 4 da V9), substituidos por
# 32_v10_especificacoes.R (V10-9, Tabela 1) e 30_v10_contrafactual.R
# (Fig. 4). Usados: 6a (Tabela S26, Painel B), 6b (Tabela S33) e 6c
# (Tabela S29, Painel A).
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_v9.R"))
suppressPackageStartupMessages({ library(sandwich); library(ggplot2) })
t0 <- Sys.time(); log_pkgs9("v9_07_tarefa6.R")
K <- 6; SEED <- SEED_V9 + K
D <- carregar_v4(); base_xts <- D$base_xts; vx <- ler_vix()
RDS <- function(nm) file.path(OUT9, "rds", paste0("v9_6_", nm, ".rds"))
run_or_load <- function(nm, expr) { f <- RDS(nm); if (file.exists(f)) { log_v9("6: bloco ", nm, " ja existe -- carregado"); return(readRDS(f)) }
  x <- eval.parent(substitute(expr)); saveRDS(x, f); x }

## ---------------- 6a (codigo de v7_13, estado em t-1) -------------------------
H <- 10; NLAG_CTRL <- 2
ler_fluxo  <- function(path) { d <- read.csv(path, stringsAsFactors = FALSE); d$date <- as.Date(d$date); xts(d$total_mi_usd, order.by = d$date) }
ler_volume <- function(path) { d <- read.csv(path, stringsAsFactors = FALSE); d$date <- as.Date(d$date); xts(d$volume, order.by = d$date) }
flow_btc <- ler_fluxo("data/clean/etf_flow_btc_farside_limpo.csv"); colnames(flow_btc) <- "flow"
flow_eth <- ler_fluxo("data/clean/etf_flow_eth_farside_limpo.csv"); colnames(flow_eth) <- "flow"
normaliza_fluxo <- function(flow, vol) { colnames(vol) <- "vol"; m <- merge(flow, vol, join = "left")
  m$vol_ma20 <- rollapply(m$vol, 20, mean, align = "right", fill = NA, na.rm = TRUE); m$flow_norm <- m$flow / m$vol_ma20; m }
classifica_estado <- function(fn) { x <- as.numeric(fn$flow_norm); q <- quantile(x, probs = c(0.2, 0.8), na.rm = TRUE)
  data.frame(date = index(fn), estado = ifelse(x <= q[1], "resgate", ifelse(x >= q[2], "captacao", "neutro")), flow_norm = x, stringsAsFactors = FALSE) }
ESTADO <- list(btc = classifica_estado(normaliza_fluxo(flow_btc, ler_volume("data/clean/etf_ibit_yahoo.csv"))),
               eth = classifica_estado(normaliza_fluxo(flow_eth, ler_volume("data/clean/etf_etha_yahoo.csv"))))
vix_full <- na.omit(base_xts[, "VIX"])
sel_ar_bic <- function(x, pmax = 10) which.min(sapply(1:pmax, function(p) {
  fit <- tryCatch(ar.ols(x, order.max = p, aic = FALSE, demean = TRUE, intercept = TRUE), error = function(e) NULL)
  if (is.null(fit)) return(NA); r <- na.omit(fit$resid); n <- length(r); n * log(sum(r^2) / n) + log(n) * (p + 1) }))
p_vix <- sel_ar_bic(as.numeric(vix_full))
ar_fit <- ar.ols(as.numeric(vix_full), order.max = p_vix, aic = FALSE, demean = TRUE, intercept = TRUE)
choque_vix <- xts(as.numeric(ar_fit$resid), order.by = index(vix_full)); colnames(choque_vix) <- "choque_vix"
montar_lp_data <- function(ativo, estado_df) {
  num <- merge(base_xts[, ativo], choque_vix, join = "inner"); colnames(num) <- c("ret","choque")
  lags <- list(); for (l in 1:NLAG_CTRL) { lags[[paste0("ret_lag", l)]] <- lag(num$ret, l); lags[[paste0("choque_lag", l)]] <- lag(num$choque, l) }
  num <- do.call(merge, c(list(num), lags)); colnames(num) <- c("ret","choque", names(lags))
  df <- data.frame(date = index(num), coredata(num)); df <- merge(df, estado_df[, c("date","estado")], by = "date", all.x = TRUE); df <- df[order(df$date), ]
  df$estado_t <- df$estado; df$estado <- c(NA, head(df$estado_t, -1))
  df <- df[!is.na(df$ret) & !is.na(df$choque) & !is.na(df$estado), ]
  df$D_resgate <- as.numeric(df$estado == "resgate"); df$D_captacao <- as.numeric(df$estado == "captacao"); df$D_neutro <- as.numeric(df$estado == "neutro")
  df$resgate_x_choque <- df$D_resgate * df$choque; df$captacao_x_choque <- df$D_captacao * df$choque; df$neutro_x_choque <- df$D_neutro * df$choque
  n <- nrow(df); for (h in 0:H) df[[paste0("y", h)]] <- sapply(1:n, function(i) if (i + h <= n) sum(df$ret[i:(i + h)]) else NA_real_)
  df
}
CTRL <- c(paste0("ret_lag", 1:NLAG_CTRL), paste0("choque_lag", 1:NLAG_CTRL))
X_COLS <- c("resgate_x_choque","captacao_x_choque","neutro_x_choque","D_resgate","D_captacao", CTRL)
lp_diff_fast <- function(df) { X <- as.matrix(df[, X_COLS])
  sapply(0:H, function(h) { y <- df[[paste0("y", h)]]; ok <- is.finite(y) & apply(is.finite(X), 1, all)
    f <- lm.fit(X[ok, , drop = FALSE], y[ok]); if (f$rank < ncol(X)) return(NA_real_)
    unname(f$coefficients["resgate_x_choque"] - f$coefficients["captacao_x_choque"]) }) }
F6a <- run_or_load("flow", do.call(rbind, lapply(c("btc","eth"), function(par) {
  df <- montar_lp_data(toupper(par), ESTADO[[par]]); d_pt <- lp_diff_fast(df)
  set.seed(SEED); n <- nrow(df); bm <- t(sapply(1:B_FULL, function(b) lp_diff_fast(df[mbb_idx(n), ])))
  valid <- apply(is.finite(bm), 1, all); bv <- bm[valid, , drop = FALSE]
  V <- cov(bv); W <- as.numeric(t(d_pt) %*% solve(V, d_pt)); Wb <- apply(bv, 1, function(x) { e <- x - d_pt; as.numeric(t(e) %*% solve(V, e)) })
  m05 <- rowMeans(bv[, 1:6]); m010 <- rowMeans(bv[, 1:11])
  data.frame(pair = toupper(par), spec = "Flow state at t-1", n_obs_h0 = sum(is.finite(df$y0)), B = B_FULL, n_valid = sum(valid),
             horizon_window = c("h = 0-5", "h = 0-10"), mean_differential = c(mean(d_pt[1:6]), mean(d_pt[1:11])),
             boot_se = c(sd(m05), sd(m010)), ci95_lo = c(quantile(m05, 0.025), quantile(m010, 0.025)), ci95_hi = c(quantile(m05, 0.975), quantile(m010, 0.975)),
             MDE_80pct_5pct = 2.80 * c(sd(m05), sd(m010)), wald_stat = W, wald_df = H + 1, wald_p_chi2 = 1 - pchisq(W, H + 1), wald_p_boot = mean(Wb >= W), p_vix_AR = p_vix)
})))
print(F6a); write.csv(F6a, file.path(OUT9, "csv", "V9-6a_flow_MDE_raw.csv"), row.names = FALSE)
progresso("Tarefa 6a concluida (MDE canal de fluxo).")

## ---------------- 6b ---------------------------------------------------------
F6b <- run_or_load("constructs", {
  gspc <- read.csv("data/clean/gspc_yahoo.csv"); gspc$date <- as.Date(gspc$date); gspc <- xts(gspc$close, order.by = gspc$date)
  spx_ret <- diff(log(gspc)); rv22 <- sqrt(252) * rollapply(spx_ret, 22, sd, align = "right", fill = NA, na.rm = TRUE) * 100
  vprem <- vx[index(rv22)] - rv22; colnames(vprem) <- "RISK"
  epu <- read.csv("data/clean/epu_daily.csv"); epu$date <- as.Date(epu$date); epu <- xts(epu$daily_policy_index, order.by = epu$date); colnames(epu) <- "RISK"
  dvl <- lapply(c(btc = "data/clean/dvol_btc_deribit.csv", eth = "data/clean/dvol_eth_deribit.csv"), function(f) { d <- read.csv(f); d$date <- as.Date(d$date); x <- xts(d$close, order.by = d$date); colnames(x) <- "RISK"; x })
  rows <- list()
  for (cn in c("btc","eth")) { a <- toupper(cn); r <- base_xts[, a]
    for (cx in c("VIX premium","EPU","DVOL (own-crypto implied volatility)")) {
      cs <- switch(cx, "VIX premium" = vprem, "EPU" = epu, dvl[[cn]])
      z <- na.omit(merge(r, cs, join = "inner")); z <- z[index(z) >= as.Date(if (grepl("DVOL", cx)) "2021-03-24" else "2018-01-02") & index(z) <= DATE_FIM]
      M <- coredata(z); colnames(M) <- c(a, "VIX"); dz <- as.Date(index(z)); cut <- CUTS[[cn]]
      subs <- list(total = M, pre = M[dz < cut, , drop = FALSE], post = M[dz >= cut, , drop = FALSE])
      v <- lapply(subs, function(m) npdc_taus9(m, c(0.05, 0.50, 0.95))["NPDC", ])
      for (s in names(subs)) rows[[length(rows) + 1]] <- data.frame(pair = a, measure = cx, sample = s, first = format(min(dz[if (s == "total") TRUE else if (s == "pre") dz < cut else dz >= cut])),
        last = format(max(dz[if (s == "total") TRUE else if (s == "pre") dz < cut else dz >= cut])), n = nrow(subs[[s]]),
        NPDC_0.05 = v[[s]][["0.05"]], NPDC_0.50 = v[[s]][["0.50"]], NPDC_0.95 = v[[s]][["0.95"]])
      rows[[length(rows) + 1]] <- data.frame(pair = a, measure = cx, sample = "Delta (post - pre)", first = "", last = "", n = NA,
        NPDC_0.05 = v$post[["0.05"]] - v$pre[["0.05"]], NPDC_0.50 = v$post[["0.50"]] - v$pre[["0.50"]], NPDC_0.95 = v$post[["0.95"]] - v$pre[["0.95"]]) } }
  do.call(rbind, rows)
})
print(F6b); write.csv(F6b, file.path(OUT9, "csv", "V9-6b_alt_uncertainty_raw.csv"), row.names = FALSE)
progresso("Tarefa 6b concluida (premio do VIX, EPU desde 02/01/2018; DVOL separado).")

## ---------------- 6c ---------------------------------------------------------
F6c <- run_or_load("p_H", do.call(rbind, lapply(c("btc","eth"), function(cn) { Mp <- coredata(D$amostras[[cn]]$pre); Mq <- coredata(D$amostras[[cn]]$post)
  do.call(rbind, lapply(c(1, 2, 3, 8), function(p) do.call(rbind, lapply(c(5, 10, 20), function(Hh) {
    np <- npdc_taus9(Mp, c(0.50, 0.90, 0.95), nlag = p, nfore = Hh)["NPDC", ]; nq <- npdc_taus9(Mq, c(0.50, 0.90, 0.95), nlag = p, nfore = Hh)["NPDC", ]; d <- nq - np
    data.frame(pair = toupper(cn), p = p, p_label = c("1","2 (BIC)","3","8 (AIC)")[match(p, c(1, 2, 3, 8))], H = Hh,
               Delta_0.50 = d[["0.50"]], Delta_0.90 = d[["0.90"]], Delta_0.95 = d[["0.95"]], DD_0.95 = d[["0.95"]] - d[["0.50"]]) })))) })))
rng6c <- do.call(rbind, lapply(split(F6c, F6c$pair), function(d) data.frame(pair = d$pair[1], Delta_0.95_min = min(d$Delta_0.95), Delta_0.95_max = max(d$Delta_0.95),
  n_cases = nrow(d), n_negative_Delta_0.95 = sum(d$Delta_0.95 < 0), n_negative_DD_0.95 = sum(d$DD_0.95 < 0))))
print(F6c); print(rng6c)
write.csv(F6c, file.path(OUT9, "csv", "V9-6c_p_H_raw.csv"), row.names = FALSE); write.csv(rng6c, file.path(OUT9, "csv", "V9-6c_summary.csv"), row.names = FALSE)
progresso(sprintf("Tarefa 6c concluida: Delta(0.95) BTC de %.1f a %.1f (%d/12 negativos); ETH de %.1f a %.1f (%d/12 negativos).",
                  rng6c$Delta_0.95_min[1], rng6c$Delta_0.95_max[1], rng6c$n_negative_Delta_0.95[1], rng6c$Delta_0.95_min[2], rng6c$Delta_0.95_max[2], rng6c$n_negative_Delta_0.95[2]))

saveRDS(list(F6a = F6a, F6b = F6b, F6c = F6c, rng6c = rng6c, seed = SEED), file.path(OUT9, "rds", "v9_6_all.rds"))
progresso("Tarefa 6 concluida (6a-6c).")
log_v9(sprintf("27_v9 (Tarefa 6) concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
