# =====================================================================
# V7-2 / Prompt 3, ETAPA 4 — CANAL DE FLUXO COM ESTADO DEFASADO E TESTE
# CONJUNTO NA BASE NOVA (copia parametrizada de 44_canal_fluxo_defasado.R)
# diferencial = beta_saida - beta_entrada (H3 preve < 0). Estado em t (V4)
# e em t-1 lado a lado; NW (lag h+1); bootstrap em blocos de 20 linhas LP
# (B=500, semente 20260922) -> Wald dos 11 diferenciais, medias h0-5 e
# h0-10 com IC, Holm por horizonte. Fluxos/estados identicos a V4-V6;
# muda apenas o retorno cripto (base nova).
# Tabelas V7-5.11 (por horizonte) e V7-5.12 (testes conjuntos).
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
suppressPackageStartupMessages({ library(sandwich); library(lmtest) })
t0 <- Sys.time(); log_pkgs7("v7_13_p3_etapa4_fluxo.R")
D <- carregar_v4(); base_xts <- D$base_xts
H <- 10; NLAG_CTRL <- 2; B_LP <- 500

ler_fluxo  <- function(path) { d <- read.csv(path, stringsAsFactors = FALSE); d$date <- as.Date(d$date); xts(d$total_mi_usd, order.by = d$date) }
ler_volume <- function(path) { d <- read.csv(path, stringsAsFactors = FALSE); d$date <- as.Date(d$date); xts(d$volume, order.by = d$date) }
flow_btc <- ler_fluxo("data/clean/etf_flow_btc_farside_limpo.csv"); colnames(flow_btc) <- "flow"
flow_eth <- ler_fluxo("data/clean/etf_flow_eth_farside_limpo.csv"); colnames(flow_eth) <- "flow"
vol_ibit <- ler_volume("data/clean/etf_ibit_yahoo.csv"); vol_etha <- ler_volume("data/clean/etf_etha_yahoo.csv")
normaliza_fluxo <- function(flow, vol) { colnames(vol) <- "vol"; m <- merge(flow, vol, join = "left")
  m$vol_ma20 <- rollapply(m$vol, 20, mean, align = "right", fill = NA, na.rm = TRUE); m$flow_norm <- m$flow / m$vol_ma20; m }
classifica_estado <- function(fn) { x <- as.numeric(fn$flow_norm); q <- quantile(x, probs = c(0.2, 0.8), na.rm = TRUE)
  data.frame(date = index(fn), estado = ifelse(x <= q[1], "resgate", ifelse(x >= q[2], "captacao", "neutro")), flow_norm = x, stringsAsFactors = FALSE) }
ESTADO <- list(btc = classifica_estado(normaliza_fluxo(flow_btc, vol_ibit)), eth = classifica_estado(normaliza_fluxo(flow_eth, vol_etha)))

vix_full <- na.omit(base_xts[, "VIX"])
sel_ar_bic <- function(x, pmax = 10) which.min(sapply(1:pmax, function(p) {
  fit <- tryCatch(ar.ols(x, order.max = p, aic = FALSE, demean = TRUE, intercept = TRUE), error = function(e) NULL)
  if (is.null(fit)) return(NA); r <- na.omit(fit$resid); n <- length(r); n * log(sum(r^2) / n) + log(n) * (p + 1) }))
p_vix <- sel_ar_bic(as.numeric(vix_full))
ar_fit <- ar.ols(as.numeric(vix_full), order.max = p_vix, aic = FALSE, demean = TRUE, intercept = TRUE)
choque_vix <- xts(as.numeric(ar_fit$resid), order.by = index(vix_full)); colnames(choque_vix) <- "choque_vix"

montar_lp_data <- function(ativo, estado_df, defasar_estado) {
  num <- merge(base_xts[, ativo], choque_vix, join = "inner"); colnames(num) <- c("ret","choque")
  lags <- list(); for (l in 1:NLAG_CTRL) { lags[[paste0("ret_lag", l)]] <- lag(num$ret, l); lags[[paste0("choque_lag", l)]] <- lag(num$choque, l) }
  num <- do.call(merge, c(list(num), lags)); colnames(num) <- c("ret","choque", names(lags))
  df <- data.frame(date = index(num), coredata(num)); df <- merge(df, estado_df[, c("date","estado")], by = "date", all.x = TRUE); df <- df[order(df$date), ]
  if (defasar_estado) { df$estado_t <- df$estado; df$estado <- c(NA, head(df$estado_t, -1)) }
  df <- df[!is.na(df$ret) & !is.na(df$choque) & !is.na(df$estado), ]; rownames(df) <- as.character(df$date)
  df$D_resgate <- as.numeric(df$estado == "resgate"); df$D_captacao <- as.numeric(df$estado == "captacao"); df$D_neutro <- as.numeric(df$estado == "neutro")
  df$resgate_x_choque <- df$D_resgate * df$choque; df$captacao_x_choque <- df$D_captacao * df$choque; df$neutro_x_choque <- df$D_neutro * df$choque
  n <- nrow(df); for (h in 0:H) df[[paste0("y", h)]] <- sapply(1:n, function(i) if (i + h <= n) sum(df$ret[i:(i + h)]) else NA_real_)
  df
}
CTRL <- c(paste0("ret_lag", 1:NLAG_CTRL), paste0("choque_lag", 1:NLAG_CTRL))
FORM_RHS <- paste("0 + resgate_x_choque + captacao_x_choque + neutro_x_choque + D_resgate + D_captacao +", paste(CTRL, collapse = " + "))
lp_nw <- function(df) do.call(rbind, lapply(0:H, function(h) {
  fit <- lm(as.formula(paste0("y", h, " ~ ", FORM_RHS)), data = df, na.action = na.omit)
  nw <- NeweyWest(fit, lag = h + 1, prewhite = FALSE, adjust = TRUE); b <- coef(fit)
  dse <- sqrt(nw["resgate_x_choque","resgate_x_choque"] + nw["captacao_x_choque","captacao_x_choque"] - 2 * nw["resgate_x_choque","captacao_x_choque"])
  data.frame(h = h, n_obs = nobs(fit), resgate_beta = b["resgate_x_choque"], resgate_se = sqrt(nw["resgate_x_choque","resgate_x_choque"]),
             captacao_beta = b["captacao_x_choque"], captacao_se = sqrt(nw["captacao_x_choque","captacao_x_choque"]),
             neutro_beta = b["neutro_x_choque"], neutro_se = sqrt(nw["neutro_x_choque","neutro_x_choque"]),
             diferencial = b["resgate_x_choque"] - b["captacao_x_choque"], diferencial_se = dse, row.names = NULL) }))
X_COLS <- c("resgate_x_choque","captacao_x_choque","neutro_x_choque","D_resgate","D_captacao", CTRL)
lp_diff_fast <- function(df) { X <- as.matrix(df[, X_COLS])
  sapply(0:H, function(h) { y <- df[[paste0("y", h)]]; ok <- is.finite(y) & apply(is.finite(X), 1, all)
    f <- lm.fit(X[ok, , drop = FALSE], y[ok]); if (f$rank < ncol(X)) return(NA_real_)
    unname(f$coefficients["resgate_x_choque"] - f$coefficients["captacao_x_choque"]) }) }

especs <- list(state_t = FALSE, state_tminus1 = TRUE)
resultados <- list(); boots <- list(); juntos <- list()
for (esp in names(especs)) for (par in c("btc","eth")) {
  df <- montar_lp_data(toupper(par), ESTADO[[par]], especs[[esp]])
  lp <- lp_nw(df); lp$z <- lp$diferencial / lp$diferencial_se; lp$p_nw <- 2 * pnorm(-abs(lp$z)); lp$p_holm <- p.adjust(lp$p_nw, method = "holm")
  lp$ci95_lo <- lp$diferencial - 1.96 * lp$diferencial_se; lp$ci95_hi <- lp$diferencial + 1.96 * lp$diferencial_se
  lp$spec <- esp; lp$pair <- toupper(par); lp$sample_start <- min(df$date); lp$sample_end <- max(df$date)
  d_pt <- lp$diferencial; stopifnot(max(abs(lp_diff_fast(df) - d_pt)) < 1e-8)
  run_boot <- function(B) { set.seed(SEED_V5); n <- nrow(df); t(sapply(1:B, function(b) lp_diff_fast(df[mbb_idx(n), ]))) }
  tst <- run_boot(20); bm <- run_boot(B_LP); valid <- apply(is.finite(bm), 1, all); bv <- bm[valid, , drop = FALSE]
  V <- cov(bv); W <- as.numeric(t(d_pt) %*% solve(V, d_pt)); Wb <- apply(bv, 1, function(x) { e <- x - d_pt; as.numeric(t(e) %*% solve(V, e)) })
  m05_b <- rowMeans(bv[, 1:6]); m010_b <- rowMeans(bv[, 1:11])
  juntos[[paste(esp, par)]] <- data.frame(spec = esp, pair = toupper(par), n_obs_h0 = lp$n_obs[1], B = B_LP, n_valid = sum(valid),
    n_valid_test_B20 = sum(apply(is.finite(tst), 1, all)), wald_stat = W, wald_df = H + 1, wald_p_chi2 = 1 - pchisq(W, H + 1), wald_p_boot = mean(Wb >= W),
    mean_diff_h0_5 = mean(d_pt[1:6]), mean_h0_5_ci95_lo = quantile(m05_b, 0.025, names = FALSE), mean_h0_5_ci95_hi = quantile(m05_b, 0.975, names = FALSE),
    mean_diff_h0_10 = mean(d_pt[1:11]), mean_h0_10_ci95_lo = quantile(m010_b, 0.025, names = FALSE), mean_h0_10_ci95_hi = quantile(m010_b, 0.975, names = FALSE),
    n_h_sig_5pct_unadj = sum(lp$p_nw < 0.05), n_h_sig_5pct_holm = sum(lp$p_holm < 0.05), min_p_holm = min(lp$p_holm))
  boots[[paste(esp, par)]] <- bm
  lp$boot_ci95_lo <- apply(bv, 2, quantile, 0.025); lp$boot_ci95_hi <- apply(bv, 2, quantile, 0.975)
  resultados[[paste(esp, par)]] <- lp
}
lp_tab <- do.call(rbind, resultados); rownames(lp_tab) <- NULL; joint_tab <- do.call(rbind, juntos); rownames(joint_tab) <- NULL
print(joint_tab); print(lp_tab[, c("spec","pair","h","diferencial","diferencial_se","p_nw","p_holm")])
v71 <- tryCatch(read.csv(file.path(OUT7, "csv", "V7-1_flow_LP_state_t.csv")), error = function(e) NULL)
if (!is.null(v71)) { m <- merge(lp_tab[lp_tab$spec == "state_t", c("pair","h","diferencial")], v71[, c("pair","h","differential")], by = c("pair","h"))
  log_v7(sprintf("V7-5.11: estado em t reproduz V7-1 (script 26 na base nova): dif. max %.2e", max(abs(m$diferencial - m$differential)))) }
write.csv(lp_tab, file.path(OUT7, "csv", "V7-5.11_flow_LP_by_horizon.csv"), row.names = FALSE)
write.csv(joint_tab, file.path(OUT7, "csv", "V7-5.12_flow_joint_tests.csv"), row.names = FALSE)
saveRDS(list(lp = lp_tab, joint = joint_tab, boots = boots, p_vix = p_vix), file.path(OUT7, "rds", "v7_p3_etapa4_fluxo.rds"))
progresso("V7-2 Etapa 4 (canal de fluxo, V7-5.11/5.12) concluida.")
log_v7(sprintf("v7_13 concluido em %.1f min; p_vix = %d", as.numeric(Sys.time() - t0, units = "mins"), p_vix))
