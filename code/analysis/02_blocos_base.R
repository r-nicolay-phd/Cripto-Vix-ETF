# =====================================================================
# V7-1 — BLOCOS-BASE DO PROMPT 2 (V4) NA BASE NOVA (copias parametrizadas
# de 21_selecao_var.R, 22_baseline_v4.R [22.1], 23_contrastes_v4.R [23.1-
# 23.2], 24_agosto2024.R, 26_canal_fluxo.R e 27_robustez_v4.R [27.2-27.5]).
#  * Selecao de defasagem (BIC/AIC, vars::VAR, p=1..8): reportada; p = 2
#    MANTIDO como especificacao principal (sinaliza se o BIC mudar).
#  * NPDC/NET/TCI por quantil (total/pre/pos), Delta e DD pontuais.
#  * Exclusao de agosto/2024 (janelas a e b), pontual.
#  * TVP-VAR + Bai-Perron + Quandt-Andrews com e sem ago/2024 (a quebra de
#    17/03/2023 se mantem?). A Tarefa 6 continua usando 17/03/2023.
#  * Canal de fluxo com estado em t (LP da V4 + extensao RQ).
#  * Robustez da V4 (pontual): p x H, janela movel, grade de quantis,
#    veiculo (BTC), construtos alternativos.
# Nao repete: superficie QQ completa, QQ dinamico, bootstrap da V4 (com
# o bug de reordenacao zoo, diagnosticado na V5) e placebo da V4 (desenho
# substituido na V5).
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
suppressPackageStartupMessages({ library(vars); library(strucchange); library(sandwich); library(lmtest); library(quantreg) })
t0 <- Sys.time(); log_pkgs7("v7_03_blocos_base.R")
D <- carregar_v4(); CFG <- D$CFG; base_xts <- D$base_xts
CSV7 <- file.path(OUT7, "csv")

## ---------------- 21: selecao de defasagem --------------------------------
PMAX <- 8
sel_ordem <- function(Z, pmax = PMAX) {
  Zm <- as.matrix(Z); out <- data.frame(p = 1:pmax, AIC = NA_real_, BIC = NA_real_)
  for (p in 1:pmax) {
    fit <- tryCatch(VAR(Zm, p = p, type = "const"), error = function(e) NULL)
    if (!is.null(fit)) {
      k <- length(fit$varresult); Tobs <- fit$obs
      Sigma <- crossprod(do.call(cbind, lapply(fit$varresult, resid))) / Tobs
      ldet <- log(det(Sigma)); np <- k * (1 + k * p)
      out$AIC[p] <- ldet + (2 / Tobs) * np; out$BIC[p] <- ldet + (log(Tobs) / Tobs) * np
    }
  }
  out
}
sel_long <- do.call(rbind, lapply(c("btc","eth"), function(par) do.call(rbind, lapply(c("total","pre","post"), function(s) {
  tb <- sel_ordem(D$amostras[[par]][[s]]); tb$pair <- toupper(par); tb$sample <- s; tb }))))
sel_res <- do.call(rbind, lapply(split(sel_long, list(sel_long$pair, sel_long$sample), drop = TRUE), function(d)
  data.frame(pair = d$pair[1], sample = d$sample[1], p_BIC = d$p[which.min(d$BIC)], p_AIC = d$p[which.min(d$AIC)])))
rownames(sel_res) <- NULL; print(sel_res)
p_bic_rule <- max(sel_res$p_BIC[sel_res$sample == "total"])       # regra da V4 (max entre pares, amostra total)
p_aic_rule <- max(sel_res$p_AIC[sel_res$sample == "total"])
log_v7(sprintf("V7-1 BIC (amostra total): BTC %d, ETH %d -> regra V4 = %d (%s); AIC -> %d. Especificacao principal MANTIDA em p = 2.",
               sel_res$p_BIC[sel_res$pair == "BTC" & sel_res$sample == "total"], sel_res$p_BIC[sel_res$pair == "ETH" & sel_res$sample == "total"],
               p_bic_rule, ifelse(p_bic_rule == 2, "igual a V4", "DIFERENTE DE 2 -- SINALIZADO"), p_aic_rule))
write.csv(sel_long[, c("pair","sample","p","AIC","BIC")], file.path(CSV7, "V7-1_lag_selection_full.csv"), row.names = FALSE)
write.csv(sel_res, file.path(CSV7, "V7-1_lag_selection_summary.csv"), row.names = FALSE)

## ---------------- 22.1 / 23: NPDC, NET, TCI por quantil; Delta, DD ---------
qq_one <- function(Z, tau, nlag = 2, nfore = 10) {
  dca <- suppressMessages(ConnectednessApproach(Z, model = "QVAR", connectedness = "Time", nlag = nlag, nfore = nfore,
         window.size = NULL, VAR_config = list(QVAR = list(tau = tau, method = "fn"))))
  a <- colnames(Z)[colnames(Z) != "VIX"]
  m <- dca$NPDC[,,dim(dca$NPDC)[3]]; net <- drop(tail(dca$NET, 1))
  c(NPDC = unname(m["VIX", a]), NET = unname(net[a]), TCI = as.numeric(tail(dca$TCI, 1)))
}
npdc_tab <- do.call(rbind, lapply(c("btc","eth"), function(par) do.call(rbind, lapply(c("total","pre","post"), function(s)
  do.call(rbind, lapply(CFG$taus, function(tau) { v <- qq_one(D$amostras[[par]][[s]], tau)
    data.frame(pair = toupper(par), sample = s, tau = tau, NPDC = unname(v["NPDC"]), NET_asset = unname(v["NET"]), TCI = unname(v["TCI"])) }))))))
print(npdc_tab)
write.csv(npdc_tab, file.path(CSV7, "V7-1_npdc_net_tci_by_quantile.csv"), row.names = FALSE)
g <- function(p, s, t) npdc_tab$NPDC[npdc_tab$pair == p & npdc_tab$sample == s & abs(npdc_tab$tau - t) < 1e-9]
delta_tab <- do.call(rbind, lapply(c("BTC","ETH"), function(p) data.frame(pair = p, tau = CFG$taus,
  NPDC_pre = sapply(CFG$taus, function(t) g(p, "pre", t)), NPDC_post = sapply(CFG$taus, function(t) g(p, "post", t)))))
delta_tab$Delta <- delta_tab$NPDC_post - delta_tab$NPDC_pre
dd_tab <- do.call(rbind, lapply(c("BTC","ETH"), function(p) {
  d <- delta_tab[delta_tab$pair == p, ]; D_ <- function(t) d$Delta[abs(d$tau - t) < 1e-9]; N_ <- function(col, t) d[[col]][abs(d$tau - t) < 1e-9]
  data.frame(pair = p, Delta_0.05 = D_(0.05), Delta_0.50 = D_(0.50), Delta_0.95 = D_(0.95), DD_0.05 = D_(0.05) - D_(0.50), DD_0.95 = D_(0.95) - D_(0.50),
             asym_pre = N_("NPDC_pre", 0.05) - N_("NPDC_pre", 0.95), asym_post = N_("NPDC_post", 0.05) - N_("NPDC_post", 0.95),
             dAsym = (N_("NPDC_post", 0.05) - N_("NPDC_post", 0.95)) - (N_("NPDC_pre", 0.05) - N_("NPDC_pre", 0.95)))
}))
print(delta_tab); print(dd_tab)
write.csv(delta_tab, file.path(CSV7, "V7-1_delta_all_quantiles_point.csv"), row.names = FALSE)
write.csv(dd_tab, file.path(CSV7, "V7-1_DD_asymmetry_point.csv"), row.names = FALSE)
progresso("V7-1: selecao de defasagem, NPDC/NET/TCI por quantil e Delta/DD pontuais gravados.")

## ---------------- 24.1-24.2: exclusao de agosto/2024 (pontual) -------------
janelas <- list(a = c(as.Date("2024-07-22"), as.Date("2024-08-16")), b = c(as.Date("2024-07-01"), as.Date("2024-09-30")))
excl_rows <- list(); excl_am <- list()
for (jn in names(janelas)) for (par in c("btc","eth")) {
  jw <- janelas[[jn]]; Zt <- D$amostras[[par]]$total; cut <- CUTS[[par]]
  Z <- Zt[!(index(Zt) >= jw[1] & index(Zt) <= jw[2])]
  Mp <- coredata(Z[index(Z) < cut]); Mq <- coredata(Z[index(Z) >= cut]); excl_am[[paste(jn, par)]] <- Z
  np <- npdc_taus(Mp); nq <- npdc_taus(Mq)
  excl_rows[[length(excl_rows) + 1]] <- data.frame(window = jn, pair = toupper(par), tau = TAUS, n_pre = nrow(Mp), n_post = nrow(Mq),
                                                   NPDC_pre = as.numeric(np), NPDC_post = as.numeric(nq), Delta = as.numeric(nq - np))
}
excl_tab <- do.call(rbind, excl_rows)
excl_dd <- do.call(rbind, lapply(split(excl_tab, list(excl_tab$window, excl_tab$pair)), function(d) {
  D_ <- function(t) d$Delta[abs(d$tau - t) < 1e-9]
  data.frame(window = d$window[1], pair = d$pair[1], Delta_0.05 = D_(0.05), Delta_0.50 = D_(0.50), Delta_0.95 = D_(0.95),
             DD_0.05 = D_(0.05) - D_(0.50), DD_0.95 = D_(0.95) - D_(0.50)) }))
rownames(excl_dd) <- NULL; print(excl_dd)
write.csv(excl_tab, file.path(CSV7, "V7-1_exclusion_Aug2024_delta_point.csv"), row.names = FALSE)
write.csv(excl_dd, file.path(CSV7, "V7-1_exclusion_Aug2024_DD_point.csv"), row.names = FALSE)

## ---------------- 24.3-24.4 / 27.2: TVP-VAR + quebras ---------------------
quebra <- function(Z, cut, rotulo) {
  tvp <- suppressMessages(ConnectednessApproach(Z, model = "TVP-VAR", connectedness = "Time", nlag = 2, nfore = 10,
         VAR_config = list(TVPVAR = list(kappa1 = 0.99, kappa2 = 0.96, prior = "BayesPrior"))))
  tci <- zoo(as.numeric(tvp$TCI), order.by = as.Date(rownames(as.matrix(tvp$TCI))))
  bp <- tryCatch(breakpoints(tci ~ 1), error = function(e) NULL)
  bpd <- tryCatch(as.Date(index(tci))[bp$breakpoints], error = function(e) as.Date(NA))
  fs <- Fstats(tci ~ 1, from = 0.15); qa_p <- tryCatch(sctest(fs, type = "supF")$p.value, error = function(e) NA)
  qad <- as.Date(index(tci))[which.max(fs$Fstats)]
  dist <- function(x) if (all(is.na(x))) NA else min(abs(as.numeric(x - cut)))
  list(tci = tci, tab = data.frame(sample = rotulo, test = c("Bai-Perron (closest to the ETF date)", "Quandt-Andrews supF"),
       date = c(if (all(is.na(bpd))) NA else format(bpd[which.min(abs(bpd - cut))]), format(qad)),
       all_BP_dates = paste(format(bpd), collapse = "; "), days_to_ETF = c(dist(bpd), dist(qad)), p_value = c(NA, qa_p),
       BP_includes_2023_03_17 = as.Date("2023-03-17") %in% bpd,
       BP_date_nearest_2023_03_17 = if (all(is.na(bpd))) NA else format(bpd[which.min(abs(bpd - as.Date("2023-03-17")))])))
}
br_rows <- list(); tci_series <- list()
for (par in c("btc","eth")) {
  tt <- Sys.time()
  q1 <- quebra(D$amostras[[par]]$total, CUTS[[par]], "with Aug 2024"); q2 <- quebra(excl_am[[paste("a", par)]], CUTS[[par]], "without 22 Jul-16 Aug 2024")
  br_rows[[par]] <- cbind(pair = toupper(par), rbind(q1$tab, q2$tab)); tci_series[[par]] <- list(with = q1$tci, without = q2$tci)
  log_v7(sprintf("V7-1 quebras %s: BP (com ago) %s; QA %s (p=%.4g) [%.1f min]", toupper(par), q1$tab$all_BP_dates[1], q1$tab$date[2], q1$tab$p_value[2],
                 as.numeric(Sys.time() - tt, units = "mins")))
}
br_tab <- do.call(rbind, br_rows); rownames(br_tab) <- NULL; print(br_tab)
write.csv(br_tab, file.path(CSV7, "V7-1_breaks_TVP_TCI.csv"), row.names = FALSE)
saveRDS(tci_series, file.path(OUT7, "rds", "v7_tvp_tci_series.rds"))
progresso(sprintf("V7-1: quebras TVP-VAR (BP/QA) gravadas; 17/03/2023 entre as datas BP: BTC %s, ETH %s.",
                  br_tab$BP_includes_2023_03_17[br_tab$pair == "BTC" & br_tab$sample == "with Aug 2024"][1],
                  br_tab$BP_includes_2023_03_17[br_tab$pair == "ETH" & br_tab$sample == "with Aug 2024"][1]))

## ---------------- 26: canal de fluxo com estado em t (LP + RQ) --------------
ler_fluxo  <- function(path) { d <- read.csv(path, stringsAsFactors = FALSE); d$date <- as.Date(d$date); xts(d$total_mi_usd, order.by = d$date) }
ler_volume <- function(path) { d <- read.csv(path, stringsAsFactors = FALSE); d$date <- as.Date(d$date); xts(d$volume, order.by = d$date) }
normaliza_fluxo <- function(flow, vol) { colnames(vol) <- "vol"; m <- merge(flow, vol, join = "left")
  m$vol_ma20 <- rollapply(m$vol, 20, mean, align = "right", fill = NA, na.rm = TRUE); m$flow_norm <- m$flow / m$vol_ma20; m }
classifica_estado <- function(fn) { x <- as.numeric(fn$flow_norm); q <- quantile(x, probs = c(0.2, 0.8), na.rm = TRUE)
  data.frame(date = index(fn), estado = ifelse(x <= q[1], "resgate", ifelse(x >= q[2], "captacao", "neutro")), flow_norm = x, stringsAsFactors = FALSE) }
fb <- ler_fluxo("data/clean/etf_flow_btc_farside_limpo.csv"); colnames(fb) <- "flow"
fe <- ler_fluxo("data/clean/etf_flow_eth_farside_limpo.csv"); colnames(fe) <- "flow"
ESTADO <- list(btc = classifica_estado(normaliza_fluxo(fb, ler_volume("data/clean/etf_ibit_yahoo.csv"))),
               eth = classifica_estado(normaliza_fluxo(fe, ler_volume("data/clean/etf_etha_yahoo.csv"))))
vix_full <- na.omit(base_xts[, "VIX"])
sel_ar_bic <- function(x, pmax = 10) which.min(sapply(1:pmax, function(p) {
  fit <- tryCatch(ar.ols(x, order.max = p, aic = FALSE, demean = TRUE, intercept = TRUE), error = function(e) NULL)
  if (is.null(fit)) return(NA); r <- na.omit(fit$resid); n <- length(r); n * log(sum(r^2) / n) + log(n) * (p + 1) }))
p_vix <- sel_ar_bic(as.numeric(vix_full))
ar_fit <- ar.ols(as.numeric(vix_full), order.max = p_vix, aic = FALSE, demean = TRUE, intercept = TRUE)
choque_vix <- xts(as.numeric(ar_fit$resid), order.by = index(vix_full)); colnames(choque_vix) <- "choque_vix"
montar_lp <- function(ativo, estado_df) {
  num <- merge(base_xts[, ativo], choque_vix, join = "inner"); colnames(num) <- c("ret","choque")
  lags <- list(); for (l in 1:2) { lags[[paste0("ret_lag", l)]] <- lag(num$ret, l); lags[[paste0("choque_lag", l)]] <- lag(num$choque, l) }
  num <- do.call(merge, c(list(num), lags)); colnames(num) <- c("ret","choque", names(lags))
  df <- data.frame(date = index(num), coredata(num)); df <- merge(df, estado_df[, c("date","estado")], by = "date", all.x = TRUE)
  df <- df[!is.na(df$ret) & !is.na(df$choque) & !is.na(df$estado), ]; df <- df[order(df$date), ]
  df$D_resgate <- as.numeric(df$estado == "resgate"); df$D_captacao <- as.numeric(df$estado == "captacao"); df$D_neutro <- as.numeric(df$estado == "neutro")
  df$resgate_x_choque <- df$D_resgate * df$choque; df$captacao_x_choque <- df$D_captacao * df$choque; df$neutro_x_choque <- df$D_neutro * df$choque
  df
}
lp_rows <- list(); rq_rows <- list()
RHS <- "0 + resgate_x_choque + captacao_x_choque + neutro_x_choque + D_resgate + D_captacao + ret_lag1 + ret_lag2 + choque_lag1 + choque_lag2"
for (par in c("btc","eth")) {
  df <- montar_lp(toupper(par), ESTADO[[par]]); n <- nrow(df)
  for (h in 0:10) {
    dfh <- df; dfh$y <- sapply(1:n, function(i) if (i + h <= n) sum(df$ret[i:(i + h)]) else NA_real_)
    fit <- lm(as.formula(paste("y ~", RHS)), data = dfh, na.action = na.omit)
    nw <- NeweyWest(fit, lag = h + 1, prewhite = FALSE, adjust = TRUE); b <- coef(fit)
    dse <- sqrt(nw["resgate_x_choque","resgate_x_choque"] + nw["captacao_x_choque","captacao_x_choque"] - 2 * nw["resgate_x_choque","captacao_x_choque"])
    lp_rows[[length(lp_rows) + 1]] <- data.frame(pair = toupper(par), h = h, n_obs = nobs(fit), outflow_beta = b["resgate_x_choque"], outflow_se = sqrt(nw["resgate_x_choque","resgate_x_choque"]),
      inflow_beta = b["captacao_x_choque"], inflow_se = sqrt(nw["captacao_x_choque","captacao_x_choque"]), neutral_beta = b["neutro_x_choque"],
      differential = b["resgate_x_choque"] - b["captacao_x_choque"], differential_se = dse, row.names = NULL)
  }
  dfr <- na.omit(df)
  for (tau in c(0.05, 0.95)) {
    f <- rq(as.formula(paste("ret ~", RHS)), tau = tau, data = dfr)
    s <- tryCatch(summary(f, se = "nid"), error = function(e) tryCatch(summary(f), error = function(e2) NULL)); co <- s$coefficients
    rq_rows[[length(rq_rows) + 1]] <- data.frame(pair = toupper(par), tau = tau, n_obs = nrow(dfr), outflow_beta = co["resgate_x_choque", 1], outflow_se = co["resgate_x_choque", 2],
      inflow_beta = co["captacao_x_choque", 1], inflow_se = co["captacao_x_choque", 2], differential = co["resgate_x_choque", 1] - co["captacao_x_choque", 1], row.names = NULL)
  }
}
lp_tab <- do.call(rbind, lp_rows); lp_tab$ci95_lo <- lp_tab$differential - 1.96 * lp_tab$differential_se; lp_tab$ci95_hi <- lp_tab$differential + 1.96 * lp_tab$differential_se
rq_tab <- do.call(rbind, rq_rows); print(lp_tab); print(rq_tab)
write.csv(lp_tab, file.path(CSV7, "V7-1_flow_LP_state_t.csv"), row.names = FALSE)
write.csv(rq_tab, file.path(CSV7, "V7-1_flow_RQ_state_t.csv"), row.names = FALSE)
log_v7(sprintf("V7-1 canal de fluxo (estado em t): AR(p) do VIX p=%d; diferencial h=0 BTC %.3f, ETH %.3f; h=10 BTC %.3f, ETH %.3f", p_vix,
               lp_tab$differential[lp_tab$pair == "BTC" & lp_tab$h == 0], lp_tab$differential[lp_tab$pair == "ETH" & lp_tab$h == 0],
               lp_tab$differential[lp_tab$pair == "BTC" & lp_tab$h == 10], lp_tab$differential[lp_tab$pair == "ETH" & lp_tab$h == 10]))

## ---------------- 27.3: robustez de especificacao (pontual) ---------------
p_grid <- unique(c(1, 2, 3, p_bic_rule, p_aic_rule)); H_grid <- c(5, 10, 20)
rob <- list()
for (par in c("btc","eth")) {
  Mp <- coredata(D$amostras[[par]]$pre); Mq <- coredata(D$amostras[[par]]$post)
  for (H in H_grid) for (p in p_grid) {
    np <- npdc_taus(Mp, c(0.05, 0.50, 0.95), nlag = p, nfore = H); nq <- npdc_taus(Mq, c(0.05, 0.50, 0.95), nlag = p, nfore = H); d <- nq - np
    rob[[length(rob) + 1]] <- data.frame(pair = toupper(par), p = p, p_label = paste0(p, ifelse(p == p_bic_rule, " (BIC)", ""), ifelse(p == p_aic_rule, " (AIC)", "")),
      H = H, Delta_0.05 = unname(d["0.05"]), Delta_0.50 = unname(d["0.50"]), Delta_0.95 = unname(d["0.95"]),
      DD_0.05 = unname(d["0.05"] - d["0.50"]), DD_0.95 = unname(d["0.95"] - d["0.50"]))
  }
}
rob_tab <- do.call(rbind, rob); print(rob_tab)
write.csv(rob_tab, file.path(CSV7, "V7-1_robustness_p_H_point.csv"), row.names = FALSE)

taus_alt <- seq(0.025, 0.975, by = 0.025)
alt <- do.call(rbind, lapply(c("btc","eth"), function(par) {
  np <- npdc_taus(coredata(D$amostras[[par]]$pre), taus_alt); nq <- npdc_taus(coredata(D$amostras[[par]]$post), taus_alt)
  data.frame(pair = toupper(par), tau = taus_alt, NPDC_pre = as.numeric(np), NPDC_post = as.numeric(nq), Delta = as.numeric(nq - np)) }))
write.csv(alt, file.path(CSV7, "V7-1_robustness_alt_quantile_grid.csv"), row.names = FALSE)

m_fut <- as.Date("2021-10-19"); Zb <- D$amostras$btc$total
reg <- list(pre_futures = Zb[index(Zb) < m_fut], futures_ETF_phase = Zb[index(Zb) >= m_fut & index(Zb) < CUTS$btc], spot_ETF_phase = Zb[index(Zb) >= CUTS$btc])
veic <- do.call(rbind, lapply(names(reg), function(nm) { v <- npdc_taus(coredata(reg[[nm]]), c(0.05, 0.50, 0.95))
  data.frame(regime = nm, first = format(min(index(reg[[nm]]))), last = format(max(index(reg[[nm]]))), n = nrow(reg[[nm]]),
             NPDC_0.05 = v[["0.05"]], NPDC_0.50 = v[["0.50"]], NPDC_0.95 = v[["0.95"]]) }))
print(veic); write.csv(veic, file.path(CSV7, "V7-1_robustness_vehicle_BTC.csv"), row.names = FALSE)

gspc <- read.csv("data/clean/gspc_yahoo.csv"); gspc$date <- as.Date(gspc$date); gspc <- xts(gspc$close, order.by = gspc$date)
vxx <- ler_vix(); spx_ret <- diff(log(gspc)); rv22 <- sqrt(252) * rollapply(spx_ret, 22, sd, align = "right", fill = NA, na.rm = TRUE) * 100
vprem <- vxx[index(rv22)] - rv22; colnames(vprem) <- "RISK"
epu <- read.csv("data/clean/epu_daily.csv"); epu$date <- as.Date(epu$date); epu <- xts(epu$daily_policy_index, order.by = epu$date); colnames(epu) <- "RISK"
dvl <- lapply(c(btc = "data/clean/dvol_btc_deribit.csv", eth = "data/clean/dvol_eth_deribit.csv"), function(f) { d <- read.csv(f); d$date <- as.Date(d$date); x <- xts(d$close, order.by = d$date); colnames(x) <- "RISK"; x })
npdc_risk <- function(z, tau) { dca <- suppressMessages(ConnectednessApproach(z, model = "QVAR", connectedness = "Time", nlag = 2, nfore = 10, window.size = NULL,
  VAR_config = list(QVAR = list(tau = tau, method = "fn")))); m <- dca$NPDC[,,dim(dca$NPDC)[3]]; m["RISK", colnames(z)[1]] }
cons <- list()
for (par in c("btc","eth")) {
  a <- toupper(par); r <- base_xts[, a]
  for (cx in c("VIX_premium","EPU","DVOL")) {
    cs <- switch(cx, VIX_premium = vprem, EPU = epu, DVOL = dvl[[par]])
    z <- na.omit(merge(r, cs, join = "inner")); colnames(z) <- c(a, "RISK")
    if (cx == "DVOL") z <- z[index(z) >= as.Date("2021-03-24")]
    z <- zoo(coredata(z), order.by = as.Date(index(z)))
    cons[[length(cons) + 1]] <- data.frame(pair = a, construct = cx, first = format(min(index(z))), last = format(max(index(z))), n = nrow(z),
      NPDC_0.05 = tryCatch(npdc_risk(z, 0.05), error = function(e) NA), NPDC_0.95 = tryCatch(npdc_risk(z, 0.95), error = function(e) NA))
  }
}
cons_tab <- do.call(rbind, cons); print(cons_tab)
write.csv(cons_tab, file.path(CSV7, "V7-1_robustness_alt_constructs.csv"), row.names = FALSE)
progresso("V7-1: exclusao ago/2024, canal de fluxo (estado t), robustez p x H, grade de quantis, veiculo e construtos gravados.")

## ---------------- 27.3b: janela movel {150,200,250} (paralelo) -------------
jt <- list(); for (par in c("btc","eth")) for (w in c(150, 200, 250)) for (tau in c(0.05, 0.95)) jt[[length(jt) + 1]] <- list(par = par, w = w, tau = tau)
.ZT <- lapply(D$amostras[c("btc","eth")], function(a) a$total)
jan_task <- function(tk) {
  Z <- get(".ZT", envir = globalenv())[[tk$par]]
  dca <- suppressMessages(ConnectednessApproach(Z, model = "QVAR", connectedness = "Time", nlag = 2, nfore = 10, window.size = tk$w,
         VAR_config = list(QVAR = list(tau = tk$tau, method = "fn"))))
  tci <- as.numeric(dca$TCI); a <- toupper(tk$par)
  np <- dca$NPDC["VIX", a, ]
  list(TCI_mean = mean(tci, na.rm = TRUE), TCI_final = tail(tci, 1), NPDC_mean = mean(np, na.rm = TRUE), n_windows = length(tci))
}
assign(".ZT", .ZT, envir = globalenv())
tt <- Sys.time()
jr <- par_map_timeout(jt, jan_task, n_workers = 12, timeout = 7200, export_names = ".ZT", export_env = globalenv())
jan_tab <- do.call(rbind, lapply(seq_along(jt), function(i) data.frame(pair = toupper(jt[[i]]$par), window = jt[[i]]$w, tau = jt[[i]]$tau,
  TCI_mean = if (is.null(jr[[i]]$TCI_mean)) NA else jr[[i]]$TCI_mean, TCI_final = if (is.null(jr[[i]]$TCI_final)) NA else jr[[i]]$TCI_final,
  NPDC_mean = if (is.null(jr[[i]]$NPDC_mean)) NA else jr[[i]]$NPDC_mean, error = if (is.null(jr[[i]]$err)) "" else jr[[i]]$err)))
print(jan_tab); write.csv(jan_tab, file.path(CSV7, "V7-1_robustness_rolling_window.csv"), row.names = FALSE)
log_v7(sprintf("V7-1 janela movel: %d/%d tarefas ok (%.1f min)", sum(jan_tab$error == ""), nrow(jan_tab), as.numeric(Sys.time() - tt, units = "mins")))

saveRDS(list(sel_long = sel_long, sel_res = sel_res, p_bic_rule = p_bic_rule, p_aic_rule = p_aic_rule, npdc_tab = npdc_tab, delta_tab = delta_tab, dd_tab = dd_tab,
             excl_tab = excl_tab, excl_dd = excl_dd, br_tab = br_tab, lp_tab = lp_tab, rq_tab = rq_tab, p_vix = p_vix, rob_tab = rob_tab, alt = alt,
             veic = veic, cons_tab = cons_tab, jan_tab = jan_tab), file.path(OUT7, "rds", "v7_1_blocos_base.rds"))
progresso("V7-1 concluida (inclui janela movel).")
log_v7(sprintf("v7_03 (V7-1) concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
