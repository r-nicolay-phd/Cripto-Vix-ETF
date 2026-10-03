# =====================================================================
# V7-3 / Prompt 4, TAREFA 0 — diagnostico com a base nova (copia
# parametrizada de v6_02_tarefa0.R)
#  0a exemplo numerico de segunda-feira na base nova (CMC re-rotulada,
#     construcao ii) + verificacao global da construcao;
#  0b rotulo/horario: teste de correlacao CMC re-rotulada x Binance
#     (deve ser maximo SEM deslocamento: mesmo fechamento 24:00 UTC);
#  0d Tabela S15 (RQ do canal de fluxo) com estado em t e t-1 na base nova.
# Semente 20260930 + 0. Saidas V7-6.0*.
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
suppressPackageStartupMessages({ library(quantreg); library(readxl) })
t0 <- Sys.time(); log_pkgs7("v7_20_p4_tarefa0.R")
K <- 0; SEED <- SEED_BASE_V6 + K

## ---------------- 0a ------------------------------------------------------
base <- read.csv(BASE_V7); base$date <- as.Date(base$date)
cmc <- as.data.frame(read_excel("data/raw/manual/BTC_cmc.xlsx")); cmc$Date <- as.Date(cmc$Date)
cmc_e <- as.data.frame(read_excel("data/raw/manual/ETH_cmc.xlsx")); cmc_e$Date <- as.Date(cmc_e$Date)
rel <- data.frame(date = cmc$Date - 1, close = cmc$BTC.Close, export_label = cmc$Date)        # re-rotulo D -> D-1
rel_e <- data.frame(date = cmc_e$Date - 1, close = cmc_e$ETH.Close)
vix <- ler_vix()
ex_days <- as.Date(c("2024-02-29","2024-03-01","2024-03-02","2024-03-03","2024-03-04","2024-03-05"))
ex <- data.frame(date = ex_days, weekday = weekdays(ex_days),
                 export_label_of_value = rel$export_label[match(ex_days, rel$date)],
                 BTC_close_relabelled = rel$close[match(ex_days, rel$date)],
                 VIX = as.numeric(vix[ex_days])[match(ex_days, as.Date(index(vix[ex_days])))],
                 BTC_ret_base_v7 = base$BTC[match(ex_days, base$date)])
P <- function(d) rel$close[rel$date == as.Date(d)]
ex_calc <- data.frame(quantity = c("ret(ii) Mon 2024-03-04 = 100*log(P[Mon]/P[Fri 2024-03-01])", "ret(i)  Mon 2024-03-04 = 100*log(P[Mon]/P[Sun 2024-03-03])",
                                   "base_estendida_v7.csv BTC on 2024-03-04"),
                      value = c(100 * log(P("2024-03-04") / P("2024-03-01")), 100 * log(P("2024-03-04") / P("2024-03-03")), base$BTC[base$date == as.Date("2024-03-04")]))
print(ex); print(ex_calc)
chk_one <- function(r, col) {
  inter <- merge(r, data.frame(date = as.Date(index(vix))), by = "date"); inter <- inter[order(inter$date), ]; inter$ret_ii <- c(NA, diff(log(inter$close))) * 100
  ch <- merge(base[, c("date", col)], inter[, c("date","ret_ii")], by = "date"); ch <- ch[ch$date >= as.Date("2018-01-02") & ch$date <= as.Date("2026-09-18") & !is.na(ch[[col]]), ]
  alld <- r[order(r$date), ]; alld$ret_i <- c(NA, diff(log(alld$close))) * 100
  ch2 <- merge(ch, alld[, c("date","ret_i")], by = "date"); seg <- weekdays(ch2$date) == "Monday"
  data.frame(asset = col, n = nrow(ch2), construction = c("(ii) VIX calendar first", "(i) 7-day then intersect"),
             match_all = c(mean(abs(ch2[[col]] - ch2$ret_ii) < 1e-8), mean(abs(ch2[[col]] - ch2$ret_i) < 1e-8)),
             match_mondays = c(mean(abs(ch2[[col]][seg] - ch2$ret_ii[seg]) < 1e-8), mean(abs(ch2[[col]][seg] - ch2$ret_i[seg]) < 1e-8)),
             match_other_days = c(mean(abs(ch2[[col]][!seg] - ch2$ret_ii[!seg]) < 1e-8), mean(abs(ch2[[col]][!seg] - ch2$ret_i[!seg]) < 1e-8)))
}
ii_i <- rbind(chk_one(rel[, c("date","close")], "BTC"), chk_one(rel_e, "ETH")); print(ii_i)

## ---------------- 0b ------------------------------------------------------
bin <- list(BTC = read.csv("data/clean/btc_binance.csv"), ETH = read.csv("data/clean/eth_binance.csv"))
lab <- do.call(rbind, lapply(c("BTC","ETH"), function(a) {
  cm <- if (a == "BTC") rel[, c("date","close")] else rel_e; names(cm) <- c("date","c")
  bn <- bin[[a]]; bn$date <- as.Date(bn$date); bn <- bn[order(bn$date), ]; cm <- cm[order(cm$date), ]
  rc <- data.frame(date = cm$date, rc = c(NA, diff(log(cm$c)))); rb <- data.frame(date = bn$date, rb = c(NA, diff(log(bn$close))))
  do.call(rbind, lapply(c(-1, 0, 1), function(s) {
    rb2 <- rb; rb2$date <- rb2$date + s
    m <- merge(rc, rb2, by = "date"); m <- m[m$date >= as.Date("2019-01-01") & m$date <= as.Date("2026-09-18"), ]
    lv <- merge(cm, transform(bn[, c("date","close")], date = date + s), by = "date"); lv <- lv[lv$date >= as.Date("2019-01-01") & lv$date <= as.Date("2026-09-18"), ]
    data.frame(asset = a, series = "CMC relabelled (V7 base)", binance_label_shift_days = s, n = nrow(m), corr_daily_returns = cor(m$rc, m$rb, use = "complete.obs"),
               median_abs_level_diff_pct = median(abs(lv$c / lv$close - 1)) * 100)
  }))
}))
print(lab)
ex_lab <- merge(rel[, c("date","close","export_label")], transform(bin$BTC[, c("date","close")], date = as.Date(date)), by = "date", suffixes = c("_CMC_relabelled","_Binance"))
ex_lab <- ex_lab[ex_lab$date >= as.Date("2024-01-06") & ex_lab$date <= as.Date("2024-01-12"), ]; print(ex_lab)

## ---------------- 0d: S15 com estado em t e t-1 ---------------------------
D <- carregar_v4(); base_xts <- D$base_xts
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
montar_rq_data <- function(ativo, estado_df, defasar) {
  num <- merge(base_xts[, ativo], choque_vix, join = "inner"); colnames(num) <- c("ret","choque")
  lags <- list(); for (l in 1:2) { lags[[paste0("ret_lag", l)]] <- lag(num$ret, l); lags[[paste0("choque_lag", l)]] <- lag(num$choque, l) }
  num <- do.call(merge, c(list(num), lags)); colnames(num) <- c("ret","choque", names(lags))
  df <- data.frame(date = index(num), coredata(num)); df <- merge(df, estado_df[, c("date","estado")], by = "date", all.x = TRUE); df <- df[order(df$date), ]
  if (defasar) df$estado <- c(NA, head(df$estado, -1))
  df <- df[!is.na(df$ret) & !is.na(df$choque) & !is.na(df$estado), ]; df <- na.omit(df)
  df$D_resgate <- as.numeric(df$estado == "resgate"); df$D_captacao <- as.numeric(df$estado == "captacao"); df$D_neutro <- as.numeric(df$estado == "neutro")
  df$resgate_x_choque <- df$D_resgate * df$choque; df$captacao_x_choque <- df$D_captacao * df$choque; df$neutro_x_choque <- df$D_neutro * df$choque
  df
}
FORM <- ret ~ 0 + resgate_x_choque + captacao_x_choque + neutro_x_choque + D_resgate + D_captacao + ret_lag1 + choque_lag1 + ret_lag2 + choque_lag2
rq_diff <- function(df, tau) { f <- rq(FORM, tau = tau, data = df, method = "br"); unname(coef(f)["resgate_x_choque"] - coef(f)["captacao_x_choque"]) }
TAUS_S15 <- c(0.05, 0.50, 0.95)
res <- list(); set.seed(SEED)
for (esp in c("state_t","state_tminus1")) for (par in c("btc","eth")) {
  df <- montar_rq_data(toupper(par), ESTADO[[par]], esp == "state_tminus1")
  bt <- t(sapply(1:B_FULL, function(b) { d <- df[mbb_idx(nrow(df)), ]; sapply(TAUS_S15, function(tau) tryCatch(rq_diff(d, tau), error = function(e) NA)) }))
  for (j in seq_along(TAUS_S15)) {
    tau <- TAUS_S15[j]; f <- rq(FORM, tau = tau, data = df, method = "br")
    s <- tryCatch(summary(f, se = "nid", covariance = TRUE), error = function(e) NULL)
    co <- coef(f); V <- if (!is.null(s)) { vv <- s$cov; dimnames(vv) <- list(names(co), names(co)); vv } else matrix(NA, length(co), length(co), dimnames = list(names(co), names(co)))
    dse <- sqrt(V["resgate_x_choque","resgate_x_choque"] + V["captacao_x_choque","captacao_x_choque"] - 2 * V["resgate_x_choque","captacao_x_choque"])
    dif <- unname(co["resgate_x_choque"] - co["captacao_x_choque"])
    res[[length(res) + 1]] <- cbind(resumir_v6(dif, bt[, j], spec = esp, pair = toupper(par), tau = tau, n_obs = nrow(df),
      beta_outflow = unname(co["resgate_x_choque"]), se_outflow_nid = sqrt(V["resgate_x_choque","resgate_x_choque"]),
      beta_inflow = unname(co["captacao_x_choque"]), se_inflow_nid = sqrt(V["captacao_x_choque","captacao_x_choque"])), diff_se_nid = dse, p_nid = 2 * pnorm(-abs(dif / dse)))
  }
}
s15 <- do.call(rbind, res); rownames(s15) <- NULL
print(s15[, c("spec","pair","tau","n_obs","point","diff_se_nid","p_nid","boot_median","ci95_lo","ci95_hi","n_valid","ci95_excludes_0")])
v71 <- read.csv(file.path(OUT7, "csv", "V7-1_flow_RQ_state_t.csv"))
mm <- merge(s15[s15$spec == "state_t", c("pair","tau","point")], v71[, c("pair","tau","differential")], by = c("pair","tau"))
log_v7(sprintf("V7-6.0d: S15 estado em t (base nova) = V7-1 RQ (script 26 na base nova): dif. max %.2e", max(abs(mm$point - mm$differential))))

write.csv(s15, file.path(OUT7, "csv", "V7-6.0d_S15_flow_RQ_state_t_vs_tminus1.csv"), row.names = FALSE)
write.csv(lab, file.path(OUT7, "csv", "V7-6.0b_cmc_relabelled_vs_binance_label_test.csv"), row.names = FALSE)
write.csv(ex_lab, file.path(OUT7, "csv", "V7-6.0b_label_example.csv"), row.names = FALSE)
write.csv(ex, file.path(OUT7, "csv", "V7-6.0a_monday_example.csv"), row.names = FALSE)
write.csv(ex_calc, file.path(OUT7, "csv", "V7-6.0a_monday_example_calc.csv"), row.names = FALSE)
write.csv(ii_i, file.path(OUT7, "csv", "V7-6.0a_construction_match.csv"), row.names = FALSE)
saveRDS(list(ex = ex, ex_calc = ex_calc, ii_i = ii_i, lab = lab, ex_lab = ex_lab, s15 = s15, p_vix = p_vix), file.path(OUT7, "rds", "v7_6_0.rds"))
progresso("V7-3 Tarefa 0 (diagnostico base nova, S15 t/t-1) concluida.")
log_v7(sprintf("v7_20 (Tarefa 0) concluido em %.1f s", as.numeric(Sys.time() - t0, units = "secs")))
