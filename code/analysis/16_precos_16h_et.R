# =====================================================================
# V7-3 / Prompt 4, TAREFA 4 — horario e fins de semana, base nova
# (copia parametrizada de v6_11_tarefa4.R). Sem nova coleta.
#  4a comparacao das series (CMC re-rotulada = base V7; Coinbase 16h ET;
#     Binance 24:00 UTC; Binance 16h ET) -- retornos construcao (ii)
#  4c combinacoes:
#     A  CMC re-rotulada, construcao (ii) = REFERENCIA DA V7 (mesma serie
#        da base nova); controles Binance diarios (24:00 UTC)
#     B  Coinbase 16:00 ET, construcao (ii); controles Binance 1h 16:00 ET
#        (TRX nao existe na Coinbase; mesmo desenho da combinacao C da V6)
#     Para comparacao: CMC NAO re-rotulada = numeros da V6 (V5-4, V6-1a),
#     trazidos sem reestimar (v7_30).
#  Em cada combinacao: NPDC por quantil, Tabela 1 (bootstrap V5-4) e
#  contrafactual tratado - cesta B em 0.50 e 0.95 (bootstrap conjunto).
# Semente 20260930 + 4 (a mesma da V6: a combinacao A deve reproduzir
# exatamente a combinacao X1 da V6, inclusive os IC).
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
t0 <- Sys.time(); log_pkgs7("v7_28_p4_tarefa4.R")
K <- 4; SEED <- SEED_BASE_V6 + K
D <- carregar_v4(); vx <- ler_vix()
rd <- function(f) { d <- read.csv(f); d$date <- as.Date(d$date); d }
cb16 <- list(BTC = rd("data/clean/px16et_btc_coinbase.csv"), ETH = rd("data/clean/px16et_eth_coinbase.csv"))
bn16 <- lapply(setNames(c(CONTROLES, "BTC", "ETH"), c(CONTROLES, "BTC", "ETH")), function(a) rd(sprintf("data/clean/px16et_%s_binance.csv", tolower(a))))
cbc16 <- lapply(setNames(c("ADA","XLM","XRP","LTC"), c("ADA","XLM","XRP","LTC")), function(a) { f <- sprintf("data/clean/px16et_%s_coinbase.csv", tolower(a)); if (file.exists(f)) rd(f) else NULL })
bnd <- list(BTC = rd("data/clean/btc_binance.csv"), ETH = rd("data/clean/eth_binance.csv"))
fallback <- rbind(do.call(rbind, lapply(names(cb16), function(a) data.frame(series = paste(a, "Coinbase"), n_days = nrow(cb16[[a]]), n_fallback = sum(cb16[[a]]$source_candle != "15:00-16:00 ET")))),
                  do.call(rbind, lapply(names(bn16), function(a) data.frame(series = paste(a, "Binance 1h"), n_days = nrow(bn16[[a]]), n_fallback = sum(bn16[[a]]$source_candle != "15:00-16:00 ET")))))
in_win <- function(Z) Z[index(Z) >= DATE_INI & index(Z) <= DATE_FIM]
trunc_z <- function(Z, a) { if (!is.na(TRUNCA[a])) Z <- Z[index(Z) <= as.Date(TRUNCA[a])]; Z }
tr <- list(A = lapply(c(BTC = "btc", ETH = "eth"), function(p) as.xts(D$amostras[[p]]$total)),
           B = lapply(c(BTC = "BTC", ETH = "ETH"), function(a) in_win(ret_ii(cb16[[a]]$date, cb16[[a]]$close_16et, vx, a))))
cr <- list(A = lapply(setNames(CONTROLES, CONTROLES), serie_ctrl_xts, vix_xts = vx),
           B = lapply(setNames(CONTROLES, CONTROLES), function(a) trunc_z(in_win(ret_ii(bn16[[a]]$date, bn16[[a]]$close_16et, vx, a)), a)))

## ---- 4a ------------------------------------------------------------------
cmp_ret <- function(x, y, lx, ly, grp) {
  m <- na.omit(merge(x[, 1], y[, 1], join = "inner")); a <- as.numeric(m[, 1]); b <- as.numeric(m[, 2]); dd <- abs(a - b); seg <- weekdays(as.Date(index(m))) == "Monday"
  data.frame(asset = grp, series_x = lx, series_y = ly, n = nrow(m), corr = cor(a, b), corr_mondays = cor(a[seg], b[seg]), corr_other_days = cor(a[!seg], b[!seg]),
             mean_abs_diff_pp = mean(dd), n_absdiff_gt_1pp = sum(dd > 1), n_absdiff_gt_3pp = sum(dd > 3), n_sign_diff = sum(sign(a) != sign(b)),
             max_abs_diff_pp = max(dd), date_max_diff = format(as.Date(index(m))[which.max(dd)]))
}
t4a <- list()
for (a in c("BTC","ETH")) {
  bin24 <- in_win(ret_ii(bnd[[a]]$date, bnd[[a]]$close, vx, a))
  t4a[[length(t4a) + 1]] <- cmp_ret(tr$A[[a]], bin24, "CMC relabelled (V7 base)", "Binance daily (24:00 UTC)", a)
  t4a[[length(t4a) + 1]] <- cmp_ret(tr$A[[a]], tr$B[[a]], "CMC relabelled (V7 base)", "Coinbase 16:00 ET", a)
  t4a[[length(t4a) + 1]] <- cmp_ret(tr$B[[a]], in_win(ret_ii(bn16[[a]]$date, bn16[[a]]$close_16et, vx, a)), "Coinbase 16:00 ET", "Binance 16:00 ET (1h)", a)
  t4a[[length(t4a) + 1]] <- cmp_ret(tr$A[[a]], in_win(ret_ii(bn16[[a]]$date, bn16[[a]]$close_16et, vx, a)), "CMC relabelled (V7 base)", "Binance 16:00 ET (1h)", a)
}
for (a in CONTROLES) { b16 <- cr$B[[a]]
  t4a[[length(t4a) + 1]] <- cmp_ret(cr$A[[a]], b16, "Binance daily 24:00 UTC (reference)", "Binance 16:00 ET (1h)", a)
  if (!is.null(cbc16[[a]])) t4a[[length(t4a) + 1]] <- cmp_ret(in_win(ret_ii(bn16[[a]]$date, bn16[[a]]$close_16et, vx, a)), in_win(ret_ii(cbc16[[a]]$date, cbc16[[a]]$close_16et, vx, a)), "Binance 16:00 ET (1h)", "Coinbase 16:00 ET", a) }
t4a <- do.call(rbind, t4a); print(t4a)
write.csv(t4a, file.path(OUT7, "csv", "V7-6.4a_series_comparison_full.csv"), row.names = FALSE)
write.csv(fallback, file.path(OUT7, "csv", "V7-6.4a_fallback_counts.csv"), row.names = FALSE)

## ---- 4c ------------------------------------------------------------------
res <- list()
for (cb in c("A","B")) {
  pairs <- lapply(c(BTC = "BTC", ETH = "ETH"), function(a) { Z <- tr[[cb]][[a]]; cut <- CUTS[[tolower(a)]]; list(pre = coredata(Z[index(Z) < cut]), post = coredata(Z[index(Z) >= cut])) })
  nn <- sapply(pairs, function(p) c(n_pre = nrow(p$pre), n_post = nrow(p$post)))
  pt <- point_sep(pairs)
  tst <- run_sep_boot(pairs, B_TEST, SEED); if (min(sum(tst$BTC$valid), sum(tst$ETH$valid)) < 19) stop(paste("4c", cb, "teste abaixo de 95%"))
  bt <- run_sep_boot(pairs, B_FULL, SEED); tab <- tab_sep(pt, bt)
  sis <- list()
  for (cn in c("btc","eth")) { S <- c(list(TREATED = to_list(tr[[cb]][[toupper(cn)]])), lapply(cr[[cb]], to_list)); todas <- sort(unique(do.call(c, lapply(S, `[[`, "date"))))
    sis[[cn]] <- list(series = S, taus = c(0.50, 0.95), baskets = CESTAS, treated = "TREATED", master_pre = todas[todas < CUTS[[cn]]], master_post = todas[todas >= CUTS[[cn]]]) }
  sis <- point_joint(sis)
  tstj <- run_joint_boot(sis, B_TEST, SEED); btj <- run_joint_boot(sis, B_FULL, SEED); tabc <- tab_joint(sis, btj)
  log_v7(sprintf("4c %s: n BTC %d/%d, ETH %d/%d; Tabela 1 validas (teste %d/%d) BTC %d ETH %d; contraf. validas (teste %d/%d) BTC %d ETH %d; timeouts %d/%d",
                 cb, nn[1,1], nn[2,1], nn[1,2], nn[2,2], sum(tst$BTC$valid), sum(tst$ETH$valid), sum(bt$BTC$valid), sum(bt$ETH$valid),
                 sum(tstj$btc$valid), sum(tstj$eth$valid), sum(btj$btc$valid), sum(btj$eth$valid), attr(bt, "n_timeout"), attr(btj, "n_timeout")))
  res[[cb]] <- list(nn = nn, pt = pt, bt = bt, tab = tab, point_cf = lapply(sis, `[[`, "point"), btj = btj, tabc = tabc)
  saveRDS(res, file.path(OUT7, "rds", "v7_6_4.rds"))
  progresso(sprintf("V7-3 Tarefa 4: combinacao %s concluida.", cb))
}
npdc4 <- do.call(rbind, lapply(names(res), function(cb) do.call(rbind, lapply(c("BTC","ETH"), function(a)
  data.frame(combination = cb, pair = a, tau = TAUS, NPDC_pre = as.numeric(res[[cb]]$pt[[a]]$pre), NPDC_post = as.numeric(res[[cb]]$pt[[a]]$post))))))
write.csv(npdc4, file.path(OUT7, "csv", "V7-6.4b_npdc_by_quantile_combos.csv"), row.names = FALSE)
write.csv(do.call(rbind, lapply(names(res), function(cb) cbind(combination = cb, res[[cb]]$tab))), file.path(OUT7, "csv", "V7-6.4b_table1_combos.csv"), row.names = FALSE)
write.csv(do.call(rbind, lapply(names(res), function(cb) cbind(combination = cb, res[[cb]]$tabc))), file.path(OUT7, "csv", "V7-6.4b_counterfactual_combos.csv"), row.names = FALSE)
saveRDS(c(res, list(t4a = t4a, fallback = fallback)), file.path(OUT7, "rds", "v7_6_4.rds"))
progresso("V7-3 Tarefa 4 (A e B) concluida.")
log_v7(sprintf("v7_28 (Tarefa 4) concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
