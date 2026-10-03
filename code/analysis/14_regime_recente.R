# =====================================================================
# V7-3 / Prompt 4, TAREFA 6 — regime pre-ETF recente (desde 17/03/2023,
# data pre-definida da quebra Bai-Perron da V4; mantida mesmo que a V7-1
# localize a quebra em outra data) na base nova (copia de v6_08_tarefa6.R).
# Bootstrap (V5-4) em tau 0.25/0.50/0.75; 0.05/0.95 so pontual.
# Semente 20260930 + 6.
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
t0 <- Sys.time(); log_pkgs7("v7_26_p4_tarefa6.R")
K <- 6; SEED <- SEED_BASE_V6 + K
D <- carregar_v4(); INI <- as.Date("2023-03-17")
mk <- function(par, pre_end, post_ini) { Z <- D$amostras[[par]]$total; d <- as.Date(index(Z)); list(pre = coredata(Z[d >= INI & d <= pre_end]), post = coredata(Z[d >= post_ini])) }
pairs <- list(BTC = mk("btc", CUTS$btc - 1, CUTS$btc), ETH = mk("eth", CUTS$eth - 1, CUTS$eth), ETH_pre_to_2024_01_10 = mk("eth", as.Date("2024-01-10"), CUTS$eth))
nn <- sapply(pairs, function(p) c(n_pre = nrow(p$pre), n_post = nrow(p$post))); print(nn)
TB <- c(0.25, 0.50, 0.75)
pt_all <- point_sep(pairs, taus = c(0.05, TB, 0.95)); pt <- point_sep(pairs, taus = TB)
tst <- run_sep_boot(pairs, B_TEST, SEED, taus = TB); log_v7(sprintf("6 teste B=20: validas %s", paste(names(tst), sapply(tst, function(x) sum(x$valid)), collapse = "; ")))
bt <- run_sep_boot(pairs, B_FULL, SEED, taus = TB)
fail <- data.frame(series = names(bt), B = B_FULL, n_valid = sapply(bt, function(x) sum(x$valid)), n_errors = sapply(bt, function(x) length(x$errs)), n_timeout_total = attr(bt, "n_timeout"))
fail$failure_rate_pct <- 100 * (1 - fail$n_valid / B_FULL); print(fail)
log_v7(sprintf("6 B=500: %s; timeouts %d", paste(fail$series, fail$n_valid, collapse = "; "), attr(bt, "n_timeout")))
tab6 <- tab_sep(pt, bt, taus = TB)
pts <- do.call(rbind, lapply(names(pt_all), function(nm) data.frame(series = nm, statistic = paste0("Delta_", c("0.05","0.95")),
  point = unname(pt_all[[nm]]$stats[paste0("Delta_", c("0.05","0.95"))]), note = "point estimate only (imprecise)")))
tab6$n_pre <- nn["n_pre", tab6$series]; tab6$n_post <- nn["n_post", tab6$series]
print(tab6[, c("series","statistic","point","boot_median","ci95_lo","ci95_hi","n_valid","ci95_excludes_0","n_pre","n_post")]); print(pts)
write.csv(tab6, file.path(OUT7, "csv", "V7-6.6_recent_regime_boot.csv"), row.names = FALSE)
write.csv(pts, file.path(OUT7, "csv", "V7-6.6_recent_regime_tails_point.csv"), row.names = FALSE)
write.csv(fail, file.path(OUT7, "csv", "V7-6.6_failure_rates.csv"), row.names = FALSE)
saveRDS(list(pt_all = pt_all, pt = pt, bt = bt, tab6 = tab6, pts = pts, fail = fail, nn = nn), file.path(OUT7, "rds", "v7_6_6.rds"))
progresso("V7-3 Tarefa 6 (regime desde 17/03/2023) concluida.")
log_v7(sprintf("v7_26 (Tarefa 6) concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
