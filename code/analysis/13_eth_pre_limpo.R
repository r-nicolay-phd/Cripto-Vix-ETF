# =====================================================================
# V7-3 / Prompt 4, TAREFA 5 — pre-periodo do ETH sem contaminacao pelo ETF
# de BTC, base nova (copia parametrizada de v6_07_tarefa5.R).
#  pre = ate 2024-01-10; pos = desde 2024-07-23; 11/01-22/07/2024 fora.
#  (i) Delta/DD em todos os quantis (bootstrap separado, V5-4);
#  (ii) contrafactual no corte ETH com o mesmo pre (bootstrap conjunto);
#  (iii) mediana: janela 11/01-22/07/2024 vs pre. Semente 20260930 + 5.
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
t0 <- Sys.time(); log_pkgs7("v7_25_p4_tarefa5.R")
K <- 5; SEED <- SEED_BASE_V6 + K
D <- carregar_v4(); vx <- ler_vix()
PRE_END <- as.Date("2024-01-10"); POST_INI <- CUTS$eth
Zt <- D$amostras$eth$total; dz <- as.Date(index(Zt))
pairs <- list(ETH_pre2024_01_10 = list(pre = coredata(Zt[dz <= PRE_END]), post = coredata(Zt[dz >= POST_INI])))
pairs_w <- list(ETH_window_vs_pre = list(pre = coredata(Zt[dz <= PRE_END]), post = coredata(Zt[dz > PRE_END & dz < POST_INI])))
nn <- data.frame(sample = c("pre (to 2024-01-10)","window 2024-01-11..2024-07-22","post (from 2024-07-23)"),
                 n = c(nrow(pairs[[1]]$pre), nrow(pairs_w[[1]]$post), nrow(pairs[[1]]$post))); print(nn)
pt <- point_sep(pairs); ptw <- point_sep(pairs_w, taus = 0.50)
tst <- run_sep_boot(pairs, B_TEST, SEED); log_v7(sprintf("5 teste B=20: validas %d/20", sum(tst[[1]]$valid)))
bt <- run_sep_boot(pairs, B_FULL, SEED); log_v7(sprintf("5 B=500: validas %d; timeouts %d", sum(bt[[1]]$valid), attr(bt, "n_timeout")))
tab5 <- tab_sep(pt, bt)
btw <- run_sep_boot(pairs_w, B_FULL, SEED, taus = 0.50); log_v7(sprintf("5 janela: validas %d; timeouts %d", sum(btw[[1]]$valid), attr(btw, "n_timeout")))
tab5w <- tab_sep(ptw, btw, taus = 0.50)
lev <- data.frame(sample = c("pre","window","post"), NPDC_0.50 = c(pt[[1]]$pre["0.50"], ptw[[1]]$post["0.50"], pt[[1]]$post["0.50"])); print(lev)
ctrl <- lapply(setNames(CONTROLES, CONTROLES), serie_ctrl_xts, vix_xts = vx)
S <- c(list(TREATED = to_list(Zt)), lapply(ctrl, to_list)); todas <- sort(unique(do.call(c, lapply(S, `[[`, "date"))))
sis <- list(eth = list(series = S, taus = TAUS_CF, baskets = CESTAS, treated = "TREATED", master_pre = todas[todas <= PRE_END], master_post = todas[todas >= POST_INI]))
sis <- point_joint(sis)
chk <- abs(sis$eth$point[paste0("TREATED_Delta_", sprintf("%.2f", TAUS_CF))] - pt[[1]]$stats[paste0("Delta_", sprintf("%.2f", TAUS_CF))])
log_v7(sprintf("5: tratado no sistema conjunto = bootstrap separado (pontual): dif. max %.2e", max(chk)))
tstj <- run_joint_boot(sis, B_TEST, SEED); log_v7(sprintf("5 contrafactual teste B=20: %d/20", sum(tstj$eth$valid)))
btj <- run_joint_boot(sis, B_FULL, SEED); log_v7(sprintf("5 contrafactual B=500: %d validas; timeouts %d", sum(btj$eth$valid), attr(btj, "n_timeout")))
tab5c <- tab_joint(sis, btj)
print(tab5[, c("statistic","point","ci95_lo","ci95_hi","ci95_excludes_0")]); print(tab5w)
print(tab5c[grepl("TREATED|Basket", tab5c$statistic), c("statistic","point","ci95_lo","ci95_hi","ci95_excludes_0")])
write.csv(tab5, file.path(OUT7, "csv", "V7-6.5_eth_clean_pre_all.csv"), row.names = FALSE)
write.csv(tab5w, file.path(OUT7, "csv", "V7-6.5_eth_window_vs_pre.csv"), row.names = FALSE)
write.csv(tab5c, file.path(OUT7, "csv", "V7-6.5_eth_clean_pre_counterfactual.csv"), row.names = FALSE)
write.csv(nn, file.path(OUT7, "csv", "V7-6.5_sample_sizes.csv"), row.names = FALSE)
saveRDS(list(pt = pt, bt = bt, tab5 = tab5, ptw = ptw, btw = btw, tab5w = tab5w, lev = lev, point_cf = sis$eth$point, btj = btj, tab5c = tab5c, nn = nn), file.path(OUT7, "rds", "v7_6_5.rds"))
progresso("V7-3 Tarefa 5 (ETH pre limpo) concluida.")
log_v7(sprintf("v7_25 (Tarefa 5) concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
