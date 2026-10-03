# =====================================================================
# V7-3 / Prompt 4, TAREFA 3 — contrafactual ortogonalizado ao BTC e cesta
# B' (SOL) na base nova (copia parametrizada de v6_06_tarefa3.R).
#  3a r_c = a + b0 r_BTC,t + b1 r_BTC,t-1 + e (MQO no pre do corte;
#     robustez: amostra inteira). Regressor principal mantido como na V6:
#     BTC da Binance (24:00 UTC); variante "literal": BTC da base (agora a
#     CMC re-rotulada, no MESMO relogio dos controles).
#  3b ETH (base) ortogonalizado ao BTC (base) no corte do ETH.
#  3c datas de listagem: reutilizadas da V6 (V6-3c, com fontes), sem nova
#     coleta; B' = B + SOL (trunc. 27/10/2025), B'' (trunc. 01/07/2025).
# Semente 20260930 + 3.
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
t0 <- Sys.time(); log_pkgs7("v7_24_p4_tarefa3.R")
K <- 3; SEED <- SEED_BASE_V6 + K
D <- carregar_v4(); vx <- ler_vix()
ctrl <- lapply(setNames(CONTROLES, CONTROLES), serie_ctrl_xts, vix_xts = vx)
ctrl$SOL <- serie_ctrl_xts("SOL", vx, arq = "data/clean/cesta_sol_binance.csv", trunca = "2025-10-27")
ctrl$SOLssk <- serie_ctrl_xts("SOL", vx, arq = "data/clean/cesta_sol_binance.csv", trunca = "2025-07-01"); colnames(ctrl$SOLssk) <- c("SOLssk","VIX")
bb <- read.csv("data/clean/btc_binance.csv"); bb$date <- as.Date(bb$date)
rb_bin <- ret_ii(bb$date, bb$close, vx, "BTC")[, "BTC"]
rb_cmc <- D$base_xts[, "BTC"]; rb_cmc <- rb_cmc[!is.na(rb_cmc)]
mk_reg <- function(r) { z <- merge(r, lag(r, 1)); colnames(z) <- c("b0","b1"); z }
REG <- list(bin = mk_reg(rb_bin), cmc = mk_reg(rb_cmc))
ortog <- function(Z, nome, reg, cut, sample) {
  Z <- as.xts(Z); y <- Z[, 1]; m <- merge(y, reg, join = "left"); colnames(m) <- c("y","b0","b1"); m <- na.omit(m)
  d <- as.Date(index(m)); fitset <- if (sample == "pre") d < cut else rep(TRUE, length(d))
  f <- lm(y ~ b0 + b1, data = as.data.frame(coredata(m))[fitset, ])
  e <- coredata(m$y) - (coef(f)[1] + coef(f)[2] * coredata(m$b0) + coef(f)[3] * coredata(m$b1)); ex <- xts(as.numeric(e), order.by = d)
  out <- merge(ex, vx[d], join = "left"); colnames(out) <- c(nome, "VIX")
  r2 <- function(s) 1 - sum(e[s]^2) / sum((coredata(m$y)[s] - mean(coredata(m$y)[s]))^2)
  list(Z = na.omit(out), info = data.frame(series = nome, fit_sample = sample, n_fit = sum(fitset), alpha = coef(f)[1], beta0 = coef(f)[2], beta1 = coef(f)[3],
       R2_pre = r2(d < cut), R2_post = r2(d >= cut), n_total = length(d)))
}
sistemas <- list(); infos <- list()
for (cn in c("btc","eth")) {
  cut <- CUTS[[cn]]; Zt <- D$amostras[[cn]]$total
  S <- c(list(TREATED = to_list(Zt)), lapply(ctrl, to_list))
  bk <- list(A = CESTAS$A, B = CESTAS$B, Bp = c(CESTAS$B, "SOL"), Bpp = c(CESTAS$B, "SOLssk"))
  for (rg in c("bin","cmc")) for (smp in c("pre","full")) { suf <- paste0("_o_", rg, "_", smp)
    for (a in CONTROLES) { o <- ortog(ctrl[[a]], paste0(a, suf), REG[[rg]], cut, smp); S[[paste0(a, suf)]] <- to_list(o$Z); infos[[length(infos) + 1]] <- cbind(cut = toupper(cn), regressor = rg, o$info) }
    bk[[paste0("A", suf)]] <- paste0(CESTAS$A, suf); bk[[paste0("B", suf)]] <- paste0(CESTAS$B, suf) }
  treated <- "TREATED"
  if (cn == "eth") for (smp in c("pre","full")) { o <- ortog(Zt, paste0("ETH_o_cmc_", smp), REG$cmc, cut, smp)
    S[[paste0("ETH_o_cmc_", smp)]] <- to_list(o$Z); treated <- c(treated, paste0("ETH_o_cmc_", smp)); infos[[length(infos) + 1]] <- cbind(cut = "ETH", regressor = "cmc", o$info) }
  todas <- sort(unique(do.call(c, lapply(S, `[[`, "date"))))
  sistemas[[cn]] <- list(series = S, taus = TAUS_CF, baskets = bk, treated = treated, master_pre = todas[todas < cut], master_post = todas[todas >= cut])
}
info <- do.call(rbind, infos); rownames(info) <- NULL; print(info)
cobert <- do.call(rbind, lapply(names(sistemas), function(cn) do.call(rbind, lapply(names(sistemas[[cn]]$series), function(nm) { d <- sistemas[[cn]]$series[[nm]]$date
  data.frame(cut = toupper(cn), series = nm, first = min(d), last = max(d), n_pre = sum(d < CUTS[[cn]]), n_post = sum(d >= CUTS[[cn]])) }))))
sistemas <- point_joint(sistemas)
R1 <- readRDS(file.path(OUT7, "rds", "v7_6_1a.rds"))
chk <- do.call(rbind, lapply(c("btc","eth"), function(cn) { nm <- intersect(names(R1$sistemas_point[[cn]]), names(sistemas[[cn]]$point))
  data.frame(cut = cn, stat = nm, d = abs(R1$sistemas_point[[cn]][nm] - sistemas[[cn]]$point[nm])) }))
log_v7(sprintf("3: estatisticas brutas coincidem com a V7-6.1a: %d comparadas, dif. max %.2e", nrow(chk), max(chk$d)))
tt <- Sys.time(); tst <- run_joint_boot(sistemas, B_TEST, SEED, timeout = 400)
log_v7(sprintf("3 teste B=20: validas BTC %d/20, ETH %d/20 (%.0f s)", sum(tst$btc$valid), sum(tst$eth$valid), as.numeric(Sys.time() - tt, units = "secs")))
if (min(sum(tst$btc$valid), sum(tst$eth$valid)) < 19) stop("3 teste abaixo de 95%")
tt <- Sys.time(); bt3 <- run_joint_boot(sistemas, B_FULL, SEED, timeout = 400)
log_v7(sprintf("3 B=500: validas BTC %d, ETH %d; timeouts %d (%.1f min)", sum(bt3$btc$valid), sum(bt3$eth$valid), attr(bt3, "n_timeout"), as.numeric(Sys.time() - tt, units = "mins")))
tab3 <- tab_joint(sistemas, bt3)
write.csv(tab3, file.path(OUT7, "csv", "V7-6.3_counterfactual_orth_all.csv"), row.names = FALSE)
write.csv(info, file.path(OUT7, "csv", "V7-6.3a_orthogonalization_betas.csv"), row.names = FALSE)
write.csv(cobert, file.path(OUT7, "csv", "V7-6.3_coverage.csv"), row.names = FALSE)
file.copy("data/clean/etf_listing_dates.csv", file.path(OUT7, "csv", "V7-6.3c_listing_dates.csv"), overwrite = TRUE)
saveRDS(list(point = lapply(sistemas, `[[`, "point"), bt = bt3, tab = tab3, info = info, cobert = cobert), file.path(OUT7, "rds", "v7_6_3.rds"))
progresso("V7-3 Tarefa 3 (ortogonalizacao, SOL) concluida.")
log_v7(sprintf("v7_24 (Tarefa 3) concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
