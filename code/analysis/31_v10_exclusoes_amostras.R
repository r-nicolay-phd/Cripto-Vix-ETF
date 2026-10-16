# =====================================================================
# V10 — 04: exclusoes (regra das lacunas da V9: as p primeiras observacoes
# apos cada lacuna saem como dependentes; as linhas do desenho sao
# montadas dentro de cada segmento e o MBB reamostra blocos de 20 linhas
# consecutivas do conjunto de linhas retidas), pre do ETH ate 10/01/2024
# (Tabela 1 e contrafactual) e regime desde 17/03/2023 (tau 0.25-0.75).
# Sementes: exclusoes 20261016 + 7; pre limpo e regime 20261016 + 8.
# Origem: v10_04_exclusoes_amostras.R. Alteracoes: caminhos; removidas as
# conferencias dos pontuais com os arquivos da V9 (cmp_pts), que so
# validavam contra a versao anterior. Usados: Tabelas S17 (Paineis A e C),
# S31, S32 (Painel A).
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_v10.R"))
t0 <- Sys.time(); log_pkgs10("v10_04_exclusoes_amostras.R")
D <- carregar_v4(); vx <- ler_vix()
SEEDX <- SEED_V10 + 7; SEEDR <- SEED_V10 + 8

mk_excl10 <- function(cn, wins, nlag = NLAG) {
  Zt <- as.xts(D$amostras[[cn]]$total); fd <- as.Date(index(Zt)); cut <- CUTS[[cn]]; Z <- Zt
  for (w in wins) Z <- Z[!(index(Z) >= w[1] & index(Z) <= w[2])]
  dz <- as.Date(index(Z)); sp <- dz < cut; sq <- dz >= cut
  segp <- mk_seg(dz[sp], fd[fd < cut]); segq <- mk_seg(dz[sq], fd[fd >= cut])
  P <- prep_pair(D_from_xts(Z[sp], if (length(unique(segp)) > 1) segp else NULL, nlag), D_from_xts(Z[sq], if (length(unique(segq)) > 1) segq else NULL, nlag))
  P$info <- data.frame(obs_removed_by_window = nrow(Zt) - nrow(Z), n_pre = sum(sp), n_post = sum(sq), gaps_pre = length(unique(segp)) - 1, gaps_post = length(unique(segq)) - 1,
                       design_rows_pre = nrow(P$pre$src), design_rows_post = nrow(P$post$src))
  P$info$obs_dropped_gap_rule <- nlag * (P$info$gaps_pre + P$info$gaps_post)
  P
}
boot_block <- function(pairs, seed, rot, taus = TAUS) {
  tst <- run_sep_boot10(pairs, B_TEST, seed, taus = taus, nfores = 10); chk_test10(tst, rot)
  tt <- Sys.time(); bt <- run_sep_boot10(pairs, B_FULL, seed, taus = taus, nfores = 10)
  log_v10(sprintf("%s B=500: %s; timeouts %d (%.1f min)", rot, paste(names(bt), sapply(bt, function(x) sum(x$valid)), collapse = "; "), attr(bt, "n_timeout"), as.numeric(Sys.time() - tt, units = "mins")))
  bt
}

## ---------------- 22/07-16/08/2024 -------------------------------------------
EX <- run_or_load10("excl_aug2024", {
  pr <- list(BTC = mk_excl10("btc", list(JANELA_AGO24)), ETH = mk_excl10("eth", list(JANELA_AGO24)))
  ptx <- point_sep10(pr, nfores = 10)
  bt <- boot_block(pr, SEEDX, "04 exclusao ago/2024")
  list(pt = ptx, bt = bt, tab = tab_sep10(ptx, bt), sizes = cbind(pair = names(pr), do.call(rbind, lapply(pr, `[[`, "info"))))
})
write.csv(EX$tab, file.path(OUT10, "csv", "V10-5a_exclusion_Aug2024_raw.csv"), row.names = FALSE)
write.csv(EX$sizes, file.path(OUT10, "csv", "V10-5a_exclusion_Aug2024_sizes.csv"), row.names = FALSE)
progresso(sprintf("04 exclusao 22/07-16/08/2024 (B=500, semente %d) concluida.", SEEDX))

## ---------------- exclusao curta do ETH ---------------------------------------
W5b <- list("Exclude 1-9 Aug 2024" = list(c(as.Date("2024-08-01"), as.Date("2024-08-09"))),
            "Exclude 2-9 Aug 2024" = list(c(as.Date("2024-08-02"), as.Date("2024-08-09"))))
ES <- run_or_load10("excl_eth_short", {
  out <- list(); sz <- list()
  for (dn in names(W5b)) { pr <- list(ETH = mk_excl10("eth", W5b[[dn]])); ptx <- point_sep10(pr, nfores = 10)
    bt <- boot_block(pr, SEEDX, paste("04", dn)); out[[dn]] <- cbind(design = dn, tab_sep10(ptx, bt)); sz[[dn]] <- cbind(design = dn, pr$ETH$info) }
  list(tab = do.call(rbind, out), sizes = do.call(rbind, sz))
})
write.csv(ES$tab, file.path(OUT10, "csv", "V10-5b_eth_short_exclusion_raw.csv"), row.names = FALSE)
write.csv(ES$sizes, file.path(OUT10, "csv", "V10-5b_sizes.csv"), row.names = FALSE)
progresso("04 exclusao curta do ETH (1-9/08 e 2-9/08/2024) concluida.")

## ---------------- exclusao conjunta (p = 2 bootstrap; p = 1, 3 pontuais) --------
W5c <- list(JANELA_AGO24, c(as.Date("2025-04-08"), as.Date("2025-05-06")))
EJ <- run_or_load10("excl_joint", {
  pts <- do.call(rbind, lapply(1:3, function(p) { pr <- list(BTC = mk_excl10("btc", W5c, p), ETH = mk_excl10("eth", W5c, p)); ptp <- point_sep10(pr, nfores = 10)
    do.call(rbind, lapply(names(ptp), function(nm) data.frame(pair = nm, p = p, statistic = names(ptp[[nm]]$stats), point = unname(ptp[[nm]]$stats),
                                                              obs_dropped_gap_rule = pr[[nm]]$info$obs_dropped_gap_rule))) }))
  pr <- list(BTC = mk_excl10("btc", W5c), ETH = mk_excl10("eth", W5c)); ptx <- point_sep10(pr, nfores = 10)
  bt <- boot_block(pr, SEEDX, "04 exclusao conjunta p=2")
  list(points = pts, tab = tab_sep10(ptx, bt), sizes = cbind(pair = names(pr), do.call(rbind, lapply(pr, `[[`, "info"))))
})
write.csv(EJ$points, file.path(OUT10, "csv", "V10-5c_joint_exclusion_points_raw.csv"), row.names = FALSE)
write.csv(EJ$tab, file.path(OUT10, "csv", "V10-5c_joint_exclusion_boot_p2_raw.csv"), row.names = FALSE)
write.csv(EJ$sizes, file.path(OUT10, "csv", "V10-5c_sizes.csv"), row.names = FALSE)
progresso("04 exclusao conjunta (ago/2024 + abr-mai/2025), p = 2 com bootstrap, concluida.")

## ---------------- pre do ETH ate 10/01/2024 ---------------------------------------
PRE_END <- as.Date("2024-01-10")
EC <- run_or_load10("eth_clean_pre", {
  Zt <- as.xts(D$amostras$eth$total); dz <- as.Date(index(Zt))
  pairs <- list(ETH_pre_to_2024_01_10 = prep_pair(D_from_xts(Zt[dz <= PRE_END]), D_from_xts(Zt[dz >= CUTS$eth])))
  ptc <- point_sep10(pairs, nfores = 10)
  bt <- boot_block(pairs, SEEDR, "04 ETH pre limpo")
  ctrl <- lapply(setNames(CONTROLES, CONTROLES), serie_ctrl_xts, vix_xts = vx)
  ctrl$SOL <- serie_ctrl_xts("SOL", vx, arq = "data/clean/cesta_sol_binance.csv", trunca = "2025-10-27")
  S <- c(list(TREATED = mk_series10(Zt, CUTS$eth, pre_end = PRE_END)), lapply(ctrl, mk_series10, cut = CUTS$eth, pre_end = PRE_END))
  todas <- sort(unique(do.call(c, lapply(S, function(s) c(s$pre$date, s$post$date)))))
  sis <- list(eth = mk_sys10(S, TAUS_CF, list(A = CESTAS$A, B = CESTAS$B, Bp = c(CESTAS$B, "SOL")), "TREATED", todas[todas <= PRE_END], todas[todas >= CUTS$eth]))
  sis <- point_joint10(sis)
  tst <- run_joint_boot10(sis, B_TEST, SEEDR); chk_joint_test(tst, "04 ETH pre limpo contrafactual")
  btj <- run_joint_boot10(sis, B_FULL, SEEDR)
  log_v10(sprintf("04 ETH pre limpo contrafactual B=500: validas %d; timeouts %d", sum(btj$eth$valid), attr(btj, "n_timeout")))
  list(pt = ptc, bt = bt, tab = tab_sep10(ptc, bt), point_cf = sis$eth$point, btj = btj, tab_cf = tab_joint(sis, btj),
       nn = data.frame(n_pre = nrow(pairs[[1]]$pre$M), n_post = nrow(pairs[[1]]$post$M)))
})
write.csv(EC$tab, file.path(OUT10, "csv", "V10-6a_eth_clean_pre_raw.csv"), row.names = FALSE)
write.csv(EC$tab_cf, file.path(OUT10, "csv", "V10-6a_eth_clean_pre_counterfactual_raw.csv"), row.names = FALSE)
progresso(sprintf("04 pre do ETH ate 10/01/2024 (Tabela 1 e contrafactual; semente %d) concluido.", SEEDR))

## ---------------- regime desde 17/03/2023 -------------------------------------------
RG <- run_or_load10("regime", {
  INI <- as.Date("2023-03-17"); TB <- c(0.25, 0.50, 0.75)
  mk <- function(cn, pre_end, post_ini) { Z <- as.xts(D$amostras[[cn]]$total); d <- as.Date(index(Z)); prep_pair(D_from_xts(Z[d >= INI & d <= pre_end]), D_from_xts(Z[d >= post_ini])) }
  pairs <- list(BTC = mk("btc", CUTS$btc - 1, CUTS$btc), ETH = mk("eth", CUTS$eth - 1, CUTS$eth), ETH_pre_to_2024_01_10 = mk("eth", PRE_END, CUTS$eth))
  ptr <- point_sep10(pairs, taus = TB, nfores = 10)
  bt <- boot_block(pairs, SEEDR, "04 regime", taus = TB)
  tab <- tab_sep10(ptr, bt, taus = TB)
  tab$n_pre <- sapply(pairs, function(p) nrow(p$pre$M))[tab$series]; tab$n_post <- sapply(pairs, function(p) nrow(p$post$M))[tab$series]
  list(pt = ptr, bt = bt, tab = tab)
})
write.csv(RG$tab, file.path(OUT10, "csv", "V10-6b_regime_raw.csv"), row.names = FALSE)
progresso(sprintf("04 regime desde 17/03/2023 (semente %d) concluido.", SEEDR))
log_v10(sprintf("v10_04 concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
