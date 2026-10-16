# =====================================================================
# V10 — 02: Tabela 1 com o esquema principal (MBB de linhas do desenho,
# bloco 20, B = 500; semente 20261016 + 1), robustez A (estacionario de
# linhas, bloco medio 20; + 2), robustez B (sieve VAR(2); + 3), BC/BCa
# com B = 2.000 (+ 4; aceleracao por jackknife de blocos de 20 linhas do
# desenho), diagnosticos de validade (i)-(iii) de cada esquema.
# As replicas do esquema principal tambem fornecem os componentes
# theta (item 7), H = 5 (Decisao 4) e H = 1 (variante (c)).
# Origem: v10_02_tabela1.R (inalterado salvo caminhos). Usados: Tabela 1
# (coluna Baseline, via V10-9 de 32_*), Tabelas S7, S8, S9 (Painel D),
# S14 (Painel B, variante c), S15 (Painel A), S30.
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_v10.R"))
t0 <- Sys.time(); log_pkgs10("v10_02_tabela1.R")
PAIRS <- readRDS(RDS10("pairs_main"))
pt <- point_sep10(PAIRS)

run_block <- function(nm, scheme, k, B = B_FULL) run_or_load10(nm, {
  tt <- Sys.time(); seed <- SEED_V10 + k
  tst <- run_sep_boot10(PAIRS, B_TEST, seed, scheme = scheme); chk_test10(tst, paste(nm, scheme))
  bt <- run_sep_boot10(PAIRS, B, seed, scheme = scheme)
  log_v10(sprintf("%s (%s) B=%d: validas BTC %d, ETH %d; timeouts %d (%.1f min)", nm, scheme, B, sum(bt$BTC$valid), sum(bt$ETH$valid),
                  attr(bt, "n_timeout"), as.numeric(Sys.time() - tt, units = "mins")))
  list(bt = bt, test = tst, seed = seed, scheme = scheme)
})
summ_tab <- function(R) add_btc_eth10(tab_sep10(pt, R$bt), R$bt, pt)

## ---------------- esquema principal --------------------------------------
MB <- run_block("t1_mbb", "MBB", 1)
progresso(sprintf("02 Tabela 1, esquema principal (MBB linhas do desenho, B=500, semente %d): validas BTC %d, ETH %d.", MB$seed, sum(MB$bt$BTC$valid), sum(MB$bt$ETH$valid)))
SB <- run_block("t1_sb", "SB", 2)
progresso(sprintf("02 Tabela 1, estacionario (B=500, semente %d): validas BTC %d, ETH %d.", SB$seed, sum(SB$bt$BTC$valid), sum(SB$bt$ETH$valid)))
SV <- run_block("t1_sieve", "SIEVE", 3)
progresso(sprintf("02 Tabela 1, sieve VAR(2) (B=500, semente %d): validas BTC %d, ETH %d.", SV$seed, sum(SV$bt$BTC$valid), sum(SV$bt$ETH$valid)))

TAB <- list(MBB = summ_tab(MB), SB = summ_tab(SB), SIEVE = summ_tab(SV))
for (s in names(TAB)) write.csv(TAB[[s]], file.path(OUT10, "csv", sprintf("V10-1_table1_%s_raw.csv", s)), row.names = FALSE)

## ---------------- diagnosticos de validade -------------------------------
diag_scheme <- function(R, label) do.call(rbind, lapply(names(PAIRS), function(nm) do.call(rbind, lapply(c("pre","post"), function(s) {
  rg <- R$bt[[nm]]$range; mn <- rg[, paste0(s, "_min")]; mx <- rg[, paste0(s, "_max")]
  o <- PAIRS[[nm]][[s]]$M[, "VIX"]; inr <- mn >= 0.5 * min(o) & mx <= 2 * max(o)
  sr <- splice_ratio(R$bt[[nm]]$splice[[s]]); vs <- mean(R$bt[[nm]]$valid)
  data.frame(scheme = label, pair = nm, sample = s, B = length(R$bt[[nm]]$valid), obs_min = min(o), obs_max = max(o),
             VIXstar_min = min(mn, na.rm = TRUE), VIXstar_max = max(mx, na.rm = TRUE), share_reps_in_range = mean(inr, na.rm = TRUE),
             pass_i = mean(inr, na.rm = TRUE) >= 0.95, t(sr), pass_ii = sr[["ratio_orig"]] <= 1.5, share_valid = vs, pass_iii = vs >= 0.95)
}))))
DG <- rbind(diag_scheme(MB, "Design-row MBB (principal)"), diag_scheme(SB, "Design-row stationary"), diag_scheme(SV, "Sieve VAR(2)"))
DG$approved <- DG$pass_i & DG$pass_ii & DG$pass_iii
ref <- read.csv(file.path(OUT10, "csv", "V10-0d_validity_reference_schemes.csv"))
ref$scheme <- ifelse(ref$scheme == "V7", "Concatenated level blocks (previous)", "Rebuilt from log changes (pre-registered, previous)")
ref$share_valid <- 1; ref$pass_iii <- TRUE; ref$approved <- ref$pass_i & ref$pass_ii
sv <- read.csv(file.path(OUT10, "csv", "V10-0c_sieve_VAR_roots.csv"))
DGall <- rbind(ref[, names(DG)], DG)
DGall$sieve_max_modulus <- ifelse(DGall$scheme == "Sieve VAR(2)", sv$max_modulus_mean_VAR[match(paste(DGall$pair, DGall$sample), paste(sv$pair, sv$sample))], NA)
print(DGall)
write.csv(DGall, file.path(OUT10, "csv", "V10-1_validity_diagnostics.csv"), row.names = FALSE)
appr <- tapply(DG$approved, DG$scheme, all)
progresso(sprintf("02 diagnosticos: %s.", paste(names(appr), ifelse(appr, "APROVADO", "REPROVADO"), collapse = "; ")))

## ---------------- jackknife (linhas do desenho) e BC/BCa ------------------
JK <- run_or_load10("t1_jackknife", {
  tasks <- list()
  for (nm in names(PAIRS)) for (s in c("pre","post")) { m <- nrow(PAIRS[[nm]][[s]]$src); bl <- split(seq_len(m), ceiling(seq_len(m) / BLOCK))
    for (i in seq_along(bl)) tasks[[length(tasks) + 1]] <- list(nm = nm, s = s, i = i, rows = bl[[i]]) }
  jk_task10 <- function(tk) { Dd <- get(".PAIRS_V10", envir = globalenv())[[tk$nm]][[tk$s]]; keep <- setdiff(seq_len(nrow(Dd$src)), tk$rows)
    YX <- build_YX(Dd, keep); v <- npdc_YX(YX$Y, YX$X, TAUS, 10); list(v = as.numeric(v["NPDC", , 1]), err = attr(v, "erro")) }
  assign(".PAIRS_V10", PAIRS, envir = globalenv())
  jr <- par_map_timeout(tasks, jk_task10, n_workers = N_WORKERS, timeout = 120, export_names = ".PAIRS_V10", export_env = globalenv())
  log_v10(sprintf("02 jackknife linhas do desenho: %d ajustes, falhas %d", length(jr), sum(sapply(jr, function(x) is.null(x$v) || any(!is.finite(x$v))))))
  k7 <- sprintf("%.2f", TAUS); jk <- list()
  for (nm in names(PAIRS)) for (s in c("pre","post")) { ii <- which(sapply(tasks, function(t) t$nm == nm && t$s == s))
    jk[[nm]][[s]] <- do.call(rbind, lapply(jr[ii], function(x) setNames(x$v, k7))) }
  jk
})
BC <- run_or_load10("t1_bca", { tt <- Sys.time(); seed <- SEED_V10 + 4
  tst <- run_sep_boot10(PAIRS, B_TEST, seed, scheme = "MBB", nfores = 10); chk_test10(tst, "BCa MBB")
  bt2 <- run_sep_boot10(PAIRS, B_BCA, seed, scheme = "MBB", nfores = 10)
  log_v10(sprintf("02 BCa MBB B=2000: validas BTC %d, ETH %d; timeouts %d (%.1f min)", sum(bt2$BTC$valid), sum(bt2$ETH$valid), attr(bt2, "n_timeout"), as.numeric(Sys.time() - tt, units = "mins")))
  list(bt2 = bt2, seed = seed) })
th <- lapply(pt, `[[`, "stats"); rows <- list()
jkst <- lapply(setNames(names(PAIRS), names(PAIRS)), function(nm) list(pre = t(apply(JK[[nm]]$pre, 1, function(v) stats_gen(v, pt[[nm]]$post, TAUS))),
                                                                     post = t(apply(JK[[nm]]$post, 1, function(v) stats_gen(pt[[nm]]$pre, v, TAUS)))))
SS <- lapply(BC$bt2, boot_stats)
for (nm in names(PAIRS)) for (st in names(th[[nm]])) { a <- accel(list(jkst[[nm]]$pre[, st], jkst[[nm]]$post[, st]))
  rows[[length(rows) + 1]] <- data.frame(pair = nm, statistic = st, point = unname(th[[nm]][st]), boot_median = median(SS[[nm]][, st], na.rm = TRUE), t(ints(th[[nm]][[st]], SS[[nm]][, st], a))) }
for (st in c(paste0("Delta_", sprintf("%.2f", TAUS)), "DD_0.95")) { pnt <- th$BTC[[st]] - th$ETH[[st]]
  a <- accel(list(jkst$BTC$pre[, st] - th$ETH[[st]], jkst$BTC$post[, st] - th$ETH[[st]], th$BTC[[st]] - jkst$ETH$pre[, st], th$BTC[[st]] - jkst$ETH$post[, st]))
  rows[[length(rows) + 1]] <- data.frame(pair = "BTC-ETH", statistic = st, point = pnt, boot_median = median(SS$BTC[, st] - SS$ETH[, st], na.rm = TRUE), t(ints(pnt, SS$BTC[, st] - SS$ETH[, st], a))) }
BCT <- do.call(rbind, rows); rownames(BCT) <- NULL
BCT$pct_excl0 <- sig_flag(BCT$pct_lo, BCT$pct_hi); BCT$bc_excl0 <- sig_flag(BCT$bc_lo, BCT$bc_hi); BCT$bca_excl0 <- sig_flag(BCT$bca_lo, BCT$bca_hi)
BCT$bca_order_below_1_over_B <- pmin(BCT$bca_order_lo, 1 - BCT$bca_order_hi) < 1 / BCT$n_valid
BCT$bc_order_below_1_over_B <- pmin(BCT$bc_order_lo, 1 - BCT$bc_order_hi) < 1 / BCT$n_valid
print(BCT[, c("pair","statistic","point","z0","a","pct_lo","pct_hi","bca_lo","bca_hi","bca_order_lo","bca_order_hi","bca_excl0","bca_order_below_1_over_B")])
write.csv(BCT, file.path(OUT10, "csv", "V10-1_BC_BCa_B2000_raw.csv"), row.names = FALSE)
progresso(sprintf("02 BC/BCa B=2000 (semente %d) concluido; validas BTC %d, ETH %d.", BC$seed, sum(BC$bt2$BTC$valid), sum(BC$bt2$ETH$valid)))

## ---------------- componentes (item 7) e H = 5 / H = 1 --------------------
comp <- list()
for (nm in c("BTC","ETH")) for (tk in c("0.50","0.95")) for (cp in c("a_from_VIX","VIX_from_a")) {
  sfx <- if (cp == "a_from_VIX") "aV" else "Va"; bt <- MB$bt[[nm]]; v <- bt$valid
  ppre <- pt[[nm]]$pre_full[cp, tk]; ppost <- pt[[nm]]$post_full[cp, tk]
  bpre <- bt[[paste0("pre_", sfx)]][, tk]; bpost <- bt[[paste0("post_", sfx)]][, tk]; bpre[!v] <- NA; bpost[!v] <- NA
  lab <- if (cp == "a_from_VIX") sprintf("theta(%s <- VIX)", nm) else sprintf("theta(VIX <- %s)", nm)
  comp[[length(comp) + 1]] <- rbind(resumir_v6(ppre, bpre, pair = nm, tau = as.numeric(tk), component = lab, quantity = "Pre"),
                                    resumir_v6(ppost, bpost, pair = nm, tau = as.numeric(tk), component = lab, quantity = "Post"),
                                    resumir_v6(ppost - ppre, bpost - bpre, pair = nm, tau = as.numeric(tk), component = lab, quantity = "Delta (post - pre)")) }
COMP <- do.call(rbind, comp); rownames(COMP) <- NULL
write.csv(COMP, file.path(OUT10, "csv", "V10-7_components_raw.csv"), row.names = FALSE)

h5 <- list()
for (nm in c("BTC","ETH")) { b5 <- pick_H(MB$bt[[nm]], "H5"); p5 <- pick_H(pt[[nm]], "H5"); v <- b5$valid
  for (tk in c("0.90","0.95")) for (s in c("pre","post")) { x <- b5[[s]][, tk]; x[!v] <- NA
    h5[[length(h5) + 1]] <- resumir_v6(unname(p5[[s]][tk]), x, pair = nm, H = 5, statistic = sprintf("NPDC_%s(%s)", s, tk)) }
  S5 <- boot_stats(b5, TAUS)
  for (st in c("Delta_0.90","Delta_0.95","DD_0.95")) h5[[length(h5) + 1]] <- resumir_v6(unname(p5$stats[st]), S5[, st], pair = nm, H = 5, statistic = st) }
H5 <- do.call(rbind, h5); rownames(H5) <- NULL
write.csv(H5, file.path(OUT10, "csv", "V10-10_H5_table1_part_raw.csv"), row.names = FALSE)
H1 <- tab_sep10(pt, MB$bt, h = "H1"); write.csv(H1, file.path(OUT10, "csv", "V10-8_variant_c_H1_raw.csv"), row.names = FALSE)

saveRDS(list(pt = pt, TAB = TAB, DG = DGall, BCT = BCT, COMP = COMP, H5 = H5, H1 = H1, seeds = c(MBB = MB$seed, SB = SB$seed, SIEVE = SV$seed, BCA = BC$seed)), RDS10("t1_summary"))

## ---------------- Regra B -------------------------------------------------
principal_ok <- all(DG$approved[DG$scheme == "Design-row MBB (principal)"])
rb <- TAB$MBB[TAB$MBB$statistic == "Delta_0.95" & TAB$MBB$series %in% c("BTC","ETH"), c("series","point","boot_median","ci95_lo","ci95_hi","n_valid","ci95_excludes_0")]
rb$principal_scheme_approved <- principal_ok
rb$verdict <- if (!principal_ok) "Not evaluable (principal scheme failed the validity diagnostics)" else
  ifelse(rb$ci95_excludes_0, "CI excludes zero: 'significant deepening in the tail, concentrated in episodes and indistinguishable from controls'", "CI includes zero: keep fragility wording")
for (s in c("SB","SIEVE")) { x <- TAB[[s]][TAB[[s]]$statistic == "Delta_0.95" & TAB[[s]]$series %in% c("BTC","ETH"), ]
  rb[[paste0(s, "_ci95_lo")]] <- x$ci95_lo; rb[[paste0(s, "_ci95_hi")]] <- x$ci95_hi; rb[[paste0(s, "_excludes_0")]] <- x$ci95_excludes_0 }
print(rb); write.csv(rb, file.path(OUT10, "csv", "V10-RuleB_verdict.csv"), row.names = FALSE)
progresso(sprintf("02 Regra B (esquema principal %s): BTC D(0.95) %.1f [%.1f; %.1f]; ETH %.1f [%.1f; %.1f] -> %s / %s.", ifelse(principal_ok, "aprovado", "reprovado"),
  rb$point[1], rb$ci95_lo[1], rb$ci95_hi[1], rb$point[2], rb$ci95_lo[2], rb$ci95_hi[2], rb$verdict[1], rb$verdict[2]))
log_v10(sprintf("v10_02 concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
