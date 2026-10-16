# =====================================================================
# V10 — 01: validacao inicial e diagnosticos de validade dos esquemas de
# referencia (V7: blocos de linhas com o VIX em nivel concatenados; V9:
# blocos de Dlog VIX reintegrados). Semente set.seed(20261016 + 0).
#  (a) Tabela V7-5.4 (pontuais) reproduzida com o QVAR estimado
#      diretamente nas linhas originais do desenho (tol. 0,01; senao PARA).
#  (b) qvar_fit_YX = qvar_fit (V9) = pacote; identidade com ruido zero.
#  (c) Sieve VAR(2) em medias: maior modulo de autovalor por subamostra.
#  (d) Diagnosticos (i) e (ii) dos esquemas V7 e V9, B = 500.
# Origem: v10_01_validacao_diagnostico.R (inalterado salvo caminhos).
# Usados: V10-0a (Tabela S6, Painel C), V10-0c e V10-0d (Tabela S7, via
# V10-1_validity_diagnostics.csv de 29_*); results/rds/pairs_main.rds.
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_v10.R"))
t0 <- Sys.time(); log_pkgs10("v10_01_validacao_diagnostico.R")
K <- 0; SEED <- SEED_V10 + K
D <- carregar_v4()

PAIRS <- list(BTC = prep_pair(D_from_xts(D$amostras$btc$pre), D_from_xts(D$amostras$btc$post)),
              ETH = prep_pair(D_from_xts(D$amostras$eth$pre), D_from_xts(D$amostras$eth$post)))
saveRDS(PAIRS, RDS10("pairs_main"))

## ---------------- (a) -----------------------------------------------------
pt <- point_sep10(PAIRS)
v74 <- read.csv(file.path(OUT7, "csv", "V7-5.4_stats_MBB.csv"))
val <- do.call(rbind, lapply(seq_len(nrow(v74)), function(i) {
  p <- v74$pair[i]; st <- v74$statistic[i]
  rep <- if (p == "BTC-ETH") pt$BTC$stats[[st]] - pt$ETH$stats[[st]] else pt[[p]]$stats[[st]]
  data.frame(pair = p, statistic = st, V7_table = v74$point[i], reproduced_design_rows = rep, abs_diff = abs(rep - v74$point[i]), pass = abs(rep - v74$point[i]) <= 0.01)
}))
print(val)
log_v10(sprintf("01a: Tabela V7-5.4 reproduzida nas linhas do desenho em %d/%d (dif. max %.2e)", sum(val$pass), nrow(val), max(val$abs_diff)))
write.csv(val, file.path(OUT10, "csv", "V10-0a_validation_V7-5.4.csv"), row.names = FALSE)
if (!all(val$pass)) { progresso("PARADA: Tabela V7-5.4 nao reproduzida com as linhas do desenho."); stop("Validacao falhou") }

## ---------------- (b) -----------------------------------------------------
eqv <- do.call(rbind, lapply(names(PAIRS), function(nm) do.call(rbind, lapply(c("pre","post"), function(s) {
  Dd <- PAIRS[[nm]][[s]]; YX <- build_YX(Dd); win <- PAIRS[[nm]]$win
  YX0 <- build_YX(Dd, seq_len(nrow(Dd$src)), u = rep(0, win[2] - win[1] + 1), win = win)
  do.call(rbind, lapply(c(10, 5, 1), function(H) data.frame(pair = nm, sample = s, H = H, tau = TAUS,
    v9_qvar_fit = sapply(TAUS, function(tt) npdc_fast(Dd$M, tt, NLAG, H)[["NPDC"]]),
    package = if (H == 10) sapply(TAUS, function(tt) npdc_one(Dd$M, tt, NLAG, H)) else NA,
    design_rows = npdc_YX(YX$Y, YX$X, TAUS, H)["NPDC", , 1],
    design_rows_zero_noise = npdc_YX(YX0$Y, YX0$X, TAUS, H)["NPDC", , 1])))
}))))
eqv$max_abs_diff <- pmax(abs(eqv$design_rows - eqv$v9_qvar_fit), abs(eqv$design_rows_zero_noise - eqv$v9_qvar_fit), abs(eqv$design_rows - eqv$package), na.rm = TRUE)
log_v10(sprintf("01b: QVAR nas linhas do desenho vs qvar_fit (V9) e pacote: %d comparacoes, dif. max %.2e", nrow(eqv), max(eqv$max_abs_diff)))
write.csv(eqv, file.path(OUT10, "csv", "V10-0b_design_rows_qvar_check.csv"), row.names = FALSE)
if (max(eqv$max_abs_diff) > 1e-8) { progresso("PARADA: QVAR nas linhas do desenho difere"); stop("QVAR YX difere") }
progresso(sprintf("01 validacao: V7-5.4 reproduzida %d/%d (dif. max %.1e) com o QVAR estimado nas linhas do desenho; identico a qvar_fit/pacote (dif. max %.1e).",
                  sum(val$pass), nrow(val), max(val$abs_diff), max(eqv$max_abs_diff)))

## ---------------- (c) sieve ------------------------------------------------
sv <- do.call(rbind, lapply(names(PAIRS), function(nm) do.call(rbind, lapply(c("pre","post"), function(s) { x <- PAIRS[[nm]][[s]]$sieve
  data.frame(pair = nm, sample = s, n = x$n, VIX_own_lag1 = x$A["VIX", 2], VIX_own_lag2 = x$A["VIX", 4], max_modulus_mean_VAR = x$maxmod, usable = x$maxmod < 1) }))))
print(sv); write.csv(sv, file.path(OUT10, "csv", "V10-0c_sieve_VAR_roots.csv"), row.names = FALSE)
log_v10(sprintf("01c: sieve VAR(2) maior modulo: %s", paste(sv$pair, sv$sample, sprintf("%.4f", sv$max_modulus_mean_VAR), collapse = "; ")))

## ---------------- (d) esquemas de referencia V7 e V9 ------------------------
set.seed(SEED)
diag_ref <- list()
for (nm in names(PAIRS)) for (s in c("pre","post")) {
  Dd <- PAIRS[[nm]][[s]]; M <- Dd$M; n <- nrow(M); b95 <- Dd$b95
  S9 <- mk_S(xts(M, order.by = Dd$date))
  for (b in 1:B_FULL) {
    idx <- mbb_idx(n); z <- ruido(n); z1 <- ruido(n + 1)
    f2 <- first2_flag(idx)[(NLAG + 1):n]
    for (sc in c("V7","V9")) {
      if (sc == "V7") { M0 <- M[idx, , drop = FALSE]; Mn <- M0; Mn[, "VIX"] <- Mn[, "VIX"] + z }
      else { Mn <- recon(S9, idx, z1); M0 <- recon(S9, idx) }
      e0 <- embed(M0, NLAG + 1); YX0 <- list(Y = e0[, 1:2], X = e0[, -(1:2)]); colnames(YX0$Y) <- colnames(M)
      en <- embed(Mn, NLAG + 1)
      fr <- suppressWarnings(rq(en[, 2] ~ en[, -(1:2)], tau = 0.95, method = "fn"))
      cnt <- splice_counts(f2, res_vix(YX0, b95) > 0, fr$residuals > 0)
      diag_ref[[length(diag_ref) + 1]] <- data.frame(scheme = sc, pair = nm, sample = s, b = b, vmin = min(Mn[, "VIX"]), vmax = max(Mn[, "VIX"]), t(cnt))
    }
  }
}
DR <- do.call(rbind, diag_ref)
saveRDS(DR, RDS10("diag_reference_schemes"))
obs <- do.call(rbind, lapply(names(PAIRS), function(nm) do.call(rbind, lapply(c("pre","post"), function(s)
  data.frame(pair = nm, sample = s, obs_min = min(PAIRS[[nm]][[s]]$M[, "VIX"]), obs_max = max(PAIRS[[nm]][[s]]$M[, "VIX"]))))))
summ <- do.call(rbind, lapply(split(DR, list(DR$scheme, DR$pair, DR$sample), drop = TRUE), function(d) {
  o <- obs[obs$pair == d$pair[1] & obs$sample == d$sample[1], ]
  inr <- d$vmin >= 0.5 * o$obs_min & d$vmax <= 2 * o$obs_max
  sr <- splice_ratio(as.matrix(d[, c("m","n_f2","n_pos_orig","n_pos_orig_f2","n_pos_refit","n_pos_refit_f2")]))
  data.frame(scheme = d$scheme[1], pair = d$pair[1], sample = d$sample[1], B = nrow(d), obs_min = o$obs_min, obs_max = o$obs_max,
             VIXstar_min = min(d$vmin), VIXstar_max = max(d$vmax), share_reps_in_range = mean(inr), pass_i = mean(inr) >= 0.95, t(sr), pass_ii = sr[["ratio_orig"]] <= 1.5)
}))
rownames(summ) <- NULL; print(summ)
write.csv(summ, file.path(OUT10, "csv", "V10-0d_validity_reference_schemes.csv"), row.names = FALSE)
progresso(sprintf("01 diagnosticos esquemas de referencia (B=500): V7 razao emendas %s; V9 faixa em %s das replicas.",
  paste(sprintf("%.2f", summ$ratio_orig[summ$scheme == "V7"]), collapse = "/"), paste(sprintf("%.2f", summ$share_reps_in_range[summ$scheme == "V9"]), collapse = "/")))
log_v10(sprintf("v10_01 concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
