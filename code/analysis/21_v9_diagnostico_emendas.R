# =====================================================================
# V9 — TAREFA 0: validacao e diagnostico das emendas
#  0a  reproduz as estimativas pontuais da Tabela V7-5.4 (tol. 0,01) com o
#      codigo da V7 (npdc_taus) e confere que o QVAR reimplementado
#      (npdc_fast) e a reconstrucao identidade do VIX sao identicos.
#  0b  bootstrap ANTIGO (blocos de linhas com o VIX em nivel): 20 replicas
#      do pos-ETF de BTC e ETH; emendas por replica; salto |VIX| nas
#      emendas vs |dVIX| diario observado; fracao das observacoes com
#      residuo positivo na equacao do VIX em tau = 0,95 que caem nas 2
#      primeiras posicoes apos uma emenda (e o mesmo no esquema novo, com
#      os mesmos indices, como referencia). Semente 20261015 + 0.
# Origem: v9_01_tarefa0.R (inalterado salvo caminhos). Usado no artigo:
# V9-0b_splice_jump_distribution.csv (Tabela S7, Painel B).
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_v9.R"))
t0 <- Sys.time(); log_pkgs9("v9_01_tarefa0.R")
K <- 0; SEED <- SEED_V9 + K
D <- carregar_v4()
AMM <- lapply(D$amostras[c("btc","eth")], function(a) list(pre = coredata(a$pre), post = coredata(a$post)))

## ---------------- 0a -----------------------------------------------------
pt_npdc <- lapply(AMM, function(a) list(pre = npdc_taus(a$pre), post = npdc_taus(a$post)))
pt_stats <- lapply(pt_npdc, function(x) stats_from_npdc(x$pre, x$post))
v74 <- read.csv(file.path(OUT7, "csv", "V7-5.4_stats_MBB.csv"))
val <- do.call(rbind, lapply(seq_len(nrow(v74)), function(i) {
  p <- v74$pair[i]; st <- v74$statistic[i]
  rep <- if (p == "BTC-ETH") pt_stats$btc[[st]] - pt_stats$eth[[st]] else pt_stats[[tolower(p)]][[st]]
  data.frame(pair = p, statistic = st, V7_table = v74$point[i], reproduced = rep, abs_diff = abs(rep - v74$point[i]), pass = abs(rep - v74$point[i]) <= 0.01)
}))
print(val)
log_v9(sprintf("0a: Tabela V7-5.4 reproduzida em %d/%d estatisticas (dif. max %.2e)", sum(val$pass), nrow(val), max(val$abs_diff)))
if (!all(val$pass)) { progresso("PARADA: Tabela V7-5.4 nao reproduzida."); stop("Validacao 0a falhou") }

## QVAR reimplementado = npdc_one (todos os taus, pre/pos, 2 pares; tambem p = 1, 3 e H = 1, 20)
eqv <- do.call(rbind, lapply(c("btc","eth"), function(par) do.call(rbind, lapply(c("pre","post"), function(s) {
  M <- AMM[[par]][[s]]
  do.call(rbind, lapply(list(c(2, 10), c(1, 10), c(3, 10), c(2, 1), c(2, 20)), function(pH)
    data.frame(pair = toupper(par), sample = s, p = pH[1], H = pH[2], tau = TAUS,
               package = sapply(TAUS, function(tt) npdc_one(M, tt, pH[1], pH[2])),
               reimpl = sapply(TAUS, function(tt) npdc_fast(M, tt, pH[1], pH[2])[["NPDC"]]))))
}))))
eqv$abs_diff <- abs(eqv$package - eqv$reimpl)
log_v9(sprintf("0a: QVAR reimplementado vs ConnectednessApproach: %d comparacoes, dif. max %.2e", nrow(eqv), max(eqv$abs_diff)))
if (max(eqv$abs_diff) > 1e-8) stop("QVAR reimplementado difere do pacote")

## reconstrucao identidade (sem ruido) reproduz o VIX observado
PAIRS <- lapply(D$amostras[c("btc","eth")], function(a) list(pre = mk_S(a$pre), post = mk_S(a$post)))
idc <- do.call(rbind, lapply(c("btc","eth"), function(par) do.call(rbind, lapply(c("pre","post"), function(s) {
  S <- PAIRS[[par]][[s]]; n <- nrow(S$M)
  data.frame(pair = toupper(par), sample = s, n = n,
             max_abs_diff_level = max(abs(recon(S, 1:n)[, "VIX"] - S$M[, "VIX"])),
             max_abs_diff_dlog_vs_rowdiff = max(abs(S$d[-1] - diff(log(S$M[, "VIX"])))),
             max_abs_diff_dlogspec = max(abs(recon(S, 1:n, spec = "dlog")[, "VIX"] - 100 * S$d)))
}))))
print(idc)
log_v9(sprintf("0a: reconstrucao identidade: dif. max nivel %.2e; Dlog FRED = diferenca entre linhas consecutivas: dif. max %.2e",
               max(idc$max_abs_diff_level), max(idc$max_abs_diff_dlog_vs_rowdiff)))
write.csv(val, file.path(OUT9, "csv", "V9-0a_validation_V7-5.4.csv"), row.names = FALSE)
write.csv(eqv, file.path(OUT9, "csv", "V9-0a_qvar_reimplementation_check.csv"), row.names = FALSE)
write.csv(idc, file.path(OUT9, "csv", "V9-0a_identity_reconstruction_check.csv"), row.names = FALSE)
progresso(sprintf("Tarefa 0a concluida: V7-5.4 reproduzida %d/%d (dif. max %.1e); QVAR reimplementado = pacote (dif. max %.1e); reconstrucao identidade exata (%.1e).",
                  sum(val$pass), nrow(val), max(val$abs_diff), max(eqv$abs_diff), max(idc$max_abs_diff_level)))

## ---------------- 0b -----------------------------------------------------
set.seed(SEED)
NREP <- 20
diag_rows <- list(); jumps_all <- list(); rep_rows <- list()
for (par in c("btc","eth")) {
  S <- PAIRS[[par]]$post; n <- nrow(S$M); vobs <- S$M[, "VIX"]
  obs_abs <- abs(diff(vobs))
  for (b in 1:NREP) {
    idx <- mbb_idx(n); z <- ruido(n); z1 <- c(0, z)
    spl <- which(c(FALSE, idx[-1] != idx[-n] + 1))                # 1a observacao de cada bloco nao contiguo
    nblk_bound <- sum(seq_len(n) %% BLOCK == 1 & seq_len(n) > 1)  # fronteiras de bloco
    Mold <- S$M[idx, , drop = FALSE]; Mold[, "VIX"] <- Mold[, "VIX"] + z
    Mnew <- recon(S, idx, z1)
    jump_old <- abs(vobs[idx[spl]] - vobs[idx[spl - 1]])
    lev_new <- attr(Mnew, "range"); vnew <- Mnew[, "VIX"] - z
    jump_new <- abs(vnew[spl] - vnew[spl - 1])
    after <- sort(unique(c(spl, spl + 1))); after <- after[after <= n]
    frac <- function(M) {
      f <- qvar_fit(M, 0.95); r <- f$Res[, "VIX"]; tt <- (NLAG + 1):n
      pos <- tt[r > 0]
      c(n_pos = length(pos), share_pos_in_first2 = mean(pos %in% after), baseline_share = mean(tt %in% after))
    }
    fo <- frac(Mold); fn <- frac(Mnew)
    rep_rows[[length(rep_rows) + 1]] <- data.frame(pair = toupper(par), replicate = b, n = n, block_boundaries = nblk_bound, splices = length(spl),
      mean_jump_old = mean(jump_old), max_jump_old = max(jump_old), mean_jump_new = mean(jump_new), max_jump_new = max(jump_new),
      old_n_pos = fo[["n_pos"]], old_share_pos_first2 = fo[["share_pos_in_first2"]], new_n_pos = fn[["n_pos"]], new_share_pos_first2 = fn[["share_pos_in_first2"]],
      baseline_share_first2 = fo[["baseline_share"]])
    jumps_all[[length(jumps_all) + 1]] <- rbind(data.frame(pair = toupper(par), type = "Old scheme: |VIX jump| at splices", x = jump_old),
                                                data.frame(pair = toupper(par), type = "Corrected scheme: |VIX jump| at splices", x = jump_new))
  }
  jumps_all[[length(jumps_all) + 1]] <- data.frame(pair = toupper(par), type = "Observed daily |change in VIX| (post-ETF)", x = obs_abs)
}
reps <- do.call(rbind, rep_rows); J <- do.call(rbind, jumps_all)
qs <- function(x) c(n = length(x), mean = mean(x), median = median(x), p90 = unname(quantile(x, 0.90)), p99 = unname(quantile(x, 0.99)), max = max(x),
                    share_gt_2 = mean(x > 2), share_gt_5 = mean(x > 5))
jt <- do.call(rbind, lapply(split(J, list(J$pair, J$type), drop = TRUE), function(d) data.frame(pair = d$pair[1], distribution = d$type[1], t(qs(d$x)))))
rownames(jt) <- NULL; jt <- jt[order(jt$pair, jt$distribution), ]
sm <- do.call(rbind, lapply(split(reps, reps$pair), function(d) data.frame(pair = d$pair[1], replicates = nrow(d), n_post = d$n[1],
  splices_mean = mean(d$splices), splices_min = min(d$splices), splices_max = max(d$splices), block_boundaries = d$block_boundaries[1],
  old_share_pos_res_in_first2 = mean(d$old_share_pos_first2), new_share_pos_res_in_first2 = mean(d$new_share_pos_first2),
  baseline_share_first2 = mean(d$baseline_share_first2), old_ratio_to_baseline = mean(d$old_share_pos_first2) / mean(d$baseline_share_first2),
  new_ratio_to_baseline = mean(d$new_share_pos_first2) / mean(d$baseline_share_first2))))
print(sm); print(jt)
write.csv(reps, file.path(OUT9, "csv", "V9-0b_splices_by_replicate.csv"), row.names = FALSE)
write.csv(jt, file.path(OUT9, "csv", "V9-0b_splice_jump_distribution.csv"), row.names = FALSE)
write.csv(sm, file.path(OUT9, "csv", "V9-0b_splice_summary.csv"), row.names = FALSE)
saveRDS(list(val = val, eqv = eqv, idc = idc, reps = reps, jt = jt, sm = sm, J = J, seed = SEED), file.path(OUT9, "rds", "v9_0.rds"))
log_v9(sprintf("0b: emendas por replica BTC %.1f, ETH %.1f; fracao de residuos >0 (tau 0,95, eq. VIX) nas 2 1as obs. apos emenda: antigo BTC %.3f / ETH %.3f vs base %.3f / %.3f",
               sm$splices_mean[1], sm$splices_mean[2], sm$old_share_pos_res_in_first2[1], sm$old_share_pos_res_in_first2[2], sm$baseline_share_first2[1], sm$baseline_share_first2[2]))
progresso(sprintf("Tarefa 0b concluida (diagnostico das emendas): emendas/replica BTC %.1f, ETH %.1f; fracao residuos>0 apos emenda (antigo) BTC %.2f, ETH %.2f vs base %.2f/%.2f.",
                  sm$splices_mean[1], sm$splices_mean[2], sm$old_share_pos_res_in_first2[1], sm$old_share_pos_res_in_first2[2], sm$baseline_share_first2[1], sm$baseline_share_first2[2]))
log_v9(sprintf("v9_01 concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
