# =====================================================================
# V7-2 / Prompt 3, ETAPA 1 — BOOTSTRAP CORRIGIDO NA BASE NOVA
# (copia parametrizada de 41_bootstrap_corrigido.R + 41b)
#  * 1.1 (diagnostico do bug da V4) NAO e repetido: resolvido na V5
#    (Tabelas V5-1/V5-2; zoo reordenava a reamostra).
#  * Verificacao do ruido (1 sorteio, semente 20260922; + 20 sorteios,
#    semente 20260924, como no 41b).
#  * MBB (bloco 20, B=500) dentro de pre/pos + ruido; robustez:
#    bootstrap estacionario (bloco medio 20). Teste B=20 antes de cada.
#    Mesma semente e mesma ordem de consumo do RNG da V5 (20260922).
#  * Criterio >= 95% de replicas validas.
#  * Estatisticas (a)-(d) + sinalizacao de pontual fora do IC 90%.
# Tabelas V7-5.1 (validade), V7-5.3 (ruido), V7-5.4 (MBB), V7-5.5 (SB).
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
t0 <- Sys.time(); log_pkgs7("v7_10_p3_etapa1_bootstrap.R")
D  <- carregar_v4()
AMM <- lapply(D$amostras[c("btc","eth")], function(a) list(pre = coredata(a$pre), post = coredata(a$post)))
ATIVO <- c(btc = "BTC", eth = "ETH")
TIMEOUT_S <- 120

## ---- pontuais e verificacao do ruido -----------------------------------
pt_npdc  <- lapply(AMM, function(a) list(pre = npdc_taus(a$pre), post = npdc_taus(a$post)))
pt_stats <- lapply(pt_npdc, function(x) stats_from_npdc(x$pre, x$post))
set.seed(SEED_V5)
noise_chk <- do.call(rbind, lapply(names(AMM), function(par) {
  Mp <- AMM[[par]]$pre;  Mp[, "VIX"] <- Mp[, "VIX"] + ruido(nrow(Mp))
  Mq <- AMM[[par]]$post; Mq[, "VIX"] <- Mq[, "VIX"] + ruido(nrow(Mq))
  np <- npdc_taus(Mp); nq <- npdc_taus(Mq); s_n <- stats_from_npdc(np, nq)
  data.frame(pair = ATIVO[[par]], stat = c(paste0("NPDC_pre_", names(np)), paste0("NPDC_post_", names(nq)), names(s_n)),
             no_noise = c(pt_npdc[[par]]$pre, pt_npdc[[par]]$post, pt_stats[[par]]), with_noise = c(np, nq, s_n))
}))
noise_chk$abs_diff <- abs(noise_chk$with_noise - noise_chk$no_noise); rownames(noise_chk) <- NULL
log_v7(sprintf("V7-5.3 verificacao do ruido (1 sorteio): dif. max absoluta = %.5f (%s %s)", max(noise_chk$abs_diff),
               noise_chk$pair[which.max(noise_chk$abs_diff)], noise_chk$stat[which.max(noise_chk$abs_diff)]))
write.csv(noise_chk, file.path(OUT7, "csv", "V7-5.3_noise_check_point.csv"), row.names = FALSE)
set.seed(SEED_V5 + 2)
noise20 <- do.call(rbind, lapply(c("btc","eth"), function(par) {
  sims <- t(sapply(1:20, function(r) {
    Mp <- AMM[[par]]$pre;  Mp[, "VIX"] <- Mp[, "VIX"] + ruido(nrow(Mp))
    Mq <- AMM[[par]]$post; Mq[, "VIX"] <- Mq[, "VIX"] + ruido(nrow(Mq))
    stats_from_npdc(npdc_taus(Mp), npdc_taus(Mq)) }))
  pt <- pt_stats[[par]]
  data.frame(pair = toupper(par), statistic = colnames(sims), point_no_noise = pt[colnames(sims)], mean_with_noise = colMeans(sims),
             sd_with_noise = apply(sims, 2, sd), max_abs_diff = apply(abs(sweep(sims, 2, pt[colnames(sims)])), 2, max), draws = 20, row.names = NULL)
}))
write.csv(noise20, file.path(OUT7, "csv", "V7-5.3_noise_sensitivity_20draws.csv"), row.names = FALSE)

## ---- bootstrap (identico ao 41) -------------------------------------------
gerar_draws <- function(B, tipo) {
  set.seed(SEED_V5)
  f <- if (tipo == "MBB") mbb_idx else sb_idx
  idx <- lapply(c(btc = "btc", eth = "eth"), function(par) lapply(1:B, function(b) list(ip = f(nrow(AMM[[par]]$pre)), iq = f(nrow(AMM[[par]]$post)))))
  noi <- lapply(c(btc = "btc", eth = "eth"), function(par) lapply(1:B, function(b) list(zp = ruido(nrow(AMM[[par]]$pre)), zq = ruido(nrow(AMM[[par]]$post)))))
  lapply(c(btc = "btc", eth = "eth"), function(par) lapply(1:B, function(b) c(list(par = par), idx[[par]][[b]], noi[[par]][[b]])))
}
boot_task <- function(tk) {
  Mp <- AMM[[tk$par]]$pre[tk$ip, , drop = FALSE];  Mp[, "VIX"] <- Mp[, "VIX"] + tk$zp
  Mq <- AMM[[tk$par]]$post[tk$iq, , drop = FALSE]; Mq[, "VIX"] <- Mq[, "VIX"] + tk$zq
  vp <- npdc_taus(Mp); vq <- npdc_taus(Mq)
  list(pre = as.numeric(vp), post = as.numeric(vq),
       err = c(if (!is.null(attr(vp, "erro"))) paste("pre", attr(vp, "erro")), if (!is.null(attr(vq, "erro"))) paste("post", attr(vq, "erro"))))
}
rodar_boot <- function(B, tipo) {
  dr <- gerar_draws(B, tipo)
  lapply(dr, function(tasks) {
    r <- par_map_timeout(tasks, boot_task, timeout = TIMEOUT_S, export_names = "AMM", export_env = globalenv())
    k <- sprintf("%.2f", TAUS)
    pre  <- do.call(rbind, lapply(r, function(x) if (is.null(x$pre))  rep(NA, 7) else x$pre))
    post <- do.call(rbind, lapply(r, function(x) if (is.null(x$post)) rep(NA, 7) else x$post))
    colnames(pre) <- colnames(post) <- k
    valid <- apply(cbind(pre, post), 1, function(x) all(is.finite(x)))
    stats <- t(sapply(1:B, function(b) stats_from_npdc(pre[b, ], post[b, ])))
    list(pre = pre, post = post, stats = stats, valid = valid, errs = unlist(lapply(r, `[[`, "err")), n_timeout = attr(r, "n_timeout"))
  })
}
tt <- Sys.time(); teste_mbb <- rodar_boot(20, "MBB"); teste_sb <- rodar_boot(20, "SB")
log_v7(sprintf("V7-5 teste B=20 (%.0f s): MBB validas BTC %d/20, ETH %d/20 | SB BTC %d/20, ETH %d/20", as.numeric(Sys.time() - tt, units = "secs"),
               sum(teste_mbb$btc$valid), sum(teste_mbb$eth$valid), sum(teste_sb$btc$valid), sum(teste_sb$eth$valid)))
if (min(sum(teste_mbb$btc$valid), sum(teste_mbb$eth$valid)) < 19) stop("Teste B=20 do MBB abaixo de 95% -- parar.")
tt <- Sys.time(); boot_mbb <- rodar_boot(500, "MBB")
log_v7(sprintf("V7-5.4 MBB B=500: validas BTC %d, ETH %d; timeouts %d/%d (%.1f min)", sum(boot_mbb$btc$valid), sum(boot_mbb$eth$valid),
               boot_mbb$btc$n_timeout, boot_mbb$eth$n_timeout, as.numeric(Sys.time() - tt, units = "mins")))
progresso(sprintf("V7-2 Etapa 1: MBB B=500 concluido (validas BTC %d, ETH %d).", sum(boot_mbb$btc$valid), sum(boot_mbb$eth$valid)))
tt <- Sys.time(); boot_sb <- rodar_boot(500, "SB")
log_v7(sprintf("V7-5.5 SB B=500: validas BTC %d, ETH %d; timeouts %d/%d (%.1f min)", sum(boot_sb$btc$valid), sum(boot_sb$eth$valid),
               boot_sb$btc$n_timeout, boot_sb$eth$n_timeout, as.numeric(Sys.time() - tt, units = "mins")))

linha_val <- function(bt, rotulo, B) data.frame(scheme = rotulo, pair = c("BTC","ETH"), B = B,
  n_valid = c(sum(bt$btc$valid), sum(bt$eth$valid)), n_timeout = c(bt$btc$n_timeout, bt$eth$n_timeout))
validade <- rbind(linha_val(teste_mbb, "B=20 test: MBB + noise", 20), linha_val(teste_sb, "B=20 test: stationary + noise", 20),
                  linha_val(boot_mbb, "Corrected: MBB (block 20) within pre/post + noise", 500),
                  linha_val(boot_sb, "Robustness: stationary bootstrap (mean block 20) + noise", 500))
validade$pct_valid <- round(100 * validade$n_valid / validade$B, 1); validade$meets_95pct <- validade$pct_valid >= 95
print(validade); write.csv(validade, file.path(OUT7, "csv", "V7-5.1_bootstrap_validity.csv"), row.names = FALSE)
criterio_ok <- all(validade$meets_95pct[startsWith(validade$scheme, "Corrected")])
criterio_sb <- all(validade$meets_95pct[startsWith(validade$scheme, "Robustness")])

resumir <- function(point, draws, pair, stat, scheme) {
  x <- draws[is.finite(draws)]; q <- quantile(x, c(0.025, 0.05, 0.5, 0.95, 0.975), names = FALSE)
  data.frame(scheme = scheme, pair = pair, statistic = stat, point = point, boot_mean = mean(x), boot_median = q[3], ci95_lo = q[1], ci95_hi = q[5],
             ci90_lo = q[2], ci90_hi = q[4], n_valid = length(x), B = length(draws), ci95_excludes_0 = (q[1] > 0 | q[5] < 0),
             point_outside_ci90 = (point < q[2] | point > q[4]))
}
tabela_stats <- function(bt, scheme) {
  rows <- list()
  for (par in c("btc","eth")) { S <- bt[[par]]$stats; S[!bt[[par]]$valid, ] <- NA
    for (st in c(paste0("Delta_", sprintf("%.2f", TAUS)), "DD_0.05", "DD_0.95", "dAsym")) rows[[length(rows) + 1]] <- resumir(pt_stats[[par]][[st]], S[, st], ATIVO[[par]], st, scheme) }
  for (tk in c("0.05","0.50","0.95")) { st <- paste0("Delta_", tk)
    Sb <- bt$btc$stats[, st]; Sb[!bt$btc$valid] <- NA; Se <- bt$eth$stats[, st]; Se[!bt$eth$valid] <- NA
    rows[[length(rows) + 1]] <- resumir(pt_stats$btc[[st]] - pt_stats$eth[[st]], Sb - Se, "BTC-ETH", st, scheme) }
  do.call(rbind, rows)
}
st_mbb <- st_sb <- NULL
if (criterio_ok) { st_mbb <- tabela_stats(boot_mbb, "MBB"); write.csv(st_mbb, file.path(OUT7, "csv", "V7-5.4_stats_MBB.csv"), row.names = FALSE)
  print(st_mbb[, c("pair","statistic","point","boot_median","ci95_lo","ci95_hi","n_valid","ci95_excludes_0","point_outside_ci90")])
} else log_v7("PARADA: criterio de 95% nao atingido no MBB -- V7-5.4 nao reportada.")
if (criterio_sb) { st_sb <- tabela_stats(boot_sb, "Stationary"); write.csv(st_sb, file.path(OUT7, "csv", "V7-5.5_stats_stationary.csv"), row.names = FALSE) }

saveRDS(list(noise_chk = noise_chk, noise20 = noise20, validade = validade, criterio_ok = criterio_ok, criterio_sb = criterio_sb,
             pt_npdc = pt_npdc, pt_stats = pt_stats, boot_mbb = boot_mbb, boot_sb = boot_sb, st_mbb = st_mbb, st_sb = st_sb),
        file.path(OUT7, "rds", "v7_p3_etapa1_bootstrap.rds"))
writeLines(capture.output(sessionInfo()), file.path(OUT7, "sessionInfo.txt"))
progresso("V7-2 Etapa 1 (bootstrap corrigido, V7-5.1/5.3/5.4/5.5) concluida.")
log_v7(sprintf("v7_10 concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
