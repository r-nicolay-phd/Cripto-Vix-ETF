# =====================================================================
# V9 — TAREFA 4: componentes do NPDC, efeito escala e estatistica com sinal.
# Semente 20261015 + 4.
#  4a  theta_{a<-VIX} e theta_{VIX<-a}, pre, pos e Delta, tau 0,50 e 0,95,
#      com as replicas do bootstrap corrigido da Tarefa 1 (as mesmas da
#      Tabela 1; o worker devolve os dois componentes de cada ajuste).
#  4b  variancia do retorno cripto e dos residuos do QVAR (tau 0,50 e
#      0,95; as duas equacoes), pre e pos.
#  4c  dias com VIX_{t-1} no 5% (alt. 10%) superior da amostra total do
#      par: r_a,t sobre 100 x Dlog VIX_t (contemporaneo) por MQO e por
#      regressao quantilica (tau 0,05 e 0,50), pre e pos, diferenca
#      pos - pre; IC por bootstrap em blocos de 20 linhas dentro de cada
#      subamostra (B = 500), limiar fixo na amostra observada. Unidades:
#      retorno esperado (%) para alta de 10% do VIX = beta x 100 log(1,1).
# Origem: v9_05_tarefa4.R. Alteracoes: caminhos; removido o item 4a
# (componentes com o bootstrap da V9), substituido por V10-7_components_raw
# (29_v10_tabela1.R; Tabela S15, Painel A). Usados: 4b (Tabela S15,
# Painel B) e 4c (Tabela S18, Painel A). A semente de 4c e a mesma.
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_v9.R"))
t0 <- Sys.time(); log_pkgs9("v9_05_tarefa4.R")
K <- 4; SEED <- SEED_V9 + K
D <- carregar_v4()

## ---------------- 4b -----------------------------------------------------
r4b <- list()
for (cn in c("btc","eth")) for (s in c("pre","post")) { M <- coredata(D$amostras[[cn]][[s]]); a <- toupper(cn)
  row <- data.frame(pair = a, sample = s, n = nrow(M), var_crypto_return = var(M[, a]), var_VIX_level = var(M[, "VIX"]), var_dVIX = var(diff(M[, "VIX"])))
  for (tau in c(0.50, 0.95)) { f <- qvar_fit(M, tau)
    row[[sprintf("Q_crypto_tau%.2f", tau)]] <- f$Q[a, a]; row[[sprintf("var_resid_crypto_tau%.2f", tau)]] <- var(f$Res[, a])
    row[[sprintf("Q_VIX_tau%.2f", tau)]] <- f$Q["VIX", "VIX"]; row[[sprintf("var_resid_VIX_tau%.2f", tau)]] <- var(f$Res[, "VIX"])
    row[[sprintf("resid_corr_tau%.2f", tau)]] <- cor(f$Res[, a], f$Res[, "VIX"]) }
  r4b[[length(r4b) + 1]] <- row }
T4b <- do.call(rbind, r4b); print(T4b)
write.csv(T4b, file.path(OUT9, "csv", "V9-4b_scale_effect_raw.csv"), row.names = FALSE)
progresso("Tarefa 4b concluida.")

## ---------------- 4c -----------------------------------------------------
E10 <- 100 * log(1.10)
est_fun <- function(y, x) {
  ols <- tryCatch(unname(coef(lm(y ~ x))[2]), error = function(e) NA_real_)
  q05 <- tryCatch(unname(coef(suppressWarnings(rq(y ~ x, tau = 0.05, method = "br")))[2]), error = function(e) NA_real_)
  q50 <- tryCatch(unname(coef(suppressWarnings(rq(y ~ x, tau = 0.50, method = "br")))[2]), error = function(e) NA_real_)
  c(OLS = ols, QR_0.05 = q05, QR_0.50 = q50)
}
set.seed(SEED); B4 <- B_FULL
rows <- list(); nn <- list()
for (top in c(0.05, 0.10)) for (cn in c("btc","eth")) {
  Z <- D$amostras[[cn]]$total; a <- toupper(cn); dt <- as.Date(index(Z)); v <- as.numeric(Z[, "VIX"]); r <- as.numeric(Z[, a])
  dl <- 100 * as.numeric(dlog_vix_raw()[dt]); vlag <- c(NA, v[-length(v)])
  thr <- quantile(v, 1 - top, names = FALSE)
  df <- data.frame(date = dt, r = r, x = dl, vlag = vlag)[-1, ]
  sub <- list(pre = df[df$date < CUTS[[cn]], ], post = df[df$date >= CUTS[[cn]], ])
  pe <- lapply(sub, function(d) { w <- d$vlag > thr; est_fun(d$r[w], d$x[w]) })
  nn[[length(nn) + 1]] <- data.frame(pair = a, top_share = top, threshold_VIX = thr, n_high_pre = sum(sub$pre$vlag > thr), n_high_post = sum(sub$post$vlag > thr))
  bs <- t(replicate(B4, { bp <- sub$pre[mbb_idx(nrow(sub$pre)), ]; bq <- sub$post[mbb_idx(nrow(sub$post)), ]
    wp <- bp$vlag > thr; wq <- bq$vlag > thr
    ep <- if (sum(wp) >= 5) est_fun(bp$r[wp], bp$x[wp]) else rep(NA, 3); eq <- if (sum(wq) >= 5) est_fun(bq$r[wq], bq$x[wq]) else rep(NA, 3)
    c(ep, eq) }))
  for (j in 1:3) { m <- c("OLS","QR_0.05","QR_0.50")[j]
    for (q in c("Pre","Post","Post - pre")) {
      pt <- switch(q, Pre = pe$pre[[m]], Post = pe$post[[m]], "Post - pre" = pe$post[[m]] - pe$pre[[m]])
      bx <- switch(q, Pre = bs[, j], Post = bs[, 3 + j], "Post - pre" = bs[, 3 + j] - bs[, j])
      rr <- resumir_v6(pt, bx, pair = a, high_VIX_days = sprintf("VIX(t-1) in top %d%%", round(100 * top)), estimator = m, quantity = q)
      rr$effect_10pct_VIX_point <- pt * E10; rr$effect_10pct_VIX_ci95_lo <- rr$ci95_lo * E10; rr$effect_10pct_VIX_ci95_hi <- rr$ci95_hi * E10
      rows[[length(rows) + 1]] <- rr } }
}
T4c <- do.call(rbind, rows); rownames(T4c) <- NULL; NN <- do.call(rbind, nn)
print(NN); print(T4c[, c("pair","high_VIX_days","estimator","quantity","point","ci95_lo","ci95_hi","n_valid","ci95_excludes_0","effect_10pct_VIX_point")])
write.csv(T4c, file.path(OUT9, "csv", "V9-4c_signed_statistic_raw.csv"), row.names = FALSE)
write.csv(NN, file.path(OUT9, "csv", "V9-4c_sample_sizes.csv"), row.names = FALSE)
saveRDS(list(T4b = T4b, T4c = T4c, NN = NN, seed = SEED), file.path(OUT9, "rds", "v9_4.rds"))
progresso("Tarefa 4 concluida (4b escala, 4c estatistica com sinal).")
log_v9(sprintf("25_v9 (Tarefa 4) concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
