# =====================================================================
# V7-2 / Prompt 3, ETAPA 5 — CAUDA SUPERIOR POR ORDEM DE DEFASAGEM NA BASE
# NOVA (copia parametrizada de 45_cauda_sup_por_p.R; so pontuais).
#  5.1 p in {1,2,3,7} x H in {5,10,20}: Delta(0.90), Delta(0.95), DD(0.95)
#      (p = 7 = AIC da V4, mantido para comparabilidade com V5-13; o AIC
#      da base nova e reportado em V7-1).
#  5.2 p in {1,2,3}, H = 10: Delta(0.95) excluindo 22/07-16/08/2024 e
#      tratado - cesta B em Delta(0.95).
# Tabelas V7-5.13 e V7-5.14.
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
t0 <- Sys.time(); log_pkgs7("v7_14_p3_etapa5_por_p.R")
D <- carregar_v4(); CFG <- D$CFG
P_GRID <- c(1, 2, 3, 7); H_GRID <- c(5, 10, 20)
rotulo_p <- function(p) ifelse(p == 2, "2 (BIC)", ifelse(p == 7, "7 (AIC, V4)", as.character(p)))
t51 <- list()
for (par in c("btc","eth")) { Mp <- coredata(D$amostras[[par]]$pre); Mq <- coredata(D$amostras[[par]]$post)
  for (H in H_GRID) for (p in P_GRID) {
    np <- npdc_taus(Mp, c(0.50, 0.90, 0.95), nlag = p, nfore = H); nq <- npdc_taus(Mq, c(0.50, 0.90, 0.95), nlag = p, nfore = H); d <- nq - np
    t51[[length(t51) + 1]] <- data.frame(pair = toupper(par), p = p, p_label = rotulo_p(p), H = H, Delta_0.90 = unname(d["0.90"]), Delta_0.95 = unname(d["0.95"]),
                                         DD_0.95 = unname(d["0.95"] - d["0.50"]), Delta_0.50 = unname(d["0.50"])) } }
t51 <- do.call(rbind, t51); print(t51)
vx <- ler_vix(); ctrl <- lapply(setNames(CONTROLES, CONTROLES), serie_ctrl_xts, vix_xts = vx)
CESTA_B <- c("ADA","XLM","XRP","TRX","LTC")
d95 <- function(Z, cut, p) { Mp <- coredata(Z[index(Z) < cut]); Mq <- coredata(Z[index(Z) >= cut]); npdc_one(Mq, 0.95, nlag = p, nfore = 10) - npdc_one(Mp, 0.95, nlag = p, nfore = 10) }
t52 <- list(); ctrl_rows <- list()
for (par in c("btc","eth")) { cut <- CUTS[[par]]; Zt <- D$amostras[[par]]$total; Zx <- Zt[!(index(Zt) >= JANELA_AGO24[1] & index(Zt) <= JANELA_AGO24[2])]
  for (p in 1:3) { dt <- d95(Zt, cut, p); dx <- d95(Zx, cut, p); dc <- sapply(CESTA_B, function(a) d95(ctrl[[a]], cut, p))
    for (a in CESTA_B) ctrl_rows[[length(ctrl_rows) + 1]] <- data.frame(cut = toupper(par), p = p, control = a, Delta_0.95 = dc[[a]])
    t52[[length(t52) + 1]] <- data.frame(pair = toupper(par), p = p, p_label = rotulo_p(p), H = 10, Delta_0.95 = dt, Delta_0.95_excl_Aug2024 = dx,
                                         BasketB_Delta_0.95 = mean(dc), Treated_minus_BasketB_Delta_0.95 = dt - mean(dc)) } }
t52 <- do.call(rbind, t52); ctrl_rows <- do.call(rbind, ctrl_rows); print(t52)
write.csv(t51, file.path(OUT7, "csv", "V7-5.13_upper_tail_by_p_H.csv"), row.names = FALSE)
write.csv(t52, file.path(OUT7, "csv", "V7-5.14_upper_tail_excl_basketB_by_p.csv"), row.names = FALSE)
write.csv(ctrl_rows, file.path(OUT7, "csv", "V7-5.14_controls_Delta095_by_p.csv"), row.names = FALSE)
saveRDS(list(t51 = t51, t52 = t52, ctrl = ctrl_rows), file.path(OUT7, "rds", "v7_p3_etapa5_por_p.rds"))
progresso("V7-2 Etapa 5 (cauda superior por p, V7-5.13/5.14) concluida.")
log_v7(sprintf("v7_14 concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
