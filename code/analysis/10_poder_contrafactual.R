# =====================================================================
# V7-3 / Prompt 4, TAREFA 2 — poder do contrafactual (replicas da V7-6.1a)
# (copia parametrizada de v6_04_tarefa2.R)
# SE_boot = DP das replicas; MDE = 2,80 x SE_boot; TOST com IC 90% e
# margens |Delta_tratado| e 0,5|Delta_tratado|.
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
t0 <- Sys.time(); log_pkgs7("v7_22_p4_tarefa2.R")
R1 <- readRDS(file.path(OUT7, "rds", "v7_6_1a.rds"))
rows <- list()
for (cn in c("btc","eth")) for (tk in c("0.50","0.75","0.95")) for (cs in c("A","B")) {
  st <- paste0("TREATEDminusBasket", cs, "_Delta_", tk)
  x <- R1$bt[[cn]]$mat[R1$bt[[cn]]$valid, st]
  pt <- unname(R1$sistemas_point[[cn]][st]); dtr <- unname(R1$sistemas_point[[cn]][paste0("TREATED_Delta_", tk)])
  q <- quantile(x, c(0.025, 0.05, 0.95, 0.975), names = FALSE); se <- sd(x)
  for (fr in c(1, 0.5)) { dl <- fr * abs(dtr)
    rows[[length(rows) + 1]] <- data.frame(cut = toupper(cn), tau = as.numeric(tk), comparison = paste0("Treated - basket ", cs), delta_treated = dtr, point = pt,
      boot_median = median(x), ci95_lo = q[1], ci95_hi = q[4], ci90_lo = q[2], ci90_hi = q[3], se_boot = se, MDE_80 = 2.80 * se, n_valid = length(x),
      ci95_excludes_0 = (q[1] > 0 | q[4] < 0), margin_rule = ifelse(fr == 1, "|Delta_treated|", "0.5 x |Delta_treated|"), margin = dl,
      ci90_within_margin = (q[2] > -dl & q[3] < dl), MDE_over_abs_delta_treated = 2.80 * se / abs(dtr)) }
}
t2 <- do.call(rbind, rows); rownames(t2) <- NULL
print(t2[, c("cut","tau","comparison","delta_treated","point","se_boot","MDE_80","ci90_lo","ci90_hi","margin_rule","ci90_within_margin")])
write.csv(t2, file.path(OUT7, "csv", "V7-6.2_power_equivalence_full.csv"), row.names = FALSE)
saveRDS(t2, file.path(OUT7, "rds", "v7_6_2.rds"))
progresso(sprintf("V7-3 Tarefa 2 (poder) concluida; TOST equivalencia em %d de %d linhas.", sum(t2$ci90_within_margin), nrow(t2)))
log_v7(sprintf("v7_22 (Tarefa 2) concluido em %.1f s; TOST equivalencia estabelecida em %d de %d linhas", as.numeric(Sys.time() - t0, units = "secs"), sum(t2$ci90_within_margin), nrow(t2)))
