# =====================================================================
# V7-2 / Prompt 3, ETAPA 3 — PLACEBO DA CAUDA SUPERIOR (desenho corrigido
# da V5) NA BASE NOVA (copia parametrizada de 43_placebo_cauda_sup.R).
#  * Item 3.1 (verificacao do placebo da V4) NAO repetido: ver V5-10.
#  * So dados pre-ETF: pre = [inicio, d), pos = [d, d+L), L = n pos-ETF
#    (BTC 695, ETH 558), >= 500 obs. pre; datas espacadas de 20 pregoes;
#    as 4 datas da V4 a parte, se elegiveis.
#  * DD(0.95) e Delta(0.95) (+ Delta(0.50)) por data; criterio direcional
#    real < p5 placebo; p-valor = prop. placebos <= real.
# Tabelas V7-5.8 (resumo) e V7-5.9 (todas as datas).
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
t0 <- Sys.time(); log_pkgs7("v7_12_p3_etapa3_placebo.R")
D <- carregar_v4()
MIN_PRE <- 500; ESPAC <- 20
datas_v4 <- as.Date(c("2021-06-01","2022-01-03","2022-06-01","2023-01-02"))
stat_fun <- function(Mp, Mq) {
  np <- npdc_taus(Mp, c(0.50, 0.95)); nq <- npdc_taus(Mq, c(0.50, 0.95)); d <- nq - np
  c(Delta_0.95 = unname(d["0.95"]), DD_0.95 = unname(d["0.95"] - d["0.50"]), Delta_0.50 = unname(d["0.50"]), n_pre = nrow(Mp), n_post = nrow(Mq))
}
res <- list(); resumo <- list(); elig_tab <- list()
for (par in c("btc","eth")) {
  Zpre <- D$amostras[[par]]$pre; Mpre <- coredata(Zpre); dpre <- as.Date(index(Zpre))
  n_pre <- nrow(Mpre); L <- nrow(D$amostras[[par]]$post); i_min <- MIN_PRE + 1; i_max <- n_pre - L + 1
  grade <- if (i_max >= i_min) seq(i_min, i_max, by = ESPAC) else integer(0)
  elig_tab[[par]] <- data.frame(pair = toupper(par), n_pre_ETF = n_pre, L = L, first_eligible = dpre[i_min], last_eligible = dpre[i_max],
                                first_pos = i_min, last_pos = i_max, n_grid_placebos = length(grade), min_p_incl_real = 1 / (1 + length(grade)))
  pos_v4 <- match(datas_v4, dpre); v4_ok <- !is.na(pos_v4) & pos_v4 >= i_min & pos_v4 <= i_max
  linhas <- list()
  for (i in grade) linhas[[length(linhas) + 1]] <- data.frame(pair = toupper(par), type = "placebo (grid)", fake_date = dpre[i], post_end = dpre[i + L - 1],
                                                              t(stat_fun(Mpre[1:(i - 1), , drop = FALSE], Mpre[i:(i + L - 1), , drop = FALSE])))
  for (k in which(v4_ok)) { i <- pos_v4[k]
    linhas[[length(linhas) + 1]] <- data.frame(pair = toupper(par), type = "placebo (V4 date)", fake_date = dpre[i], post_end = dpre[i + L - 1],
                                               t(stat_fun(Mpre[1:(i - 1), , drop = FALSE], Mpre[i:(i + L - 1), , drop = FALSE]))) }
  for (k in which(!v4_ok)) linhas[[length(linhas) + 1]] <- data.frame(pair = toupper(par), type = "V4 date NOT eligible", fake_date = datas_v4[k], post_end = as.Date(NA),
    Delta_0.95 = NA, DD_0.95 = NA, Delta_0.50 = NA, n_pre = ifelse(is.na(pos_v4[k]), NA, pos_v4[k] - 1), n_post = NA)
  sr <- stat_fun(coredata(D$amostras[[par]]$pre), coredata(D$amostras[[par]]$post))
  linhas[[length(linhas) + 1]] <- data.frame(pair = toupper(par), type = "real ETF date", fake_date = CUTS[[par]], post_end = max(index(D$amostras[[par]]$post)), t(sr))
  tab <- do.call(rbind, linhas); res[[par]] <- tab
  g <- tab[tab$type == "placebo (grid)", ]
  for (st in c("DD_0.95","Delta_0.95","Delta_0.50")) {
    real <- sr[[st]]; pl <- g[[st]]; p5 <- unname(quantile(pl, 0.05, type = 7))
    resumo[[length(resumo) + 1]] <- data.frame(pair = toupper(par), statistic = st, real = real, n_placebos = length(pl), placebo_min = min(pl), placebo_p5 = p5,
      placebo_median = median(pl), placebo_max = max(pl), empirical_percentile_of_real = 100 * mean(pl <= real), p_value_placebo = mean(pl <= real),
      p_value_incl_real = (1 + sum(pl <= real)) / (1 + length(pl)), real_below_p5 = real < p5, min_p_incl_real = 1 / (1 + length(pl)))
  }
}
placebo_tab <- do.call(rbind, res); resumo_tab <- do.call(rbind, resumo); elig <- do.call(rbind, elig_tab)
print(elig); print(resumo_tab)
write.csv(placebo_tab, file.path(OUT7, "csv", "V7-5.9_placebo_all_dates.csv"), row.names = FALSE)
write.csv(resumo_tab, file.path(OUT7, "csv", "V7-5.8_placebo_summary.csv"), row.names = FALSE)
write.csv(elig, file.path(OUT7, "csv", "V7-5.8_placebo_eligibility.csv"), row.names = FALSE)
saveRDS(list(placebo = placebo_tab, resumo = resumo_tab, elig = elig), file.path(OUT7, "rds", "v7_p3_etapa3_placebo.rds"))
log_v7(sprintf("V7-5.8 placebo: DD(0.95) p BTC %.3f (N=%d), ETH %.3f (N=%d); Delta(0.95) p BTC %.3f, ETH %.3f",
               resumo_tab$p_value_placebo[1], resumo_tab$n_placebos[1], resumo_tab$p_value_placebo[4], resumo_tab$n_placebos[4],
               resumo_tab$p_value_placebo[2], resumo_tab$p_value_placebo[5]))
progresso("V7-2 Etapa 3 (placebo, V7-5.8/5.9) concluida.")
log_v7(sprintf("v7_12 concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
