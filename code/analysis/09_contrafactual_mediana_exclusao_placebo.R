# =====================================================================
# V7-3 / Prompt 4, TAREFA 1 — testes de atribuicao na mediana na base
# nova (copia parametrizada de v6_03_tarefa1.R)
#  1a contrafactual (bootstrap conjunto por data), tau 0.25/0.50/0.75/0.95
#  1b exclusao 22/07-16/08/2024, Delta em todos os quantis (bootstrap
#     separado, desenho V5-4)
#  1c placebo pre-ETF (desenho V5-8), espacamentos 20, 10 e 5
# Semente 20260930 + 1. As checagens contra V5/V4 da V6 sao substituidas
# por checagens internas contra a V7 (Tabela V7-5.4 / V7-1).
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
t0 <- Sys.time(); log_pkgs7("v7_21_p4_tarefa1.R")
K <- 1; SEED <- SEED_BASE_V6 + K
D <- carregar_v4(); vx <- ler_vix()
E1 <- readRDS(file.path(OUT7, "rds", "v7_p3_etapa1_bootstrap.rds"))

## ======================= 1a ============================================
ctrl <- lapply(setNames(CONTROLES, CONTROLES), serie_ctrl_xts, vix_xts = vx)
sistemas <- list()
for (cn in c("btc","eth")) {
  cut <- CUTS[[cn]]; Zt <- D$amostras[[cn]]$total
  excl <- Zt[!(index(Zt) >= JANELA_AGO24[1] & index(Zt) <= JANELA_AGO24[2])]
  S <- c(list(TREATED = to_list(Zt), TREATED_EXCL = to_list(excl)), lapply(ctrl, to_list))
  todas <- sort(unique(do.call(c, lapply(S, `[[`, "date"))))
  sistemas[[cn]] <- list(series = S, taus = TAUS_CF, baskets = CESTAS, treated = c("TREATED","TREATED_EXCL"),
                         master_pre = todas[todas < cut], master_post = todas[todas >= cut])
}
sistemas <- point_joint(sistemas)
chk <- sapply(c("btc","eth"), function(cn) max(abs(sistemas[[cn]]$point[paste0("TREATED_Delta_", sprintf("%.2f", TAUS_CF))] - E1$pt_stats[[cn]][paste0("Delta_", sprintf("%.2f", TAUS_CF))])))
log_v7(sprintf("1a: tratado (conjunto) = V7-5.4 (pontual): dif. max %.2e", max(chk)))
tt <- Sys.time(); tst <- run_joint_boot(sistemas, B_TEST, SEED)
log_v7(sprintf("1a teste B=20: validas BTC %d/20, ETH %d/20 (%.0f s)", sum(tst$btc$valid), sum(tst$eth$valid), as.numeric(Sys.time() - tt, units = "secs")))
if (min(sum(tst$btc$valid), sum(tst$eth$valid)) < 19) stop("1a teste abaixo de 95%")
tt <- Sys.time(); bt1a <- run_joint_boot(sistemas, B_FULL, SEED)
log_v7(sprintf("1a B=500: validas BTC %d, ETH %d; timeouts %d (%.1f min)", sum(bt1a$btc$valid), sum(bt1a$eth$valid), attr(bt1a, "n_timeout"), as.numeric(Sys.time() - tt, units = "mins")))
tab1a <- tab_joint(sistemas, bt1a)
write.csv(tab1a, file.path(OUT7, "csv", "V7-6.1a_counterfactual_all.csv"), row.names = FALSE)
saveRDS(list(sistemas_point = lapply(sistemas, `[[`, "point"), bt = bt1a, tab = tab1a, test = tst), file.path(OUT7, "rds", "v7_6_1a.rds"))
progresso("V7-3 Tarefa 1a (contrafactual na mediana) concluida.")

## ======================= 1b ============================================
pairs_ex <- lapply(D$amostras[c("btc","eth")], function(a) {
  Zt <- a$total; ex <- Zt[!(index(Zt) >= JANELA_AGO24[1] & index(Zt) <= JANELA_AGO24[2])]
  list(pre = coredata(ex[index(ex) < min(index(a$post))]), post = coredata(ex[index(ex) >= min(index(a$post))]))
})
names(pairs_ex) <- c("BTC","ETH")
pt1b <- point_sep(pairs_ex)
v71 <- read.csv(file.path(OUT7, "csv", "V7-1_exclusion_Aug2024_delta_point.csv")); v71 <- v71[v71$window == "a", ]
log_v7(sprintf("1b: pontuais = V7-1 exclusao (janela a): dif. max %.2e (n pos BTC %d, ETH %d)",
               max(abs(mapply(function(p, t) unname(pt1b[[p]]$stats[sprintf("Delta_%.2f", t)]), v71$pair, v71$tau) - v71$Delta)), nrow(pairs_ex$BTC$post), nrow(pairs_ex$ETH$post)))
tst <- run_sep_boot(pairs_ex, B_TEST, SEED); log_v7(sprintf("1b teste B=20: validas BTC %d, ETH %d", sum(tst$BTC$valid), sum(tst$ETH$valid)))
bt1b <- run_sep_boot(pairs_ex, B_FULL, SEED); log_v7(sprintf("1b B=500: validas BTC %d, ETH %d; timeouts %d", sum(bt1b$BTC$valid), sum(bt1b$ETH$valid), attr(bt1b, "n_timeout")))
tab1b <- tab_sep(pt1b, bt1b)
write.csv(tab1b, file.path(OUT7, "csv", "V7-6.1b_exclusion_all.csv"), row.names = FALSE)
saveRDS(list(pt = pt1b, bt = bt1b, tab = tab1b, n = sapply(pairs_ex, function(p) c(pre = nrow(p$pre), post = nrow(p$post)))), file.path(OUT7, "rds", "v7_6_1b.rds"))

## ======================= 1c ============================================
MIN_PRE <- 500
stat_pl <- function(Mp, Mq) { np <- npdc_taus(Mp, c(0.50, 0.75, 0.95)); nq <- npdc_taus(Mq, c(0.50, 0.75, 0.95)); d <- nq - np
  c(Delta_0.50 = unname(d["0.50"]), Delta_0.75 = unname(d["0.75"]), Delta_0.95 = unname(d["0.95"]), DD_0.95 = unname(d["0.95"] - d["0.50"])) }
pl_all <- list(); resumo <- list()
for (par in c("btc","eth")) {
  Mpre <- coredata(D$amostras[[par]]$pre); dpre <- as.Date(index(D$amostras[[par]]$pre))
  L <- nrow(D$amostras[[par]]$post); i_min <- MIN_PRE + 1; i_max <- nrow(Mpre) - L + 1
  real <- stat_pl(Mpre, coredata(D$amostras[[par]]$post)); cache <- list()
  for (esp in c(20, 10, 5)) {
    grade <- seq(i_min, i_max, by = esp)
    vals <- t(sapply(grade, function(i) { key <- as.character(i)
      if (is.null(cache[[key]])) cache[[key]] <<- stat_pl(Mpre[1:(i - 1), , drop = FALSE], Mpre[i:(i + L - 1), , drop = FALSE]); cache[[key]] }))
    pl_all[[paste(par, esp)]] <- data.frame(pair = toupper(par), spacing = esp, fake_date = dpre[grade], vals)
    sts <- if (par == "btc") c("Delta_0.50","DD_0.95") else c("Delta_0.50","Delta_0.75","DD_0.95")
    for (st in sts) { pl <- vals[, st]; N <- length(pl)
      resumo[[length(resumo) + 1]] <- data.frame(pair = toupper(par), statistic = st, spacing = esp, real = unname(real[st]), n_placebos = N,
        placebo_min = min(pl), placebo_median = median(pl), placebo_max = max(pl), rank_of_real = 1 + sum(pl < real[st]),
        n_placebos_le_real = sum(pl <= real[st]), p_value_incl_real = (1 + sum(pl <= real[st])) / (1 + N), min_attainable_p = 1 / (N + 1)) }
  }
}
pl_tab <- do.call(rbind, pl_all); rs1c <- do.call(rbind, resumo); rownames(rs1c) <- NULL
v712 <- read.csv(file.path(OUT7, "csv", "V7-5.9_placebo_all_dates.csv")); v712 <- v712[v712$type == "placebo (grid)", ]
m <- merge(pl_tab[pl_tab$spacing == 20, c("pair","fake_date","DD_0.95")], transform(v712, fake_date = as.Date(fake_date))[, c("pair","fake_date","DD_0.95")], by = c("pair","fake_date"))
log_v7(sprintf("1c: espacamento 20 = V7-5.9 (DD_0.95): %d datas, max dif %.2e", nrow(m), max(abs(m$DD_0.95.x - m$DD_0.95.y))))
print(rs1c)
write.csv(pl_tab, file.path(OUT7, "csv", "V7-6.1c_placebo_all_dates.csv"), row.names = FALSE)
write.csv(rs1c, file.path(OUT7, "csv", "V7-6.1c_placebo_summary_full.csv"), row.names = FALSE)
saveRDS(list(placebo = pl_tab, resumo = rs1c), file.path(OUT7, "rds", "v7_6_1c.rds"))
progresso("V7-3 Tarefa 1 (1a/1b/1c) concluida.")
log_v7(sprintf("v7_21 (Tarefa 1) concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
