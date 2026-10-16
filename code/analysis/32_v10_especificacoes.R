# =====================================================================
# V10 — 05: especificacoes alternativas (a)-(d) da Tarefa 2c do Prompt 5
# e nova Tabela 1 (baseline, 16:00 ET, Dlog VIX com p = 2) com o esquema
# principal (MBB de linhas do desenho). Semente 20261016 + 9 (variantes)
# e 20261016 + 10 (16:00 ET).
#  (a) Dlog VIX, p = 2: ruido somado ao nivel de cada data; a variavel e
#      100 x [log(VIX_t + u_t) - log(VIX_{t-1} + u_{t-1})] (t-1 = pregao
#      anterior do FRED), em todas as posicoes (dependente e defasagens).
#  (b) log(VIX + u).  (c) H = 1: das mesmas replicas da Tabela 1.
#  (d) retornos padronizados por GARCH(1,1) (estimados uma vez em cada
#      subamostra observada, os mesmos da V9; fixos no bootstrap).
# Origem: v10_05_especificacoes.R. Alteracoes: caminhos (retornos GARCH em
# results/rds/v9_2_garch.rds, de 23_*); removidas as conferencias dos
# pontuais com os arquivos da V9 (v9_2_alt_*, v9_6_table1_new). Usados:
# Tabela 1 (V10-9_new_table1_raw.csv) e Tabela S14 (Painel B).
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_v10.R"))
t0 <- Sys.time(); log_pkgs10("v10_05_especificacoes.R")
D <- carregar_v4(); vx <- ler_vix()
SEEDV <- SEED_V10 + 9; SEED16 <- SEED_V10 + 10
G <- readRDS(file.path(OUT9, "rds", "v9_2_garch.rds"))
PAIRS <- readRDS(RDS10("pairs_main"))
PG <- lapply(c(BTC = "btc", ETH = "eth"), function(cn) prep_pair(D_from_xts(G$Z[[cn]]$pre), D_from_xts(G$Z[[cn]]$post)))
VARIANTS <- list(
  a = list(label = "(a) Dlog VIX, p = 2", spec = "dlog", pairs = PAIRS, taus = TAUS),
  b = list(label = "(b) log VIX level, p = 2", spec = "log", pairs = PAIRS, taus = c(0.50, 0.95)),
  d = list(label = "(d) VIX level, GARCH(1,1)-standardized crypto returns", spec = "level", pairs = PG, taus = c(0.50, 0.95)))
ALT <- list()
for (vn in names(VARIANTS)) { v <- VARIANTS[[vn]]
  ALT[[vn]] <- run_or_load10(paste0("alt_", vn), {
    ptv <- point_sep10(v$pairs, spec = v$spec, nfores = 10)
    tst <- run_sep_boot10(v$pairs, B_TEST, SEEDV, taus = v$taus, nfores = 10, spec = v$spec); chk_test10(tst, paste("05 variante", vn))
    tt <- Sys.time(); bt <- run_sep_boot10(v$pairs, B_FULL, SEEDV, taus = v$taus, nfores = 10, spec = v$spec)
    log_v10(sprintf("05 variante %s B=500: validas BTC %d, ETH %d; timeouts %d (%.1f min)", vn, sum(bt$BTC$valid), sum(bt$ETH$valid), attr(bt, "n_timeout"), as.numeric(Sys.time() - tt, units = "mins")))
    ptb <- lapply(ptv, function(x) { x <- pick_H(x, "H10"); x$pre <- x$pre[sprintf("%.2f", v$taus)]; x$post <- x$post[sprintf("%.2f", v$taus)]; x$stats <- stats_gen(x$pre, x$post, v$taus); x })
    tab_all <- tab_sep9(ptb, lapply(bt, pick_H, h = "H10"), v$taus)
    npdc <- do.call(rbind, lapply(names(ptv), function(nm) data.frame(pair = nm, tau = TAUS, NPDC_pre = as.numeric(ptv[[nm]]$pre), NPDC_post = as.numeric(ptv[[nm]]$post),
                                                                    Delta = as.numeric(ptv[[nm]]$post - ptv[[nm]]$pre))))
    list(label = v$label, pt = ptv, bt = bt, tab_all = cbind(variant = v$label, tab_all), npdc = cbind(variant = v$label, npdc), seed = SEEDV)
  })
  progresso(sprintf("05 variante %s (B=500, semente %d) concluida.", vn, SEEDV))
}
## (c) H = 1 das replicas da Tabela 1
T1 <- readRDS(RDS10("t1_summary")); H1 <- T1$H1
tabc <- cbind(variant = "(c) VIX level, H = 1", H1[H1$series %in% c("BTC","ETH"), ])
keep3 <- c("Delta_0.50","Delta_0.95","DD_0.95")
TAB8 <- rbind(do.call(rbind, lapply(ALT, function(x) x$tab_all[x$tab_all$statistic %in% keep3, ])), tabc[tabc$statistic %in% keep3, ])
TAB8 <- TAB8[order(TAB8$variant, TAB8$series, match(TAB8$statistic, keep3)), ]
write.csv(TAB8, file.path(OUT10, "csv", "V10-8_alt_specs_boot_raw.csv"), row.names = FALSE)
NP8 <- do.call(rbind, lapply(ALT, `[[`, "npdc"))
write.csv(NP8, file.path(OUT10, "csv", "V10-8_alt_specs_npdc_raw.csv"), row.names = FALSE)

## ---------------- 16:00 ET --------------------------------------------------------
in_win <- function(Z) Z[index(Z) >= DATE_INI & index(Z) <= DATE_FIM]
rd <- function(f) { d <- read.csv(f); d$date <- as.Date(d$date); d }
E16 <- run_or_load10("et16", {
  cb16 <- list(BTC = rd("data/clean/px16et_btc_coinbase.csv"), ETH = rd("data/clean/px16et_eth_coinbase.csv"))
  p16 <- lapply(c(BTC = "BTC", ETH = "ETH"), function(a) { Z <- in_win(ret_ii(cb16[[a]]$date, cb16[[a]]$close_16et, vx, a)); cut <- CUTS[[tolower(a)]]
    prep_pair(D_from_xts(Z[index(Z) < cut]), D_from_xts(Z[index(Z) >= cut])) })
  ptx <- point_sep10(p16, nfores = 10)
  tst <- run_sep_boot10(p16, B_TEST, SEED16, nfores = 10); chk_test10(tst, "05 16:00 ET")
  bt <- run_sep_boot10(p16, B_FULL, SEED16, nfores = 10)
  log_v10(sprintf("05 16:00 ET B=500: validas BTC %d, ETH %d; timeouts %d", sum(bt$BTC$valid), sum(bt$ETH$valid), attr(bt, "n_timeout")))
  list(pt = ptx, bt = bt, tab = tab_sep10(ptx, bt), seed = SEED16,
       nn = data.frame(pair = names(p16), n_pre = sapply(p16, function(p) nrow(p$pre$M)), n_post = sapply(p16, function(p) nrow(p$post$M))),
       npdc = do.call(rbind, lapply(names(ptx), function(nm) data.frame(column = "16:00 ET prices", pair = nm, tau = TAUS, NPDC_pre = as.numeric(ptx[[nm]]$pre), NPDC_post = as.numeric(ptx[[nm]]$post)))))
})
progresso(sprintf("05 16:00 ET (B=500, semente %d) concluido.", SEED16))
st9 <- c(paste0("Delta_", sprintf("%.2f", TAUS)), "DD_0.95")
T9 <- rbind(cbind(column = "Baseline (24:00 UTC)", T1$TAB$MBB[T1$TAB$MBB$series %in% c("BTC","ETH"), ]),
            cbind(column = "16:00 ET prices", E16$tab), cbind(column = "Dlog VIX, p = 2", ALT$a$tab_all[, -1]))
T9 <- T9[T9$statistic %in% st9, ]
write.csv(T9, file.path(OUT10, "csv", "V10-9_new_table1_raw.csv"), row.names = FALSE)
write.csv(E16$nn, file.path(OUT10, "csv", "V10-9_sample_sizes_16ET.csv"), row.names = FALSE)
progresso("05 nova Tabela 1 (baseline, 16:00 ET, Dlog VIX p = 2) concluida.")
log_v10(sprintf("v10_05 concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
