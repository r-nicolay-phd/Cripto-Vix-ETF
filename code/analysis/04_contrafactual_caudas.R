# =====================================================================
# V7-2 / Prompt 3, ETAPA 2 — INTERVALOS DO CONTRAFACTUAL (Fig. 3) NA BASE
# NOVA (copia parametrizada de 42_contrafactual_ic.R).
# Reamostragem CONJUNTA POR DATA (blocos de 20 datas do calendario-mestre,
# separados antes/depois do corte; mesma sequencia de datas para tratado e
# controles; ruido U(-0.005,0.005) por posicao sorteada, somado ao VIX).
# Estatisticas: Delta(0.05) e Delta(0.95) por ativo; cestas A e B;
# tratado - cesta; Delta(0.95) do tratado sem 22/07-16/08/2024.
# Controles Binance e truncamentos inalterados. Semente 20260922.
# A reproducao da V4 (+-0,01) e substituida pela Etapa V7-0; aqui so se
# confere que o tratado reproduz a Tabela 1 da V7 (pontual).
# Tabelas V7-5.6 (IC) e V7-5.7 (cobertura).
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
t0 <- Sys.time(); log_pkgs7("v7_11_p3_etapa2_contrafactual.R")
D <- carregar_v4(); CFG <- D$CFG

vix_raw <- read.csv("data/clean/vix_fred.csv"); vix_raw$date <- as.Date(vix_raw$date)
vix_xts <- xts(vix_raw$VIXCLS, order.by = vix_raw$date); colnames(vix_xts) <- "VIX"
controles_arq <- c(XRP="data/clean/cesta_xrp_binance.csv", TRX="data/clean/cesta_trx_binance.csv",
                   ADA="data/clean/cesta_ada_binance.csv", XLM="data/clean/cesta_xlm_binance.csv",
                   LTC="data/clean/cesta_ltc_binance.csv")
constroi_retorno_controle <- function(path, vix_xts) {
  d <- read.csv(path); d$date <- as.Date(d$date); d <- d[order(d$date), ]
  px <- xts(d$close, order.by = d$date); colnames(px) <- "close"
  px_i <- px[as.Date(intersect(index(px), index(vix_xts)))]
  ret <- diff(log(px_i)) * 100; colnames(ret) <- "ret"
  merge(ret, vix_xts[index(ret)], join = "left")
}
controles_xts <- lapply(controles_arq, constroi_retorno_controle, vix_xts = vix_xts)
for (a in names(controles_xts)) colnames(controles_xts[[a]]) <- c(a, "VIX")
trunca_ate <- c(XRP = as.Date("2025-11-12"), TRX = as.Date("2026-09-08"), LTC = as.Date("2025-10-27"), ADA = NA, XLM = NA)
serie_controle <- function(ativo) {
  Z <- controles_xts[[ativo]][paste0(CFG$date_ini, "/", CFG$date_fim)]
  if (!is.na(trunca_ate[ativo])) Z <- Z[index(Z) <= trunca_ate[ativo]]
  na.omit(Z)
}
CONTROLES <- c("XRP","LTC","XLM","ADA","TRX"); CESTAS <- list(A = c("ADA","XLM"), B = c("ADA","XLM","XRP","TRX","LTC"))
to_list0 <- function(Z) list(date = as.Date(index(Z)), M = coredata(Z))
sistemas <- list()
for (cn in c("btc","eth")) {
  ativo <- toupper(cn); cut <- CUTS[[cn]]; Zt <- D$amostras[[cn]]$total
  excl <- Zt[!(index(Zt) >= JANELA_AGO24[1] & index(Zt) <= JANELA_AGO24[2])]
  S <- c(list(TREATED = to_list0(Zt), TREATED_EXCL = to_list0(excl)), lapply(setNames(CONTROLES, CONTROLES), function(a) to_list0(serie_controle(a))))
  todas <- sort(unique(do.call(c, lapply(S, `[[`, "date"))))
  sistemas[[cn]] <- list(ativo = ativo, cut = cut, series = S, master_pre = todas[todas < cut], master_post = todas[todas >= cut])
}
cobertura <- do.call(rbind, lapply(names(sistemas), function(cn) { s <- sistemas[[cn]]
  do.call(rbind, lapply(names(s$series), function(nm) { d <- s$series[[nm]]$date
    data.frame(cut = s$ativo, series = nm, first = min(d), last = max(d), n_pre = sum(d < s$cut), n_post = sum(d >= s$cut),
               master_pre = length(s$master_pre), master_post = length(s$master_post)) })) }))
print(cobertura); write.csv(cobertura, file.path(OUT7, "csv", "V7-5.7_counterfactual_coverage.csv"), row.names = FALSE)

TAUS_SERIE <- list(TREATED = c(0.05, 0.95), TREATED_EXCL = 0.95)
calc_sistema <- function(sis, datas_pre, datas_post, noise_pre = NULL, noise_post = NULL) {
  out <- c(); err <- NULL
  for (nm in names(sis$series)) {
    s <- sis$series[[nm]]; taus <- if (nm %in% names(TAUS_SERIE)) TAUS_SERIE[[nm]] else c(0.05, 0.95); npdc <- list()
    for (sub in c("pre","post")) {
      dd <- if (sub == "pre") datas_pre else datas_post; nz <- if (sub == "pre") noise_pre else noise_post
      pos <- match(dd, s$date); keep <- !is.na(pos); M <- s$M[pos[keep], , drop = FALSE]
      if (!is.null(nz)) M[, "VIX"] <- M[, "VIX"] + nz[keep]
      v <- npdc_taus(M, taus); if (!is.null(attr(v, "erro"))) err <- c(err, paste(nm, sub, attr(v, "erro"))); npdc[[sub]] <- v
    }
    d <- npdc$post - npdc$pre; out <- c(out, setNames(d, paste0(nm, "_Delta_", names(d))))
  }
  for (tk in c("0.05","0.95")) for (cs in names(CESTAS)) {
    m <- mean(out[paste0(CESTAS[[cs]], "_Delta_", tk)]); out[paste0("Basket", cs, "_Delta_", tk)] <- m
    out[paste0("TREATEDminusBasket", cs, "_Delta_", tk)] <- out[paste0("TREATED_Delta_", tk)] - m
  }
  list(stats = out, err = err)
}
pontos <- lapply(sistemas, function(s) calc_sistema(s, s$master_pre, s$master_post)$stats)
E1 <- readRDS(file.path(OUT7, "rds", "v7_p3_etapa1_bootstrap.rds"))
chk <- c(abs(pontos$btc["TREATED_Delta_0.95"] - E1$pt_stats$btc["Delta_0.95"]), abs(pontos$eth["TREATED_Delta_0.95"] - E1$pt_stats$eth["Delta_0.95"]),
         abs(pontos$btc["TREATED_Delta_0.05"] - E1$pt_stats$btc["Delta_0.05"]), abs(pontos$eth["TREATED_Delta_0.05"] - E1$pt_stats$eth["Delta_0.05"]))
log_v7(sprintf("V7-5.6: tratado no sistema conjunto = Tabela V7-5.4 (pontual): dif. max %.2e", max(chk)))

gerar_draws <- function(B, seed) { set.seed(seed)
  lapply(sistemas, function(s) { np <- length(s$master_pre); nq <- length(s$master_post)
    lapply(1:B, function(b) list(ip = mbb_idx(np), iq = mbb_idx(nq), zp = ruido(np), zq = ruido(nq))) }) }
task_fun <- function(tk) { s <- sistemas[[tk$cn]]; calc_sistema(s, s$master_pre[tk$ip], s$master_post[tk$iq], tk$zp, tk$zq) }
rodar <- function(B) {
  dr <- gerar_draws(B, SEED_V5)
  lapply(setNames(c("btc","eth"), c("btc","eth")), function(cn) {
    tasks <- lapply(dr[[cn]], function(d) c(list(cn = cn), d))
    r <- par_map_timeout(tasks, task_fun, timeout = 180, export_names = c("sistemas","calc_sistema","TAUS_SERIE","CESTAS"), export_env = globalenv())
    nomes <- names(pontos[[cn]])
    mat <- do.call(rbind, lapply(r, function(x) if (is.null(x$stats)) setNames(rep(NA_real_, length(nomes)), nomes) else x$stats[nomes]))
    list(mat = mat, valid = apply(mat, 1, function(x) all(is.finite(x))), errs = unlist(lapply(r, `[[`, "err")), n_timeout = attr(r, "n_timeout"))
  })
}
tt <- Sys.time(); teste <- rodar(20)
log_v7(sprintf("V7-5.6 teste B=20 (%.0f s): validas BTC-corte %d/20, ETH-corte %d/20", as.numeric(Sys.time() - tt, units = "secs"), sum(teste$btc$valid), sum(teste$eth$valid)))
if (min(sum(teste$btc$valid), sum(teste$eth$valid)) < 19) stop("Teste B=20 abaixo de 95% -- parar.")
tt <- Sys.time(); boot <- rodar(500)
log_v7(sprintf("V7-5.6 B=500: validas BTC-corte %d, ETH-corte %d; timeouts %d/%d (%.1f min)", sum(boot$btc$valid), sum(boot$eth$valid),
               boot$btc$n_timeout, boot$eth$n_timeout, as.numeric(Sys.time() - tt, units = "mins")))
validade <- data.frame(cut = c("BTC","ETH","BTC","ETH"), B = c(20, 20, 500, 500),
                       n_valid = c(sum(teste$btc$valid), sum(teste$eth$valid), sum(boot$btc$valid), sum(boot$eth$valid)),
                       n_timeout = c(teste$btc$n_timeout, teste$eth$n_timeout, boot$btc$n_timeout, boot$eth$n_timeout))
validade$pct_valid <- round(100 * validade$n_valid / validade$B, 1); validade$meets_95pct <- validade$pct_valid >= 95
write.csv(validade, file.path(OUT7, "csv", "V7-5.6_counterfactual_validity.csv"), row.names = FALSE)
ic_tab <- do.call(rbind, lapply(c("btc","eth"), function(cn) {
  mat <- boot[[cn]]$mat; mat[!boot[[cn]]$valid, ] <- NA
  do.call(rbind, lapply(colnames(mat), function(st) {
    x <- mat[, st]; x <- x[is.finite(x)]; q <- quantile(x, c(0.025, 0.05, 0.5, 0.95, 0.975), names = FALSE); pt <- unname(pontos[[cn]][st])
    data.frame(cut = toupper(cn), statistic = st, point = pt, boot_mean = mean(x), boot_median = q[3], ci95_lo = q[1], ci95_hi = q[5], ci90_lo = q[2], ci90_hi = q[4],
               n_valid = length(x), B = nrow(mat), ci95_excludes_0 = (q[1] > 0 | q[5] < 0), point_outside_ci90 = (pt < q[2] | pt > q[4]))
  }))
}))
print(ic_tab[, c("cut","statistic","point","ci95_lo","ci95_hi","ci95_excludes_0")])
write.csv(ic_tab, file.path(OUT7, "csv", "V7-5.6_counterfactual_CI.csv"), row.names = FALSE)
saveRDS(list(cobertura = cobertura, pontos = pontos, boot = boot, teste = teste, validade = validade, ic_tab = ic_tab),
        file.path(OUT7, "rds", "v7_p3_etapa2_contrafactual.rds"))
progresso("V7-2 Etapa 2 (contrafactual com IC, V7-5.6/5.7) concluida.")
log_v7(sprintf("v7_11 concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
