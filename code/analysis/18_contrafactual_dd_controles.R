# =====================================================================
# V7b — Table S8: contrafactual por ativo de controle com Delta(0.05),
# Delta(0.95), DD(0.05), DD(0.95), N pre, N pos, por corte, com IC 95% do
# bootstrap conjunto por data (desenho V7-5.6 / script 42).
# As replicas salvas da V7-5.6 guardam so Delta(0.05) e Delta(0.95), sem a
# mediana -> o bootstrap conjunto e refeito com tau = 0.05/0.50/0.95 para
# todas as series, mesma semente (20260922) e mesmos sorteios (mesmos
# calendarios-mestre): Delta(0.05)/Delta(0.95) devem reproduzir a V7-5.6.
# Tambem gera DD do tratado, das cestas e dos gaps (criterio iii da S13).
# Teste B = 20 antes.
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
t0 <- Sys.time(); log_pkgs7("v7_41_s8_contrafactual_dd.R")
D <- carregar_v4(); CFG <- D$CFG
vix_raw <- read.csv("data/clean/vix_fred.csv"); vix_raw$date <- as.Date(vix_raw$date)
vix_xts <- xts(vix_raw$VIXCLS, order.by = vix_raw$date); colnames(vix_xts) <- "VIX"
controles_arq <- c(XRP="data/clean/cesta_xrp_binance.csv", TRX="data/clean/cesta_trx_binance.csv", ADA="data/clean/cesta_ada_binance.csv",
                   XLM="data/clean/cesta_xlm_binance.csv", LTC="data/clean/cesta_ltc_binance.csv")
constroi <- function(path) { d <- read.csv(path); d$date <- as.Date(d$date); d <- d[order(d$date), ]
  px <- xts(d$close, order.by = d$date); px_i <- px[as.Date(intersect(index(px), index(vix_xts)))]
  ret <- diff(log(px_i)) * 100; colnames(ret) <- "ret"; merge(ret, vix_xts[index(ret)], join = "left") }
controles_xts <- lapply(controles_arq, constroi); for (a in names(controles_xts)) colnames(controles_xts[[a]]) <- c(a, "VIX")
trunca_ate <- c(XRP = as.Date("2025-11-12"), TRX = as.Date("2026-09-08"), LTC = as.Date("2025-10-27"), ADA = NA, XLM = NA)
serie_controle <- function(a) { Z <- controles_xts[[a]][paste0(CFG$date_ini, "/", CFG$date_fim)]; if (!is.na(trunca_ate[a])) Z <- Z[index(Z) <= trunca_ate[a]]; na.omit(Z) }
CONTROLES <- c("XRP","LTC","XLM","ADA","TRX"); CESTAS <- list(A = c("ADA","XLM"), B = c("ADA","XLM","XRP","TRX","LTC"))
to_list0 <- function(Z) list(date = as.Date(index(Z)), M = coredata(Z))
sistemas <- list()
for (cn in c("btc","eth")) { cut <- CUTS[[cn]]; Zt <- D$amostras[[cn]]$total; excl <- Zt[!(index(Zt) >= JANELA_AGO24[1] & index(Zt) <= JANELA_AGO24[2])]
  S <- c(list(TREATED = to_list0(Zt), TREATED_EXCL = to_list0(excl)), lapply(setNames(CONTROLES, CONTROLES), function(a) to_list0(serie_controle(a))))
  todas <- sort(unique(do.call(c, lapply(S, `[[`, "date")))); sistemas[[cn]] <- list(ativo = toupper(cn), cut = cut, series = S, master_pre = todas[todas < cut], master_post = todas[todas >= cut]) }
TAUS_SERIE <- list(TREATED_EXCL = 0.95)
calc_sistema <- function(sis, datas_pre, datas_post, noise_pre = NULL, noise_post = NULL) {
  out <- c(); err <- NULL
  for (nm in names(sis$series)) { s <- sis$series[[nm]]; taus <- if (nm %in% names(TAUS_SERIE)) TAUS_SERIE[[nm]] else c(0.05, 0.50, 0.95); npdc <- list()
    for (sub in c("pre","post")) { dd <- if (sub == "pre") datas_pre else datas_post; nz <- if (sub == "pre") noise_pre else noise_post
      pos <- match(dd, s$date); keep <- !is.na(pos); M <- s$M[pos[keep], , drop = FALSE]; if (!is.null(nz)) M[, "VIX"] <- M[, "VIX"] + nz[keep]
      v <- npdc_taus(M, taus); if (!is.null(attr(v, "erro"))) err <- c(err, paste(nm, sub, attr(v, "erro"))); npdc[[sub]] <- v }
    d <- npdc$post - npdc$pre; out <- c(out, setNames(d, paste0(nm, "_Delta_", names(d))))
    if (length(taus) == 3) out <- c(out, setNames(c(d["0.05"] - d["0.50"], d["0.95"] - d["0.50"]), paste0(nm, c("_DD_0.05","_DD_0.95")))) }
  for (st in c("Delta_0.05","Delta_0.50","Delta_0.95","DD_0.05","DD_0.95")) for (cs in names(CESTAS)) {
    m <- mean(out[paste0(CESTAS[[cs]], "_", st)]); out[paste0("Basket", cs, "_", st)] <- m; out[paste0("TREATEDminusBasket", cs, "_", st)] <- out[paste0("TREATED_", st)] - m }
  list(stats = out, err = err)
}
pontos <- lapply(sistemas, function(s) calc_sistema(s, s$master_pre, s$master_post)$stats)
gerar_draws <- function(B, seed) { set.seed(seed)
  lapply(sistemas, function(s) { np <- length(s$master_pre); nq <- length(s$master_post)
    lapply(1:B, function(b) list(ip = mbb_idx(np), iq = mbb_idx(nq), zp = ruido(np), zq = ruido(nq))) }) }
task_fun <- function(tk) { s <- sistemas[[tk$cn]]; calc_sistema(s, s$master_pre[tk$ip], s$master_post[tk$iq], tk$zp, tk$zq) }
rodar <- function(B) { dr <- gerar_draws(B, SEED_V5)
  lapply(setNames(c("btc","eth"), c("btc","eth")), function(cn) { tasks <- lapply(dr[[cn]], function(d) c(list(cn = cn), d))
    r <- par_map_timeout(tasks, task_fun, timeout = 240, export_names = c("sistemas","calc_sistema","TAUS_SERIE","CESTAS"), export_env = globalenv())
    nomes <- names(pontos[[cn]])
    mat <- do.call(rbind, lapply(r, function(x) if (is.null(x$stats)) setNames(rep(NA_real_, length(nomes)), nomes) else x$stats[nomes]))
    list(mat = mat, valid = apply(mat, 1, function(x) all(is.finite(x))), n_timeout = attr(r, "n_timeout")) }) }
tt <- Sys.time(); teste <- rodar(20)
log_v7(sprintf("V7b S8 teste B=20: validas BTC %d/20, ETH %d/20 (%.0f s)", sum(teste$btc$valid), sum(teste$eth$valid), as.numeric(Sys.time() - tt, units = "secs")))
if (min(sum(teste$btc$valid), sum(teste$eth$valid)) < 19) stop("teste abaixo de 95%")
tt <- Sys.time(); boot <- rodar(500)
log_v7(sprintf("V7b S8 B=500: validas BTC %d, ETH %d; timeouts %d/%d (%.1f min)", sum(boot$btc$valid), sum(boot$eth$valid), boot$btc$n_timeout, boot$eth$n_timeout, as.numeric(Sys.time() - tt, units = "mins")))
ic <- do.call(rbind, lapply(c("btc","eth"), function(cn) { mat <- boot[[cn]]$mat; mat[!boot[[cn]]$valid, ] <- NA
  do.call(rbind, lapply(colnames(mat), function(st) resumir_v6(unname(pontos[[cn]][st]), mat[, st], cut = toupper(cn), statistic = st))) }))
## checagem com V7-5.6
v56 <- read.csv(file.path(OUT7, "csv", "V7-5.6_counterfactual_CI.csv")); mm <- merge(ic, v56, by = c("cut","statistic"), suffixes = c("", "_v56"))
log_v7(sprintf("V7b S8: Delta(0.05)/Delta(0.95) reproduzem V7-5.6: %d estatisticas, dif. max pontual %.2e, IC %.2e", nrow(mm),
               max(abs(mm$point - mm$point_v56)), max(abs(c(mm$ci95_lo - mm$ci95_lo_v56, mm$ci95_hi - mm$ci95_hi_v56)))))
saveRDS(list(pontos = pontos, boot = boot, teste = teste, ic = ic), file.path(OUT7, "rds", "v7b_s8.rds"))
write.csv(ic, file.path(CSV7 <- file.path(OUT7, "csv"), "V7-S8_counterfactual_DD_all.csv"), row.names = FALSE)
## formato S8
cob <- do.call(rbind, lapply(names(sistemas), function(cn) { s <- sistemas[[cn]]
  do.call(rbind, lapply(CONTROLES, function(a) { d <- s$series[[a]]$date; data.frame(cut = toupper(cn), asset = a, n_pre = sum(d < s$cut), n_post = sum(d >= s$cut)) })) }))
g <- function(cn, a, st, col) ic[[col]][ic$cut == cn & ic$statistic == paste0(a, "_", st)]
s8 <- do.call(rbind, lapply(c("BTC","ETH"), function(cn) do.call(rbind, lapply(CONTROLES, function(a) {
  row <- data.frame(`Split date` = ifelse(cn == "BTC", "BTC (11 Jan 2024)", "ETH (23 Jul 2024)"), Asset = paste0(a, ifelse(a %in% c("XRP","LTC","TRX"), "*", "")), check.names = FALSE)
  for (st in c("Delta_0.05","Delta_0.95","DD_0.05","DD_0.95")) { nm <- sub("Delta_", "Delta(", sub("DD_", "DD(", st)); nm <- paste0(nm, ")")
    row[[nm]] <- g(cn, a, st, "point"); row[[paste(nm, "CI95")]] <- sprintf("[%.1f; %.1f]", g(cn, a, st, "ci95_lo"), g(cn, a, st, "ci95_hi")) }
  row[["N pre"]] <- cob$n_pre[cob$cut == cn & cob$asset == a]; row[["N post"]] <- cob$n_post[cob$cut == cn & cob$asset == a]; row }))))
write.csv(s8, file.path(CSV7, "V7-S8_counterfactual_controls.csv"), row.names = FALSE)
saveRDS(list(pontos = pontos, boot = boot, teste = teste, ic = ic, s8 = s8), file.path(OUT7, "rds", "v7b_s8.rds"))
progresso(sprintf("V7b item 3: Table S8 (DD por controle, bootstrap conjunto B=500, validas BTC %d, ETH %d) gravada.", sum(boot$btc$valid), sum(boot$eth$valid)))
log_v7(sprintf("v7_41 concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
