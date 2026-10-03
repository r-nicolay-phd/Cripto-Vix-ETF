# =====================================================================
# FUNCOES DO CONTRAFACTUAL E BOOTSTRAP CONJUNTO (origem: v6_00_funcoes.R;
# carregadas via funcoes_pipeline.R). Reutiliza funcoes_qvar_bootstrap.R:
# npdc_one/npdc_taus (QVAR p=2, H=10), mbb_idx, ruido, stats_from_npdc,
# par_map_timeout (executor paralelo com timeout por replica).
# =====================================================================

source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_qvar_bootstrap.R"))
FUNCS_PATH <- file.path(PROJ_DIR, "code/functions/funcoes_contrafactual.R")

OUT6 <- "results"
SEED_BASE_V6 <- 20260930          # semente da tarefa k = SEED_BASE_V6 + k
B_FULL <- 500; B_TEST <- 20
N_WORKERS <- 14
TAUS_CF <- c(0.25, 0.50, 0.75, 0.95)

log_v6 <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste0(...))
  cat(msg, "\n")
  cat(msg, "\n", file = file.path(OUT6, "log.txt"), append = TRUE)
}
log_pkgs <- function(script) {
  pk <- c("ConnectednessApproach","quantreg","zoo","xts","urca","tseries","vars","flextable","officer","ggplot2","httr2","jsonlite")
  v <- sapply(pk, function(p) tryCatch(as.character(packageVersion(p)), error = function(e) "n/a"))
  log_v6("Script ", script, " | ", R.version.string, " | ", paste(names(v), v, sep = " ", collapse = "; "))
}

## ---------------------------------------------------------------------
## Resumo de uma estatistica: pontual + distribuicao bootstrap
## ---------------------------------------------------------------------
resumir_v6 <- function(point, draws, ...) {
  x <- draws[is.finite(draws)]
  if (!length(x)) return(data.frame(..., point = point, boot_median = NA, ci95_lo = NA, ci95_hi = NA,
                                    ci90_lo = NA, ci90_hi = NA, se_boot = NA, n_valid = 0L, B = length(draws),
                                    ci95_excludes_0 = NA))
  q <- quantile(x, c(0.025, 0.05, 0.5, 0.95, 0.975), names = FALSE)
  data.frame(..., point = point, boot_median = q[3], ci95_lo = q[1], ci95_hi = q[5],
             ci90_lo = q[2], ci90_hi = q[4], se_boot = sd(x), n_valid = length(x), B = length(draws),
             ci95_excludes_0 = (q[1] > 0 | q[5] < 0))
}

## ---------------------------------------------------------------------
## (A) Bootstrap SEPARADO por subamostra (desenho da Tabela V5-4):
## MBB de 20 obs. dentro de pre e dentro de pos, blocos concatenados na
## ordem sorteada, ruido U(-0.005,0.005) somado ao VIX (coluna "VIX").
## pairs: lista nomeada de list(pre = matriz, post = matriz)
## ---------------------------------------------------------------------
sep_task <- function(tk) {
  P <- get(".PAIRS_V6", envir = globalenv())[[tk$nm]]
  Mp <- P$pre[tk$ip, , drop = FALSE];  Mq <- P$post[tk$iq, , drop = FALSE]
  if (!is.null(tk$zp)) { Mp[, "VIX"] <- Mp[, "VIX"] + tk$zp; Mq[, "VIX"] <- Mq[, "VIX"] + tk$zq }
  nl <- if (is.null(tk$nlag)) NLAG else tk$nlag
  vp <- npdc_taus(Mp, tk$taus, nlag = nl); vq <- npdc_taus(Mq, tk$taus, nlag = nl)
  list(pre = as.numeric(vp), post = as.numeric(vq),
       err = c(if (!is.null(attr(vp, "erro"))) paste("pre", attr(vp, "erro")),
               if (!is.null(attr(vq, "erro"))) paste("post", attr(vq, "erro"))))
}
run_sep_boot <- function(pairs, B, seed, taus = TAUS, noise = TRUE, timeout = 120, nlag = NULL) {
  set.seed(seed)
  tasks <- list()
  for (nm in names(pairs)) {
    np <- nrow(pairs[[nm]]$pre); nq <- nrow(pairs[[nm]]$post)
    for (b in 1:B) tasks[[length(tasks) + 1]] <- list(nm = nm, b = b, taus = taus, nlag = nlag,
      ip = mbb_idx(np), iq = mbb_idx(nq),
      zp = if (noise) ruido(np) else NULL, zq = if (noise) ruido(nq) else NULL)
  }
  assign(".PAIRS_V6", pairs, envir = globalenv())
  r <- par_map_timeout(tasks, sep_task, n_workers = N_WORKERS, timeout = timeout,
                       export_names = ".PAIRS_V6", export_env = globalenv())
  k <- sprintf("%.2f", taus)
  out <- list()
  for (nm in names(pairs)) {
    ii <- which(sapply(tasks, `[[`, "nm") == nm)
    pre  <- do.call(rbind, lapply(r[ii], function(x) if (is.null(x$pre))  rep(NA, length(taus)) else x$pre))
    post <- do.call(rbind, lapply(r[ii], function(x) if (is.null(x$post)) rep(NA, length(taus)) else x$post))
    colnames(pre) <- colnames(post) <- k
    valid <- apply(cbind(pre, post), 1, function(x) all(is.finite(x)))
    out[[nm]] <- list(pre = pre, post = post, valid = valid,
                      errs = unlist(lapply(r[ii], `[[`, "err")))
  }
  attr(out, "n_timeout") <- attr(r, "n_timeout")
  out
}
## estatisticas (Delta, DD, dAsym) a partir de vetores NPDC nomeados por tau
stats_gen <- function(pre, post, taus) {
  k <- sprintf("%.2f", taus)
  d <- post[k] - pre[k]; names(d) <- paste0("Delta_", k)
  out <- d
  if (all(c("0.05","0.50") %in% k)) out <- c(out, DD_0.05 = unname(d["Delta_0.05"] - d["Delta_0.50"]))
  if (all(c("0.95","0.50") %in% k)) out <- c(out, DD_0.95 = unname(d["Delta_0.95"] - d["Delta_0.50"]))
  if (all(c("0.05","0.95") %in% k)) out <- c(out, dAsym = unname((post["0.05"] - post["0.95"]) - (pre["0.05"] - pre["0.95"])))
  out
}
point_sep <- function(pairs, taus = TAUS, nlag = NLAG) lapply(pairs, function(p) {
  np <- npdc_taus(p$pre, taus, nlag = nlag); nq <- npdc_taus(p$post, taus, nlag = nlag)
  list(pre = np, post = nq, stats = stats_gen(np, nq, taus))
})
tab_sep <- function(pts, bt, taus = TAUS, extra = list()) {
  rows <- list()
  for (nm in names(pts)) {
    S <- do.call(rbind, lapply(seq_len(nrow(bt[[nm]]$pre)), function(b) stats_gen(bt[[nm]]$pre[b, ], bt[[nm]]$post[b, ], taus)))
    S[!bt[[nm]]$valid, ] <- NA
    for (st in names(pts[[nm]]$stats))
      rows[[length(rows) + 1]] <- resumir_v6(unname(pts[[nm]]$stats[st]), S[, st], series = nm, statistic = st)
  }
  do.call(rbind, rows)
}

## ---------------------------------------------------------------------
## (B) Bootstrap CONJUNTO POR DATA (desenho da Tabela V5-6, script 42)
## sis: list(series = list(NOME = list(date, M[, c(ativo,"VIX")], taus?)),
##           taus, master_pre, master_post, baskets = list(A = c(...)),
##           treated = c("TREATED", ...))
## ---------------------------------------------------------------------
calc_joint <- function(sis, datas_pre, datas_post, noise_pre = NULL, noise_post = NULL) {
  out <- c(); err <- NULL
  for (nm in names(sis$series)) {
    s <- sis$series[[nm]]
    taus <- if (!is.null(s$taus)) s$taus else sis$taus
    npdc <- list()
    for (sub in c("pre","post")) {
      dd <- if (sub == "pre") datas_pre else datas_post
      nz <- if (sub == "pre") noise_pre else noise_post
      pos <- match(dd, s$date); keep <- !is.na(pos)
      M <- s$M[pos[keep], , drop = FALSE]
      if (!is.null(nz)) M[, "VIX"] <- M[, "VIX"] + nz[keep]
      v <- npdc_taus(M, taus, nlag = if (is.null(sis$nlag)) NLAG else sis$nlag)
      if (!is.null(attr(v, "erro"))) err <- c(err, paste(nm, sub, attr(v, "erro")))
      npdc[[sub]] <- v
    }
    d <- npdc$post - npdc$pre
    out <- c(out, setNames(d, paste0(nm, "_Delta_", names(d))))
    if (isTRUE(sis$keep_npdc)) out <- c(out, setNames(npdc$pre, paste0(nm, "_NPDCpre_", names(d))),
                                        setNames(npdc$post, paste0(nm, "_NPDCpost_", names(d))))
  }
  for (tk in sprintf("%.2f", sis$taus)) {
    for (cs in names(sis$baskets)) {
      m <- mean(out[paste0(sis$baskets[[cs]], "_Delta_", tk)])
      out[paste0("Basket", cs, "_Delta_", tk)] <- m
      for (tr in sis$treated) out[paste0(tr, "minusBasket", cs, "_Delta_", tk)] <- out[paste0(tr, "_Delta_", tk)] - m
    }
  }
  list(stats = out, err = err)
}
joint_task <- function(tk) {
  s <- get(".SIS_V6", envir = globalenv())[[tk$cn]]
  calc_joint(s, s$master_pre[tk$ip], s$master_post[tk$iq], tk$zp, tk$zq)
}
run_joint_boot <- function(sistemas, B, seed, noise = TRUE, timeout = 180) {
  set.seed(seed)
  tasks <- list()
  for (cn in names(sistemas)) {
    np <- length(sistemas[[cn]]$master_pre); nq <- length(sistemas[[cn]]$master_post)
    for (b in 1:B) tasks[[length(tasks) + 1]] <- list(cn = cn, b = b, ip = mbb_idx(np), iq = mbb_idx(nq),
      zp = if (noise) ruido(np) else NULL, zq = if (noise) ruido(nq) else NULL)
  }
  assign(".SIS_V6", sistemas, envir = globalenv())
  r <- par_map_timeout(tasks, joint_task, n_workers = N_WORKERS, timeout = timeout,
                       export_names = ".SIS_V6", export_env = globalenv())
  out <- list()
  for (cn in names(sistemas)) {
    ii <- which(sapply(tasks, `[[`, "cn") == cn)
    nomes <- names(calc_joint_names(sistemas[[cn]]))
    mat <- do.call(rbind, lapply(r[ii], function(x) if (is.null(x$stats)) setNames(rep(NA_real_, length(nomes)), nomes) else x$stats[nomes]))
    valid <- apply(mat, 1, function(x) all(is.finite(x)))
    out[[cn]] <- list(mat = mat, valid = valid, errs = unlist(lapply(r[ii], `[[`, "err")))
  }
  attr(out, "n_timeout") <- attr(r, "n_timeout")
  out
}
## nomes das estatisticas (via estimativa pontual guardada no sistema)
calc_joint_names <- function(sis) sis$point
point_joint <- function(sistemas) {
  for (cn in names(sistemas)) {
    s <- sistemas[[cn]]
    sistemas[[cn]]$point <- calc_joint(s, s$master_pre, s$master_post)$stats
  }
  sistemas
}
tab_joint <- function(sistemas, bt) {
  do.call(rbind, lapply(names(sistemas), function(cn) {
    mat <- bt[[cn]]$mat; mat[!bt[[cn]]$valid, ] <- NA
    do.call(rbind, lapply(colnames(mat), function(st)
      resumir_v6(unname(sistemas[[cn]]$point[st]), mat[, st], cut = cn, statistic = st)))
  }))
}

## ---------------------------------------------------------------------
## Dados: amostras V4/V5 e series de controle (construcao identica ao 42)
## ---------------------------------------------------------------------
ler_vix <- function() {
  v <- read.csv("data/clean/vix_fred.csv"); v$date <- as.Date(v$date)
  v <- v[!is.na(v$VIXCLS), ]
  xts(v$VIXCLS, order.by = v$date, dimnames = list(NULL, "VIX"))
}
## precos diarios (date, close) -> retorno construcao (ii): intersecao com
## calendario do VIX PRIMEIRO, log-retorno x100 entre pregoes consecutivos
ret_ii <- function(date, close, vix_xts, nome) {
  px <- xts(close, order.by = as.Date(date))
  px <- px[!is.na(px)]
  comuns <- as.Date(intersect(index(px), index(vix_xts)))
  pxi <- px[comuns]
  r <- diff(log(pxi)) * 100
  Z <- merge(r, vix_xts[index(r)], join = "left"); colnames(Z) <- c(nome, "VIX")
  na.omit(Z)
}
CTRL_ARQ <- c(XRP = "data/clean/cesta_xrp_binance.csv", TRX = "data/clean/cesta_trx_binance.csv",
              ADA = "data/clean/cesta_ada_binance.csv", XLM = "data/clean/cesta_xlm_binance.csv",
              LTC = "data/clean/cesta_ltc_binance.csv")
TRUNCA <- c(XRP = "2025-11-12", TRX = "2026-09-08", LTC = "2025-10-27", ADA = NA, XLM = NA, SOL = "2025-10-27")
CONTROLES <- c("XRP","LTC","XLM","ADA","TRX")
CESTAS <- list(A = c("ADA","XLM"), B = c("ADA","XLM","XRP","TRX","LTC"))
DATE_INI <- as.Date("2018-01-01"); DATE_FIM <- as.Date("2026-09-18")
serie_ctrl_xts <- function(ativo, vix_xts, arq = CTRL_ARQ[ativo], trunca = TRUNCA[ativo]) {
  d <- read.csv(arq); d$date <- as.Date(d$date); d <- d[order(d$date), ]
  Z <- ret_ii(d$date, d$close, vix_xts, ativo)
  Z <- Z[index(Z) >= DATE_INI & index(Z) <= DATE_FIM]
  if (!is.na(trunca)) Z <- Z[index(Z) <= as.Date(trunca)]
  Z
}
to_list <- function(Z, taus = NULL) list(date = as.Date(index(Z)), M = coredata(Z), taus = taus)
