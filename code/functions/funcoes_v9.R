# =====================================================================
# V9 — FUNCOES COMUNS (origem: v9_00_funcoes.R; carregadas pelos scripts
# code/analysis/21_* a 27_* e pelos trabalhadores paralelos; carregadas
# tambem por funcoes_v10.R). Reutiliza integralmente a cadeia da V7:
#   funcoes_qvar_bootstrap.R -> funcoes_contrafactual.R -> funcoes_pipeline.R
# (npdc_one/npdc_taus, mbb_idx, sb_idx, ruido, stats_from_npdc,
#  par_map_timeout com timeout por replica, point_joint, resumir_v6,
#  ret_ii, serie_ctrl_xts, carregar_v4 -> base V7, ...).
# Nada da V7 e alterado. DIFERENCAS introduzidas aqui (documentadas):
#  (1) qvar_fit()/npdc_fast(): reimplementacao do QVAR do pacote
#      ConnectednessApproach (mesmo embed(), mesmo rq(method = "fn"),
#      mesma Q = t(Res) %*% Res / n, mesma ConnectednessApproach::FEVD
#      generalizada). Numericamente identica a npdc_one() (verificado em
#      v9_01: dif. max. ~1e-12), mas (a) nao chama summary.rq() (erro-padrao
#      nao usado na conectividade, origem dos travamentos), (b) devolve os
#      componentes theta_{a<-VIX} e theta_{VIX<-a}, coeficientes, residuos
#      e o maior modulo de autovalor da matriz companheira, (c) aceita
#      'seg' (segmentos): as defasagens sao montadas DENTRO de cada
#      segmento, de modo que as p primeiras observacoes apos uma lacuna
#      saem como variavel dependente e as defasagens nunca atravessam a
#      lacuna (regra das exclusoes da Tarefa 5).
#  (2) Bootstrap corrigido (Tarefa 1a): reamostra blocos de linhas de
#      (Dlog VIX_t, r_a,t) e reconstroi o nivel
#      VIX*_1 = VIX_0 (nivel observado na 1a data da subamostra),
#      VIX*_t = VIX_0 * exp(sum_{s=2..t} Dlog VIX*_s),
#      de modo que a reamostra identidade reproduz exatamente o VIX
#      observado; ruido U(-0.005, 0.005) somado ao nivel reconstruido.
#      Dlog VIX_t = log VIX_t - log VIX do pregao anterior (FRED), i.e. a
#      variacao de um dia que acompanha o retorno r_a,t.
#      Especificacoes alternativas: 'log' usa log(VIX* + ruido); 'dlog'
#      reconstroi o caminho desde o nivel da vespera da 1a data, soma o
#      ruido ao nivel e rediferencia (100 x Dlog).
#      No repositorio, este esquema (pre-registrado numa revisao anterior e
#      reprovado nos diagnosticos de validade da V10) so e usado como
#      esquema de referencia nos diagnosticos (21_*, 28_*; Tabela S7). A
#      inferencia final usa as linhas do desenho (funcoes_v10.R).
#  (3) Saidas em results/ (OUT6 redirecionado; log.txt e progresso.md).
# =====================================================================

source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
FUNCS_PATH <- file.path(PROJ_DIR, "code/functions/funcoes_v9.R")
suppressPackageStartupMessages(library(quantreg))

OUT9 <- "results"
OUT6 <- OUT9                                   # log_v6() -> results/log.txt
for (d in c("csv","rds","fig","logs")) dir.create(file.path(OUT9, d), recursive = TRUE, showWarnings = FALSE)
SEED_V9 <- 20261015                            # semente da tarefa k = SEED_V9 + k
N_WORKERS <- 14
B_FULL <- 500; B_TEST <- 20; B_BCA <- 2000

log_v9 <- function(...) log_v6(...)
progresso <- function(txt) {
  f <- file.path(OUT9, "progresso.md")
  if (!file.exists(f)) writeLines(c("# V9 — progresso", ""), f)
  cat(paste0("- [", format(Sys.time(), "%Y-%m-%d %H:%M"), "] ", txt, "\n"), file = f, append = TRUE)
}
log_pkgs9 <- function(script) {
  pk <- c("ConnectednessApproach","quantreg","zoo","xts","rugarch","sandwich","flextable","officer","ggplot2")
  v <- sapply(pk, function(p) tryCatch(as.character(packageVersion(p)), error = function(e) "n/a"))
  log_v9("Script ", script, " | ", R.version.string, " | ", paste(names(v), v, sep = " ", collapse = "; "))
}

## ---------------------------------------------------------------------
## (1) QVAR reimplementado (identico ao ConnectednessApproach::QVAR)
## ---------------------------------------------------------------------
mk_seg <- function(dates_kept, dates_full) {        # novo segmento apos cada lacuna
  pos <- match(dates_kept, dates_full)
  cumsum(c(1L, as.integer(diff(pos) > 1)))
}
qvar_fit <- function(M, tau, nlag = NLAG, seg = NULL) {
  M <- as.matrix(M); k <- ncol(M); nm <- colnames(M)
  if (is.null(seg) || length(unique(seg)) == 1) z <- embed(M, nlag + 1)
  else z <- do.call(rbind, lapply(split(seq_len(nrow(M)), factor(seg, levels = unique(seg))), function(ii)
    if (length(ii) > nlag) embed(M[ii, , drop = FALSE], nlag + 1) else NULL))
  X <- z[, -(1:k), drop = FALSE]
  B <- NULL; Res <- NULL
  for (i in 1:k) {
    fit <- suppressWarnings(rq(z[, i] ~ X, tau = tau, method = "fn"))
    B <- rbind(B, fit$coefficients[-1]); Res <- cbind(Res, fit$residuals)
  }
  Q <- t(Res) %*% Res / nrow(Res); dimnames(Q) <- list(nm, nm); rownames(B) <- nm; colnames(Res) <- nm
  list(B = B, Q = Q, Res = Res, n_eff = nrow(z), n_dropped = nrow(M) - nrow(z), k = k, nlag = nlag, names = nm)
}
ct_from_fit <- function(f, nfore = NFORE) {
  CT <- ConnectednessApproach::FEVD(Phi = f$B, Sigma = f$Q, nfore = nfore, generalized = TRUE, type = "time")$FEVD * 100
  dimnames(CT) <- list(f$names, f$names); CT
}
comp_maxmod <- function(f) {
  k <- f$k; p <- f$nlag
  A <- if (p == 1) f$B else rbind(f$B, cbind(diag(k * (p - 1)), matrix(0, k * (p - 1), k)))
  max(Mod(eigen(A, only.values = TRUE)$values))
}
vix_own_sum <- function(f) { iv <- which(f$names == "VIX"); sum(f$B[iv, (0:(f$nlag - 1)) * f$k + iv]) }
## NPDC e componentes num tau
npdc_fast <- function(M, tau, nlag = NLAG, nfore = NFORE, seg = NULL) {
  f <- qvar_fit(M, tau, nlag, seg); CT <- ct_from_fit(f, nfore)
  a <- setdiff(f$names, "VIX")
  c(NPDC = CT["VIX", a] - CT[a, "VIX"], a_from_VIX = CT[a, "VIX"], VIX_from_a = CT["VIX", a])
}
## varios taus: lista(npdc, a_from_VIX, VIX_from_a) nomeados por tau; atributo 'erro'
npdc_taus9 <- function(M, taus = TAUS, nlag = NLAG, nfore = NFORE, seg = NULL) {
  k <- sprintf("%.2f", taus); out <- matrix(NA_real_, 3, length(taus), dimnames = list(c("NPDC","a_from_VIX","VIX_from_a"), k))
  err <- NULL
  for (i in seq_along(taus)) {
    r <- tryCatch(npdc_fast(M, taus[i], nlag, nfore, seg), error = function(e) e)
    if (inherits(r, "error")) { err <- conditionMessage(r); break }
    out[, i] <- r
  }
  attr(out, "erro") <- err
  out
}

## ---------------------------------------------------------------------
## (2) Dados por subamostra para o bootstrap corrigido
## S = list(M = cbind(ativo, VIX) [nivel], d = Dlog VIX (log, nao x100),
##          date, seg (segmentos p/ estimativa pontual; NULL = contiguo))
## ---------------------------------------------------------------------
VIX_RAW <- NULL
dlog_vix_raw <- function() {
  if (is.null(VIX_RAW)) { vx <- ler_vix(); d <- diff(log(vx)); d <- d[!is.na(d)]; colnames(d) <- "dlv"; VIX_RAW <<- d }
  VIX_RAW
}
mk_S <- function(Z, seg = NULL) {          # Z: xts/zoo (ativo, VIX) ja na subamostra
  dl <- dlog_vix_raw(); dt <- as.Date(index(Z))
  d <- as.numeric(dl[dt]); if (length(d) != length(dt) || any(!is.finite(d))) stop("Dlog VIX ausente para alguma data")
  list(M = coredata(Z), d = d, date = dt, seg = seg)
}
## transforma a amostra OBSERVADA conforme a especificacao
obs_matrix <- function(S, spec = "level") {
  M <- S$M
  if (spec == "log") M[, "VIX"] <- log(M[, "VIX"])
  if (spec == "dlog") M[, "VIX"] <- 100 * S$d
  M
}
## reamostra reconstruida (idx: indices de linhas; z: ruido de comprimento n + 1)
recon <- function(S, idx, z = NULL, spec = "level") {
  n <- length(idx); r <- S$M[idx, 1]; d <- S$d[idx]
  if (spec == "dlog") {
    P <- (S$M[1, "VIX"] * exp(-S$d[1])) * exp(cumsum(c(0, d)))          # n + 1 niveis
    if (!is.null(z)) P <- P + z
    v <- 100 * diff(log(P)); lev <- P[-1]
  } else {
    lev <- S$M[1, "VIX"] * exp(c(0, cumsum(d[-1])))
    v <- if (!is.null(z)) lev + z[-1] else lev
    if (spec == "log") v <- log(v)
  }
  M <- cbind(r, v); colnames(M) <- colnames(S$M)
  attr(M, "range") <- range(lev)
  M
}
idx_fun <- function(scheme) if (scheme == "SB") sb_idx else mbb_idx

## ---------------------------------------------------------------------
## Bootstrap SEPARADO por subamostra, esquema corrigido
## pairs: list(NOME = list(pre = S, post = S)); devolve NPDC e componentes
## ---------------------------------------------------------------------
sep_task9 <- function(tk) {
  P <- get(".PAIRS_V9", envir = globalenv())[[tk$nm]]
  out <- list(err = NULL)
  for (sub in c("pre","post")) {
    S <- P[[sub]]; ii <- if (sub == "pre") tk$ip else tk$iq; zz <- if (sub == "pre") tk$zp else tk$zq
    M <- recon(S, ii, zz, tk$spec)
    v <- npdc_taus9(M, tk$taus, nlag = tk$nlag, nfore = tk$nfore)
    out[[sub]] <- v; out[[paste0("range_", sub)]] <- attr(M, "range")
    if (!is.null(attr(v, "erro"))) out$err <- c(out$err, paste(sub, attr(v, "erro")))
  }
  out
}
run_sep_boot9 <- function(pairs, B, seed, taus = TAUS, spec = "level", nlag = NLAG, nfore = NFORE,
                          scheme = "MBB", noise = TRUE, timeout = 120) {
  set.seed(seed); f <- idx_fun(scheme)
  tasks <- list()
  for (nm in names(pairs)) {
    np <- nrow(pairs[[nm]]$pre$M); nq <- nrow(pairs[[nm]]$post$M)
    for (b in 1:B) tasks[[length(tasks) + 1]] <- list(nm = nm, b = b, taus = taus, spec = spec, nlag = nlag, nfore = nfore,
      ip = f(np), iq = f(nq), zp = if (noise) ruido(np + 1) else NULL, zq = if (noise) ruido(nq + 1) else NULL)
  }
  assign(".PAIRS_V9", pairs, envir = globalenv())
  r <- par_map_timeout(tasks, sep_task9, n_workers = N_WORKERS, timeout = timeout,
                       export_names = ".PAIRS_V9", export_env = globalenv())
  k <- sprintf("%.2f", taus); out <- list()
  for (nm in names(pairs)) {
    ii <- which(sapply(tasks, `[[`, "nm") == nm)
    grab <- function(sub, row) { m <- do.call(rbind, lapply(r[ii], function(x) if (is.null(x[[sub]])) rep(NA, length(taus)) else x[[sub]][row, ])); colnames(m) <- k; m }
    pre <- grab("pre", "NPDC"); post <- grab("post", "NPDC")
    valid <- apply(cbind(pre, post), 1, function(x) all(is.finite(x)))
    rng <- do.call(rbind, lapply(r[ii], function(x) c(pre_min = x$range_pre[1] %||% NA, pre_max = x$range_pre[2] %||% NA,
                                                     post_min = x$range_post[1] %||% NA, post_max = x$range_post[2] %||% NA)))
    out[[nm]] <- list(pre = pre, post = post, valid = valid,
                      pre_aV = grab("pre", "a_from_VIX"), pre_Va = grab("pre", "VIX_from_a"),
                      post_aV = grab("post", "a_from_VIX"), post_Va = grab("post", "VIX_from_a"),
                      range = rng, errs = unlist(lapply(r[ii], `[[`, "err")))
  }
  attr(out, "n_timeout") <- attr(r, "n_timeout")
  out
}
`%||%` <- function(a, b) if (is.null(a)) b else a

## pontuais (amostra observada; com 'seg' se houver lacunas)
point_sep9 <- function(pairs, taus = TAUS, spec = "level", nlag = NLAG, nfore = NFORE) lapply(pairs, function(p) {
  vp <- npdc_taus9(obs_matrix(p$pre, spec), taus, nlag, nfore, seg = p$pre$seg)
  vq <- npdc_taus9(obs_matrix(p$post, spec), taus, nlag, nfore, seg = p$post$seg)
  list(pre = vp["NPDC", ], post = vq["NPDC", ], pre_full = vp, post_full = vq, stats = stats_gen(vp["NPDC", ], vq["NPDC", ], taus),
       n_pre = nrow(p$pre$M), n_post = nrow(p$post$M),
       dropped_pre = if (is.null(p$pre$seg)) nlag else nlag * length(unique(p$pre$seg)),
       dropped_post = if (is.null(p$post$seg)) nlag else nlag * length(unique(p$post$seg)))
})
## tabela (pontual, mediana, IC percentil, validas...) para Delta/DD/dAsym
tab_sep9 <- function(pts, bt, taus = TAUS) {
  rows <- list()
  for (nm in names(pts)) {
    S <- do.call(rbind, lapply(seq_len(nrow(bt[[nm]]$pre)), function(b) stats_gen(bt[[nm]]$pre[b, ], bt[[nm]]$post[b, ], taus)))
    S[!bt[[nm]]$valid, ] <- NA
    for (st in names(pts[[nm]]$stats))
      rows[[length(rows) + 1]] <- resumir_v6(unname(pts[[nm]]$stats[st]), S[, st], series = nm, statistic = st)
  }
  do.call(rbind, rows)
}
boot_stats <- function(bt1, taus = TAUS) {
  S <- do.call(rbind, lapply(seq_len(nrow(bt1$pre)), function(b) stats_gen(bt1$pre[b, ], bt1$post[b, ], taus)))
  S[!bt1$valid, ] <- NA; S
}

## ---------------------------------------------------------------------
## Bootstrap CONJUNTO POR DATA, esquema corrigido (adaptado de calc_joint
## da V6): cada serie reconstroi o proprio VIX* a partir do seu Dlog VIX
## nas datas sorteadas (mesmo sorteio para todas), ancorado no VIX
## observado na 1a data da serie na subamostra; ruido por posicao sorteada.
## Series: list(date, M, d, seg_pre?, seg_post?)
## ---------------------------------------------------------------------
calc_joint9 <- function(sis, datas_pre, datas_post, noise_pre = NULL, noise_post = NULL, boot = FALSE) {
  out <- c(); err <- NULL
  nl <- if (is.null(sis$nlag)) NLAG else sis$nlag
  for (nm in names(sis$series)) {
    s <- sis$series[[nm]]
    taus <- if (!is.null(s$taus)) s$taus else sis$taus
    npdc <- list()
    for (sub in c("pre","post")) {
      dd <- if (sub == "pre") datas_pre else datas_post
      nz <- if (sub == "pre") noise_pre else noise_post
      pos <- match(dd, s$date); keep <- !is.na(pos)
      if (boot) {
        msub <- if (sub == "pre") sis$master_pre else sis$master_post
        anchor <- s$M[match(TRUE, s$date %in% msub), "VIX"]   # VIX observado na 1a data da serie na subamostra
        idx <- pos[keep]
        lev <- anchor * exp(c(0, cumsum(s$d[idx][-1])))
        if (!is.null(nz)) lev <- lev + nz[keep]
        M <- cbind(s$M[idx, 1], lev); colnames(M) <- colnames(s$M); seg <- NULL
      } else {
        M <- s$M[pos[keep], , drop = FALSE]
        seg <- s[[paste0("seg_", sub)]]
      }
      v <- npdc_taus9(M, taus, nlag = nl, seg = seg)
      if (!is.null(attr(v, "erro"))) err <- c(err, paste(nm, sub, attr(v, "erro")))
      npdc[[sub]] <- v["NPDC", ]
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
joint_task9 <- function(tk) {
  s <- get(".SIS_V9", envir = globalenv())[[tk$cn]]
  calc_joint9(s, s$master_pre[tk$ip], s$master_post[tk$iq], tk$zp, tk$zq, boot = TRUE)
}
point_joint9 <- function(sistemas) {
  for (cn in names(sistemas)) { s <- sistemas[[cn]]; sistemas[[cn]]$point <- calc_joint9(s, s$master_pre, s$master_post)$stats }
  sistemas
}
run_joint_boot9 <- function(sistemas, B, seed, noise = TRUE, timeout = 300) {
  set.seed(seed)
  tasks <- list()
  for (cn in names(sistemas)) {
    np <- length(sistemas[[cn]]$master_pre); nq <- length(sistemas[[cn]]$master_post)
    for (b in 1:B) tasks[[length(tasks) + 1]] <- list(cn = cn, b = b, ip = mbb_idx(np), iq = mbb_idx(nq),
      zp = if (noise) ruido(np) else NULL, zq = if (noise) ruido(nq) else NULL)
  }
  assign(".SIS_V9", sistemas, envir = globalenv())
  r <- par_map_timeout(tasks, joint_task9, n_workers = N_WORKERS, timeout = timeout,
                       export_names = ".SIS_V9", export_env = globalenv())
  out <- list()
  for (cn in names(sistemas)) {
    ii <- which(sapply(tasks, `[[`, "cn") == cn)
    nomes <- names(sistemas[[cn]]$point)
    mat <- do.call(rbind, lapply(r[ii], function(x) if (is.null(x$stats)) setNames(rep(NA_real_, length(nomes)), nomes) else x$stats[nomes]))
    valid <- apply(mat, 1, function(x) all(is.finite(x)))
    out[[cn]] <- list(mat = mat, valid = valid, errs = unlist(lapply(r[ii], `[[`, "err")))
  }
  attr(out, "n_timeout") <- attr(r, "n_timeout")
  out
}
tab_joint9 <- function(sistemas, bt) tab_joint(sistemas, bt)
## lista de serie para o sistema conjunto (com Dlog VIX e segmentos opcionais)
to_list9 <- function(Z, taus = NULL, full_dates = NULL, cut = NULL) {
  dt <- as.Date(index(Z)); dl <- dlog_vix_raw(); d <- as.numeric(dl[dt])
  if (any(!is.finite(d))) stop("Dlog VIX ausente")
  L <- list(date = dt, M = coredata(Z), d = d, taus = taus)
  if (!is.null(full_dates)) {   # serie com lacunas: segmentos por subamostra
    ip <- dt < cut; iq <- dt >= cut
    L$seg_pre <- mk_seg(dt[ip], full_dates[full_dates < cut]); L$seg_post <- mk_seg(dt[iq], full_dates[full_dates >= cut])
    if (length(unique(L$seg_pre)) == 1) L$seg_pre <- NULL
    if (length(unique(L$seg_post)) == 1) L$seg_post <- NULL
  }
  L
}

## ---------------------------------------------------------------------
## Formatacao de tabelas (estrutura de v7_30_tabelas_figuras.R)
## ---------------------------------------------------------------------
round_df <- function(df, d) { for (k in names(df)) if (is.numeric(df[[k]]) && !all(is.na(df[[k]]) | df[[k]] == round(df[[k]]))) df[[k]] <- round(df[[k]], d); df }
sig_flag <- function(lo, hi) ifelse(is.na(lo) | is.na(hi), NA, lo > 0 | hi < 0)
