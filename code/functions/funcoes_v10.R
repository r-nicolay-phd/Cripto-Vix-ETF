# =====================================================================
# V10 — FUNCOES COMUNS (origem: v10_00_funcoes.R; carregadas pelos scripts
# code/analysis/28_* a 32_* e pelos trabalhadores paralelos). Reutiliza a
# cadeia funcoes_qvar_bootstrap.R -> funcoes_contrafactual.R ->
# funcoes_pipeline.R -> funcoes_v9.R (qvar_fit, ct_from_fit, comp_maxmod,
#  npdc_taus9, recon, mk_S, mk_seg, to_list9, ...; mbb_idx, sb_idx,
#  ruido, par_map_timeout com timeout por replica). Nada e alterado.
# NOVO (Prompt 6, Decisao 1):
#  (1) Linhas do desenho: para cada subamostra, z_t = (y_t, y_{t-1}, y_{t-2})
#      com y = (ativo, VIX), montadas DENTRO da subamostra (e de cada
#      segmento, se houver lacunas). Cada linha guarda os indices das
#      linhas de origem (src: dependente, defasagem 1, defasagem 2).
#  (2) qvar_fit_YX(): mesmo QVAR de qvar_fit()/ConnectednessApproach
#      (rq(method = "fn") equacao a equacao, Q = t(Res) Res / n, FEVD
#      generalizada), mas estimado diretamente em matrizes Y e X.
#  (3) Ruido U(-0.005, 0.005) sorteado UMA vez por data original
#      (calendario do VIX/FRED) e somado ao VIX dessa data onde quer que
#      ele apareca (dependente e defasagens). Especificacoes: nivel,
#      log(VIX + u), 100 x Dlog com ruido nas duas datas.
#  (4) Esquemas: MBB de linhas do desenho (bloco 20; principal), bootstrap
#      estacionario de linhas (bloco medio 20), sieve VAR(2) em medias
#      (blocos de 20 residuos centrados, regeneracao recursiva).
#  (5) Diagnosticos de validade (faixa do VIX*, razao de emendas, validas).
#  (6) Contrafactual conjunto por data com linhas do desenho.
# =====================================================================

source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_v9.R"))
FUNCS_PATH <- file.path(PROJ_DIR, "code/functions/funcoes_v10.R")
Sys.setlocale("LC_TIME", "C")

OUT10 <- "results"
OUT6 <- OUT10                                    # log_v6() -> results/log.txt
for (d in c("csv","rds","fig","logs")) dir.create(file.path(OUT10, d), recursive = TRUE, showWarnings = FALSE)
SEED_V10 <- 20261016                             # semente da tarefa k = SEED_V10 + k
N_WORKERS <- 14
B_FULL <- 500; B_TEST <- 20; B_BCA <- 2000
HS <- c(10, 5, 1)                                # horizontes calculados nas mesmas estimativas

log_v10 <- function(...) log_v6(...)
progresso <- function(txt) {
  f <- file.path(OUT10, "progresso.md")
  if (!file.exists(f)) writeLines(c("# V10 — progresso", ""), f)
  cat(paste0("- [", format(Sys.time(), "%Y-%m-%d %H:%M"), "] ", txt, "\n"), file = f, append = TRUE)
}
log_pkgs10 <- function(script) {
  pk <- c("ConnectednessApproach","quantreg","zoo","xts","rugarch","flextable","officer","ggplot2")
  v <- sapply(pk, function(p) tryCatch(as.character(packageVersion(p)), error = function(e) "n/a"))
  log_v10("Script ", script, " | ", R.version.string, " | ", paste(names(v), v, sep = " ", collapse = "; "))
}
RDS10 <- function(nm) file.path(OUT10, "rds", paste0(nm, ".rds"))
run_or_load10 <- function(nm, expr) { f <- RDS10(nm); if (file.exists(f)) { log_v10("bloco ", nm, " ja existe -- carregado"); return(readRDS(f)) }
  x <- eval.parent(substitute(expr)); saveRDS(x, f); x }

## ---------------------------------------------------------------------
## Calendario do VIX (FRED) para o ruido por data
## ---------------------------------------------------------------------
VX_CAL <- NULL
vx_cal <- function() { if (is.null(VX_CAL)) { v <- ler_vix(); VX_CAL <<- list(date = as.Date(index(v)), lev = as.numeric(v)) }; VX_CAL }

## indices de origem das linhas do desenho (dependente, lag 1, ..., lag p)
mk_src <- function(n, nlag = NLAG, seg = NULL) {
  if (is.null(seg) || length(unique(seg)) == 1) return(embed(seq_len(n), nlag + 1))
  do.call(rbind, lapply(split(seq_len(n), factor(seg, levels = unique(seg))), function(ii)
    if (length(ii) > nlag) embed(ii, nlag + 1) else NULL))
}
## Subamostra com desenho. M: (ativo, VIX) [nivel observado]; date; seg
mk_D <- function(M, date, seg = NULL, nlag = NLAG) {
  M <- as.matrix(M); date <- as.Date(date); cal <- vx_cal()
  g <- match(date, cal$date); if (any(is.na(g))) stop("data fora do calendario do VIX")
  if (max(abs(cal$lev[g] - M[, "VIX"])) > 1e-8) stop("VIX da amostra difere do FRED")
  src <- mk_src(nrow(M), nlag, seg)
  list(M = M, date = date, seg = seg, nlag = nlag, src = src, rdate = date[src[, 1]], g = g, gprev = g - 1L,
       vprev = cal$lev[g - 1L], iv = which(colnames(M) == "VIX"), names = colnames(M))
}
D_from_xts <- function(Z, seg = NULL, nlag = NLAG) mk_D(coredata(Z), as.Date(index(Z)), seg, nlag)
## janela de ids do calendario coberta por um conjunto de subamostras
g_window <- function(Ds) { lo <- min(sapply(Ds, function(d) min(d$gprev))); hi <- max(sapply(Ds, function(d) max(d$g))); c(lo, hi) }
draw_noise <- function(win) runif(win[2] - win[1] + 1, -NOISE_A, NOISE_A)

## VIX transformado em cada linha de M, com ruido por data (u na janela win)
vix_t <- function(Dd, u = NULL, win = NULL, spec = "level") {
  v <- Dd$M[, "VIX"]; vp <- Dd$vprev
  if (!is.null(u)) { v <- v + u[Dd$g - win[1] + 1L]; vp <- vp + u[Dd$gprev - win[1] + 1L] }
  switch(spec, level = v, log = log(v), dlog = 100 * (log(v) - log(vp)))
}
## matrizes Y, X das linhas 'idx' do desenho
build_YX <- function(Dd, idx = NULL, u = NULL, win = NULL, spec = "level") {
  Mt <- Dd$M; Mt[, "VIX"] <- vix_t(Dd, u, win, spec)
  src <- if (is.null(idx)) Dd$src else Dd$src[idx, , drop = FALSE]
  Y <- Mt[src[, 1], , drop = FALSE]
  X <- do.call(cbind, lapply(2:ncol(src), function(j) Mt[src[, j], , drop = FALSE]))
  list(Y = Y, X = X)
}

## ---------------------------------------------------------------------
## QVAR em matrizes Y, X (identico a qvar_fit / ConnectednessApproach)
## ---------------------------------------------------------------------
qvar_fit_YX <- function(Y, X, tau, nlag = NLAG) {
  k <- ncol(Y); nm <- colnames(Y); B <- NULL; Res <- NULL; b_vix <- NULL
  for (i in 1:k) {
    fit <- suppressWarnings(rq(Y[, i] ~ X, tau = tau, method = "fn"))
    B <- rbind(B, fit$coefficients[-1]); Res <- cbind(Res, fit$residuals)
    if (nm[i] == "VIX") b_vix <- fit$coefficients
  }
  Q <- t(Res) %*% Res / nrow(Res); dimnames(Q) <- list(nm, nm); rownames(B) <- nm; colnames(Res) <- nm
  list(B = B, Q = Q, Res = Res, b_vix = b_vix, n_eff = nrow(Y), k = k, nlag = nlag, names = nm)
}
## NPDC e componentes em varios taus e horizontes: array [3, taus, H]
npdc_YX <- function(Y, X, taus = TAUS, nfores = HS, nlag = NLAG, keep_fit_tau = NULL) {
  a <- setdiff(colnames(Y), "VIX")
  out <- array(NA_real_, c(3, length(taus), length(nfores)),
               dimnames = list(c("NPDC","a_from_VIX","VIX_from_a"), sprintf("%.2f", taus), paste0("H", nfores)))
  err <- NULL; kf <- NULL
  for (i in seq_along(taus)) {
    f <- tryCatch(qvar_fit_YX(Y, X, taus[i], nlag), error = function(e) e)
    if (inherits(f, "error")) { err <- conditionMessage(f); break }
    if (!is.null(keep_fit_tau) && abs(taus[i] - keep_fit_tau) < 1e-9) kf <- f
    for (h in seq_along(nfores)) {
      CT <- tryCatch(ct_from_fit(f, nfores[h]), error = function(e) e)
      if (inherits(CT, "error")) { err <- conditionMessage(CT); break }
      out[, i, h] <- c(CT["VIX", a] - CT[a, "VIX"], CT[a, "VIX"], CT["VIX", a])
    }
    if (!is.null(err)) break
  }
  attr(out, "erro") <- err; attr(out, "fit") <- kf
  out
}

## ---------------------------------------------------------------------
## Emendas: 1as 2 linhas apos cada fronteira nao contigua da sequencia idx
## ---------------------------------------------------------------------
first2_flag <- function(idx) {
  m <- length(idx); spl <- which(c(FALSE, idx[-1] != idx[-m] + 1L))
  f <- rep(FALSE, m); f[spl] <- TRUE; s2 <- spl + 1L; f[s2[s2 <= m]] <- TRUE; f
}
## residuo da equacao do VIX com os coeficientes (com intercepto) b
res_vix <- function(YX, b) as.numeric(YX$Y[, "VIX"] - cbind(1, YX$X) %*% b)
splice_counts <- function(f2, pos_orig, pos_refit) c(m = length(f2), n_f2 = sum(f2), n_pos_orig = sum(pos_orig), n_pos_orig_f2 = sum(pos_orig & f2),
                                                     n_pos_refit = sum(pos_refit), n_pos_refit_f2 = sum(pos_refit & f2))
splice_ratio <- function(C) {               # C: matriz de contagens (replicas x 6); razao agregada
  s <- colSums(C, na.rm = TRUE)
  c(share_first2 = s[["n_f2"]] / s[["m"]],
    share_pos_orig_in_first2 = s[["n_pos_orig_f2"]] / s[["n_pos_orig"]], ratio_orig = (s[["n_pos_orig_f2"]] / s[["n_pos_orig"]]) / (s[["n_f2"]] / s[["m"]]),
    share_pos_refit_in_first2 = s[["n_pos_refit_f2"]] / s[["n_pos_refit"]], ratio_refit = (s[["n_pos_refit_f2"]] / s[["n_pos_refit"]]) / (s[["n_f2"]] / s[["m"]]))
}

## ---------------------------------------------------------------------
## Preparo de um par para o bootstrap separado: desenho, coef. tau = 0,95
## originais (diagnostico de emendas) e parametros do sieve VAR(2)
## ---------------------------------------------------------------------
sieve_fit <- function(M, nlag = NLAG) {
  M <- as.matrix(M); n <- nrow(M); k <- ncol(M); z <- embed(M, nlag + 1)
  Y <- z[, 1:k]; X <- cbind(1, z[, -(1:k)])
  Bh <- solve(crossprod(X), crossprod(X, Y))                     # (1 + k p) x k
  E <- Y - X %*% Bh; Ec <- sweep(E, 2, colMeans(E))
  A <- t(Bh[-1, , drop = FALSE]); rownames(A) <- colnames(M)      # k x (k p)
  Cm <- if (nlag == 1) A else rbind(A, cbind(diag(k * (nlag - 1)), matrix(0, k * (nlag - 1), k)))
  list(c0 = Bh[1, ], A = A, E = Ec, init = M[1:nlag, , drop = FALSE], maxmod = max(Mod(eigen(Cm, only.values = TRUE)$values)), n = n, k = k, nlag = nlag)
}
prep_pair <- function(pre, post) {
  out <- list(pre = pre, post = post); out$win <- g_window(list(pre, post))
  for (s in c("pre","post")) { Dd <- out[[s]]; YX <- build_YX(Dd)
    f95 <- qvar_fit_YX(YX$Y, YX$X, 0.95, Dd$nlag)
    out[[s]]$b95 <- f95$b_vix; out[[s]]$pos95 <- res_vix(YX, f95$b_vix) > 0
    out[[s]]$sieve <- if (is.null(Dd$seg)) sieve_fit(Dd$M, Dd$nlag) else NULL }
  out
}

## ---------------------------------------------------------------------
## Bootstrap SEPARADO por subamostra (Tabela 1 e afins)
## scheme: "MBB" | "SB" (linhas do desenho) | "SIEVE"
## ---------------------------------------------------------------------
regen_sieve <- function(sv, ie, z) {
  n <- sv$n; k <- sv$k; p <- sv$nlag; Y <- matrix(NA_real_, n, k); Y[1:p, ] <- sv$init
  for (t in (p + 1):n) { x <- as.numeric(t(Y[(t - 1):(t - p), , drop = FALSE])); Y[t, ] <- sv$c0 + sv$A %*% x + sv$E[ie[t - p], ] }
  colnames(Y) <- colnames(sv$init); if (!is.null(z)) Y[, "VIX"] <- Y[, "VIX"] + z
  Y
}
sep_task10 <- function(tk) {
  P <- get(".PAIRS_V10", envir = globalenv())[[tk$nm]]
  out <- list(err = NULL)
  for (sub in c("pre","post")) {
    Dd <- P[[sub]]; ii <- tk[[paste0("i_", sub)]]
    if (tk$scheme == "SIEVE") {
      Ms <- regen_sieve(Dd$sieve, ii, tk[[paste0("z_", sub)]])
      z <- embed(Ms, Dd$nlag + 1); YX <- list(Y = z[, 1:2, drop = FALSE], X = z[, -(1:2), drop = FALSE]); colnames(YX$Y) <- colnames(Ms)
      f2 <- first2_flag(ii); rng <- range(Ms[, "VIX"])
    } else {
      YX <- build_YX(Dd, ii, tk$u, P$win, tk$spec)
      f2 <- first2_flag(ii); rng <- range(YX$Y[, "VIX"], YX$X[, (0:(Dd$nlag - 1)) * 2 + Dd$iv])
    }
    v <- npdc_YX(YX$Y, YX$X, tk$taus, tk$nfores, Dd$nlag, keep_fit_tau = if (tk$diag) 0.95 else NULL)
    out[[sub]] <- v; out[[paste0("range_", sub)]] <- rng
    if (tk$diag) {
      f95 <- attr(v, "fit")
      if (tk$scheme == "SIEVE") { YX0 <- YX; YX0$Y[, "VIX"] <- YX0$Y[, "VIX"]; pos_o <- res_vix(YX0, Dd$b95) > 0 } else pos_o <- Dd$pos95[ii]
      pos_r <- if (!is.null(f95)) f95$Res[, "VIX"] > 0 else rep(NA, length(f2))
      out[[paste0("splice_", sub)]] <- splice_counts(f2, pos_o, pos_r)
    }
    if (!is.null(attr(v, "erro"))) out$err <- c(out$err, paste(sub, attr(v, "erro")))
  }
  out
}
draw_idx <- function(scheme, m) if (scheme == "SB") sb_idx(m) else mbb_idx(m)
run_sep_boot10 <- function(pairs, B, seed, scheme = "MBB", taus = TAUS, nfores = HS, spec = "level", noise = TRUE, timeout = 180) {
  set.seed(seed); tasks <- list()
  for (nm in names(pairs)) {
    P <- pairs[[nm]]
    if (scheme == "SIEVE") for (s in c("pre","post")) if (is.null(P[[s]]$sieve) || P[[s]]$sieve$maxmod >= 1) stop("sieve indisponivel/instavel em ", nm, " ", s)
    for (b in 1:B) {
      tk <- list(nm = nm, b = b, scheme = scheme, taus = taus, nfores = nfores, spec = spec, diag = (spec == "level"))
      if (scheme == "SIEVE") {
        for (s in c("pre","post")) { sv <- P[[s]]$sieve; tk[[paste0("i_", s)]] <- mbb_idx(nrow(sv$E)); tk[[paste0("z_", s)]] <- if (noise) ruido(sv$n) else NULL }
      } else {
        tk$i_pre <- draw_idx(scheme, nrow(P$pre$src)); tk$i_post <- draw_idx(scheme, nrow(P$post$src))
        tk$u <- if (noise) draw_noise(P$win) else NULL
      }
      tasks[[length(tasks) + 1]] <- tk
    }
  }
  assign(".PAIRS_V10", pairs, envir = globalenv())
  r <- par_map_timeout(tasks, sep_task10, n_workers = N_WORKERS, timeout = timeout, export_names = ".PAIRS_V10", export_env = globalenv())
  k <- sprintf("%.2f", taus); hn <- paste0("H", nfores); out <- list()
  for (nm in names(pairs)) {
    ii <- which(sapply(tasks, `[[`, "nm") == nm)
    grab <- function(sub, row, h) { m <- do.call(rbind, lapply(r[ii], function(x) if (is.null(x[[sub]])) rep(NA, length(taus)) else x[[sub]][row, , h])); colnames(m) <- k; m }
    res <- list()
    for (h in hn) res[[h]] <- list(pre = grab("pre", "NPDC", h), post = grab("post", "NPDC", h),
                                   pre_aV = grab("pre", "a_from_VIX", h), pre_Va = grab("pre", "VIX_from_a", h),
                                   post_aV = grab("post", "a_from_VIX", h), post_Va = grab("post", "VIX_from_a", h))
    valid <- apply(cbind(res[[hn[1]]]$pre, res[[hn[1]]]$post), 1, function(x) all(is.finite(x)))
    for (h in hn) res[[h]]$valid <- valid
    rng <- do.call(rbind, lapply(r[ii], function(x) c(pre_min = x$range_pre[1] %||% NA, pre_max = x$range_pre[2] %||% NA,
                                                     post_min = x$range_post[1] %||% NA, post_max = x$range_post[2] %||% NA)))
    spl <- lapply(c(pre = "splice_pre", post = "splice_post"), function(s) do.call(rbind, lapply(r[ii], function(x) x[[s]])))
    out[[nm]] <- c(res[[hn[1]]], list(byH = res, range = rng, splice = spl, errs = unlist(lapply(r[ii], `[[`, "err"))))
  }
  attr(out, "n_timeout") <- attr(r, "n_timeout"); attr(out, "scheme") <- scheme; attr(out, "seed") <- seed
  out
}
## pontuais (linhas originais; idx = todas, sem ruido)
point_sep10 <- function(pairs, taus = TAUS, nfores = HS, spec = "level") lapply(pairs, function(P) {
  v <- lapply(c(pre = "pre", post = "post"), function(s) { YX <- build_YX(P[[s]], spec = spec); npdc_YX(YX$Y, YX$X, taus, nfores, P[[s]]$nlag) })
  byH <- lapply(setNames(paste0("H", nfores), paste0("H", nfores)), function(h)
    list(pre = v$pre["NPDC", , h], post = v$post["NPDC", , h], pre_full = v$pre[, , h], post_full = v$post[, , h], stats = stats_gen(v$pre["NPDC", , h], v$post["NPDC", , h], taus)))
  c(byH[[1]], list(byH = byH, n_pre = nrow(P$pre$M), n_post = nrow(P$post$M), rows_pre = nrow(P$pre$src), rows_post = nrow(P$post$src)))
})
## tabela via funcoes da V9 (pontos/replicas em H escolhido)
pick_H <- function(x, h) { if (is.null(x$byH)) return(x); y <- x$byH[[h]]; y$valid <- x$valid; y }
tab_sep10 <- function(pts, bt, taus = TAUS, h = "H10") tab_sep9(lapply(pts, pick_H, h = h), lapply(bt, pick_H, h = h), taus)
add_btc_eth10 <- function(tab, bt, pt, taus = TAUS, h = "H10") {
  Sb <- boot_stats(pick_H(bt$BTC, h), taus); Se <- boot_stats(pick_H(bt$ETH, h), taus); pb <- pick_H(pt$BTC, h); pe <- pick_H(pt$ETH, h)
  for (st in c(paste0("Delta_", sprintf("%.2f", taus)), "DD_0.95"))
    tab <- rbind(tab, resumir_v6(unname(pb$stats[st] - pe$stats[st]), Sb[, st] - Se[, st], series = "BTC-ETH", statistic = st))
  tab
}
chk_test10 <- function(bt, rot) { v <- sapply(bt, function(x) sum(x$valid)); n <- sapply(bt, function(x) length(x$valid))
  log_v10(sprintf("%s teste B=20: %s; timeouts %s", rot, paste(names(v), v, sep = " ", collapse = "; "), attr(bt, "n_timeout")))
  if (any(v / n < 0.95)) { progresso(paste("PARADA:", rot, "teste B=20 abaixo de 95%")); stop(paste(rot, "teste abaixo de 95%")) } }

## ---------------------------------------------------------------------
## Contrafactual CONJUNTO POR DATA com linhas do desenho
## sis$series[[nm]] = list(pre = D, post = D, taus); master_pre/post; win
## ---------------------------------------------------------------------
mk_series10 <- function(Z, cut, taus = NULL, full_dates = NULL, pre_end = NULL, post_ini = NULL) {
  Z <- as.xts(Z); dt <- as.Date(index(Z))
  ip <- if (is.null(pre_end)) dt < cut else dt <= pre_end
  iq <- if (is.null(post_ini)) dt >= cut else dt >= post_ini
  sp <- sq <- NULL
  if (!is.null(full_dates)) { sp <- mk_seg(dt[ip], full_dates[full_dates < cut]); sq <- mk_seg(dt[iq], full_dates[full_dates >= cut])
    if (length(unique(sp)) == 1) sp <- NULL; if (length(unique(sq)) == 1) sq <- NULL }
  list(pre = D_from_xts(Z[ip], sp), post = D_from_xts(Z[iq], sq), taus = taus)
}
calc_joint10 <- function(sis, rows_fun, u = NULL) {
  out <- c(); err <- NULL; nf <- if (isTRUE(sis$h5)) c(10, 5) else 10
  for (nm in names(sis$series)) {
    s <- sis$series[[nm]]; taus <- if (!is.null(s$taus)) s$taus else sis$taus; npdc <- list()
    for (sub in c("pre","post")) {
      Dd <- s[[sub]]; idx <- rows_fun(Dd, sub)
      YX <- build_YX(Dd, idx, u, sis$win)
      v <- npdc_YX(YX$Y, YX$X, taus, nf, Dd$nlag)
      if (!is.null(attr(v, "erro"))) err <- c(err, paste(nm, sub, attr(v, "erro")))
      npdc[[sub]] <- v["NPDC", , , drop = FALSE]
    }
    for (h in seq_along(nf)) { sfx <- if (nf[h] == 10) "" else paste0("_H", nf[h])
      pre <- npdc$pre[1, , h]; post <- npdc$post[1, , h]; d <- post - pre
      out <- c(out, setNames(d, paste0(nm, "_Delta_", names(d), sfx)))
      if (isTRUE(sis$keep_npdc)) out <- c(out, setNames(pre, paste0(nm, "_NPDCpre_", names(d), sfx)), setNames(post, paste0(nm, "_NPDCpost_", names(d), sfx))) }
  }
  for (h in nf) { sfx <- if (h == 10) "" else paste0("_H", h)
    for (tk in sprintf("%.2f", sis$taus)) for (cs in names(sis$baskets)) {
      m <- mean(out[paste0(sis$baskets[[cs]], "_Delta_", tk, sfx)])
      out[paste0("Basket", cs, "_Delta_", tk, sfx)] <- m
      for (tr in sis$treated) out[paste0(tr, "minusBasket", cs, "_Delta_", tk, sfx)] <- out[paste0(tr, "_Delta_", tk, sfx)] - m } }
  list(stats = out, err = err)
}
point_joint10 <- function(sistemas) { for (cn in names(sistemas)) sistemas[[cn]]$point <- calc_joint10(sistemas[[cn]], function(Dd, sub) NULL)$stats; sistemas }
joint_task10 <- function(tk) {
  s <- get(".SIS_V10", envir = globalenv())[[tk$cn]]
  dd <- list(pre = s$master_pre[tk$ip], post = s$master_post[tk$iq])
  calc_joint10(s, function(Dd, sub) { r <- match(dd[[sub]], Dd$rdate); r[!is.na(r)] }, tk$u)
}
mk_sys10 <- function(series, taus, baskets, treated, master_pre, master_post, h5 = FALSE) {
  s <- list(series = series, taus = taus, baskets = baskets, treated = treated, master_pre = master_pre, master_post = master_post, h5 = h5)
  s$win <- g_window(unlist(lapply(series, function(x) list(x$pre, x$post)), recursive = FALSE)); s
}
run_joint_boot10 <- function(sistemas, B, seed, noise = TRUE, timeout = 400) {
  set.seed(seed); tasks <- list()
  for (cn in names(sistemas)) { np <- length(sistemas[[cn]]$master_pre); nq <- length(sistemas[[cn]]$master_post)
    for (b in 1:B) tasks[[length(tasks) + 1]] <- list(cn = cn, b = b, ip = mbb_idx(np), iq = mbb_idx(nq), u = if (noise) draw_noise(sistemas[[cn]]$win) else NULL) }
  assign(".SIS_V10", sistemas, envir = globalenv())
  r <- par_map_timeout(tasks, joint_task10, n_workers = N_WORKERS, timeout = timeout, export_names = ".SIS_V10", export_env = globalenv())
  out <- list()
  for (cn in names(sistemas)) { ii <- which(sapply(tasks, `[[`, "cn") == cn); nomes <- names(sistemas[[cn]]$point)
    mat <- do.call(rbind, lapply(r[ii], function(x) if (is.null(x$stats)) setNames(rep(NA_real_, length(nomes)), nomes) else x$stats[nomes]))
    out[[cn]] <- list(mat = mat, valid = apply(mat, 1, function(x) all(is.finite(x))), errs = unlist(lapply(r[ii], `[[`, "err"))) }
  attr(out, "n_timeout") <- attr(r, "n_timeout"); attr(out, "seed") <- seed
  out
}
chk_joint_test <- function(bt, rot) { v <- sapply(bt, function(x) sum(x$valid)); n <- sapply(bt, function(x) length(x$valid))
  log_v10(sprintf("%s teste B=20: %s", rot, paste(names(v), v, collapse = "; ")))
  if (any(v / n < 0.95)) { progresso(paste("PARADA:", rot, "teste B=20 < 95%")); stop(rot) } }

## ---------------------------------------------------------------------
## BC / BCa (mesmas formulas da V9)
## ---------------------------------------------------------------------
accel <- function(jlist) { num <- 0; den <- 0
  for (th in jlist) { m <- length(th); U <- (m - 1) * (mean(th) - th); num <- num + sum((U / m)^3); den <- den + sum((U / m)^2) }
  num / (6 * den^1.5) }
ints <- function(point, x, a, alpha = 0.05) {
  x <- x[is.finite(x)]; B <- length(x); pbelow <- mean(x < point); z0 <- qnorm(pbelow); zl <- qnorm(alpha / 2); zu <- qnorm(1 - alpha / 2)
  bc <- pnorm(c(2 * z0 + zl, 2 * z0 + zu)); bca <- pnorm(c(z0 + (z0 + zl) / (1 - a * (z0 + zl)), z0 + (z0 + zu) / (1 - a * (z0 + zu))))
  qq <- function(p) if (all(is.finite(p))) quantile(x, p, names = FALSE) else c(NA, NA)
  c(prop_below = pbelow, z0 = z0, a = a, pct_lo = quantile(x, 0.025, names = FALSE), pct_hi = quantile(x, 0.975, names = FALSE),
    bc_lo = qq(bc)[1], bc_hi = qq(bc)[2], bc_order_lo = bc[1], bc_order_hi = bc[2], bca_lo = qq(bca)[1], bca_hi = qq(bca)[2], bca_order_lo = bca[1], bca_order_hi = bca[2], n_valid = B)
}
