# =====================================================================
# FUNCOES QVAR E BOOTSTRAP (origem: 40_funcoes_v5.R; carregadas via
# funcoes_contrafactual.R e funcoes_pipeline.R)
# ---------------------------------------------------------------------
# Especificacao travada (identica a V4): VIX em nivel; sistemas
# bivariados ativo-VIX; QVAR p=2 (BIC), H=10; grade de quantis
# 0.05..0.95; cortes BTC 2024-01-11, ETH 2024-07-23.
# =====================================================================

PROJ_DIR <- normalizePath(Sys.getenv("CVE_ROOT", "."), winslash = "/")
Sys.setenv(CVE_ROOT = PROJ_DIR)          # herdado pelos trabalhadores paralelos
setwd(PROJ_DIR)
Sys.setlocale("LC_TIME", "C")

suppressPackageStartupMessages({
  library(ConnectednessApproach); library(zoo); library(xts)
})

TAUS    <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)
NLAG    <- 2
NFORE   <- 10
BLOCK   <- 20
NOISE_A <- 0.005          # ruido U(-0.005, 0.005) no VIX (metade do tique 0.01)
SEED_V5 <- 20260922
CUTS    <- list(btc = as.Date("2024-01-11"), eth = as.Date("2024-07-23"))
JANELA_AGO24 <- c(as.Date("2024-07-22"), as.Date("2024-08-16"))


## Matriz numerica (colunas: ativo, VIX) -> zoo com indice sintetico
## estritamente crescente (a data nao entra na estimacao; so precisa ser
## Date valido e unico, exigencia do ConnectednessApproach).
as_zoo_synth <- function(M) {
  M <- as.matrix(M)
  zoo(M, order.by = as.Date("2000-01-01") + seq_len(nrow(M)))
}

## NPDC["VIX", ativo] para um tau (mesma chamada da V4)
npdc_one <- function(M, tau, nlag = NLAG, nfore = NFORE) {
  Z <- if (is.zoo(M)) M else as_zoo_synth(M)
  dca <- suppressMessages(suppressWarnings(ConnectednessApproach(
    Z, model = "QVAR", connectedness = "Time", nlag = nlag, nfore = nfore,
    window.size = NULL, VAR_config = list(QVAR = list(tau = tau, method = "fn")))))
  m <- dca$NPDC[,,dim(dca$NPDC)[3]]
  m["VIX", colnames(Z)[colnames(Z) != "VIX"]]
}
## Varios taus; retorna vetor nomeado (NA + atributo 'erro' se falhar)
npdc_taus <- function(M, taus = TAUS, nlag = NLAG, nfore = NFORE) {
  out <- setNames(rep(NA_real_, length(taus)), sprintf("%.2f", taus))
  err <- NULL
  for (i in seq_along(taus)) {
    r <- tryCatch(npdc_one(M, taus[i], nlag, nfore), error = function(e) e)
    if (inherits(r, "error")) { err <- conditionMessage(r); break }
    out[i] <- r
  }
  attr(out, "erro") <- err
  out
}

## Bootstrap em blocos moveis (Kunsch 1989): blocos de 'block' obs.,
## inicio uniforme em 1..(n-block+1), concatenados NA ORDEM SORTEADA e
## truncados em n (mesma regra de sorteio da V4).
mbb_idx <- function(n, block = BLOCK) {
  idx <- integer(0)
  while (length(idx) < n) { s <- sample.int(n - block + 1, 1); idx <- c(idx, s:(s + block - 1)) }
  idx[1:n]
}
## Bootstrap estacionario (Politis & Romano 1994): blocos de tamanho
## geometrico com media 'mean_block', inicio uniforme, circular DENTRO da
## subamostra (nunca atravessa a data do ETF).
sb_idx <- function(n, mean_block = BLOCK) {
  p <- 1 / mean_block
  idx <- integer(n)
  idx[1] <- sample.int(n, 1)
  for (t in 2:n) {
    if (runif(1) < p) idx[t] <- sample.int(n, 1)
    else idx[t] <- if (idx[t-1] == n) 1L else idx[t-1] + 1L
  }
  idx
}
ruido <- function(n) runif(n, -NOISE_A, NOISE_A)

## Amostras V4 (mesma construcao; reproduz 2.223 obs.)
## carregar_v4() e definida em funcoes_pipeline.R (amostras da base final)

## Estatisticas derivadas de um par de vetores NPDC (pre, post) nos 7 taus
stats_from_npdc <- function(pre, post) {
  k <- sprintf("%.2f", TAUS)
  d <- post[k] - pre[k]; names(d) <- paste0("Delta_", k)
  c(d,
    DD_0.05 = unname(d["Delta_0.05"] - d["Delta_0.50"]),
    DD_0.95 = unname(d["Delta_0.95"] - d["Delta_0.50"]),
    dAsym   = unname((post["0.05"] - post["0.95"]) - (pre["0.05"] - pre["0.95"])))
}

## ---------------------------------------------------------------------
## Executor paralelo com TIMEOUT por tarefa.
## Motivo: dentro do QVAR, ConnectednessApproach chama summary.rq() com o
## metodo de erro-padrao default (se="rank" quando n <= 1000), cuja rotina
## Fortran pode entrar em laco sem fim em dados degenerados. Uma tarefa
## (replica) que exceda 'timeout' segundos e registrada como FALHA
## ("TIMEOUT"); o processo-trabalhador e encerrado e substituido.
## ---------------------------------------------------------------------
FUNCS_PATH <- file.path(PROJ_DIR, "code/functions/funcoes_qvar_bootstrap.R")
novo_trabalhador <- function(export_names, export_env) {
  cl1 <- parallel::makeCluster(1)
  parallel::clusterCall(cl1, function(p) { source(p); NULL }, FUNCS_PATH)
  if (length(export_names)) parallel::clusterExport(cl1, export_names, envir = export_env)
  list(node = cl1[[1]], pid = parallel::clusterCall(cl1, Sys.getpid)[[1]])
}
par_map_timeout <- function(tasks, FUN, n_workers = 14, timeout = 120,
                            export_names = character(0), export_env = parent.frame()) {
  W <- lapply(seq_len(n_workers), function(i) novo_trabalhador(export_names, export_env))
  n <- length(tasks); res <- vector("list", n)
  busy <- rep(NA_integer_, n_workers); t_ini <- rep(NA_real_, n_workers)
  prox <- 1L; n_timeout <- 0L
  enviar <- function(w, i) {
    parallel:::sendCall(W[[w]]$node, FUN, list(tasks[[i]]), tag = i)
    busy[w] <<- i; t_ini[w] <<- as.numeric(Sys.time())
  }
  for (w in seq_len(n_workers)) if (prox <= n) { enviar(w, prox); prox <- prox + 1L }
  while (any(!is.na(busy))) {
    ativos <- which(!is.na(busy))
    prontos <- socketSelect(lapply(W[ativos], function(x) x$node$con), timeout = 1)
    for (k in which(prontos)) {
      w <- ativos[k]
      msg <- parallel:::recvData(W[[w]]$node)
      res[[busy[w]]] <- if (inherits(msg$value, "try-error")) list(err = paste("R error:", as.character(msg$value))) else msg$value
      busy[w] <- NA
      if (prox <= n) { enviar(w, prox); prox <- prox + 1L }
    }
    agora <- as.numeric(Sys.time())
    for (w in which(!is.na(busy))) {
      if (agora - t_ini[w] > timeout) {
        res[[busy[w]]] <- list(err = "TIMEOUT", timeout = TRUE)
        n_timeout <- n_timeout + 1L
        try(tools::pskill(W[[w]]$pid), silent = TRUE)
        try(close(W[[w]]$node$con), silent = TRUE)
        W[[w]] <- novo_trabalhador(export_names, export_env)
        busy[w] <- NA
        if (prox <= n) { enviar(w, prox); prox <- prox + 1L }
      }
    }
  }
  for (w in seq_along(W)) { try(parallel:::postNode(W[[w]]$node, "DONE"), silent = TRUE); try(close(W[[w]]$node$con), silent = TRUE) }
  attr(res, "n_timeout") <- n_timeout
  res
}

## Paleta Okabe-Ito
OKABE_ITO <- c(orange="#E69F00", skyblue="#56B4E9", green="#009E73", yellow="#F0E442",
               blue="#0072B2", vermillion="#D55E00", purple="#CC79A7", black="#000000", grey="#999999")
