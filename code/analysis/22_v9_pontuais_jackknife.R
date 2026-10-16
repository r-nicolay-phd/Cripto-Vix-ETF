# =====================================================================
# V9 — TAREFA 1 (parte usada): estimativas pontuais da Tabela 1 com o QVAR
# reimplementado (point_sep9) e jackknife de blocos de 20 observacoes com
# a regra das lacunas (as p primeiras observacoes apos o bloco removido
# saem como dependentes), base da Fig. 3 / Tabela S16 (26_*) e da Regra A
# (23_*). Nenhuma reamostragem.
# Origem: v9_02_tarefa1.R. Mantidos sem alteracao os blocos PAIRS, pt e
# "jackknife de blocos"; os demais blocos de v9_02 (bootstrap com o VIX
# reconstruido a partir de Dlog VIX: Tabela 1 MBB/SB, BC/BCa, contrafactual,
# poder, ortogonalizado, exclusoes, pre do ETH limpo, regime, Regra B) foram
# substituidos pelos scripts 28_* a 32_* (V10, linhas do desenho) e nao
# sao reproduzidos aqui.
# Saidas: results/rds/v9_1_points.rds (= v9_1_table1.rds$pt do projeto) e
# results/rds/v9_1_jackknife.rds.
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_v9.R"))
t0 <- Sys.time(); log_pkgs9("22_v9_pontuais_jackknife.R")
D <- carregar_v4()
RDS <- function(nm) file.path(OUT9, "rds", paste0("v9_1_", nm, ".rds"))
run_or_load <- function(nm, expr) { f <- RDS(nm); if (file.exists(f)) { log_v9("1: bloco ", nm, " ja existe -- carregado"); return(readRDS(f)) }
  x <- eval.parent(substitute(expr)); saveRDS(x, f); x }

PAIRS <- list(BTC = list(pre = mk_S(D$amostras$btc$pre), post = mk_S(D$amostras$btc$post)),
              ETH = list(pre = mk_S(D$amostras$eth$pre), post = mk_S(D$amostras$eth$post)))
pt <- point_sep9(PAIRS)
saveRDS(pt, RDS("points"))

## ======================= jackknife de blocos (BCa e Tarefa 5a) ===========
JK <- run_or_load("jackknife", {
  blocos <- function(n, b = BLOCK) split(seq_len(n), ceiling(seq_len(n) / b))
  tasks <- list()
  for (nm in names(PAIRS)) for (s in c("pre","post")) { bl <- blocos(nrow(PAIRS[[nm]][[s]]$M)); for (i in seq_along(bl)) tasks[[length(tasks) + 1]] <- list(nm = nm, s = s, i = i, rows = bl[[i]]) }
  jk_task9 <- function(tk) { M <- get(".PAIRS_V9", envir = globalenv())[[tk$nm]][[tk$s]]$M; keep <- setdiff(seq_len(nrow(M)), tk$rows)
    seg <- cumsum(c(1L, as.integer(diff(keep) > 1)))
    v <- npdc_taus9(M[keep, , drop = FALSE], TAUS, seg = seg); v0 <- npdc_taus9(M[keep, , drop = FALSE], TAUS)
    list(v = as.numeric(v["NPDC", ]), v_splice = as.numeric(v0["NPDC", ]), err = c(attr(v, "erro"), attr(v0, "erro"))) }
  assign(".PAIRS_V9", PAIRS, envir = globalenv())
  jr <- par_map_timeout(tasks, jk_task9, n_workers = N_WORKERS, timeout = 120, export_names = ".PAIRS_V9", export_env = globalenv())
  log_v9(sprintf("1c jackknife de blocos: %d ajustes, falhas %d", length(jr), sum(sapply(jr, function(x) is.null(x$v) || any(!is.finite(x$v))))))
  k7 <- sprintf("%.2f", TAUS); jk <- list(); jk_sp <- list(); info <- list()
  for (nm in names(PAIRS)) for (s in c("pre","post")) { ii <- which(sapply(tasks, function(t) t$nm == nm && t$s == s))
    jk[[nm]][[s]] <- do.call(rbind, lapply(jr[ii], function(x) setNames(x$v, k7)))
    jk_sp[[nm]][[s]] <- do.call(rbind, lapply(jr[ii], function(x) setNames(x$v_splice, k7)))
    dts <- PAIRS[[nm]][[s]]$date
    info[[nm]][[s]] <- data.frame(block = sapply(tasks[ii], `[[`, "i"), start = dts[sapply(tasks[ii], function(t) min(t$rows))], end = dts[sapply(tasks[ii], function(t) max(t$rows))],
                                  n_removed = sapply(tasks[ii], function(t) length(t$rows))) }
  list(jk = jk, jk_splice = jk_sp, info = info)
})
progresso("V9 Tarefa 1 (parte usada): pontuais e jackknife de blocos concluidos.")
log_v9(sprintf("22_v9_pontuais_jackknife concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
