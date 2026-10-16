# =====================================================================
# V9 — TAREFA 5: influencia e exclusoes. Semente 20261015 + 5.
# Regra das exclusoes (aplicada em 5a, 5b e 5c): depois de cada lacuna as
# p primeiras observacoes saem como variavel dependente (o QVAR monta as
# defasagens dentro de cada segmento; qvar_fit(seg = ...)), de modo que
# nenhuma defasagem atravessa a lacuna. Retirar um bloco do meio de uma
# subamostra cria uma lacuna (p obs. descartadas); retirar o 1o bloco nao.
#  5a  influencia (remove um bloco nao sobreposto de 20 dias por vez) no
#      pre e no pos (jackknife da Tarefa 1), Delta(0.50) e Delta(0.95);
#      5 blocos mais influentes; janelas deslizantes (passo 5); Figura
#      V9-F5 (paineis pre e pos) e Fig. 3 regenerada (pos) com o mesmo eixo.
#  5b  ETH: exclusao de 01-09/08/2024 e, alt., 02-09/08/2024; Delta em todos
#      os quantis com o bootstrap da Tarefa 1.
#  5c  exclusao conjunta 22/07-16/08/2024 + 08/04-06/05/2025, p = 1, 2, 3
#      (pontual) e bootstrap para p = 2, dois pares.
# Origem: v9_06_tarefa5.R. Alteracoes: caminhos; pontuais e jackknife lidos
# de 22_v9_pontuais_jackknife.R (v9_1_points.rds, v9_1_jackknife.rds);
# removidos a conferencia com a V7 (V7-6.8b, V7-1_exclusion_*), a
# Fig. 3 regenerada so com o pos (V9-Fig3, nao usada) e os itens 5b/5c
# (bootstrap da V9), substituidos por 31_v10_exclusoes_amostras.R.
# Usados: 5a (Tabela S16) e a Figura V9-F5 (Fig. 3 do artigo).
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_v9.R"))
suppressPackageStartupMessages(library(ggplot2))
t0 <- Sys.time(); log_pkgs9("v9_06_tarefa5.R")
K <- 5; SEED <- SEED_V9 + K
D <- carregar_v4()
RDS <- function(nm) file.path(OUT9, "rds", paste0("v9_5_", nm, ".rds"))
run_or_load <- function(nm, expr) { f <- RDS(nm); if (file.exists(f)) { log_v9("5: bloco ", nm, " ja existe -- carregado"); return(readRDS(f)) }
  x <- eval.parent(substitute(expr)); saveRDS(x, f); x }
JK <- readRDS(file.path(OUT9, "rds", "v9_1_jackknife.rds"))
pt <- readRDS(file.path(OUT9, "rds", "v9_1_points.rds"))
PAIRS <- list(BTC = list(pre = mk_S(D$amostras$btc$pre), post = mk_S(D$amostras$btc$post)),
              ETH = list(pre = mk_S(D$amostras$eth$pre), post = mk_S(D$amostras$eth$post)))

## ---------------- 5a: blocos nao sobrepostos -------------------------------
inf_rows <- list()
for (nm in c("BTC","ETH")) for (s in c("pre","post")) for (st in c("Delta_0.50","Delta_0.95")) {
  tk <- sub("Delta_", "", st); info <- JK$info[[nm]][[s]]
  full <- unname(pt[[nm]]$stats[st])
  wo <- if (s == "pre") pt[[nm]]$post[tk] - JK$jk[[nm]]$pre[, tk] else JK$jk[[nm]]$post[, tk] - pt[[nm]]$pre[tk]
  wo_sp <- if (s == "pre") pt[[nm]]$post[tk] - JK$jk_splice[[nm]]$pre[, tk] else JK$jk_splice[[nm]]$post[, tk] - pt[[nm]]$pre[tk]
  inf_rows[[length(inf_rows) + 1]] <- data.frame(pair = nm, sample = s, statistic = st, info, full_estimate = full, estimate_without_block = unname(wo),
    change = unname(wo) - full, estimate_without_block_no_gap_rule = unname(wo_sp),
    obs_dropped_gap_rule = ifelse(info$block == 1 | info$block == max(info$block), 0L, NLAG))
}
INF <- do.call(rbind, inf_rows); rownames(INF) <- NULL
INF$overlaps_aug2024 <- INF$start <= JANELA_AGO24[2] & INF$end >= JANELA_AGO24[1]
INF$overlaps_apr2025 <- INF$start <= as.Date("2025-05-06") & INF$end >= as.Date("2025-04-08")
TOP5 <- do.call(rbind, lapply(split(INF, paste(INF$pair, INF$sample, INF$statistic)), function(d) { d <- d[order(-abs(d$change)), ][1:5, ]; d$rank <- 1:5; d }))
rownames(TOP5) <- NULL
print(TOP5[, c("pair","sample","statistic","rank","start","end","full_estimate","estimate_without_block","change")])
write.csv(INF, file.path(OUT9, "csv", "V9-5a_influence_all_blocks_raw.csv"), row.names = FALSE)
write.csv(TOP5, file.path(OUT9, "csv", "V9-5a_influence_top5_raw.csv"), row.names = FALSE)

## ---------------- 5a: janelas deslizantes (passo 5) --------------------------
SL <- run_or_load("sliding", {
  tasks <- list()
  for (nm in names(PAIRS)) for (s in c("pre","post")) { n <- nrow(PAIRS[[nm]][[s]]$M)
    for (st in seq(1, n - BLOCK + 1, by = 5)) tasks[[length(tasks) + 1]] <- list(nm = nm, s = s, rows = st:(st + BLOCK - 1)) }
  sl_task <- function(tk) { M <- get(".PAIRS_V9", envir = globalenv())[[tk$nm]][[tk$s]]$M; keep <- setdiff(seq_len(nrow(M)), tk$rows)
    seg <- cumsum(c(1L, as.integer(diff(keep) > 1))); v <- npdc_taus9(M[keep, , drop = FALSE], c(0.50, 0.95), seg = seg); list(v = as.numeric(v["NPDC", ])) }
  assign(".PAIRS_V9", PAIRS, envir = globalenv())
  sr <- par_map_timeout(tasks, sl_task, n_workers = N_WORKERS, timeout = 120, export_names = ".PAIRS_V9", export_env = globalenv())
  do.call(rbind, lapply(seq_along(tasks), function(j) { tk <- tasks[[j]]; dq <- PAIRS[[tk$nm]][[tk$s]]$date; v <- sr[[j]]$v; if (is.null(v)) v <- c(NA, NA)
    d50 <- if (tk$s == "pre") pt[[tk$nm]]$post["0.50"] - v[1] else v[1] - pt[[tk$nm]]$pre["0.50"]
    d95 <- if (tk$s == "pre") pt[[tk$nm]]$post["0.95"] - v[2] else v[2] - pt[[tk$nm]]$pre["0.95"]
    data.frame(pair = tk$nm, sample = tk$s, start = dq[min(tk$rows)], end = dq[max(tk$rows)], mid = dq[min(tk$rows)] + as.numeric(dq[max(tk$rows)] - dq[min(tk$rows)]) / 2,
               Delta_0.50 = unname(d50), Delta_0.95 = unname(d95)) }))
})
write.csv(SL, file.path(OUT9, "csv", "V9-5a_influence_sliding_step5_raw.csv"), row.names = FALSE)
SLTOP <- do.call(rbind, lapply(split(SL, paste(SL$pair, SL$sample)), function(d) do.call(rbind, lapply(c("Delta_0.50","Delta_0.95"), function(st) {
  full <- pt[[d$pair[1]]]$stats[[st]]; ch <- d[[st]] - full; j <- order(-abs(ch))[1:5]
  data.frame(pair = d$pair[1], sample = d$sample[1], statistic = st, rank = 1:5, start = d$start[j], end = d$end[j], estimate_without_window = d[[st]][j], change = ch[j]) }))))
rownames(SLTOP) <- NULL
write.csv(SLTOP, file.path(OUT9, "csv", "V9-5a_influence_sliding_top5_raw.csv"), row.names = FALSE)

## ---------------- Figuras V9-F5 e Fig. 3 ------------------------------------
INF$mid <- INF$start + as.numeric(INF$end - INF$start) / 2
rowlab <- function(p, st) paste0(p, ", ", sub("Delta_", "Delta(", st), ")")
collab <- function(s) ifelse(s == "pre", "Pre-ETF block removed", "Post-ETF block removed")
fd <- rbind(data.frame(row = rowlab(INF$pair, INF$statistic), col = collab(INF$sample), x = INF$mid, y = INF$estimate_without_block, type = "blocks"),
            do.call(rbind, lapply(c("Delta_0.50","Delta_0.95"), function(st) data.frame(row = rowlab(SL$pair, st), col = collab(SL$sample), x = SL$mid, y = SL[[st]], type = "sliding"))))
rows_lv <- c("BTC, Delta(0.50)","BTC, Delta(0.95)","ETH, Delta(0.50)","ETH, Delta(0.95)")
fd$row <- factor(fd$row, levels = rows_lv); fd$col <- factor(fd$col, levels = c("Pre-ETF block removed","Post-ETF block removed"))
hl <- unique(data.frame(row = factor(rowlab(INF$pair, INF$statistic), levels = rows_lv), full = INF$full_estimate))
lims <- do.call(rbind, lapply(split(fd, fd$row), function(d) data.frame(row = d$row[1], lo = min(c(d$y, hl$full[hl$row == d$row[1]]), na.rm = TRUE), hi = max(c(d$y, hl$full[hl$row == d$row[1]]), na.rm = TRUE))))
shade <- data.frame(xmin = c(JANELA_AGO24[1], as.Date("2025-04-08")), xmax = c(JANELA_AGO24[2], as.Date("2025-05-06")),
                    col = factor("Post-ETF block removed", levels = c("Pre-ETF block removed","Post-ETF block removed")))
tema <- theme_classic(base_size = 9, base_family = "serif") + theme(plot.title = element_blank(), strip.background = element_blank(), strip.text = element_text(hjust = 0))
pF5 <- ggplot(fd, aes(x, y)) +
  geom_rect(data = shade, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "grey88") +
  geom_hline(data = hl, aes(yintercept = full), linetype = "dashed", linewidth = 0.4) +
  geom_line(data = fd[fd$type == "sliding", ], colour = "grey55", linewidth = 0.35) +
  geom_point(data = fd[fd$type == "blocks", ], shape = 21, fill = "black", size = 1.1) +
  geom_blank(data = rbind(data.frame(row = lims$row, y = lims$lo), data.frame(row = lims$row, y = lims$hi)), aes(y = y), inherit.aes = FALSE) +
  facet_grid(row ~ col, scales = "free", space = "free_x") + labs(x = "Removed block (mid date)", y = "Pre/post change with block removed") + tema
ggsave(file.path(OUT9, "fig", "V9-F5_influence_pre_post.png"), pF5, width = 9, height = 8, dpi = 300)
write.csv(lims, file.path(OUT9, "csv", "V9-5a_figure_axis_limits.csv"), row.names = FALSE)
progresso("Tarefa 5a concluida (influencia pre/pos; V9-F5).")

saveRDS(list(INF = INF, TOP5 = TOP5, SL = SL, SLTOP = SLTOP, lims = lims, seed = SEED), file.path(OUT9, "rds", "v9_5_all.rds"))
progresso("Tarefa 5 concluida (5a influencia).")
log_v9(sprintf("26_v9 (Tarefa 5) concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
