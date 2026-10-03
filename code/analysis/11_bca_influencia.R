# =====================================================================
# V7-3 / Prompt 4, TAREFA 8 — BC/BCa (a partir das 500 replicas da V7-5.4,
# salvas em results/rds/v7_p3_etapa1_bootstrap.rds) e influencia na base
# nova (copia parametrizada de v6_05_tarefa8.R; mesmos algoritmos:
# jackknife de blocos nao sobrepostos de 20 obs.; aceleracao multi-amostra;
# 8b remocao de um bloco por vez na pos + janelas deslizantes passo 5).
# Figura V7-F8 (cinza, 300 dpi, sem titulo).
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
suppressPackageStartupMessages(library(ggplot2))
t0 <- Sys.time(); log_pkgs7("v7_23_p4_tarefa8.R")
D <- carregar_v4(); E1 <- readRDS(file.path(OUT7, "rds", "v7_p3_etapa1_bootstrap.rds"))
AM <- lapply(D$amostras[c("btc","eth")], function(a) list(pre = coredata(a$pre), post = coredata(a$post), dpre = as.Date(index(a$pre)), dpost = as.Date(index(a$post))))
blocos <- function(n, b = BLOCK) split(seq_len(n), ceiling(seq_len(n) / b))
tasks <- list()
for (par in c("btc","eth")) for (s in c("pre","post")) { bl <- blocos(nrow(AM[[par]][[s]])); for (i in seq_along(bl)) tasks[[length(tasks) + 1]] <- list(par = par, s = s, i = i, rows = bl[[i]]) }
jk_task <- function(tk) { M <- get(".AM_V6", envir = globalenv())[[tk$par]][[tk$s]]; v <- npdc_taus(M[-tk$rows, , drop = FALSE], TAUS); list(v = as.numeric(v), err = attr(v, "erro")) }
assign(".AM_V6", AM, envir = globalenv())
tt <- Sys.time(); jr <- par_map_timeout(tasks, jk_task, n_workers = N_WORKERS, timeout = 120, export_names = ".AM_V6", export_env = globalenv())
log_v7(sprintf("8: jackknife de blocos: %d ajustes, falhas %d, %.1f s", length(jr), sum(sapply(jr, function(x) is.null(x$v) || any(!is.finite(x$v)))), as.numeric(Sys.time() - tt, units = "secs")))
k7 <- sprintf("%.2f", TAUS)
pt <- lapply(AM, function(a) list(pre = npdc_taus(a$pre), post = npdc_taus(a$post)))
jk <- list()
for (par in c("btc","eth")) for (s in c("pre","post")) { ii <- which(sapply(tasks, function(t) t$par == par && t$s == s))
  jk[[par]][[s]] <- t(sapply(ii, function(j) { v <- setNames(jr[[j]]$v, k7); if (s == "pre") stats_from_npdc(v, pt[[par]]$post) else stats_from_npdc(pt[[par]]$pre, v) })) }
theta_hat <- lapply(pt, function(x) stats_from_npdc(x$pre, x$post))
accel <- function(jlist) { num <- 0; den <- 0
  for (th in jlist) { m <- length(th); U <- (m - 1) * (mean(th) - th); num <- num + sum((U / m)^3); den <- den + sum((U / m)^2) }
  num / (6 * den^1.5) }
ints <- function(point, x, a, alpha = 0.05) {
  x <- x[is.finite(x)]; B <- length(x); pbelow <- mean(x < point); z0 <- qnorm(pbelow); zl <- qnorm(alpha / 2); zu <- qnorm(1 - alpha / 2)
  bc <- pnorm(c(2 * z0 + zl, 2 * z0 + zu)); bca <- pnorm(c(z0 + (z0 + zl) / (1 - a * (z0 + zl)), z0 + (z0 + zu) / (1 - a * (z0 + zu))))
  qq <- function(p) if (all(is.finite(p))) quantile(x, p, names = FALSE) else c(NA, NA)
  c(prop_below = pbelow, z0 = z0, a = a, pct_lo = quantile(x, 0.025, names = FALSE), pct_hi = quantile(x, 0.975, names = FALSE),
    bc_lo = qq(bc)[1], bc_hi = qq(bc)[2], bc_alpha_lo = bc[1], bc_alpha_hi = bc[2], bca_lo = qq(bca)[1], bca_hi = qq(bca)[2], bca_alpha_lo = bca[1], bca_alpha_hi = bca[2], n_valid = B)
}
rows <- list()
for (par in c("btc","eth")) { S <- E1$boot_mbb[[par]]$stats; S[!E1$boot_mbb[[par]]$valid, ] <- NA
  for (st in colnames(S)) { a <- accel(list(jk[[par]]$pre[, st], jk[[par]]$post[, st]))
    rows[[length(rows) + 1]] <- data.frame(pair = toupper(par), statistic = st, point = unname(theta_hat[[par]][st]), boot_median = median(S[, st], na.rm = TRUE), t(ints(theta_hat[[par]][[st]], S[, st], a))) } }
for (tk in c("0.05","0.50","0.95")) { st <- paste0("Delta_", tk)
  Sb <- E1$boot_mbb$btc$stats[, st]; Se <- E1$boot_mbb$eth$stats[, st]; Sb[!E1$boot_mbb$btc$valid] <- NA; Se[!E1$boot_mbb$eth$valid] <- NA
  pnt <- theta_hat$btc[[st]] - theta_hat$eth[[st]]
  a <- accel(list(jk$btc$pre[, st] - theta_hat$eth[[st]], jk$btc$post[, st] - theta_hat$eth[[st]], theta_hat$btc[[st]] - jk$eth$pre[, st], theta_hat$btc[[st]] - jk$eth$post[, st]))
  rows[[length(rows) + 1]] <- data.frame(pair = "BTC-ETH", statistic = st, point = pnt, boot_median = median(Sb - Se, na.rm = TRUE), t(ints(pnt, Sb - Se, a))) }
t8a <- do.call(rbind, rows); rownames(t8a) <- NULL
t8a$pct_excl0 <- t8a$pct_lo > 0 | t8a$pct_hi < 0; t8a$bc_excl0 <- t8a$bc_lo > 0 | t8a$bc_hi < 0; t8a$bca_excl0 <- t8a$bca_lo > 0 | t8a$bca_hi < 0
v74 <- read.csv(file.path(OUT7, "csv", "V7-5.4_stats_MBB.csv")); mm <- merge(t8a, v74[, c("pair","statistic","ci95_lo","ci95_hi")], by = c("pair","statistic"))
log_v7(sprintf("8a: IC percentil reproduz V7-5.4: dif. max %.2e", max(abs(mm$pct_lo - mm$ci95_lo), abs(mm$pct_hi - mm$ci95_hi))))
print(t8a[, c("pair","statistic","point","prop_below","z0","a","pct_lo","pct_hi","bc_lo","bc_hi","bca_lo","bca_hi","bca_excl0")])
write.csv(t8a, file.path(OUT7, "csv", "V7-6.8a_BC_BCa_full.csv"), row.names = FALSE)

inf_rows <- list()
for (par in c("btc","eth")) { bl <- blocos(nrow(AM[[par]]$post)); dq <- AM[[par]]$dpost
  for (st in c("Delta_0.50","Delta_0.95")) { v <- jk[[par]]$post[, st]
    inf_rows[[length(inf_rows) + 1]] <- data.frame(pair = toupper(par), statistic = st, block = seq_along(bl), start = dq[sapply(bl, min)], end = dq[sapply(bl, max)],
      n_removed = sapply(bl, length), full_estimate = unname(theta_hat[[par]][st]), estimate_without_block = v, change = v - theta_hat[[par]][[st]]) } }
inf <- do.call(rbind, inf_rows); inf$overlaps_aug2024 <- inf$start <= JANELA_AGO24[2] & inf$end >= JANELA_AGO24[1]
top5 <- do.call(rbind, lapply(split(inf, paste(inf$pair, inf$statistic)), function(d) { d <- d[order(-abs(d$change)), ][1:5, ]; d$rank <- 1:5; d })); rownames(top5) <- NULL
print(top5)
sl_tasks <- list()
for (par in c("btc","eth")) { n <- nrow(AM[[par]]$post); for (s in seq(1, n - BLOCK + 1, by = 5)) sl_tasks[[length(sl_tasks) + 1]] <- list(par = par, s = "post", rows = s:(s + BLOCK - 1)) }
sl_task <- function(tk) { M <- get(".AM_V6", envir = globalenv())[[tk$par]]$post; v <- npdc_taus(M[-tk$rows, , drop = FALSE], c(0.50, 0.95)); list(v = as.numeric(v)) }
sr <- par_map_timeout(sl_tasks, sl_task, n_workers = N_WORKERS, timeout = 120, export_names = ".AM_V6", export_env = globalenv())
sl <- do.call(rbind, lapply(seq_along(sl_tasks), function(j) { tk <- sl_tasks[[j]]; dq <- AM[[tk$par]]$dpost; pre <- pt[[tk$par]]$pre
  data.frame(pair = toupper(tk$par), start = dq[min(tk$rows)], end = dq[max(tk$rows)], mid = dq[min(tk$rows)] + as.numeric(dq[max(tk$rows)] - dq[min(tk$rows)]) / 2,
             Delta_0.50 = sr[[j]]$v[1] - pre["0.50"], Delta_0.95 = sr[[j]]$v[2] - pre["0.95"]) }))
sl_top <- do.call(rbind, lapply(c("BTC","ETH"), function(p) do.call(rbind, lapply(c("Delta_0.50","Delta_0.95"), function(st) {
  d <- sl[sl$pair == p, ]; full <- theta_hat[[tolower(p)]][[st]]; ch <- d[[st]] - full; j <- order(-abs(ch))[1:5]
  data.frame(pair = p, statistic = st, rank = 1:5, start = d$start[j], end = d$end[j], change = ch[j], overlaps_aug2024 = d$start[j] <= JANELA_AGO24[2] & d$end[j] >= JANELA_AGO24[1]) }))))
print(sl_top)
write.csv(inf, file.path(OUT7, "csv", "V7-6.8b_influence_all_blocks.csv"), row.names = FALSE)
write.csv(top5, file.path(OUT7, "csv", "V7-6.8b_influence_top5.csv"), row.names = FALSE)
write.csv(sl, file.path(OUT7, "csv", "V7-6.8b_influence_sliding_step5.csv"), row.names = FALSE)
write.csv(sl_top, file.path(OUT7, "csv", "V7-6.8b_influence_sliding_top5.csv"), row.names = FALSE)

inf$mid <- inf$start + as.numeric(inf$end - inf$start) / 2
lab <- function(p, st) paste0(p, ", ", sub("Delta_", "Delta(", st), ")")
fd <- rbind(data.frame(panel = lab(inf$pair, inf$statistic), x = inf$mid, y = inf$estimate_without_block, type = "Non-overlapping blocks"),
            do.call(rbind, lapply(c("Delta_0.50","Delta_0.95"), function(st) data.frame(panel = lab(sl$pair, st), x = sl$mid, y = sl[[st]], type = "Sliding windows (step 5)"))))
hl <- unique(data.frame(panel = lab(inf$pair, inf$statistic), full = inf$full_estimate))
fd$panel <- factor(fd$panel, levels = c("BTC, Delta(0.50)","BTC, Delta(0.95)","ETH, Delta(0.50)","ETH, Delta(0.95)")); hl$panel <- factor(hl$panel, levels = levels(fd$panel))
p8 <- ggplot(fd, aes(x, y)) + annotate("rect", xmin = JANELA_AGO24[1], xmax = JANELA_AGO24[2], ymin = -Inf, ymax = Inf, fill = "grey85") +
  geom_hline(data = hl, aes(yintercept = full), linetype = "dashed", linewidth = 0.4) +
  geom_line(data = fd[fd$type != "Non-overlapping blocks", ], colour = "grey55", linewidth = 0.4) +
  geom_point(data = fd[fd$type == "Non-overlapping blocks", ], shape = 21, fill = "black", size = 1.3) +
  facet_wrap(~panel, scales = "free", ncol = 2) + labs(x = "Removed post-ETF block (mid date)", y = "Pre/post change with block removed") +
  theme_classic(base_size = 10, base_family = "serif") + theme(strip.background = element_blank(), strip.text = element_text(hjust = 0), plot.title = element_blank())
ggsave(file.path(OUT7, "fig", "V7-F8_influence_blocks.png"), p8, width = 8, height = 5.5, dpi = 300)
saveRDS(list(t8a = t8a, jk = jk, theta_hat = theta_hat, inf = inf, top5 = top5, sl = sl, sl_top = sl_top), file.path(OUT7, "rds", "v7_6_8.rds"))
progresso("V7-3 Tarefa 8 (BC/BCa, influencia, V7-F8) concluida.")
log_v7(sprintf("v7_23 (Tarefa 8) concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
