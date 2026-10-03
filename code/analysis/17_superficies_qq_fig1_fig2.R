# =====================================================================
# V7b — Fig. 1, Fig. 2 / Table S5 (superficies QQ) e Table S18 na base nova
#  * Fig. 1: NPDC VIX->ativo por quantil, pre vs pos (de V7-1_npdc_net_tci_by_quantile.csv;
#    sem reestimar).
#  * S5: superficie NPDC(tau_VIX, tau_ativo) total/pre/pos + mudanca (pos - pre), mesma
#    funcao do 22_baseline_v4.R (22.2) / 07_superficie_qq.R; QVAR p = 2, H = 10, grade 7x7.
#  * Fig. 2: pre, pos e mudanca, dois pares, ESCALA DE CINZA (negativo = escuro; positivo =
#    celula branca com contorno tracejado e sinal "+"), valores impressos, 300 dpi.
#  * S18: grade alternativa (0.025-0.975) formatada a partir de V7-1_robustness_alt_quantile_grid.csv.
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
t0 <- Sys.time(); log_pkgs7("v7_40_fig1_fig2_s5_s18.R")
D <- carregar_v4(); CSV7 <- file.path(OUT7, "csv"); FIG <- file.path(OUT7, "fig")

## ---------------- Fig. 1 -------------------------------------------------
nq <- read.csv(file.path(CSV7, "V7-1_npdc_net_tci_by_quantile.csv"))
f1 <- nq[nq$sample %in% c("pre","post"), ]
f1$Sample <- factor(ifelse(f1$sample == "pre", "Pre-ETF", "Post-ETF"), levels = c("Pre-ETF","Post-ETF"))
f1$panel <- ifelse(f1$pair == "BTC", "(a) BTC-VIX (split 11 Jan 2024)", "(b) ETH-VIX (split 23 Jul 2024)")
p1 <- ggplot(f1, aes(factor(sprintf("%.2f", tau)), NPDC, linetype = Sample, shape = Sample, group = Sample)) + geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey40") +
  geom_line(colour = "black", linewidth = 0.6) + geom_point(colour = "black", fill = "white", size = 2) +
  scale_shape_manual(values = c(16, 21)) + scale_linetype_manual(values = c("solid", "dashed")) +
  facet_wrap(~panel) + labs(x = expression("Quantile " * tau), y = "NPDC (asset vs VIX)") +
  theme_classic(base_size = 10, base_family = "serif") +
  theme(plot.title = element_blank(), strip.background = element_blank(), strip.text = element_text(hjust = 0), legend.position = "bottom", legend.title = element_blank())
ggsave(file.path(FIG, "V7-Fig1_npdc_by_quantile.png"), p1, width = 8, height = 3.8, dpi = 300)
f1w <- reshape(nq[, c("pair","sample","tau","NPDC")], idvar = c("pair","tau"), timevar = "sample", direction = "wide")
n_pos <- sum(nq$NPDC > 0)
log_v7(sprintf("V7b Fig.1: %d de %d valores NPDC (2 pares x 3 amostras x 7 tau) sao positivos", n_pos, nrow(nq)))
progresso(sprintf("V7b item 1: Fig. 1 gerada (V7-Fig1_npdc_by_quantile.png); NPDC positivo em %d de %d celulas par x amostra x tau.", n_pos, nrow(nq)))

## ---------------- S5: superficies ----------------------------------------
npdc_qq <- function(Z, ativo, tau_ativo, tau_vix) {
  tv <- setNames(c(tau_ativo, tau_vix), c(ativo, "VIX"))[colnames(Z)]
  Zz <- if (is.zoo(Z)) Z else as_zoo_synth(Z)
  dca <- suppressMessages(suppressWarnings(ConnectednessApproach(Zz, model = "QVAR", connectedness = "Time", nlag = 2, nfore = 10,
         window.size = NULL, VAR_config = list(QVAR = list(tau = unname(tv), method = "fn")))))
  dca$NPDC[,,dim(dca$NPDC)[3]]["VIX", ativo]
}
tasks <- list()
for (par in c("btc","eth")) for (s in c("total","pre","post")) for (i in seq_along(TAUS)) for (j in seq_along(TAUS))
  tasks[[length(tasks) + 1]] <- list(par = par, s = s, i = i, j = j)
.AMS <- lapply(D$amostras[c("btc","eth")], function(a) lapply(a[c("total","pre","post")], coredata))
assign(".AMS", .AMS, envir = globalenv()); assign("npdc_qq", npdc_qq, envir = globalenv())
sf_task <- function(tk) { M <- get(".AMS", envir = globalenv())[[tk$par]][[tk$s]]
  v <- tryCatch(get("npdc_qq", envir = globalenv())(M, toupper(tk$par), TAUS[tk$j], TAUS[tk$i]), error = function(e) NA_real_); list(v = v) }
tt <- Sys.time()
r <- par_map_timeout(tasks, sf_task, n_workers = N_WORKERS, timeout = 300, export_names = c(".AMS","npdc_qq"), export_env = globalenv())
sf <- do.call(rbind, lapply(seq_along(tasks), function(k) data.frame(pair = toupper(tasks[[k]]$par), sample = tasks[[k]]$s, tau_VIX = TAUS[tasks[[k]]$i], tau_asset = TAUS[tasks[[k]]$j],
                                                                   NPDC = if (is.null(r[[k]]$v)) NA else r[[k]]$v)))
log_v7(sprintf("V7b S5: %d ajustes QVAR da superficie, %d falhas, %.1f min", nrow(sf), sum(is.na(sf$NPDC)), as.numeric(Sys.time() - tt, units = "mins")))
## diagonal deve reproduzir V7-1d
dg <- merge(sf[sf$tau_VIX == sf$tau_asset, ], nq, by.x = c("pair","sample","tau_VIX"), by.y = c("pair","sample","tau"))
log_v7(sprintf("V7b S5: diagonal da superficie = NPDC por quantil (V7-1d): dif. max %.2e", max(abs(dg$NPDC.x - dg$NPDC.y))))
w <- reshape(sf, idvar = c("pair","tau_VIX","tau_asset"), timevar = "sample", direction = "wide")
w$NPDC.change <- w$NPDC.post - w$NPDC.pre
long <- rbind(sf, data.frame(pair = w$pair, sample = "change (post - pre)", tau_VIX = w$tau_VIX, tau_asset = w$tau_asset, NPDC = w$NPDC.change))
write.csv(long, file.path(CSV7, "V7-S5_qq_surfaces_long.csv"), row.names = FALSE)
## formato da S5 submetida: um painel por par x amostra, linhas = tau VIX, colunas = tau ativo
s5 <- do.call(rbind, lapply(c("BTC","ETH"), function(a) do.call(rbind, lapply(c("total","pre","post","change (post - pre)"), function(s) {
  d <- long[long$pair == a & long$sample == s, ]; m <- xtabs(NPDC ~ tau_VIX + tau_asset, d)
  data.frame(Panel = paste0(a, "-VIX, ", c(total = "full sample", pre = "pre-ETF", post = "post-ETF", `change (post - pre)` = "change (post - pre)")[s]),
             `VIX tau \\ asset tau` = sprintf("%.2f", as.numeric(rownames(m))), unclass(m)[, , drop = TRUE], check.names = FALSE, row.names = NULL) }))))
names(s5)[3:9] <- sprintf("%.2f", TAUS)
write.csv(s5, file.path(CSV7, "V7-S5_qq_surfaces.csv"), row.names = FALSE)
progresso("V7b item 2a: Table S5 (superficies QQ total/pre/pos/mudanca) estimada e gravada.")

## ---------------- Fig. 2 (cinza) ------------------------------------------
lab_s <- c(pre = "Pre-ETF", post = "Post-ETF", `change (post - pre)` = "Change (post - pre)")
mk_panel <- function(a, s, lim) {
  d <- long[long$pair == a & long$sample == s, ]
  d$neg <- pmin(d$NPDC, 0); d$pos <- d$NPDC > 0
  d$txt <- ifelse(is.na(d$NPDC), "", sprintf("%+.1f", d$NPDC)); d$txt <- sub("^-", "\u2212", d$txt)
  d$tcol <- ifelse(d$neg < -0.55 * lim, "white", "black")
  ggplot(d, aes(factor(sprintf("%.2f", tau_asset)), factor(sprintf("%.2f", tau_VIX)))) +
    geom_tile(aes(fill = neg), colour = "grey80", linewidth = 0.2) +
    geom_tile(data = d[d$pos, ], fill = NA, colour = "black", linewidth = 0.45, linetype = "dashed", width = 0.88, height = 0.88) +
    geom_text(aes(label = txt, colour = tcol), size = 2.1, family = "serif") + scale_colour_identity() +
    scale_fill_gradient(low = "grey5", high = "white", limits = c(-lim, 0), name = if (s == "change (post - pre)") "Change in NPDC\n(negative part)" else "NPDC level\n(negative part)", oob = scales::squish) +
    labs(x = expression(tau[asset]), y = expression(tau[VIX]), subtitle = paste0(a, "-VIX: ", lab_s[s])) + coord_fixed() +
    theme_minimal(base_size = 8, base_family = "serif") +
    theme(panel.grid = element_blank(), plot.title = element_blank(), plot.subtitle = element_text(size = 8.5), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "cm"))
}
lim_lvl <- max(abs(pmin(long$NPDC[long$sample %in% c("pre","post")], 0)), na.rm = TRUE)
lim_chg <- max(abs(pmin(long$NPDC[long$sample == "change (post - pre)"], 0)), na.rm = TRUE)
pl <- list()
for (a in c("BTC","ETH")) for (s in c("pre","post","change (post - pre)")) pl[[length(pl) + 1]] <- mk_panel(a, s, if (s == "change (post - pre)") lim_chg else lim_lvl)
p2 <- wrap_plots(pl, ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")
ggsave(file.path(FIG, "V7-Fig2_qq_surfaces.png"), p2, width = 10, height = 7.6, dpi = 300)
progresso("V7b item 2b: Fig. 2 (superficies QQ em cinza) gerada: V7-Fig2_qq_surfaces.png.")

## ---------------- S18 -----------------------------------------------------
al <- read.csv(file.path(CSV7, "V7-1_robustness_alt_quantile_grid.csv"))
s18 <- data.frame(tau = al$tau[al$pair == "BTC"], `BTC pre` = al$NPDC_pre[al$pair == "BTC"], `BTC post` = al$NPDC_post[al$pair == "BTC"],
                  `ETH pre` = al$NPDC_pre[al$pair == "ETH"], `ETH post` = al$NPDC_post[al$pair == "ETH"], check.names = FALSE)
write.csv(s18, file.path(CSV7, "V7-S18_alt_quantile_grid.csv"), row.names = FALSE)
progresso("V7b item 4: Table S18 (grade 0,025-0,975) formatada.")

saveRDS(list(surface = long, s5 = s5, s18 = s18, n_pos_npdc = n_pos), file.path(OUT7, "rds", "v7b_s5_s18.rds"))
log_v7(sprintf("v7_40 concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
