# =====================================================================
# FIGURAS EM TONS DE CINZA (300 dpi, sem titulo interno, rotulos em ingles)
# Codigo extraido, sem alteracao, da secao "FIGURAS" de v7_30_tabelas_figuras.R.
#   results/fig/V7-F3_counterfactual_median_and_upper_tail.png -> Fig. 4
#   results/fig/V7-F5.11_flow_differential_state_tminus1.png   -> Fig. 5 e Fig. S2
#   results/fig/V7-F5.8_placebo_DD095_histogram.png            -> Fig. S1
# Fig. 1 e Fig. 2: 17_superficies_qq_fig1_fig2.R; Fig. 3: 11_bca_influencia.R.
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
suppressPackageStartupMessages({ library(ggplot2) })
t0 <- Sys.time(); log_pkgs7("20_figuras.R")
CSV <- file.path(OUT7, "csv"); FIG <- file.path(OUT7, "fig")
rdcsv <- function(f) read.csv(file.path(CSV, f), stringsAsFactors = FALSE)

lab_ent <- function(s) {
  suf <- ""
  if (grepl("_o_bin_pre$", s)) suf <- " [orth. to Binance BTC, pre-ETF beta]"
  if (grepl("_o_bin_full$", s)) suf <- " [orth. to Binance BTC, full-sample beta]"
  if (grepl("_o_cmc_pre$", s)) suf <- " [orth. to base BTC, pre beta]"
  if (grepl("_o_cmc_full$", s)) suf <- " [orth. to base BTC, full beta]"
  b <- sub("_o_(bin|cmc)_(pre|full)$", "", s)
  b <- sub("^TREATED_EXCLminusBasket", "Treated excl. Aug 2024 - basket ", b); b <- sub("^TREATEDminusBasket", "Treated - basket ", b)
  b <- sub("^ETH_o_cmc_(pre|full)minusBasket", "ETH orth. (\\1 beta) - basket ", b); b <- sub("^Basket", "Basket ", b)
  b <- sub("Bpp$", "B'' (B + SOL, trunc. 1 Jul 2025)", b); b <- sub("Bp$", "B' (B + SOL)", b)
  b <- sub("^TREATED_EXCL$", "Treated excl. 22 Jul-16 Aug 2024", b); b <- sub("^TREATED$", "Treated", b)
  b <- sub("^ETH_o_cmc_pre$", "ETH orth. to BTC (pre-ETF beta)", b); b <- sub("^ETH_o_cmc_full$", "ETH orth. to BTC (full-sample beta)", b)
  b <- sub("^(XRP|LTC|TRX|SOL)$", "\\1*", b); b <- sub("^SOLssk$", "SOL** (trunc. 1 Jul 2025)", b)
  paste0(b, suf)
}
split_joint <- function(tab) { p <- strsplit(tab$statistic, "_Delta_"); tab$entity_code <- sapply(p, `[`, 1); tab$tau <- as.numeric(sapply(p, `[`, 2)); tab$entity <- sapply(tab$entity_code, lab_ent); tab }

pa  <- rdcsv("V7-5.9_placebo_all_dates.csv")
lp5 <- rdcsv("V7-5.11_flow_LP_by_horizon.csv")
t1a <- rdcsv("V7-6.1a_counterfactual_all.csv")

tema <- theme_classic(base_size = 10, base_family = "serif") + theme(plot.title = element_blank(), strip.background = element_blank(), strip.text = element_text(size = 10, hjust = 0), legend.position = "bottom", legend.title = element_blank())
ordem <- c("TREATED","TREATED_EXCL","XRP","LTC","XLM","ADA","TRX","BasketA","BasketB")

## Fig. S1 — distribuicao placebo de DD(0,95)
pg <- pa[pa$type == "placebo (grid)", ]; pr <- pa[pa$type == "real ETF date", ]
pg$panel <- ifelse(pg$pair == "BTC", sprintf("(a) BTC (%d placebo dates)", sum(pg$pair == "BTC")), sprintf("(b) ETH (%d placebo dates)", sum(pg$pair == "ETH")))
pr$panel <- ifelse(pr$pair == "BTC", unique(pg$panel[pg$pair == "BTC"]), unique(pg$panel[pg$pair == "ETH"]))
p5 <- ggplot(pg, aes(DD_0.95)) + geom_histogram(binwidth = 2.5, boundary = 0, fill = "grey75", colour = "black", linewidth = 0.2) +
  geom_vline(data = pr, aes(xintercept = DD_0.95), colour = "black", linewidth = 0.8, linetype = "dashed") +
  geom_text(data = pr, aes(x = DD_0.95, y = Inf, label = "Real ETF date"), vjust = 1.5, hjust = -0.05, size = 3, family = "serif") +
  facet_wrap(~panel, scales = "free") + labs(x = "DD(0.95) = Delta(0.95) - Delta(0.50)", y = "Number of placebo dates") + tema
ggsave(file.path(FIG, "V7-F5.8_placebo_DD095_histogram.png"), p5, width = 8, height = 3.8, dpi = 300)

## Fig. 5 / Fig. S2 — diferencial saida - entrada, estado em t-1
lp1 <- lp5[lp5$spec == "state_tminus1", ]; lp1$panel <- ifelse(lp1$pair == "BTC", "(a) BTC", "(b) ETH")
p4 <- ggplot(lp1, aes(h, diferencial)) + geom_hline(yintercept = 0, linewidth = 0.3) + geom_ribbon(aes(ymin = ci95_lo, ymax = ci95_hi), fill = "grey80") +
  geom_line(colour = "black", linewidth = 0.8) + geom_point(colour = "black", size = 1.5) + facet_wrap(~panel) + scale_x_continuous(breaks = 0:10) +
  labs(x = "Horizon h (trading days)", y = "Outflow - inflow differential") + tema
ggsave(file.path(FIG, "V7-F5.11_flow_differential_state_tminus1.png"), p4, width = 8, height = 3.6, dpi = 300)

## Fig. 4 — contrafactual na mediana e em tau = 0,95
f <- split_joint(t1a); f <- f[f$entity_code %in% ordem & f$tau %in% c(0.50, 0.95), ]
f$label <- c(TREATED = "Treated", TREATED_EXCL = "Treated excl.\nAug 2024", XRP = "XRP*", LTC = "LTC*", XLM = "XLM", ADA = "ADA", TRX = "TRX*", BasketA = "Basket A", BasketB = "Basket B")[f$entity_code]
f$group <- factor(c(TREATED = "Treated", TREATED_EXCL = "Treated excl. 22 Jul-16 Aug 2024", XRP = "Control", LTC = "Control", XLM = "Control", ADA = "Control", TRX = "Control", BasketA = "Basket average", BasketB = "Basket average")[f$entity_code],
                  levels = c("Treated","Treated excl. 22 Jul-16 Aug 2024","Control","Basket average"))
f$panel <- factor(sprintf("%s split (treated = %s), tau = %.2f", toupper(f$cut), toupper(f$cut), f$tau), levels = c("BTC split (treated = BTC), tau = 0.50","BTC split (treated = BTC), tau = 0.95","ETH split (treated = ETH), tau = 0.50","ETH split (treated = ETH), tau = 0.95"))
f$label <- factor(f$label, levels = c("Treated","Treated excl.\nAug 2024","XRP*","LTC*","XLM","ADA","TRX*","Basket A","Basket B"))
p3b <- ggplot(f, aes(label, point, fill = group)) + geom_hline(yintercept = 0, linewidth = 0.3) + geom_col(width = 0.7, colour = "black", linewidth = 0.2) +
  geom_errorbar(aes(ymin = ci95_lo, ymax = ci95_hi), width = 0.25, linewidth = 0.4) + facet_wrap(~panel, scales = "free_y", ncol = 2) + scale_fill_manual(values = c("grey15", "white", "grey70", "grey45")) +
  labs(x = NULL, y = "Pre/post change in NPDC (post - pre)") + theme_classic(base_size = 9, base_family = "serif") +
  theme(plot.title = element_blank(), strip.background = element_blank(), strip.text = element_text(hjust = 0), legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(FIG, "V7-F3_counterfactual_median_and_upper_tail.png"), p3b, width = 9, height = 6.5, dpi = 300)

progresso("20: figuras Fig. 4, Fig. 5/S2 e Fig. S1 gravadas.")
log_v7(sprintf("20_figuras concluido em %.1f s", as.numeric(Sys.time() - t0, units = "secs")))
