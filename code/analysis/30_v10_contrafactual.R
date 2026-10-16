# =====================================================================
# V10 — 03: contrafactual conjunto por data com linhas do desenho
# (tau 0.25/0.50/0.75/0.95; cestas A, B, B'; H = 10 e H = 5 nas mesmas
# estimativas), poder (EP, MDE = 2,80 EP, TOST com margens |Delta
# tratado| e 0,5 |Delta tratado|), contrafactual ortogonalizado ao BTC
# (betas do pre) e ETH ortogonalizado ao BTC; Fig. 4.
# Sementes: contrafactual 20261016 + 5; ortogonalizado 20261016 + 6.
# Origem: v10_03_contrafactual.R. Alteracoes: caminhos; removidas as
# conferencias dos pontuais com os arquivos da V9 (v9_1_counterfactual,
# v9_1_orthogonalized), que so validavam contra a versao anterior; mantida
# a conferencia tratado = Tabela 1. Usados: Fig. 4, Tabelas S19-S22, S30.
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_v10.R"))
suppressPackageStartupMessages(library(ggplot2))
t0 <- Sys.time(); log_pkgs10("v10_03_contrafactual.R")
D <- carregar_v4(); vx <- ler_vix()
T1 <- readRDS(RDS10("t1_summary")); pt <- T1$pt
ctrl <- lapply(setNames(CONTROLES, CONTROLES), serie_ctrl_xts, vix_xts = vx)
ctrl$SOL <- serie_ctrl_xts("SOL", vx, arq = "data/clean/cesta_sol_binance.csv", trunca = "2025-10-27")
excl_win <- function(Z) Z[!(index(Z) >= JANELA_AGO24[1] & index(Z) <= JANELA_AGO24[2])]
BASK <- list(A = CESTAS$A, B = CESTAS$B, Bp = c(CESTAS$B, "SOL"))
masters <- function(S, cut, pre_end = NULL) { todas <- sort(unique(do.call(c, lapply(S, function(s) c(s$pre$date, s$post$date)))))
  list(pre = if (is.null(pre_end)) todas[todas < cut] else todas[todas <= pre_end], post = todas[todas >= cut]) }

## ---------------- contrafactual ---------------------------------------------
CF <- run_or_load10("cf_main", {
  sis <- list()
  for (cn in c("btc","eth")) { cut <- CUTS[[cn]]; Zt <- as.xts(D$amostras[[cn]]$total); fd <- as.Date(index(Zt))
    S <- c(list(TREATED = mk_series10(Zt, cut), TREATED_EXCL = mk_series10(excl_win(Zt), cut, full_dates = fd)), lapply(ctrl, mk_series10, cut = cut))
    mm <- masters(S, cut)
    sis[[cn]] <- mk_sys10(S, TAUS_CF, BASK, c("TREATED","TREATED_EXCL"), mm$pre, mm$post, h5 = TRUE) }
  sis <- point_joint10(sis)
  chk <- sapply(c(btc = "BTC", eth = "ETH"), function(a) max(abs(sis[[tolower(a)]]$point[paste0("TREATED_Delta_", sprintf("%.2f", TAUS_CF))] - pt[[a]]$stats[paste0("Delta_", sprintf("%.2f", TAUS_CF))])))
  log_v10(sprintf("03 contrafactual: tratado = Tabela 1 (dif. max %.2e)", max(chk)))
  if (max(chk) > 1e-8) { progresso("PARADA: pontuais do contrafactual nao conferem"); stop("cf pontual") }
  seed <- SEED_V10 + 5
  tst <- run_joint_boot10(sis, B_TEST, seed); chk_joint_test(tst, "03 contrafactual")
  tt <- Sys.time(); bt <- run_joint_boot10(sis, B_FULL, seed)
  log_v10(sprintf("03 contrafactual B=500: validas BTC %d, ETH %d; timeouts %d (%.1f min)", sum(bt$btc$valid), sum(bt$eth$valid), attr(bt, "n_timeout"), as.numeric(Sys.time() - tt, units = "mins")))
  list(point = lapply(sis, `[[`, "point"), bt = bt, tab = tab_joint(sis, bt), seed = seed,
       coverage = do.call(rbind, lapply(names(sis), function(cn) do.call(rbind, lapply(names(sis[[cn]]$series), function(nm) { s <- sis[[cn]]$series[[nm]]
         data.frame(cut = toupper(cn), series = nm, first = min(s$pre$date), last = max(s$post$date), n_pre = nrow(s$pre$M), n_post = nrow(s$post$M),
                    design_rows_pre = nrow(s$pre$src), design_rows_post = nrow(s$post$src)) })))))
})
write.csv(CF$tab, file.path(OUT10, "csv", "V10-2_counterfactual_raw.csv"), row.names = FALSE)
write.csv(CF$coverage, file.path(OUT10, "csv", "V10-2_counterfactual_coverage.csv"), row.names = FALSE)
progresso(sprintf("03 contrafactual (B=500, semente %d) concluido: validas BTC %d, ETH %d.", CF$seed, sum(CF$bt$btc$valid), sum(CF$bt$eth$valid)))

## ---------------- poder / TOST ------------------------------------------------
pw <- list()
for (cn in c("btc","eth")) for (tk in sprintf("%.2f", TAUS_CF)) for (cs in c("A","B","Bp")) {
  st <- paste0("TREATEDminusBasket", cs, "_Delta_", tk); x <- CF$bt[[cn]]$mat[CF$bt[[cn]]$valid, st]
  p0 <- unname(CF$point[[cn]][st]); dtr <- unname(CF$point[[cn]][paste0("TREATED_Delta_", tk)])
  q <- quantile(x, c(0.025, 0.05, 0.95, 0.975), names = FALSE); se <- sd(x)
  for (fr in c(1, 0.5)) { dl <- fr * abs(dtr)
    pw[[length(pw) + 1]] <- data.frame(cut = toupper(cn), tau = as.numeric(tk), comparison = paste0("Treated - basket ", sub("Bp", "B'", cs)), delta_treated = dtr, point = p0,
      ci95_lo = q[1], ci95_hi = q[4], ci90_lo = q[2], ci90_hi = q[3], se_boot = se, MDE_80 = 2.80 * se, n_valid = length(x), ci95_excludes_0 = q[1] > 0 | q[4] < 0,
      margin_rule = ifelse(fr == 1, "|Delta treated|", "0.5 x |Delta treated|"), margin = dl, TOST_equivalent = (q[2] > -dl & q[3] < dl)) } }
PW <- do.call(rbind, pw); rownames(PW) <- NULL
write.csv(PW, file.path(OUT10, "csv", "V10-3_power_TOST_raw.csv"), row.names = FALSE)
progresso(sprintf("03 poder/TOST: equivalencia em %d de %d linhas; MDE em 0,95 de %.1f a %.1f.", sum(PW$TOST_equivalent), nrow(PW),
                  min(PW$MDE_80[PW$tau == 0.95]), max(PW$MDE_80[PW$tau == 0.95])))

## ---------------- H = 5: gap tratado - cesta B em 0,95 -------------------------
h5g <- do.call(rbind, lapply(c("btc","eth"), function(cn) do.call(rbind, lapply(c("TREATED_Delta_0.95_H5","BasketB_Delta_0.95_H5","TREATEDminusBasketB_Delta_0.95_H5",
  "TREATED_Delta_0.95","BasketB_Delta_0.95","TREATEDminusBasketB_Delta_0.95"), function(st) {
  x <- CF$bt[[cn]]$mat[, st]; x[!CF$bt[[cn]]$valid] <- NA; resumir_v6(unname(CF$point[[cn]][st]), x, cut = toupper(cn), statistic = st) }))))
write.csv(h5g, file.path(OUT10, "csv", "V10-10_H5_counterfactual_raw.csv"), row.names = FALSE)

## ---------------- ortogonalizado -----------------------------------------------
OR <- run_or_load10("cf_orth", {
  bb <- read.csv("data/clean/btc_binance.csv"); bb$date <- as.Date(bb$date)
  rb_bin <- ret_ii(bb$date, bb$close, vx, "BTC")[, "BTC"]
  rb_cmc <- D$base_xts[, "BTC"]; rb_cmc <- rb_cmc[!is.na(rb_cmc)]
  mk_reg <- function(r) { z <- merge(r, lag(r, 1)); colnames(z) <- c("b0","b1"); z }
  REG <- list(bin = mk_reg(rb_bin), cmc = mk_reg(rb_cmc))
  ortog <- function(Z, nome, reg, cut) {
    Z <- as.xts(Z); y <- Z[, 1]; m <- merge(y, reg, join = "left"); colnames(m) <- c("y","b0","b1"); m <- na.omit(m)
    d <- as.Date(index(m)); fitset <- d < cut
    f <- lm(y ~ b0 + b1, data = as.data.frame(coredata(m))[fitset, ])
    e <- coredata(m$y) - (coef(f)[1] + coef(f)[2] * coredata(m$b0) + coef(f)[3] * coredata(m$b1)); ex <- xts(as.numeric(e), order.by = d)
    out <- merge(ex, vx[d], join = "left"); colnames(out) <- c(nome, "VIX")
    r2 <- function(s) 1 - sum(e[s]^2) / sum((coredata(m$y)[s] - mean(coredata(m$y)[s]))^2)
    list(Z = na.omit(out), info = data.frame(series = nome, n_fit = sum(fitset), alpha = coef(f)[1], beta0 = coef(f)[2], beta1 = coef(f)[3], R2_pre = r2(d < cut), R2_post = r2(d >= cut)))
  }
  sis <- list(); infos <- list()
  for (cn in c("btc","eth")) { cut <- CUTS[[cn]]; Zt <- as.xts(D$amostras[[cn]]$total)
    S <- list(TREATED = mk_series10(Zt, cut)); treated <- "TREATED"
    for (a in c(CONTROLES, "SOL")) { o <- ortog(ctrl[[a]], paste0(a, "_o"), REG$bin, cut); S[[paste0(a, "_o")]] <- mk_series10(o$Z, cut); infos[[length(infos) + 1]] <- cbind(cut = toupper(cn), regressor = "Binance BTC", o$info) }
    if (cn == "eth") { o <- ortog(Zt, "ETH_o", REG$cmc, cut); S$ETH_o <- mk_series10(o$Z, cut); treated <- c(treated, "ETH_o"); infos[[length(infos) + 1]] <- cbind(cut = "ETH", regressor = "base BTC", o$info) }
    mm <- masters(S, cut)
    sis[[cn]] <- mk_sys10(S, TAUS_CF, list(A_o = paste0(CESTAS$A, "_o"), B_o = paste0(CESTAS$B, "_o"), Bp_o = paste0(c(CESTAS$B, "SOL"), "_o")), treated, mm$pre, mm$post) }
  sis <- point_joint10(sis)
  seed <- SEED_V10 + 6
  tst <- run_joint_boot10(sis, B_TEST, seed); chk_joint_test(tst, "03 ortogonalizado")
  tt <- Sys.time(); bt <- run_joint_boot10(sis, B_FULL, seed)
  log_v10(sprintf("03 ortogonalizado B=500: validas BTC %d, ETH %d; timeouts %d (%.1f min)", sum(bt$btc$valid), sum(bt$eth$valid), attr(bt, "n_timeout"), as.numeric(Sys.time() - tt, units = "mins")))
  list(point = lapply(sis, `[[`, "point"), bt = bt, tab = tab_joint(sis, bt), info = do.call(rbind, infos), seed = seed)
})
write.csv(OR$tab, file.path(OUT10, "csv", "V10-4_orthogonalized_raw.csv"), row.names = FALSE)
write.csv(OR$info, file.path(OUT10, "csv", "V10-4_orthogonalization_betas.csv"), row.names = FALSE)
progresso(sprintf("03 ortogonalizado (B=500, semente %d) concluido.", OR$seed))

## ---------------- Fig. 4 ------------------------------------------------------
ordem <- c("TREATED","TREATED_EXCL","XRP","LTC","XLM","ADA","TRX","BasketA","BasketB")
tb <- CF$tab; p <- strsplit(tb$statistic, "_Delta_"); tb$code <- sapply(p, `[`, 1); tb$tau <- suppressWarnings(as.numeric(sapply(p, `[`, 2)))
f <- tb[tb$code %in% ordem & !is.na(tb$tau) & tb$tau %in% c(0.50, 0.95), ]
f$label <- c(TREATED = "Treated", TREATED_EXCL = "Treated excl.\nAug 2024", XRP = "XRP*", LTC = "LTC*", XLM = "XLM", ADA = "ADA", TRX = "TRX*", BasketA = "Basket A", BasketB = "Basket B")[f$code]
f$group <- factor(c(TREATED = "Treated", TREATED_EXCL = "Treated excl. 22 Jul-16 Aug 2024", XRP = "Control", LTC = "Control", XLM = "Control", ADA = "Control", TRX = "Control", BasketA = "Basket average", BasketB = "Basket average")[f$code],
                  levels = c("Treated","Treated excl. 22 Jul-16 Aug 2024","Control","Basket average"))
f$panel <- factor(sprintf("%s split (treated = %s), tau = %.2f", toupper(f$cut), toupper(f$cut), f$tau),
                  levels = c("BTC split (treated = BTC), tau = 0.50","BTC split (treated = BTC), tau = 0.95","ETH split (treated = ETH), tau = 0.50","ETH split (treated = ETH), tau = 0.95"))
f$label <- factor(f$label, levels = c("Treated","Treated excl.\nAug 2024","XRP*","LTC*","XLM","ADA","TRX*","Basket A","Basket B"))
p4 <- ggplot(f, aes(label, point, fill = group)) + geom_hline(yintercept = 0, linewidth = 0.3) + geom_col(width = 0.7, colour = "black", linewidth = 0.2) +
  geom_errorbar(aes(ymin = ci95_lo, ymax = ci95_hi), width = 0.25, linewidth = 0.4) + facet_wrap(~panel, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("grey15", "white", "grey70", "grey45")) + labs(x = NULL, y = "Pre/post change in NPDC (post - pre)") +
  theme_classic(base_size = 9, base_family = "serif") +
  theme(plot.title = element_blank(), strip.background = element_blank(), strip.text = element_text(hjust = 0), legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(OUT10, "fig", "Fig4_counterfactual_median_upper_tail.png"), p4, width = 9, height = 6.5, dpi = 300)
progresso("03 Fig. 4 regenerada com os IC do esquema principal.")
log_v10(sprintf("v10_03 concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
