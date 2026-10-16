# =====================================================================
# V9 — TAREFA 2: estabilidade do QVAR e especificacoes alternativas.
# Semente 20261015 + 2.
#  2a  soma dos coeficientes proprios do VIX e maior modulo de autovalor da
#      matriz companheira (p = 2), par x subamostra (pre/pos/total) x tau.
#  2b  NPDC por horizonte H = 1..20, tau 0.50/0.90/0.95, pre e pos;
#      Figura V9-F2.
#  2c  (a) Dlog VIX, p = 2; (b) log VIX, p = 2; (c) VIX em nivel, H = 1;
#      (d) VIX em nivel com retornos padronizados por GARCH(1,1) estimado
#      separadamente em cada subamostra. NPDC por quantil (pre/pos) e
#      Delta(0.50), Delta(0.95), DD(0.95) com o bootstrap da Tarefa 1.
#      Em (d) o GARCH e estimado uma vez na amostra observada e os
#      retornos padronizados entram no bootstrap (nao reestimado por
#      replica).
#  2d  Regra A (mecanica).
# Origem: v9_03_tarefa2.R. Alteracoes: caminhos; em 2c so as estimativas
# pontuais (NPDC por quantil de cada variante, usadas na Tabela S14,
# Painel A, e na Regra A); o bootstrap da V9 (VIX reconstruido) das
# variantes foi substituido por 32_v10_especificacoes.R (Tabela S14,
# Painel B) e nao e reproduzido. Os retornos padronizados por GARCH
# (results/rds/v9_2_garch.rds) sao lidos por 32_v10_especificacoes.R.
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_v9.R"))
suppressPackageStartupMessages({ library(ggplot2); library(rugarch) })
t0 <- Sys.time(); log_pkgs9("v9_03_tarefa2.R")
K <- 2; SEED <- SEED_V9 + K
D <- carregar_v4()
RDS <- function(nm) file.path(OUT9, "rds", paste0("v9_2_", nm, ".rds"))
run_or_load <- function(nm, expr) { f <- RDS(nm); if (file.exists(f)) { log_v9("2: bloco ", nm, " ja existe -- carregado"); return(readRDS(f)) }
  x <- eval.parent(substitute(expr)); saveRDS(x, f); x }
SUBS <- c("pre","post","total")
Mget <- function(cn, s) coredata(D$amostras[[cn]][[s]])

## ---------------- 2a -----------------------------------------------------
R2a <- run_or_load("roots", do.call(rbind, lapply(c("btc","eth"), function(cn) do.call(rbind, lapply(SUBS, function(s) do.call(rbind, lapply(TAUS, function(tau) {
  f <- qvar_fit(Mget(cn, s), tau); iv <- which(f$names == "VIX"); ia <- setdiff(1:2, iv)
  data.frame(pair = toupper(cn), sample = s, tau = tau, n = nrow(Mget(cn, s)),
             VIX_own_lag1 = f$B[iv, iv], VIX_own_lag2 = f$B[iv, 2 + iv], VIX_own_sum = vix_own_sum(f),
             asset_own_sum = sum(f$B[ia, c(ia, 2 + ia)]), max_modulus_companion = comp_maxmod(f))
})))))))
print(R2a)
write.csv(R2a, file.path(OUT9, "csv", "V9-2a_roots_raw.csv"), row.names = FALSE)
progresso(sprintf("Tarefa 2a concluida: maior modulo (tau 0,95) max = %.3f; em todo o grid max = %.3f.",
                  max(R2a$max_modulus_companion[R2a$tau == 0.95]), max(R2a$max_modulus_companion)))

## ---------------- 2b -----------------------------------------------------
TB <- c(0.50, 0.90, 0.95)
R2b <- run_or_load("horizon", do.call(rbind, lapply(c("btc","eth"), function(cn) do.call(rbind, lapply(c("pre","post"), function(s) do.call(rbind, lapply(TB, function(tau) {
  f <- qvar_fit(Mget(cn, s), tau); a <- toupper(cn)
  do.call(rbind, lapply(1:20, function(H) { CT <- ct_from_fit(f, H)
    data.frame(pair = a, sample = s, tau = tau, H = H, NPDC = CT["VIX", a] - CT[a, "VIX"], a_from_VIX = CT[a, "VIX"], VIX_from_a = CT["VIX", a]) }))
})))))))
## confere H = 10 com a Tabela 1
v1 <- readRDS(file.path(OUT9, "rds", "v9_1_points.rds"))
chk <- max(abs(sapply(seq_len(nrow(R2b[R2b$H == 10, ])), function(i) { r <- R2b[R2b$H == 10, ][i, ]; v1[[r$pair]][[r$sample]][sprintf("%.2f", r$tau)] - r$NPDC })))
log_v9(sprintf("2b: NPDC(H = 10) = Tabela 1: dif. max %.2e", chk))
write.csv(R2b, file.path(OUT9, "csv", "V9-2b_npdc_by_horizon_raw.csv"), row.names = FALSE)
fd <- R2b; fd$panel <- factor(paste0(fd$pair, ", ", ifelse(fd$sample == "pre", "pre-ETF", "post-ETF")), levels = c("BTC, pre-ETF","BTC, post-ETF","ETH, pre-ETF","ETH, post-ETF"))
fd$tau_lab <- factor(sprintf("tau = %.2f", fd$tau), levels = sprintf("tau = %.2f", TB))
pF2 <- ggplot(fd, aes(H, NPDC, linetype = tau_lab, shape = tau_lab, colour = tau_lab)) + geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_line(linewidth = 0.6) + geom_point(size = 1.4) + facet_wrap(~panel, ncol = 2) +
  scale_colour_manual(values = c("grey60", "grey35", "black")) + scale_linetype_manual(values = c("dotted", "dashed", "solid")) + scale_shape_manual(values = c(1, 2, 16)) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20)) + labs(x = "Forecast horizon H (days)", y = "NPDC (asset with the VIX)") +
  theme_classic(base_size = 10, base_family = "serif") + theme(plot.title = element_blank(), strip.background = element_blank(), strip.text = element_text(hjust = 0),
                                                              legend.position = "bottom", legend.title = element_blank())
ggsave(file.path(OUT9, "fig", "V9-F2_npdc_by_horizon.png"), pF2, width = 8, height = 6, dpi = 300)
progresso("Tarefa 2b concluida (NPDC por horizonte; Figura V9-F2).")

## ---------------- 2c -----------------------------------------------------
garch_std <- function(x) {
  sp <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = "norm")
  f <- ugarchfit(sp, as.numeric(x), solver = "hybrid")
  list(z = as.numeric(residuals(f, standardize = TRUE)), coef = coef(f), conv = convergence(f), persistence = persistence(f))
}
G <- run_or_load("garch", {
  out <- list(); info <- list()
  for (cn in c("btc","eth")) for (s in c("pre","post")) { Z <- D$amostras[[cn]][[s]]; g <- garch_std(Z[, toupper(cn)])
    Zs <- Z; Zs[, toupper(cn)] <- g$z; out[[cn]][[s]] <- Zs
    info[[length(info) + 1]] <- data.frame(pair = toupper(cn), sample = s, n = nrow(Z), t(g$coef), persistence = g$persistence, convergence_code = g$conv,
                                           sd_standardized = sd(g$z)) }
  list(Z = out, info = do.call(rbind, info))
})
print(G$info); write.csv(G$info, file.path(OUT9, "csv", "V9-2c_garch_parameters.csv"), row.names = FALSE)

VARIANTS <- list(
  a = list(label = "(a) Dlog VIX, p = 2", spec = "dlog", nfore = 10, garch = FALSE),
  b = list(label = "(b) log VIX level, p = 2", spec = "log", nfore = 10, garch = FALSE),
  c = list(label = "(c) VIX level, H = 1", spec = "level", nfore = 1, garch = FALSE),
  d = list(label = "(d) VIX level, GARCH(1,1)-standardized crypto returns", spec = "level", nfore = 10, garch = TRUE))
pairs_v <- function(v) lapply(c(BTC = "btc", ETH = "eth"), function(cn) {
  if (v$garch) list(pre = mk_S(G$Z[[cn]]$pre), post = mk_S(G$Z[[cn]]$post)) else list(pre = mk_S(D$amostras[[cn]]$pre), post = mk_S(D$amostras[[cn]]$post)) })
R2c <- list()
for (vn in names(VARIANTS)) {
  v <- VARIANTS[[vn]]
  R2c[[vn]] <- run_or_load(paste0("alt_", vn), {
    pr <- pairs_v(v); ptv <- point_sep9(pr, spec = v$spec, nfore = v$nfore)
    npdc <- do.call(rbind, lapply(names(ptv), function(nm) data.frame(pair = nm, tau = TAUS, NPDC_pre = as.numeric(ptv[[nm]]$pre), NPDC_post = as.numeric(ptv[[nm]]$post),
                                                                    Delta = as.numeric(ptv[[nm]]$post - ptv[[nm]]$pre))))
    list(variant = vn, label = v$label, pt = ptv, npdc = cbind(variant = v$label, npdc))
  })
  progresso(sprintf("Tarefa 2c variante %s concluida.", vn))
}
npdc2c <- do.call(rbind, lapply(R2c, `[[`, "npdc"))
print(npdc2c)
write.csv(npdc2c, file.path(OUT9, "csv", "V9-2c_alt_specs_npdc_raw.csv"), row.names = FALSE)

## ---------------- 2d: Regra A ---------------------------------------------
padrao <- function(npdc_df, variant) do.call(rbind, lapply(c("BTC","ETH"), function(a) { d <- npdc_df[npdc_df$pair == a, ]
  g <- function(t) d$NPDC_pre[abs(d$tau - t) < 1e-9]
  data.frame(variant = variant, pair = a, NPDC_pre_0.95 = g(0.95), cond_095_lt_minus3 = g(0.95) < -3,
             NPDC_pre_0.25 = g(0.25), NPDC_pre_0.50 = g(0.50), NPDC_pre_0.75 = g(0.75),
             max_abs_mid = max(abs(c(g(0.25), g(0.50), g(0.75)))), cond_mid_le_3 = all(abs(c(g(0.25), g(0.50), g(0.75))) <= 3),
             pattern_present = g(0.95) < -3 & all(abs(c(g(0.25), g(0.50), g(0.75))) <= 3)) }))
base_npdc <- do.call(rbind, lapply(c("BTC","ETH"), function(a) data.frame(pair = a, tau = TAUS, NPDC_pre = as.numeric(v1[[a]]$pre), NPDC_post = as.numeric(v1[[a]]$post))))
ra <- rbind(padrao(base_npdc, "Baseline (VIX level, p = 2, H = 10)"), do.call(rbind, lapply(R2c, function(x) padrao(x$npdc, x$label))))
ra$pattern_both_pairs <- ave(ra$pattern_present, ra$variant, FUN = all)
cond1 <- R2a[R2a$tau == 0.95, c("pair","sample","max_modulus_companion")]
c1 <- all(cond1$max_modulus_companion < 1)
c2 <- all(ra$pattern_present[ra$variant == VARIANTS$c$label]) && all(ra$pattern_present[ra$variant == VARIANTS$d$label])
verd <- data.frame(condition = c("1. Max companion modulus at tau = 0.95 < 1 in all subsamples (pre, post, total) of both pairs",
                                 "2a. Pattern present in variant (c) (H = 1), both pairs", "2b. Pattern present in variant (d) (GARCH-standardized), both pairs", "Rule A verdict"),
                   holds = c(c1, all(ra$pattern_present[ra$variant == VARIANTS$c$label]), all(ra$pattern_present[ra$variant == VARIANTS$d$label]), c1 && c2),
                   detail = c(paste(sprintf("%s %s %.3f", cond1$pair, cond1$sample, cond1$max_modulus_companion), collapse = "; "),
                              paste(sprintf("%s: NPDC_pre(0.95) %.2f, max|NPDC_pre(0.25-0.75)| %.2f", ra$pair, ra$NPDC_pre_0.95, ra$max_abs_mid)[ra$variant == VARIANTS$c$label], collapse = "; "),
                              paste(sprintf("%s: NPDC_pre(0.95) %.2f, max|NPDC_pre(0.25-0.75)| %.2f", ra$pair, ra$NPDC_pre_0.95, ra$max_abs_mid)[ra$variant == VARIANTS$d$label], collapse = "; "),
                              ifelse(c1 && c2, "Finding 1 MAINTAINED", "Finding 1 DOWNGRADED (specification-dependent)")))
print(ra); print(verd)
write.csv(ra, file.path(OUT9, "csv", "V9-RuleA_pattern_by_variant.csv"), row.names = FALSE)
write.csv(verd, file.path(OUT9, "csv", "V9-RuleA_verdict.csv"), row.names = FALSE)
saveRDS(list(R2a = R2a, R2b = R2b, G = G, R2c = R2c, npdc2c = npdc2c, ra = ra, verd = verd, seed = SEED), file.path(OUT9, "rds", "v9_2_all.rds"))
progresso(sprintf("Tarefa 2 concluida. Regra A: condicao 1 %s; padrao em (c) %s; em (d) %s -> %s.", c1,
                  all(ra$pattern_present[ra$variant == VARIANTS$c$label]), all(ra$pattern_present[ra$variant == VARIANTS$d$label]), verd$detail[4]))
log_v9(sprintf("23_v9 (Tarefa 2) concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
