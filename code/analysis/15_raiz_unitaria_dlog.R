# =====================================================================
# V7-3 / Prompt 4, TAREFA 7 — VIX: estacionariedade (ADF AIC/BIC, KPSS) e
# especificacao em Dlog na base nova (copia parametrizada de v6_10_tarefa7.R).
# 7b: defasagem por BIC (regra da V4); ruido U(-0.005,0.005) somado a
# Dlog(VIX)x100 no bootstrap (mesmo desvio D6 da V6). Semente 20260930 + 7.
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
suppressPackageStartupMessages({ library(urca); library(tseries); library(vars) })
t0 <- Sys.time(); log_pkgs7("v7_27_p4_tarefa7.R")
K <- 7; SEED <- SEED_BASE_V6 + K
D <- carregar_v4(); vx <- ler_vix()
adf_sel <- function(x, crit) { n <- length(x); kmax <- trunc(12 * (n / 100)^0.25)
  u <- ur.df(x, type = "drift", lags = kmax, selectlags = crit); k <- sum(grepl("z.diff.lag", rownames(summary(u)@testreg$coefficients)))
  st <- u@teststat[1, "tau2"]; c(stat = st, lags = k, kmax = kmax, p = punitroot(st, N = n, trend = "c", statistic = "t")) }
amostras <- list("BTC pre" = D$amostras$btc$pre, "BTC post" = D$amostras$btc$post, "ETH pre" = D$amostras$eth$pre, "ETH post" = D$amostras$eth$post, "Total" = D$amostras$btc$total)
t7a <- do.call(rbind, lapply(names(amostras), function(nm) { x <- as.numeric(amostras[[nm]][, "VIX"]); a1 <- adf_sel(x, "AIC"); a2 <- adf_sel(x, "BIC")
  kp <- suppressWarnings(kpss.test(x, null = "Level", lshort = TRUE))
  data.frame(sample = nm, n = length(x), first = format(min(index(amostras[[nm]]))), last = format(max(index(amostras[[nm]]))),
             ADF_AIC_stat = a1["stat"], ADF_AIC_lags = a1["lags"], ADF_AIC_p = a1["p"], ADF_BIC_stat = a2["stat"], ADF_BIC_lags = a2["lags"], ADF_BIC_p = a2["p"], ADF_max_lags = a1["kmax"],
             KPSS_stat = unname(kp$statistic), KPSS_lags = unname(kp$parameter), KPSS_p = kp$p.value,
             KPSS_p_note = ifelse(kp$p.value <= 0.01, "p <= 0.01 (table bound)", ifelse(kp$p.value >= 0.1, "p >= 0.10 (table bound)", ""))) }))
rownames(t7a) <- NULL; print(t7a)
write.csv(t7a, file.path(OUT7, "csv", "V7-6.7a_unit_root_full.csv"), row.names = FALSE)

vdl <- diff(log(vx)) * 100; vdl <- vdl[!is.na(vdl)]; colnames(vdl) <- "VIX"
mk_pair <- function(ativo) { r <- D$base_xts[, ativo]; z <- na.omit(merge(r, vdl, join = "inner")); z[index(z) >= DATE_INI & index(z) <= DATE_FIM] }
Zd <- list(btc = mk_pair("BTC"), eth = mk_pair("ETH"))
stopifnot(all(sapply(c("btc","eth"), function(p) identical(format(as.Date(index(Zd[[p]]))), format(as.Date(index(D$amostras[[p]]$total)))))))
sel_ordem <- function(Z, pmax = 8) { Zm <- as.matrix(Z); out <- data.frame(p = 1:pmax, AIC = NA_real_, BIC = NA_real_)
  for (p in 1:pmax) { fit <- VAR(Zm, p = p, type = "const"); k <- length(fit$varresult); Tobs <- fit$obs
    Sigma <- crossprod(do.call(cbind, lapply(fit$varresult, resid))) / Tobs; ldet <- log(det(Sigma)); np <- k * (1 + k * p)
    out$AIC[p] <- ldet + (2 / Tobs) * np; out$BIC[p] <- ldet + (log(Tobs) / Tobs) * np }; out }
sel <- do.call(rbind, lapply(c("btc","eth"), function(p) do.call(rbind, lapply(c("total","pre","post"), function(s) {
  Z <- if (s == "total") Zd[[p]] else if (s == "pre") Zd[[p]][index(Zd[[p]]) < CUTS[[p]]] else Zd[[p]][index(Zd[[p]]) >= CUTS[[p]]]
  o <- sel_ordem(Z); data.frame(pair = toupper(p), sample = s, p_BIC = o$p[which.min(o$BIC)], p_AIC = o$p[which.min(o$AIC)]) })))); print(sel)
NL <- max(sel$p_BIC[sel$sample == "total"])
log_v7(sprintf("7b: defasagem por BIC (amostra total; BTC %d, ETH %d) -> p = %d", sel$p_BIC[sel$pair == "BTC" & sel$sample == "total"], sel$p_BIC[sel$pair == "ETH" & sel$sample == "total"], NL))
pairs <- lapply(c(BTC = "btc", ETH = "eth"), function(p) list(pre = coredata(Zd[[p]][index(Zd[[p]]) < CUTS[[p]]]), post = coredata(Zd[[p]][index(Zd[[p]]) >= CUTS[[p]]])))
pt <- point_sep(pairs, nlag = NL)
npdc_tab <- do.call(rbind, lapply(names(pt), function(nm) data.frame(pair = nm, tau = TAUS, NPDC_pre = as.numeric(pt[[nm]]$pre), NPDC_post = as.numeric(pt[[nm]]$post), Delta = as.numeric(pt[[nm]]$post - pt[[nm]]$pre))))
print(npdc_tab)
tst <- run_sep_boot(pairs, B_TEST, SEED, nlag = NL); log_v7(sprintf("7b teste B=20: BTC %d, ETH %d", sum(tst$BTC$valid), sum(tst$ETH$valid)))
bt <- run_sep_boot(pairs, B_FULL, SEED, nlag = NL); log_v7(sprintf("7b B=500: BTC %d, ETH %d; timeouts %d", sum(bt$BTC$valid), sum(bt$ETH$valid), attr(bt, "n_timeout")))
tab7 <- tab_sep(pt, bt)
for (tk in c("0.05","0.50","0.95")) { st <- paste0("Delta_", tk)
  f <- function(p) { S <- sapply(1:B_FULL, function(b) bt[[p]]$post[b, tk] - bt[[p]]$pre[b, tk]); S[!bt[[p]]$valid] <- NA; S }
  tab7 <- rbind(tab7, resumir_v6(unname(pt$BTC$stats[st] - pt$ETH$stats[st]), f("BTC") - f("ETH"), series = "BTC-ETH", statistic = st)) }
print(tab7[, c("series","statistic","point","boot_median","ci95_lo","ci95_hi","ci95_excludes_0")])
ctrl <- lapply(setNames(CONTROLES, CONTROLES), function(a) { d <- read.csv(CTRL_ARQ[a]); d$date <- as.Date(d$date); d <- d[order(d$date), ]
  Z <- ret_ii(d$date, d$close, vx, a); Z <- merge(Z[, a], vdl, join = "inner"); colnames(Z) <- c(a, "VIX")
  Z <- Z[index(Z) >= DATE_INI & index(Z) <= DATE_FIM]; if (!is.na(TRUNCA[a])) Z <- Z[index(Z) <= as.Date(TRUNCA[a])]; na.omit(Z) })
sis <- list()
for (cn in c("btc","eth")) { S <- c(list(TREATED = to_list(Zd[[cn]])), lapply(ctrl, to_list)); todas <- sort(unique(do.call(c, lapply(S, `[[`, "date"))))
  sis[[cn]] <- list(series = S, taus = c(0.50, 0.95), baskets = CESTAS, treated = "TREATED", nlag = NL, master_pre = todas[todas < CUTS[[cn]]], master_post = todas[todas >= CUTS[[cn]]]) }
sis <- point_joint(sis)
tstj <- run_joint_boot(sis, B_TEST, SEED); log_v7(sprintf("7b contrafactual teste B=20: BTC %d, ETH %d", sum(tstj$btc$valid), sum(tstj$eth$valid)))
btj <- run_joint_boot(sis, B_FULL, SEED); log_v7(sprintf("7b contrafactual B=500: BTC %d, ETH %d; timeouts %d", sum(btj$btc$valid), sum(btj$eth$valid), attr(btj, "n_timeout")))
tab7c <- tab_joint(sis, btj)
print(tab7c[grepl("TREATED_|Basket", tab7c$statistic), c("cut","statistic","point","ci95_lo","ci95_hi","ci95_excludes_0")])
write.csv(sel, file.path(OUT7, "csv", "V7-6.7b_lag_selection.csv"), row.names = FALSE)
write.csv(npdc_tab, file.path(OUT7, "csv", "V7-6.7b_npdc_by_quantile.csv"), row.names = FALSE)
write.csv(tab7, file.path(OUT7, "csv", "V7-6.7b_table1_dlog.csv"), row.names = FALSE)
write.csv(tab7c, file.path(OUT7, "csv", "V7-6.7b_counterfactual_dlog.csv"), row.names = FALSE)
saveRDS(list(t7a = t7a, sel = sel, NL = NL, pt = pt, npdc_tab = npdc_tab, bt = bt, tab7 = tab7, point_cf = lapply(sis, `[[`, "point"), btj = btj, tab7c = tab7c), file.path(OUT7, "rds", "v7_6_7.rds"))
progresso("V7-3 Tarefa 7 (ADF/KPSS, Dlog) concluida.")
log_v7(sprintf("v7_27 (Tarefa 7) concluido em %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
