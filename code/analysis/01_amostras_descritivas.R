# =====================================================================
# V7-1 (parte 1) — AMOSTRAS, DESCRITIVAS E ADF (copia parametrizada do
# scripts/20_amostra_atualizada.R).
#  * Monta as amostras bivariadas (total/pre/pos) por par, com a MESMA
#    funcao do script 20 (date_ini 2018-01-01; intersecao ativo x VIX),
#    para a base final (base_estendida_v7.csv).
#  * CFG$nlag = 2 fixado (especificacao travada; o BIC e so reportado em
#    v7_03_blocos_base.R).
#  * Descritivas e ADF do VIX (tseries::adf.test, k = trunc((n-1)^(1/3)),
#    constante sem tendencia -- mesma regra documentada na V4) na base nova.
# Saidas: results/rds/dados_analise_v7.rds, results/csv/V7-1_sample_sizes.csv,
#         results/csv/V7-1_descriptives.csv, V7-1_adf.csv
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
suppressPackageStartupMessages({ library(tseries) })
t0 <- Sys.time(); log_pkgs7("v7_01_dados.R")

CFG_BASE <- list(cut_btc = as.Date("2024-01-11"), cut_eth = as.Date("2024-07-23"),
                 date_ini = as.Date("2018-01-01"), date_fim = NULL,
                 taus = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95), extreme = c(0.05, 0.95), mediana = 0.50,
                 nlag = 2, nfore = 10, window = 200, pares = list(btc = c("BTC","VIX"), eth = c("ETH","VIX")))

to_zoo <- function(x) { z <- as.zoo(x); attr(z, "frequency") <- NULL; z }
montar_amostras <- function(base_xts, ativo, cut, date_ini, date_fim) {
  Z <- base_xts[, c(ativo, "VIX")]
  Z <- Z[paste0(date_ini, "/", date_fim)]
  Z <- na.omit(Z)
  list(total = to_zoo(Z), pre = to_zoo(Z[index(Z) < cut]), post = to_zoo(Z[index(Z) >= cut]))
}
montar_D <- function(arq) {
  base <- read.csv(arq, stringsAsFactors = FALSE); base$date <- as.Date(base$date); base <- base[order(base$date), ]
  base_xts <- xts(base[, c("BTC","ETH","VIX")], order.by = base$date)
  CFG <- CFG_BASE; CFG$date_fim <- max(base$date); CFG$base_file <- arq
  list(amostras = list(btc = montar_amostras(base_xts, "BTC", CFG$cut_btc, CFG$date_ini, CFG$date_fim),
                       eth = montar_amostras(base_xts, "ETH", CFG$cut_eth, CFG$date_ini, CFG$date_fim)),
       base_xts = base_xts, CFG = CFG)
}
Dn <- montar_D(BASE_V7)
saveRDS(Dn, RDS_NEW)

nn <- do.call(rbind, lapply(c("btc","eth"), function(p)
  data.frame(pair = toupper(p), first = format(min(index(Dn$amostras[[p]]$total))), last = format(max(index(Dn$amostras[[p]]$total))),
             n_total = nrow(Dn$amostras[[p]]$total), n_pre = nrow(Dn$amostras[[p]]$pre), n_post = nrow(Dn$amostras[[p]]$post))))
print(nn)
write.csv(nn, file.path(OUT7, "csv", "V7-1_sample_sizes.csv"), row.names = FALSE)

## ---- descritivas (base nova) -------------------------------------------
describe_col <- function(x) {
  x <- as.numeric(x); x <- x[!is.na(x)]
  c(n = length(x), mean = mean(x), median = median(x), sd = sd(x), min = min(x), max = max(x),
    skewness = mean((x - mean(x))^3) / sd(x)^3, kurtosis = mean((x - mean(x))^4) / sd(x)^4)
}
descr <- do.call(rbind, lapply(c("btc","eth"), function(par) do.call(rbind, lapply(c("total","pre","post"), function(s) {
  Z <- Dn$amostras[[par]][[s]]
  do.call(rbind, lapply(c(toupper(par), "VIX"), function(v) data.frame(pair = toupper(par), sample = s, variable = v, t(describe_col(Z[, v])))))
}))))
rownames(descr) <- NULL; print(descr)
write.csv(descr, file.path(OUT7, "csv", "V7-1_descriptives.csv"), row.names = FALSE)

adf_com_regra <- function(x, rotulo) {
  x <- na.omit(as.numeric(x)); a <- adf.test(x)
  data.frame(series = rotulo, n = length(x), statistic = unname(a$statistic), lag_order = unname(a$parameter),
             p_value = a$p.value, stationary_5pct = a$p.value < 0.05,
             lag_rule = "k = trunc((n-1)^(1/3)) [tseries::adf.test default, constant, no trend]")
}
adf <- rbind(adf_com_regra(Dn$amostras$btc$total[, "VIX"], "VIX level (V7 sample: from 2018-01-01, BTC intersection)"),
             adf_com_regra(Dn$amostras$eth$total[, "VIX"], "VIX level (V7 sample: from 2018-01-01, ETH intersection)"),
             adf_com_regra(Dn$base_xts[, "VIX"], "VIX level (raw, whole base_estendida_v7)"),
             adf_com_regra(Dn$amostras$btc$total[, "BTC"], "BTC return (V7 sample)"),
             adf_com_regra(Dn$amostras$eth$total[, "ETH"], "ETH return (V7 sample)"))
print(adf)
write.csv(adf, file.path(OUT7, "csv", "V7-1_adf.csv"), row.names = FALSE)
log_v7(sprintf("01: amostras BTC %d/%d/%d, ETH %d/%d/%d; %.1f s",
               nn$n_total[1], nn$n_pre[1], nn$n_post[1], nn$n_total[2], nn$n_pre[2], nn$n_post[2], as.numeric(Sys.time() - t0, units = "secs")))
