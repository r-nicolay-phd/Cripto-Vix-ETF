# =====================================================================
# CORRECAO DO DESLOCAMENTO DE DATAS DO EXPORT CMC (base da V7)
# ---------------------------------------------------------------------
# O export CoinMarketCap (data/raw/manual/{BTC,ETH}_cmc.xlsx) traz na
# linha D o fechamento do dia UTC D-1. Re-rotula cada linha para D-1
# (fechamento 24:00 UTC do proprio dia, mesma convencao dos controles
# Binance) e reconstroi os retornos pela construcao (ii): intersecao com
# o calendario do VIX primeiro, log-retorno x 100 entre pregoes
# consecutivos.
#
# Saida: data/clean/base_estendida_v7.csv
#        data/clean/verificacao_datas_cmc_v7.csv
# =====================================================================

setwd(normalizePath(Sys.getenv("CVE_ROOT", "."), winslash = "/"))
Sys.setlocale("LC_TIME", "C")
suppressPackageStartupMessages({ library(openxlsx); library(readr); library(dplyr) })

vix <- read_csv("data/clean/vix_fred.csv", show_col_types = FALSE)
names(vix)[1:2] <- c("date", "VIX"); vix$date <- as.Date(vix$date); vix <- vix[!is.na(vix$VIX), ]

ler_cmc <- function(ativo) {
  m <- read.xlsx(file.path("data/raw/manual", paste0(ativo, "_cmc.xlsx")), detectDates = TRUE)
  m$Date <- as.Date(m$Date); m <- m[order(m$Date), ]; m <- m[!duplicated(m$Date), ]
  data.frame(date = m$Date - 1, close = m[[grep("Close$", names(m), value = TRUE)[1]]])  # re-rotulo D -> D-1
}
ret_ii <- function(px, sigla) {
  x <- inner_join(px, vix[, "date"], by = "date") %>% arrange(date)
  x[[sigla]] <- c(NA, diff(log(x$close))) * 100
  x[, c("date", sigla)]
}

btc <- ler_cmc("BTC"); eth <- ler_cmc("ETH")
base <- full_join(ret_ii(btc, "BTC"), ret_ii(eth, "ETH"), by = "date") %>%
  inner_join(vix[, c("date", "VIX")], by = "date") %>%
  filter(!is.na(BTC) | !is.na(ETH)) %>% arrange(date)
write_csv(base, "data/clean/base_estendida_v7.csv")

## Verificacao contra Binance diario (dia UTC de abertura), por ano
verif <- list()
for (a in c("BTC", "ETH")) {
  bin <- read_csv(sprintf("data/raw/binance/%sUSDT_binance_1d_raw.csv", a), show_col_types = FALSE)
  bin <- data.frame(date = as.Date(bin$date), cb = bin$close)
  cm <- if (a == "BTC") btc else eth
  m <- inner_join(cm, bin, by = "date") %>% arrange(date)
  m <- m[c(TRUE, diff(m$date) == 1), ]
  r1 <- diff(log(m$close)); r2 <- diff(log(m$cb)); ano <- format(m$date[-1], "%Y")
  for (y in unique(ano)) {
    i <- ano == y
    verif[[length(verif) + 1]] <- data.frame(asset = a, year = y, n = sum(i),
      corr_same_date = cor(r1[i], r2[i]),
      max_abs_level_diff_pct = max(abs(m$close[-1][i] / m$cb[-1][i] - 1)) * 100)
  }
}
verif <- do.call(rbind, verif)
write_csv(verif, "data/clean/verificacao_datas_cmc_v7.csv")
print(verif, digits = 3)

cat("\nbase_estendida_v7:", format(min(base$date)), "a", format(max(base$date)), nrow(base), "linhas\n")
for (a in c("BTC", "ETH")) {
  n_ini <- sum(!is.na(base[[a]]) & base$date >= as.Date("2018-01-01"))
  cat(a, "- obs. desde 2018-01-01:", n_ini, "\n")
}
cat("Linhas 2024-01-08..12 (v7):\n"); print(as.data.frame(base[base$date >= as.Date("2024-01-08") & base$date <= as.Date("2024-01-12"), ]))
