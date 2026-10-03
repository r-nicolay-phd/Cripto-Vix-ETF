# =====================================================================
# COLETA 4 — Preço, volume e cotas dos ETFs (Yahoo Finance)
# BTC: IBIT, FBTC, GBTC, ARKB, BITB | ETH: ETHA, FETH, ETHE | Futuros: BITO
# =====================================================================

suppressPackageStartupMessages({
  library(quantmod); library(dplyr); library(readr); library(xts)
})

PROJ_DIR <- normalizePath(Sys.getenv("CVE_ROOT", "."), winslash = "/")
setwd(PROJ_DIR)
hoje <- format(Sys.Date(), "%Y-%m-%d")
dir_raw   <- file.path("data/raw", hoje)
dir_clean <- "data/clean"
dir.create(dir_raw,   recursive = TRUE, showWarnings = FALSE)
dir.create(dir_clean, recursive = TRUE, showWarnings = FALSE)

log_lines <- character(0)
log_msg <- function(...) { msg <- paste0(...); cat(msg, "\n"); log_lines <<- c(log_lines, msg) }

tickers_btc <- c("IBIT","FBTC","GBTC","ARKB","BITB")
tickers_eth <- c("ETHA","FETH","ETHE")
ticker_futuros <- "BITO"
todos <- c(tickers_btc, tickers_eth, ticker_futuros)

log_msg("## Item 4 — ETFs de preço/volume (Yahoo Finance via quantmod)")

coletar_ticker <- function(tk, from = "2014-01-01") {
  res <- tryCatch({
    x <- getSymbols(tk, src = "yahoo", from = from, to = Sys.Date(), auto.assign = FALSE)
    df <- data.frame(date = index(x), coredata(x))
    names(df) <- c("date","open","high","low","close","volume","adjusted")
    df
  }, error = function(e) { log_msg(sprintf("%s: ERRO — %s", tk, conditionMessage(e))); NULL })
  res
}

resultados <- list()
for (tk in todos) {
  df <- coletar_ticker(tk)
  if (!is.null(df)) {
    write_csv(df, file.path(dir_raw, paste0(tk, "_yahoo_raw.csv")))
    write_csv(df, file.path(dir_clean, paste0("etf_", tolower(tk), "_yahoo.csv")))
    log_msg(sprintf("%s: %d obs. (%s a %s)", tk, nrow(df), format(min(df$date)), format(max(df$date))))
    resultados[[tk]] <- df
  }
}

## ---- Checagem de cobertura esperada -----------------------------------
log_msg("\n## Cobertura esperada")
esperado <- list(IBIT="2024-01-11", FBTC="2024-01-11", GBTC="2013-09-25", ARKB="2024-01-11",
                  BITB="2024-01-11", ETHA="2024-07-23", FETH="2024-07-23", ETHE="2019-12-10",
                  BITO="2021-10-19")
for (tk in names(esperado)) {
  if (!is.null(resultados[[tk]])) {
    primeira <- min(resultados[[tk]]$date)
    log_msg(sprintf("%s: esperado desde ~%s | coletado desde %s", tk, esperado[[tk]], format(primeira)))
  }
}

log_msg("\nNOTA (codebook): volume negociado no mercado secundário NÃO é fluxo de criação/resgate de cotas do ETF (ver Itens 2-3 para fluxo líquido).")
log_msg("NOTA: número de cotas em circulação (shares outstanding) não está disponível no Yahoo Finance via quantmod; não coletado neste script. Ver pendência no codebook.")

writeLines(c(paste0("# Log de coleta — Item 4 (preço/volume ETFs) — ", hoje), "", log_lines),
           file.path("data", "coleta_log_item4.md"))

cat("\n== FIM COLETA 4 ==\n")
