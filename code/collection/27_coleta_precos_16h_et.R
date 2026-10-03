# =====================================================================
# V6 — TAREFA 4a: coleta de candles HORARIOS (APIs publicas, sem chave)
#  Coinbase Exchange /products/{id}/candles, granularity=3600 (max 300
#  candles por requisicao; paginado em janelas de 300 h, pausa 0,2 s):
#    BTC-USD, ETH-USD, ADA-USD, XLM-USD, XRP-USD, LTC-USD, SOL-USD
#  Binance /api/v3/klines interval=1h (max 1000): TRX (nao listado na
#    Coinbase) e, para cobertura identica a referencia, todos os
#    controles (ADA, XLM, XRP, LTC, TRX, SOL) e BTC/ETH (comparacao).
#  Preco diario "16h ET" = fechamento do candle 15:00-16:00
#  America/New_York (horario de verao respeitado). Se ausente, usa-se o
#  ultimo candle com inicio entre 13:00 e 14:00 ET do mesmo dia
#  (fallback registrado); senao NA.
# Brutos: data/raw/2026-09-30/*_1h_raw.csv (novos arquivos)
# Limpo:  data/clean/px16et_<ativo>_<fonte>.csv (novos arquivos)
# =====================================================================
setwd(normalizePath(Sys.getenv("CVE_ROOT", "."), winslash = "/"))
suppressPackageStartupMessages({ library(httr2); library(jsonlite) })
dir_raw <- "data/raw/2026-09-30"; dir.create(dir_raw, recursive = TRUE, showWarnings = FALSE)
logf <- function(...) { m <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste0(...)); cat(m, "\n"); cat(m, "\n", file = "data/raw/coleta_log.txt", append = TRUE) }
ua <- "Mozilla/5.0 (academic research; V6 pipeline)"
T_INI <- as.POSIXct("2017-12-20 00:00:00", tz = "UTC"); T_FIM <- as.POSIXct("2026-09-20 00:00:00", tz = "UTC")

cb_candles <- function(prod) {
  out <- list(); s <- T_INI; n_req <- 0; n_err <- 0
  while (s < T_FIM) {
    e <- min(s + 300 * 3600, T_FIM)
    req <- request(sprintf("https://api.exchange.coinbase.com/products/%s/candles", prod)) |>
      req_url_query(granularity = 3600, start = format(s, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), end = format(e, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) |>
      req_user_agent(ua) |> req_retry(max_tries = 5, backoff = function(i) 2 * i) |> req_error(is_error = function(r) FALSE)
    r <- tryCatch(req_perform(req), error = function(err) NULL); n_req <- n_req + 1
    if (!is.null(r) && resp_status(r) == 200) {
      d <- resp_body_json(r, simplifyVector = TRUE)
      if (length(d) && is.matrix(d) && nrow(d)) out[[length(out) + 1]] <- d
    } else n_err <- n_err + 1
    s <- e; Sys.sleep(0.2)
  }
  if (!length(out)) return(list(df = NULL, n_req = n_req, n_err = n_err))
  m <- do.call(rbind, out)
  df <- data.frame(open_time_utc = as.POSIXct(as.numeric(m[,1]), origin = "1970-01-01", tz = "UTC"),
                   low = as.numeric(m[,2]), high = as.numeric(m[,3]), open = as.numeric(m[,4]), close = as.numeric(m[,5]), volume = as.numeric(m[,6]))
  df <- df[!duplicated(df$open_time_utc), ]; df <- df[order(df$open_time_utc), ]
  list(df = df, n_req = n_req, n_err = n_err)
}
bn_candles <- function(sym) {
  out <- list(); cursor <- as.numeric(T_INI) * 1000; end_ms <- as.numeric(T_FIM) * 1000; n_req <- 0
  repeat {
    req <- request("https://api.binance.com/api/v3/klines") |>
      req_url_query(symbol = sym, interval = "1h", startTime = format(cursor, scientific = FALSE), endTime = format(end_ms, scientific = FALSE), limit = 1000) |>
      req_user_agent(ua) |> req_retry(max_tries = 5)
    d <- resp_body_json(req_perform(req), simplifyVector = TRUE); n_req <- n_req + 1
    if (!length(d) || nrow(d) == 0) break
    out[[length(out) + 1]] <- d
    last <- as.numeric(d[nrow(d), 1]); if (nrow(d) < 1000 || last >= end_ms) break
    cursor <- last + 3600000; Sys.sleep(0.15)
  }
  m <- do.call(rbind, out)
  df <- data.frame(open_time_utc = as.POSIXct(as.numeric(m[,1]) / 1000, origin = "1970-01-01", tz = "UTC"),
                   open = as.numeric(m[,2]), high = as.numeric(m[,3]), low = as.numeric(m[,4]), close = as.numeric(m[,5]), volume = as.numeric(m[,6]))
  list(df = df[!duplicated(df$open_time_utc), ], n_req = n_req, n_err = 0)
}
## preco diario as 16:00 ET
px16 <- function(df) {
  lt <- as.POSIXlt(df$open_time_utc, tz = "America/New_York")
  df$date_ny <- as.Date(format(lt, "%Y-%m-%d")); df$hour_ny <- lt$hour
  main <- df[df$hour_ny == 15, c("date_ny","close")]
  fb <- df[df$hour_ny %in% c(13, 14), c("date_ny","hour_ny","close")]
  fb <- fb[order(fb$date_ny, -fb$hour_ny), ]; fb <- fb[!duplicated(fb$date_ny), ]
  dates <- sort(unique(df$date_ny))
  out <- data.frame(date = dates, close_16et = main$close[match(dates, main$date_ny)])
  out$source_candle <- ifelse(!is.na(out$close_16et), "15:00-16:00 ET", NA)
  miss <- is.na(out$close_16et) & dates %in% fb$date_ny
  out$close_16et[miss] <- fb$close[match(dates[miss], fb$date_ny)]
  out$source_candle[miss] <- paste0("fallback ", fb$hour_ny[match(dates[miss], fb$date_ny)], ":00-", fb$hour_ny[match(dates[miss], fb$date_ny)] + 1, ":00 ET")
  out[!is.na(out$close_16et), ]
}
resumo <- list()
fontes <- list(
  coinbase = c(BTC = "BTC-USD", ETH = "ETH-USD", ADA = "ADA-USD", XLM = "XLM-USD", XRP = "XRP-USD", LTC = "LTC-USD", SOL = "SOL-USD"),
  binance  = c(BTC = "BTCUSDT", ETH = "ETHUSDT", ADA = "ADAUSDT", XLM = "XLMUSDT", XRP = "XRPUSDT", LTC = "LTCUSDT", TRX = "TRXUSDT", SOL = "SOLUSDT"))
for (src in names(fontes)) for (a in names(fontes[[src]])) {
  sym <- fontes[[src]][[a]]; tt <- Sys.time()
  rr <- tryCatch(if (src == "coinbase") cb_candles(sym) else bn_candles(sym), error = function(e) { logf("ERRO ", src, " ", sym, ": ", conditionMessage(e)); NULL })
  if (is.null(rr) || is.null(rr$df)) { resumo[[length(resumo) + 1]] <- data.frame(source = src, asset = a, symbol = sym, n_hourly = 0, first = NA, last = NA, n_days_16et = 0, n_fallback = 0, n_req = NA, n_req_err = NA); next }
  raw_f <- file.path(dir_raw, sprintf("%s_%s_1h_raw.csv", sym, src))
  write.csv(transform(rr$df, open_time_utc = format(open_time_utc, "%Y-%m-%d %H:%M:%S")), raw_f, row.names = FALSE)
  p <- px16(rr$df)
  cf <- sprintf("data/clean/px16et_%s_%s.csv", tolower(a), src)
  if (!file.exists(cf)) write.csv(p, cf, row.names = FALSE)
  resumo[[length(resumo) + 1]] <- data.frame(source = src, asset = a, symbol = sym, n_hourly = nrow(rr$df),
    first = format(min(rr$df$open_time_utc)), last = format(max(rr$df$open_time_utc)), n_days_16et = nrow(p),
    n_fallback = sum(p$source_candle != "15:00-16:00 ET"), n_req = rr$n_req, n_req_err = rr$n_err)
  logf(sprintf("4a coleta %s %s: %d candles (%s a %s), %d dias 16h ET (%d fallback), %d req (%d erros), %.0f s", src, sym, nrow(rr$df),
               format(min(rr$df$open_time_utc)), format(max(rr$df$open_time_utc)), nrow(p), sum(p$source_candle != "15:00-16:00 ET"),
               rr$n_req, rr$n_err, as.numeric(Sys.time() - tt, units = "secs")))
}
res <- do.call(rbind, resumo)
write.csv(res, "data/raw/hourly_collection_log.csv", row.names = FALSE)
print(res)
