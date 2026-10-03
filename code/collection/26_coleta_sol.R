# =====================================================================
# V6 — Coleta SOLUSDT (Binance, klines 1d, API publica sem chave).
# Mesma funcao/formato do R/13_coleta_cesta_controle.R.
# Bruto: data/raw/2026-09-30/SOLUSDT_binance_1d_raw.csv
# Limpo: data/clean/cesta_sol_binance.csv (nao sobrescreve se existir)
# =====================================================================
setwd(normalizePath(Sys.getenv("CVE_ROOT", "."), winslash = "/"))
suppressPackageStartupMessages(library(httr2))
dir_raw <- "data/raw/2026-09-30"; dir.create(dir_raw, recursive = TRUE, showWarnings = FALSE)
ua <- "Mozilla/5.0 (research; V6 pipeline)"
get_binance_klines <- function(symbol, start_date, end_date, interval = "1d", step_ms = 86400000) {
  base_url <- "https://api.binance.com/api/v3/klines"
  start_ms <- as.numeric(as.POSIXct(start_date, tz = "UTC")) * 1000
  end_ms   <- as.numeric(as.POSIXct(end_date,   tz = "UTC")) * 1000
  all_rows <- list(); cursor <- start_ms
  repeat {
    req <- request(base_url) |>
      req_url_query(symbol = symbol, interval = interval, startTime = format(cursor, scientific = FALSE),
                    endTime = format(end_ms, scientific = FALSE), limit = 1000) |> req_user_agent(ua)
    d <- resp_body_json(req_perform(req), simplifyVector = TRUE)
    if (is.null(d) || length(d) == 0 || (is.matrix(d) && nrow(d) == 0)) break
    all_rows[[length(all_rows) + 1]] <- d
    last_open <- as.numeric(d[nrow(d), 1])
    if (nrow(d) < 1000 || last_open >= end_ms) break
    cursor <- last_open + step_ms; Sys.sleep(0.25)
  }
  m <- do.call(rbind, all_rows)
  data.frame(open_time_ms = as.numeric(m[,1]),
             date = as.Date(as.POSIXct(as.numeric(m[,1])/1000, origin = "1970-01-01", tz = "UTC")),
             open = as.numeric(m[,2]), high = as.numeric(m[,3]), low = as.numeric(m[,4]),
             close = as.numeric(m[,5]), volume = as.numeric(m[,6]), stringsAsFactors = FALSE)
}
df <- get_binance_klines("SOLUSDT", "2020-01-01", "2026-09-22")
df <- df[!duplicated(df$date), ]
write.csv(df, file.path(dir_raw, "SOLUSDT_binance_1d_raw.csv"), row.names = FALSE)
out <- "data/clean/cesta_sol_binance.csv"
if (!file.exists(out)) write.csv(df[, c("date","open","high","low","close","volume")], out, row.names = FALSE) else cat("JA EXISTE, nao sobrescrito:", out, "\n")
msg <- sprintf("[%s] SOLUSDT Binance 1d: %d obs. (%s a %s); bruto %s; limpo %s", format(Sys.time()), nrow(df),
               format(min(df$date)), format(max(df$date)), file.path(dir_raw, "SOLUSDT_binance_1d_raw.csv"), out)
cat(msg, "\n"); cat(msg, "\n", file = "data/raw/coleta_log.txt", append = TRUE)
## (coleta_log.md NAO e alterado: registro em data/raw/coleta_log.txt)
