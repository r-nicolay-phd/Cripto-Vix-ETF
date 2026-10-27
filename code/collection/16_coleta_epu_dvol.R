# =====================================================================
# COLETA 9 e 10 — EPU diário dos EUA (policyuncertainty.com) e
# DVOL (Deribit, BTC e ETH, resolução diária)
# =====================================================================

suppressPackageStartupMessages({
  library(httr2); library(jsonlite); library(dplyr); library(readr)
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
ua <- "Mozilla/5.0 (academic research)"

## ---- Item 9: EPU diário ------------------------------------------------
log_msg("## Item 9 — EPU diário dos EUA (policyuncertainty.com)")
epu_raw <- tryCatch({
  req <- request("https://www.policyuncertainty.com/media/All_Daily_Policy_Data.csv") |> req_user_agent(ua)
  resp_body_string(req_perform(req))
}, error = function(e) { log_msg("EPU: ERRO — ", conditionMessage(e)); NULL })

if (!is.null(epu_raw)) {
  writeLines(epu_raw, file.path(dir_raw, "EPU_daily_raw.csv"))
  epu_df <- read_csv(I(epu_raw), show_col_types = FALSE)
  log_msg(sprintf("EPU: coluna(s) do arquivo bruto: %s", paste(names(epu_df), collapse=", ")))
  col_idx <- names(epu_df)[4]  # 4a coluna = daily_policy_index, conforme documentação da fonte
  epu_clean <- epu_df %>%
    transmute(date = as.Date(sprintf("%04d-%02d-%02d", year, month, day)),
              daily_policy_index = .data[[col_idx]]) %>%
    filter(!is.na(.data$daily_policy_index))
  write_csv(epu_clean, file.path(dir_clean, "epu_daily.csv"))
  log_msg(sprintf("EPU: %d obs. (%s a %s). Nome da coluna do índice no arquivo bruto: '%s' -> renomeada para 'daily_policy_index' no arquivo limpo.",
                   nrow(epu_clean), format(min(epu_clean$date)), format(max(epu_clean$date)), col_idx))
}

## ---- Item 10: DVOL (Deribit), BTC e ETH ---------------------------------
log_msg("\n## Item 10 — DVOL (Deribit), BTC e ETH, resolução diária, paginado")

get_dvol_full <- function(currency, from, to) {
  # A API pagina via campo `continuation` da resposta (timestamp ms do
  # próximo cursor), não apenas pelo último timestamp retornado — testado
  # nesta sessão: sem seguir `continuation`, a paginação por último-ts
  # simplesmente pararia cedo demais ou repetiria a mesma janela.
  ep <- "https://www.deribit.com/api/v2/public/get_volatility_index_data"
  from_ms <- as.numeric(as.POSIXct(from, tz = "UTC")) * 1000
  to_ms   <- as.numeric(as.POSIXct(to,   tz = "UTC")) * 1000
  all_data <- list()
  cursor <- from_ms
  iter <- 0
  repeat {
    iter <- iter + 1
    req <- request(ep) |>
      req_url_query(currency = currency, start_timestamp = format(cursor, scientific = FALSE),
                    end_timestamp = format(to_ms, scientific = FALSE), resolution = "86400") |>
      req_user_agent(ua)
    resp <- tryCatch(req_perform(req), error = function(e) e)
    if (inherits(resp, "error")) { log_msg(sprintf("%s DVOL: ERRO — %s", currency, conditionMessage(resp))); break }
    res <- resp_body_json(resp, simplifyVector = FALSE)$result
    d <- res$data
    if (is.null(d) || length(d) == 0) break
    m <- do.call(rbind, lapply(d, function(row) as.numeric(unlist(row))))
    all_data[[length(all_data) + 1]] <- m
    cont <- res$continuation
    if (is.null(cont) || iter > 30) break
    new_cursor <- suppressWarnings(as.numeric(cont))
    if (is.na(new_cursor) || new_cursor <= cursor) break  # sem avanço = fim do histórico disponível
    cursor <- new_cursor
    Sys.sleep(0.2)
  }
  if (length(all_data) == 0) return(NULL)
  m <- do.call(rbind, all_data)
  df <- data.frame(
    date = as.Date(as.POSIXct(as.numeric(m[,1])/1000, origin = "1970-01-01", tz = "UTC")),
    open = as.numeric(m[,2]), high = as.numeric(m[,3]), low = as.numeric(m[,4]), close = as.numeric(m[,5]),
    stringsAsFactors = FALSE
  )
  df[!duplicated(df$date), ]
}

for (cur in c("BTC","ETH")) {
  df <- tryCatch(get_dvol_full(cur, "2019-01-01", Sys.Date()), error = function(e) { log_msg(sprintf("%s DVOL: ERRO — %s", cur, conditionMessage(e))); NULL })
  if (!is.null(df)) {
    write_csv(df, file.path(dir_raw, paste0("DVOL_", cur, "_deribit_raw.csv")))
    write_csv(df, file.path(dir_clean, paste0("dvol_", tolower(cur), "_deribit.csv")))
    log_msg(sprintf("DVOL %s: %d obs. (%s a %s). Testado explicitamente nesta sessão: não há dados na API antes de %s (janelas anteriores, ex. 2021, retornam vazio) — não é limitação de paginação, é o início real do histórico público exposto por este endpoint.",
                     cur, nrow(df), format(min(df$date)), format(max(df$date)), format(min(df$date))))
  }
}

writeLines(c(paste0("# Log de coleta — Itens 9 e 10 (EPU, DVOL) — ", hoje), "", log_lines),
           file.path("data", "coleta_log_item9_10.md"))
cat("\n== FIM COLETA 9/10 ==\n")
