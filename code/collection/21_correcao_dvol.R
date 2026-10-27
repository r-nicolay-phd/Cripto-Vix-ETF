# =====================================================================
# PROMPT 1b — Item 3: recoleta do DVOL com paginação corrigida
# =====================================================================
# Diagnóstico: a paginação anterior (seguindo o campo `continuation` da
# resposta) parava cedo porque, quando a janela pedida (start_timestamp a
# end_timestamp) é muito ampla, a API do Deribit retorna os ÚLTIMOS ~1000
# pontos da janela, não os primeiros — então `continuation` não avança e o
# loop conclui (erroneamente) que não há dados anteriores. A correção é
# paginar por JANELAS DE TEMPO FIXAS (90 dias), não por continuation.
# Testado nesta sessão: com janelas de 90 dias a partir de 2021-01-01, a
# primeira observação aparece em 2021-03-24 — bate com o esperado no prompt.
# =====================================================================

suppressPackageStartupMessages({ library(httr2); library(jsonlite); library(dplyr); library(readr) })

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
ep <- "https://www.deribit.com/api/v2/public/get_volatility_index_data"

get_dvol_janela <- function(currency, from, to) {
  req <- request(ep) |>
    req_url_query(currency = currency,
                  start_timestamp = format(as.numeric(as.POSIXct(from, tz = "UTC")) * 1000, scientific = FALSE),
                  end_timestamp   = format(as.numeric(as.POSIXct(to,   tz = "UTC")) * 1000, scientific = FALSE),
                  resolution = "86400") |>
    req_user_agent(ua)
  resp <- tryCatch(req_perform(req), error = function(e) e)
  if (inherits(resp, "error")) return(NULL)
  resp_body_json(resp, simplifyVector = TRUE)$result$data
}

get_dvol_completo <- function(currency, from = "2021-01-01", to = Sys.Date()) {
  janelas <- seq(as.Date(from), as.Date(to), by = "90 days")
  if (tail(janelas, 1) < as.Date(to)) janelas <- c(janelas, as.Date(to))
  all_rows <- list()
  for (i in seq_len(length(janelas) - 1)) {
    d <- get_dvol_janela(currency, janelas[i], janelas[i + 1])
    if (!is.null(d) && nrow(d) > 0) all_rows[[length(all_rows) + 1]] <- d
    Sys.sleep(0.15)
  }
  if (length(all_rows) == 0) return(NULL)
  m <- do.call(rbind, all_rows)
  df <- data.frame(
    date = as.Date(as.POSIXct(as.numeric(m[,1])/1000, origin = "1970-01-01", tz = "UTC")),
    open = as.numeric(m[,2]), high = as.numeric(m[,3]), low = as.numeric(m[,4]), close = as.numeric(m[,5]),
    stringsAsFactors = FALSE
  )
  df <- df[order(df$date), ]
  df[!duplicated(df$date), ]
}

log_msg("## Item 3 — DVOL (Deribit), recoleta com paginação por janelas fixas de 90 dias, desde 2021-01-01")
for (cur in c("BTC","ETH")) {
  df <- get_dvol_completo(cur, "2021-01-01", Sys.Date())
  if (!is.null(df)) {
    write_csv(df, file.path(dir_raw, paste0("DVOL_", cur, "_deribit_completo_raw.csv")))
    write_csv(df, file.path(dir_clean, paste0("dvol_", tolower(cur), "_deribit.csv")))  # substitui a versão truncada do Prompt 1
    log_msg(sprintf("DVOL %s: %d obs. (%s a %s). Primeira data disponível na API: %s.",
                     cur, nrow(df), format(min(df$date)), format(max(df$date)), format(min(df$date))))
  } else {
    log_msg(sprintf("DVOL %s: ERRO — nenhum dado retornado.", cur))
  }
}

writeLines(c(paste0("# Log de coleta — Correções Prompt 1b — Item 3 (DVOL) — ", hoje), "", log_lines),
           file.path("data", "coleta_log_correcao_3.md"))
cat("\n== FIM ITEM 3 ==\n")
