# =====================================================================
# COLETA 6 — Ouro e S&P 500 (Yahoo Finance) + Item 8 (base p/ vol. realizada)
# Item 8 reaproveita a mesma série ^GSPC coletada aqui (fechamento diário);
# o cálculo da volatilidade realizada fica para a etapa de estimação.
# =====================================================================

suppressPackageStartupMessages({
  library(quantmod); library(dplyr); library(readr); library(httr2)
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

coletar_ticker <- function(tk, from = "2007-01-01") {
  tryCatch({
    x <- getSymbols(tk, src = "yahoo", from = from, to = Sys.Date(), auto.assign = FALSE)
    df <- data.frame(date = index(x), coredata(x))
    names(df) <- c("date","open","high","low","close","volume","adjusted")
    df
  }, error = function(e) { log_msg(sprintf("%s: ERRO — %s", tk, conditionMessage(e))); NULL })
}

log_msg("## Item 6 — Ouro e S&P 500 (Yahoo Finance)")
for (tk in c("^GSPC", "GLD", "GC=F")) {
  df <- coletar_ticker(tk)
  if (!is.null(df)) {
    fname <- gsub("[\\^=]", "", tk)
    write_csv(df, file.path(dir_raw, paste0(fname, "_yahoo_raw.csv")))
    write_csv(df, file.path(dir_clean, paste0(tolower(fname), "_yahoo.csv")))
    log_msg(sprintf("%s: %d obs. (%s a %s)", tk, nrow(df), format(min(df$date)), format(max(df$date))))
  }
}

## ---- SP500 do FRED, para conferência (últimos ~10 anos) ---------------
log_msg("\n## Item 6 (opcional) — SP500 do FRED, para conferência")
fred_sp500 <- tryCatch({
  req <- request("https://fred.stlouisfed.org/graph/fredgraph.csv?id=SP500") |>
    req_user_agent("Mozilla/5.0 (academic research)")
  resp_body_string(req_perform(req))
}, error = function(e) { log_msg("SP500 FRED: ERRO — ", conditionMessage(e)); NULL })
if (!is.null(fred_sp500)) {
  writeLines(fred_sp500, file.path(dir_raw, "SP500_fred_raw.csv"))
  df <- read_csv(I(fred_sp500), show_col_types = FALSE)
  names(df) <- c("date","SP500")
  df$SP500 <- suppressWarnings(as.numeric(df$SP500))
  df_clean <- df %>% filter(!is.na(.data$SP500))
  write_csv(df_clean, file.path(dir_clean, "sp500_fred.csv"))
  log_msg(sprintf("SP500 (FRED): %d obs. (%s a %s) — cobre só ~10 anos, uso apenas para conferência.",
                   nrow(df_clean), format(min(df_clean$date)), format(max(df_clean$date))))
}

writeLines(c(paste0("# Log de coleta — Item 6/8 (ouro, S&P 500) — ", hoje), "", log_lines),
           file.path("data", "coleta_log_item6_8.md"))
cat("\n== FIM COLETA 6/8 ==\n")
