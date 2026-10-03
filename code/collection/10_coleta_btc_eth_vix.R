# =====================================================================
# COLETA 1 — Atualização de BTC, ETH e VIX + verificação de consistência
# Projeto: Cripto x VIX x ETF — QQ Connectedness (Nelson, MEGE-UCAM)
# =====================================================================
# VIX: série VIXCLS do FRED (CSV público, sem chave, pois FRED_API_KEY
#      não está definida no ambiente).
# BTC/ETH: fonte da base atual é CoinMarketCap (planilha manual). Não há
#      arquivo atualizado em data/raw/manual/, então coletamos fechamento
#      diário (e volume) pela API pública da Binance (BTCUSDT, ETHUSDT,
#      1d, sem chave) e comparamos log-retornos com a base existente no
#      período de sobreposição.
# =====================================================================

suppressPackageStartupMessages({
  library(httr2); library(jsonlite); library(dplyr); library(lubridate)
  library(readr); library(openxlsx)
})

PROJ_DIR <- normalizePath(Sys.getenv("CVE_ROOT", "."), winslash = "/")
setwd(PROJ_DIR)
hoje <- format(Sys.Date(), "%Y-%m-%d")
dir_raw   <- file.path("data/raw", hoje)
dir_clean <- "data/clean"
dir.create(dir_raw,   recursive = TRUE, showWarnings = FALSE)
dir.create(dir_clean, recursive = TRUE, showWarnings = FALSE)
dir.create("R", showWarnings = FALSE)

log_lines <- character(0)
log_msg <- function(...) {
  msg <- paste0(...)
  cat(msg, "\n")
  log_lines <<- c(log_lines, msg)
}

## ---- 1. VIX (FRED, CSV público sem chave) ----------------------------
log_msg("## Item 1a — VIX (FRED)")
fred_key <- Sys.getenv("FRED_API_KEY", unset = "")
if (nzchar(fred_key)) {
  log_msg("FRED_API_KEY encontrada — usando fredr seria preferível, mas o CSV público cobre a necessidade; mantendo CSV para simplicidade e reprodutibilidade.")
} else {
  log_msg("FRED_API_KEY não definida no ambiente — usando CSV público do FRED (sem chave), conforme regra 3.")
}

vix_raw <- tryCatch({
  req <- request("https://fred.stlouisfed.org/graph/fredgraph.csv?id=VIXCLS") |>
    req_user_agent("Mozilla/5.0 (academic research; MEGE-UCAM; contato rtfnicolay@gmail.com)")
  resp <- req_perform(req)
  resp_body_string(resp)
}, error = function(e) { log_msg("VIX: ERRO ao baixar — ", conditionMessage(e)); NULL })

vix_status <- "falhou"
if (!is.null(vix_raw)) {
  writeLines(vix_raw, file.path(dir_raw, "VIXCLS_fred_raw.csv"))
  vix_df <- read_csv(I(vix_raw), show_col_types = FALSE)
  names(vix_df) <- c("date", "VIXCLS")
  vix_df$date <- as.Date(vix_df$date)
  vix_df$VIXCLS <- suppressWarnings(as.numeric(vix_df$VIXCLS))  # "." vira NA
  n_na_vix <- sum(is.na(vix_df$VIXCLS))
  vix_clean <- vix_df %>% filter(!is.na(VIXCLS))
  write_csv(vix_clean, file.path(dir_clean, "vix_fred.csv"))
  log_msg(sprintf("VIX: %d observações brutas (%s a %s), %d valores ausentes (\".\") removidos na versão limpa -> %d obs.",
                   nrow(vix_df), format(min(vix_df$date)), format(max(vix_df$date)), n_na_vix, nrow(vix_clean)))
  vix_status <- "completo"
}

## ---- 2. BTC e ETH — checar arquivo manual atualizado ------------------
log_msg("\n## Item 1b — BTC/ETH (checagem de arquivo manual CoinMarketCap)")
manual_files <- list.files("data/raw/manual", pattern = "\\.(csv|xlsx)$", full.names = TRUE)
if (length(manual_files) > 0) {
  log_msg("Arquivo(s) manual(is) encontrado(s) em data/raw/manual/: ", paste(basename(manual_files), collapse = ", "),
          " — mas o script ainda usa a Binance para a verificação de consistência abaixo; revisar manualmente se preferir usá-los como fonte primária.")
} else {
  log_msg("Nenhum arquivo atualizado em data/raw/manual/. Coletando fechamento diário e volume via API pública da Binance (BTCUSDT, ETHUSDT, 1d, sem chave).")
}

## ---- 3. Binance klines (paginado, sem chave) --------------------------
get_binance_klines <- function(symbol, start_date, end_date = Sys.Date()) {
  base_url <- "https://api.binance.com/api/v3/klines"
  start_ms <- as.numeric(as.POSIXct(start_date, tz = "UTC")) * 1000
  end_ms   <- as.numeric(as.POSIXct(end_date,   tz = "UTC")) * 1000
  all_rows <- list()
  cursor <- start_ms
  repeat {
    req <- request(base_url) |>
      req_url_query(symbol = symbol, interval = "1d", startTime = format(cursor, scientific = FALSE),
                    endTime = format(end_ms, scientific = FALSE), limit = 1000) |>
      req_user_agent("Mozilla/5.0 (academic research; MEGE-UCAM)")
    resp <- req_perform(req)
    d <- resp_body_json(resp, simplifyVector = TRUE)
    if (is.null(d) || (is.matrix(d) && nrow(d) == 0) || length(d) == 0) break
    all_rows[[length(all_rows) + 1]] <- d
    last_open <- as.numeric(d[nrow(d), 1])
    if (nrow(d) < 1000 || last_open >= end_ms) break
    cursor <- last_open + 86400000  # +1 dia em ms
    Sys.sleep(0.3)  # respeito a rate limit
  }
  if (length(all_rows) == 0) return(NULL)
  m <- do.call(rbind, all_rows)
  df <- data.frame(
    open_time = as.Date(as.POSIXct(as.numeric(m[,1])/1000, origin = "1970-01-01", tz = "UTC")),
    open   = as.numeric(m[,2]), high = as.numeric(m[,3]), low = as.numeric(m[,4]),
    close  = as.numeric(m[,5]), volume = as.numeric(m[,6]),
    stringsAsFactors = FALSE
  )
  df[!duplicated(df$open_time), ]
}

log_msg("\n## Coletando Binance BTCUSDT e ETHUSDT (1d) desde 2017-08-17, início do par na Binance) ----")
btc_bin <- tryCatch(get_binance_klines("BTCUSDT", "2017-08-17"),
                     error = function(e) { log_msg("BTCUSDT Binance ERRO: ", conditionMessage(e)); NULL })
eth_bin <- tryCatch(get_binance_klines("ETHUSDT", "2017-08-17"),
                     error = function(e) { log_msg("ETHUSDT Binance ERRO: ", conditionMessage(e)); NULL })

if (!is.null(btc_bin)) {
  write_csv(btc_bin, file.path(dir_raw, "BTCUSDT_binance_1d_raw.csv"))
  log_msg(sprintf("BTCUSDT: %d obs. (%s a %s)", nrow(btc_bin), format(min(btc_bin$open_time)), format(max(btc_bin$open_time))))
}
if (!is.null(eth_bin)) {
  write_csv(eth_bin, file.path(dir_raw, "ETHUSDT_binance_1d_raw.csv"))
  log_msg(sprintf("ETHUSDT: %d obs. (%s a %s)", nrow(eth_bin), format(min(eth_bin$open_time)), format(max(eth_bin$open_time))))
}

## ---- 4. Versões limpas ------------------------------------------------
if (!is.null(btc_bin)) {
  btc_clean <- btc_bin %>% dplyr::transmute(date = open_time, close = close, volume = volume) %>% dplyr::arrange(.data$date)
  write_csv(btc_clean, file.path(dir_clean, "btc_binance.csv"))
}
if (!is.null(eth_bin)) {
  eth_clean <- eth_bin %>% dplyr::transmute(date = open_time, close = close, volume = volume) %>% dplyr::arrange(.data$date)
  write_csv(eth_clean, file.path(dir_clean, "eth_binance.csv"))
}

## ---- 5. Verificação de consistência obrigatória (regra do Item 1) -----
log_msg("\n## Verificação de consistência — log-retornos Binance vs. base atual (planilha CoinMarketCap)")

checar_consistencia <- function(ativo, path_planilha, df_binance) {
  if (is.null(df_binance)) { log_msg(ativo, ": Binance indisponível, verificação não realizada."); return(invisible(NULL)) }
  base <- openxlsx::read.xlsx(path_planilha, detectDates = TRUE)
  base$Date <- as.Date(base$Date)
  # base já vem em RETORNO PERCENTUAL diário (%), não em preço.
  col_ret <- setdiff(names(base), c("Date","VIX"))[1]
  base_ret <- base %>% transmute(date = Date, ret_base = .data[[col_ret]] / 100)  # % -> fração

  bin_ret <- df_binance %>% dplyr::arrange(.data$open_time) %>%
    dplyr::mutate(ret_bin = log(.data$close / dplyr::lag(.data$close))) %>%
    dplyr::filter(!is.na(.data$ret_bin)) %>%
    dplyr::transmute(date = .data$open_time, ret_bin = .data$ret_bin)

  # A data do candle da Binance (open_time, UTC 00:00) fica 1 dia atrás da
  # data com que a planilha CoinMarketCap rotula o mesmo retorno (rótulo
  # provavelmente pelo fechamento do dia / fuso diferente). Testamos os
  # deslocamentos de -2 a +2 dias e usamos o que maximiza a correlação,
  # documentando o deslocamento escolhido.
  candidatos <- -2:2
  corrs <- sapply(candidatos, function(s) {
    br <- bin_ret %>% dplyr::mutate(date = .data$date + s)
    cc <- dplyr::inner_join(base_ret, br, by = "date")
    if (nrow(cc) < 30) return(NA_real_)
    suppressWarnings(cor(cc$ret_base, cc$ret_bin, use = "complete.obs"))
  })
  melhor_s <- candidatos[which.max(corrs)]
  log_msg(sprintf("%s: deslocamento de data que maximiza a correlação = %+d dia(s) (correlações testadas: %s).",
                   ativo, melhor_s, paste(sprintf("%+d:%.4f", candidatos, corrs), collapse=", ")))
  bin_ret <- bin_ret %>% dplyr::mutate(date = .data$date + melhor_s)

  comp <- inner_join(base_ret, bin_ret, by = "date")
  if (nrow(comp) < 30) {
    log_msg(sprintf("%s: sobreposição insuficiente para comparação (%d obs.).", ativo, nrow(comp)))
    return(invisible(NULL))
  }
  correlacao <- cor(comp$ret_base, comp$ret_bin, use = "complete.obs")
  dif_abs <- abs(comp$ret_base - comp$ret_bin)
  dif_media <- mean(dif_abs, na.rm = TRUE)
  dif_max <- max(dif_abs, na.rm = TRUE)
  dif_max_date <- comp$date[which.max(dif_abs)]

  log_msg(sprintf(
    "%s: sobreposição %s a %s (%d obs.). Correlação = %.6f | Diferença absoluta média = %.6f | Maior diferença = %.6f (em %s).",
    ativo, format(min(comp$date)), format(max(comp$date)), nrow(comp),
    correlacao, dif_media, dif_max, format(dif_max_date)))
  if (correlacao < 0.99) {
    log_msg(sprintf("ALERTA: correlação de %s (%.6f) < 0,99 — NÃO substituir a base pela fonte Binance. Investigar diferenças de metodologia de preço (CoinMarketCap agrega múltiplas exchanges; Binance é uma única exchange).", ativo, correlacao))
  } else {
    log_msg(sprintf("%s: correlação >= 0,99 — consistente com a base atual.", ativo))
  }
  write_csv(comp, file.path(dir_clean, paste0("verificacao_consistencia_", tolower(ativo), ".csv")))
  invisible(list(ativo = ativo, n = nrow(comp), correlacao = correlacao, dif_media = dif_media, dif_max = dif_max))
}

res_btc <- checar_consistencia("BTC", "data/ret_BTC_VIX_emLinha_cmc.xlsx", btc_bin)
res_eth <- checar_consistencia("ETH", "data/ret_ETH_VIX_emLinha_cmc.xlsx", eth_bin)

## ---- 6. Gravar log ------------------------------------------------------
writeLines(c(paste0("# Log de coleta — Item 1 (BTC, ETH, VIX) — ", hoje), "", log_lines),
           file.path(dirname(dir_clean), "coleta_log_item1.md"))

cat("\n== FIM COLETA 1 ==\n")
