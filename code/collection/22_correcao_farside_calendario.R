# =====================================================================
# PROMPT 1b — Item 4: Fluxos Farside — limpeza + checagem de calendário NYSE
# =====================================================================
suppressPackageStartupMessages({ library(dplyr); library(readr); library(timeDate) })
invisible(Sys.setlocale("LC_TIME", "C"))  # weekdays() em inglês (locale padrão do sistema é pt-BR)

PROJ_DIR <- normalizePath(Sys.getenv("CVE_ROOT", "."), winslash = "/")
setwd(PROJ_DIR)
hoje <- format(Sys.Date(), "%Y-%m-%d")
dir_clean <- "data/clean"
dir.create(dir_clean, recursive = TRUE, showWarnings = FALSE)

log_lines <- character(0)
log_msg <- function(...) { msg <- paste0(...); cat(msg, "\n"); log_lines <<- c(log_lines, msg) }

calendario_pregao_nyse <- function(from, to) {
  anos <- as.numeric(format(as.Date(from), "%Y")):as.numeric(format(as.Date(to), "%Y"))
  hol <- as.Date(holidayNYSE(anos))
  todos <- seq(as.Date(from), as.Date(to), by = "day")
  wd <- weekdays(todos)
  todos[!(wd %in% c("Saturday","Sunday")) & !(todos %in% hol)]
}

processar_ativo <- function(ativo, path_bruto, data_estreia) {
  log_msg(sprintf("\n## %s — fluxo Farside", ativo))
  df <- read_csv(path_bruto, show_col_types = FALSE)
  n_antes <- nrow(df)
  # Linhas não-data já foram removidas no Prompt 1 (filtro por rótulo
  # Fee/Seed/Total/Average/Maximum/Minimum antes do parse de data). Reforço
  # aqui um filtro adicional por robustez: qualquer linha cuja `date` seja
  # NA ou fora de um range plausível seria descartada (nenhuma esperada).
  df_limpo <- df %>% filter(!is.na(.data$date), .data$date >= as.Date("2020-01-01"), .data$date <= Sys.Date())
  n_depois <- nrow(df_limpo)
  log_msg(sprintf("%s: linhas antes = %d, depois da limpeza (remoção de Total/Average/Maximum/Minimum/Fee/Seed e datas inválidas) = %d.",
                   ativo, n_antes, n_depois))

  cal <- calendario_pregao_nyse(data_estreia, max(df_limpo$date))
  datas_farside <- df_limpo$date

  dias_pregao_sem_fluxo <- setdiff(as.character(cal), as.character(datas_farside))
  linhas_fora_pregao <- setdiff(as.character(datas_farside), as.character(cal))

  log_msg(sprintf("%s: calendário de pregão NYSE de %s a %s = %d dias úteis.",
                   ativo, format(data_estreia), format(max(df_limpo$date)), length(cal)))
  log_msg(sprintf("%s: dias de pregão SEM linha de fluxo na Farside = %d.", ativo, length(dias_pregao_sem_fluxo)))
  if (length(dias_pregao_sem_fluxo) > 0) log_msg(sprintf("  Datas: %s", paste(dias_pregao_sem_fluxo, collapse = ", ")))
  log_msg(sprintf("%s: linhas da Farside em dias SEM pregão NYSE = %d.", ativo, length(linhas_fora_pregao)))
  if (length(linhas_fora_pregao) > 0) log_msg(sprintf("  Datas: %s", paste(linhas_fora_pregao, collapse = ", ")))

  write_csv(df_limpo, file.path(dir_clean, paste0("etf_flow_", tolower(ativo), "_farside_limpo.csv")))
  list(n_antes = n_antes, n_depois = n_depois, dias_pregao_sem_fluxo = dias_pregao_sem_fluxo, linhas_fora_pregao = linhas_fora_pregao)
}

res_btc <- processar_ativo("BTC", "data/clean/etf_flow_btc_farside.csv", as.Date("2024-01-11"))
res_eth <- processar_ativo("ETH", "data/clean/etf_flow_eth_farside.csv", as.Date("2024-07-23"))

writeLines(c(paste0("# Log de coleta — Correções Prompt 1b — Item 4 (Farside x calendário NYSE) — ", hoje), "", log_lines),
           file.path("data", "coleta_log_correcao_4.md"))
cat("\n== FIM ITEM 4 ==\n")
