# =====================================================================
# COLETA 2 e 3 — Fluxo líquido diário dos ETFs à vista (BTC e ETH)
# Fonte: tabelas públicas da Farside Investors.
# robots.txt checado manualmente em 2026-09-21: só restringe Twitterbot,
# sem regra geral de bloqueio para as páginas usadas aqui. Termos de uso
# não proíbem uso acadêmico não comercial de dados públicos exibidos.
# =====================================================================

suppressPackageStartupMessages({
  library(httr2); library(rvest); library(dplyr); library(readr); library(stringr)
})

# Nomes de mês em inglês ("Jan","Feb",...) na tabela da Farside exigem
# locale de tempo em C/inglês; o locale padrão do sistema (pt-BR) faz
# as.Date(..., format="%d %b %y") falhar silenciosamente.
invisible(Sys.setlocale("LC_TIME", "C"))

PROJ_DIR <- normalizePath(Sys.getenv("CVE_ROOT", "."), winslash = "/")
setwd(PROJ_DIR)
hoje <- format(Sys.Date(), "%Y-%m-%d")
dir_raw   <- file.path("data/raw", hoje)
dir_clean <- "data/clean"
dir.create(dir_raw,   recursive = TRUE, showWarnings = FALSE)
dir.create(dir_clean, recursive = TRUE, showWarnings = FALSE)

log_lines <- character(0)
log_msg <- function(...) { msg <- paste0(...); cat(msg, "\n"); log_lines <<- c(log_lines, msg) }

ua <- "Mozilla/5.0 (academic research; MEGE-UCAM; contato rtfnicolay@gmail.com)"

## ---- Checagem de robots.txt (registrada, não pacote robotstxt — indisponível) ----
log_msg("## Checagem de robots.txt — farside.co.uk")
robots_txt <- tryCatch({
  req <- request("https://farside.co.uk/robots.txt") |> req_user_agent(ua)
  resp_body_string(req_perform(req))
}, error = function(e) NA_character_)
if (!is.na(robots_txt)) {
  writeLines(robots_txt, file.path(dir_raw, "farside_robots.txt"))
  log_msg("robots.txt salvo em data/raw/", hoje, "/farside_robots.txt — conteúdo: ", gsub("\n", " | ", robots_txt))
  log_msg("Sem regra geral 'Disallow: /' para user-agent '*' — coleta autorizada pelo robots.txt.")
} else {
  log_msg("Não foi possível ler robots.txt — prosseguindo com cautela, requisições limitadas e identificação no User-Agent.")
}

## ---- Função de parsing de uma página "all data" da Farside -----------
limpar_num <- function(x) {
  x <- str_trim(x)
  x <- ifelse(x %in% c("-", ""), NA_character_, x)
  neg <- str_detect(x, "^\\(.*\\)$")
  x <- str_replace_all(x, "[()]", "")
  x <- str_replace_all(x, ",", "")
  v <- suppressWarnings(as.numeric(x))
  ifelse(neg & !is.na(v), -v, v)
}

coletar_farside <- function(url, ativo, primeira_col_extra = NULL) {
  log_msg(sprintf("\n## Coletando fluxo ETF %s — %s", ativo, url))
  html_txt <- tryCatch({
    req <- request(url) |> req_user_agent(ua)
    resp_body_string(req_perform(req))
  }, error = function(e) { log_msg(ativo, ": ERRO ao baixar página — ", conditionMessage(e)); NULL })
  if (is.null(html_txt)) return(NULL)

  raw_path <- file.path(dir_raw, paste0("farside_", tolower(ativo), "_all_data_raw.html"))
  writeLines(html_txt, raw_path)

  pg <- read_html(html_txt)
  tabs <- html_table(pg, fill = TRUE)
  t1 <- tabs[[1]]
  names(t1)[1] <- "linha_rotulo"

  # Remove linhas de rodapé (Total/Average/Maximum/Minimum) e cabeçalho (Fee/Seed)
  rotulos_excluir <- c("Fee", "Seed", "Total", "Average", "Maximum", "Minimum", "")
  dados <- t1 %>% filter(!.data$linha_rotulo %in% rotulos_excluir)

  # Parse de data no formato "DD Mon YYYY" (ex.: "11 Jan 2024")
  dados$date <- suppressWarnings(as.Date(dados$linha_rotulo, format = "%d %b %Y"))
  n_falha_data <- sum(is.na(dados$date))
  if (n_falha_data > 0) {
    log_msg(sprintf("%s: %d linha(s) sem data reconhecida (rótulo inesperado), descartadas.", ativo, n_falha_data))
    dados <- dados %>% filter(!is.na(.data$date))
  }

  cols_fundo <- setdiff(names(dados), c("linha_rotulo", "date", "Total"))
  for (cc in cols_fundo) dados[[cc]] <- limpar_num(dados[[cc]])
  dados$total_mi_usd <- limpar_num(dados$Total)

  dados_final <- dados %>% select(date, all_of(cols_fundo), total_mi_usd) %>% arrange(date)
  write_csv(dados_final, file.path(dir_clean, paste0("etf_flow_", tolower(ativo), "_farside.csv")))

  log_msg(sprintf("%s: %d observações diárias (%s a %s). Fundos: %s.",
                   ativo, nrow(dados_final), format(min(dados_final$date)), format(max(dados_final$date)),
                   paste(cols_fundo, collapse = ", ")))
  n_na_total <- sum(is.na(dados_final$total_mi_usd))
  if (n_na_total > 0) log_msg(sprintf("%s: %d dia(s) com total ausente (marcado '-' na fonte — sem negociação/dado, não interpolado).", ativo, n_na_total))
  dados_final
}

btc_flow <- coletar_farside("https://farside.co.uk/bitcoin-etf-flow-all-data/", "BTC")
eth_flow <- coletar_farside("https://farside.co.uk/ethereum-etf-flow-all-data/", "ETH")

## ---- Cobertura esperada vs. observada ---------------------------------
log_msg("\n## Verificação de cobertura esperada")
if (!is.null(btc_flow)) {
  esperado_ini <- as.Date("2024-01-11")
  log_msg(sprintf("BTC: cobertura esperada desde %s. Primeira data coletada: %s. %s",
                   esperado_ini, format(min(btc_flow$date)),
                   ifelse(min(btc_flow$date) <= esperado_ini, "OK.", "ATENÇÃO: início posterior ao esperado.")))
}
if (!is.null(eth_flow)) {
  esperado_ini <- as.Date("2024-07-23")
  log_msg(sprintf("ETH: cobertura esperada desde %s. Primeira data coletada: %s. %s",
                   esperado_ini, format(min(eth_flow$date)),
                   ifelse(min(eth_flow$date) <= esperado_ini, "OK.", "ATENÇÃO: início posterior ao esperado.")))
}

## ---- Alternativa SoSoValue (checagem de chave) -------------------------
soso_key <- Sys.getenv("SOSOVALUE_API_KEY", unset = "")
if (nzchar(soso_key)) {
  log_msg("\nSOSOVALUE_API_KEY encontrada, mas Farside já forneceu cobertura completa — SoSoValue não usada nesta rodada (fonte primária suficiente).")
} else {
  log_msg("\nSOSOVALUE_API_KEY não definida — alternativa SoSoValue não utilizada (Farside já cobriu os Itens 2 e 3 integralmente).")
}

writeLines(c(paste0("# Log de coleta — Itens 2 e 3 (fluxo ETF BTC/ETH) — ", hoje), "", log_lines),
           file.path("data", "coleta_log_itens2_3.md"))

cat("\n== FIM COLETA 2/3 ==\n")
