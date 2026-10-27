# =====================================================================
# COLETA 5 — Cesta de controle (criptoativos sem ETF à vista nos EUA)
# =====================================================================
# Critérios de elegibilidade (aplicados nesta ordem):
#  (a) par USDT na Binance com histórico desde ANTES de 2019-01-01
#  (b) entre as 50 maiores capitalizações de mercado ATUAIS (CoinGecko,
#      consultado em 2026-09-21)
#  (c) excluir stablecoins, tokens embrulhados e tokens de exchange
#
# Verificação de (a) e ranking de (b): ver scratchpad de checagem nesta
# sessão — resultado documentado abaixo em `checagem_candidatos`.
# Candidatos que atendem (a)+(b)+(c) simultaneamente: XRP, TRX, ADA, XLM, LTC.
# (BCH tem par USDT só desde 2019-11-28; LINK desde 2019-01-16 — ambos
# depois do corte de 2019-01-01, portanto EXCLUÍDOS por (a), apesar de
# estarem no top-50 atual.)
# =====================================================================

suppressPackageStartupMessages({
  library(httr2); library(jsonlite); library(dplyr); library(readr)
})
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
ua <- "Mozilla/5.0 (academic research)"

## ---- 5.1 Checagem de candidatos: par USDT desde antes de 2019-01-01 ---
primeira_data_binance <- function(symbol) {
  req <- request("https://api.binance.com/api/v3/klines") |>
    req_url_query(symbol = symbol, interval = "1d", startTime = "0", limit = 1) |>
    req_user_agent(ua)
  resp <- tryCatch(req_perform(req), error = function(e) e)
  if (inherits(resp, "error")) return(NA)
  d <- resp_body_json(resp, simplifyVector = TRUE)
  if (is.null(d) || length(d) == 0) return(NA)
  as.Date(as.POSIXct(as.numeric(d[1,1])/1000, origin = "1970-01-01", tz = "UTC"))
}

log_msg("## Item 5.1 — Ranking de capitalização atual (CoinGecko, sem chave, top 250)")
req_mkt <- request("https://api.coingecko.com/api/v3/coins/markets") |>
  req_url_query(vs_currency = "usd", order = "market_cap_desc", per_page = 250, page = 1) |>
  req_user_agent(ua)
mkt <- tryCatch({
  resp <- req_perform(req_mkt)
  resp_body_json(resp, simplifyVector = TRUE)
}, error = function(e) { log_msg("CoinGecko markets: ERRO — ", conditionMessage(e)); NULL })

if (!is.null(mkt)) {
  write_csv(as_tibble(mkt[, c("id","symbol","name","market_cap_rank","market_cap")]),
            file.path(dir_raw, "coingecko_markets_top250_raw.csv"))
  top50 <- mkt[!is.na(mkt$market_cap_rank) & mkt$market_cap_rank <= 50, ]
  log_msg(sprintf("Top-50 atual obtido: %d ativos.", nrow(top50)))
} else {
  top50 <- NULL
}

stable_exchange_wrapped <- toupper(c(
  "usdt","usdc","usds","dai","usde","usd1","xaut","usdg","pyusd","usyc","rlusd",
  "buidl","usdy","paxg", # stablecoins/tokens lastreados em ouro/dólar
  "bnb","leo","okb","cro","htx","wbt" # tokens de exchange
))

candidatos_top50 <- if (!is.null(top50)) {
  setdiff(toupper(top50$symbol), stable_exchange_wrapped)
} else character(0)
log_msg(sprintf("Candidatos do top-50 após excluir stablecoins/tokens de exchange/wrapped: %s",
                 paste(candidatos_top50, collapse=", ")))

log_msg("\n## Item 5.1b — Checagem de primeira data do par USDT na Binance para cada candidato")
datas_binance <- sapply(candidatos_top50, function(s) {
  Sys.sleep(0.15)
  as.character(primeira_data_binance(paste0(s, "USDT")))
})
checagem_candidatos <- data.frame(ativo = candidatos_top50, primeira_data_usdt_binance = datas_binance,
                                   stringsAsFactors = FALSE)
checagem_candidatos$elegivel_criterio_a <- !is.na(checagem_candidatos$primeira_data_usdt_binance) &
  as.Date(checagem_candidatos$primeira_data_usdt_binance) < as.Date("2019-01-01")
write_csv(checagem_candidatos, file.path(dir_clean, "controle_checagem_candidatos.csv"))
log_msg("Checagem completa salva em data/clean/controle_checagem_candidatos.csv")
print(checagem_candidatos[order(checagem_candidatos$primeira_data_usdt_binance), ])

cesta_ativos <- checagem_candidatos$ativo[which(checagem_candidatos$elegivel_criterio_a)]
# BTC/ETH não entram na cesta de controle (são os ativos de interesse do estudo)
cesta_ativos <- setdiff(cesta_ativos, c("BTC","ETH"))
log_msg(sprintf("\nCesta de controle final (critérios a+b+c): %s", paste(cesta_ativos, collapse=", ")))

## ---- 5.2 Datas de ETF à vista nos EUA (fontes primárias, verificadas via SEC EDGAR / comunicados) ----
# Verificado nesta sessão (2026-09-21) via busca e SEC EDGAR (data.sec.gov):
#  - LTC: Canary Litecoin ETF (LTCC), Nasdaq. Início de negociação 2025-10-28.
#    Fonte: Nasdaq press release; SEC EDGAR CIK 0002039461 (Form 8-A).
#  - XRP: Canary XRP ETF (XRPC), Nasdaq. Form 8-A12B datado 2025-11-10;
#    início de negociação 2025-11-13. Fonte: SEC EDGAR CIK 0002039505,
#    filename "canaryxrp-8a12b_111025.htm"; Coindesk 2025-11-13.
#  - TRX: Canary Staked TRX ETF (TRXS), Cboe BZX. Lançado 2026-09-09.
#    Descrito pelo emissor/imprensa como "spot" (expõe ao preço à vista
#    do TRX), mas com staking do saldo — não é um ETF à vista "puro" como
#    IBIT/FBTC. Fonte: SEC EDGAR CIK 0002064768 (S-1/424B3); GlobeNewswire
#    2026-09-09. NOTA METODOLÓGICA: se a análise exigir ETF à vista puro
#    (sem staking), tratar TRX como "sem ETF à vista" e mover para Cesta A.
#  - ADA: SEM ETF à vista listado até a data desta coleta (2026-09-21).
#    Janela regulatória (75 dias) abre em 2026-08-09, prazo final da SEC
#    2026-10-23 — ainda não decidido. Fonte: buscas de imprensa
#    especializada (CryptoSlate, Medium/TapTools), sem confirmação SEC de
#    aprovação. Marcado "verificar" — reconfirmar antes de publicar.
#
# ATENÇÃO: datas de XRP/LTC também informadas no prompt como "outubro/
# novembro de 2025" foram confirmadas e não presumidas — checagem individual
# feita via SEC EDGAR e imprensa especializada nesta sessão.
etf_datas <- tibble::tribble(
  ~ativo, ~tem_etf_a_vista_eua, ~data_primeira_listagem, ~fonte,
  "XRP", "sim", "2025-11-13", "SEC EDGAR CIK 0002039505 (Form 8-A12B, 2025-11-10); Coindesk 2025-11-13 (XRPC início de negociação, Nasdaq)",
  "TRX", "sim (staked, ver nota)", "2026-09-09", "SEC EDGAR CIK 0002064768 (S-1/424B3); GlobeNewswire 2026-09-09 (TRXS, Cboe BZX) — inclui staking, não é spot puro",
  "ADA", "não (até 2026-09-21)", NA, "Sem ETF à vista aprovado/listado até a data da coleta; janela SEC 75 dias aberta em 2026-08-09, prazo final 2026-10-23 (verificar novamente antes de publicar)",
  "XLM", "não (até 2026-09-21)", NA, "Nenhuma listagem de ETF à vista confirmada até a data da coleta — verificar periodicamente",
  "LTC", "sim", "2025-10-28", "Nasdaq press release 2025-10-28 (Canary Litecoin ETF, LTCC); SEC EDGAR CIK 0002039461"
)
etf_datas <- etf_datas[etf_datas$ativo %in% cesta_ativos, ]
write_csv(etf_datas, file.path(dir_clean, "controle_etf_datas.csv"))
log_msg("\n## Item 5.3 — data/clean/controle_etf_datas.csv gravado:")
print(etf_datas)

## ---- 5.3 (Item 5.4) Coleta de fechamento e volume diários na Binance ---
log_msg("\n## Item 5.4 — Coletando fechamento/volume diário (Binance) para cada ativo da cesta")

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
      req_user_agent(ua)
    resp <- req_perform(req)
    d <- resp_body_json(resp, simplifyVector = TRUE)
    if (is.null(d) || (is.matrix(d) && nrow(d) == 0) || length(d) == 0) break
    all_rows[[length(all_rows) + 1]] <- d
    last_open <- as.numeric(d[nrow(d), 1])
    if (nrow(d) < 1000 || last_open >= end_ms) break
    cursor <- last_open + 86400000
    Sys.sleep(0.25)
  }
  if (length(all_rows) == 0) return(NULL)
  m <- do.call(rbind, all_rows)
  df <- data.frame(
    date = as.Date(as.POSIXct(as.numeric(m[,1])/1000, origin = "1970-01-01", tz = "UTC")),
    open = as.numeric(m[,2]), high = as.numeric(m[,3]), low = as.numeric(m[,4]),
    close = as.numeric(m[,5]), volume = as.numeric(m[,6]), stringsAsFactors = FALSE
  )
  df[!duplicated(df$date), ]
}

for (ativo in cesta_ativos) {
  df <- tryCatch(get_binance_klines(paste0(ativo, "USDT"), "2017-01-01"),
                  error = function(e) { log_msg(sprintf("%s: ERRO Binance — %s", ativo, conditionMessage(e))); NULL })
  if (!is.null(df)) {
    write_csv(df, file.path(dir_raw, paste0(ativo, "USDT_binance_1d_raw.csv")))
    write_csv(df, file.path(dir_clean, paste0("cesta_", tolower(ativo), "_binance.csv")))
    log_msg(sprintf("%s: %d obs. (%s a %s)", ativo, nrow(df), format(min(df$date)), format(max(df$date))))
  }
}

## ---- 5.4 (Item 5.5) Propor composições Cesta A e Cesta B ---------------
log_msg("\n## Item 5.5 — Composições propostas (apenas documentação, sem estimação)")
tem_etf <- etf_datas %>% filter(.data$tem_etf_a_vista_eua %in% c("sim") | grepl("^sim", .data$tem_etf_a_vista_eua))
sem_etf_toda_amostra <- setdiff(cesta_ativos, tem_etf$ativo)

# Cesta B: Cesta A + ativos que ganharam ETF DEPOIS de 2024-07-23 (corte ETH),
# truncados na véspera da listagem.
cortes_pos_eth <- tem_etf %>%
  filter(!is.na(.data$data_primeira_listagem)) %>%
  filter(as.Date(.data$data_primeira_listagem) > as.Date("2024-07-23"))

cesta_A <- sem_etf_toda_amostra
cesta_B_extra <- cortes_pos_eth %>%
  transmute(ativo, trunca_ate = as.Date(.data$data_primeira_listagem) - 1)

log_msg(sprintf("Cesta A (sem ETF à vista em toda a amostra): %s", paste(cesta_A, collapse=", ")))
log_msg("Cesta B (Cesta A + ativos truncados na véspera da 1ª listagem):")
print(cesta_B_extra)

composicoes <- list(
  cesta_A = cesta_A,
  cesta_B = list(base = cesta_A, extras_truncados = cesta_B_extra)
)
saveRDS(composicoes, file.path(dir_clean, "controle_composicoes_cesta.rds"))
writeLines(
  c("# Composições propostas — cesta de controle (documentação, sem estimação)",
    "",
    sprintf("Cesta A: %s", paste(cesta_A, collapse=", ")),
    "",
    "Cesta B: Cesta A + ativos abaixo, cada série truncada na véspera da 1ª listagem de ETF à vista:",
    capture.output(print(cesta_B_extra))
  ),
  file.path(dir_clean, "controle_composicoes_cesta.md")
)

writeLines(c(paste0("# Log de coleta — Item 5 (cesta de controle) — ", hoje), "", log_lines),
           file.path("data", "coleta_log_item5.md"))

cat("\n== FIM COLETA 5 ==\n")
