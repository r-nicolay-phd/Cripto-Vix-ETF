# =====================================================================
# FUNCOES DO PIPELINE FINAL (origem: v7_00_funcoes.R). Carregadas por todos
# os scripts de code/analysis e pelos trabalhadores paralelos. Em cadeia:
#   code/functions/funcoes_qvar_bootstrap.R (npdc_one/npdc_taus, mbb_idx,
#       sb_idx, ruido, stats_from_npdc, par_map_timeout c/ timeout)
#   code/functions/funcoes_contrafactual.R (run_sep_boot, run_joint_boot,
#       point_sep, point_joint, tab_sep, tab_joint, ret_ii, serie_ctrl_xts)
# Base: data/clean/base_estendida_v7.csv (export CoinMarketCap re-rotulado
# para o fechamento 24:00 UTC do proprio dia; retornos entre pregoes
# consecutivos do VIX), montada por code/collection/25_correcao_datas_cmc.R.
# Saidas em results/ (csv, rds, fig, log.txt, progresso.md).
# =====================================================================

source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_contrafactual.R"))
FUNCS_PATH <- file.path(PROJ_DIR, "code/functions/funcoes_pipeline.R")

OUT7 <- "results"
OUT6 <- OUT7                                   # log_v6() -> results/log.txt
for (d in c("csv","rds","fig","logs")) dir.create(file.path(OUT7, d), recursive = TRUE, showWarnings = FALSE)

BASE_V7   <- "data/clean/base_estendida_v7.csv"
RDS_NEW   <- file.path(OUT7, "rds", "dados_analise_v7.rds")

carregar_v7 <- function() readRDS(RDS_NEW)
## As funcoes reaproveitadas chamam carregar_v4(): devolve as amostras da base final
carregar_v4 <- function() carregar_v7()

log_v7 <- function(...) log_v6(...)
log_pkgs7 <- function(script) log_pkgs(script)

## progresso.md: acrescenta uma linha com carimbo de hora
progresso <- function(txt) {
  f <- file.path(OUT7, "progresso.md")
  if (!file.exists(f)) writeLines(c("# V7 — progresso", ""), f)
  cat(paste0("- [", format(Sys.time(), "%Y-%m-%d %H:%M"), "] ", txt, "\n"), file = f, append = TRUE)
}
