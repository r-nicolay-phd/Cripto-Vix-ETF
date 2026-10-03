# =====================================================================
# Reproduz todas as estimacoes, tabelas (CSV) e figuras do artigo e do
# material suplementar a partir de data/clean/.
# Uso (na raiz do repositorio):
#   Rscript --vanilla --encoding=UTF-8 run_all.R
# Cada script roda em um processo R separado, na ordem abaixo. Saidas em
# results/csv, results/fig, results/rds; registro em results/log.txt.
# Tempo aproximado: 60-90 min com 14 nucleos (bootstraps B = 500).
# Os scripts de code/collection NAO sao executados aqui: eles documentam
# a coleta (APIs publicas, sem chave) que gerou data/clean/.
# =====================================================================
root <- normalizePath(".", winslash = "/")
stopifnot(file.exists(file.path(root, "code/functions/funcoes_pipeline.R")))
Sys.setenv(CVE_ROOT = root)
rscript <- file.path(R.home("bin"), "Rscript")
scripts <- sort(list.files(file.path(root, "code/analysis"), pattern = "^[0-9]{2}_.*\\.R$", full.names = TRUE))
for (s in scripts) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), basename(s)))
  st <- system2(rscript, c("--vanilla", "--encoding=UTF-8", shQuote(s)))
  if (st != 0) stop("Falha em ", basename(s), " (codigo ", st, ")")
}
cat("Concluido.\n")
