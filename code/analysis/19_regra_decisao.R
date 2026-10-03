# =====================================================================
# V7b — Table S12 (placebo da cauda inferior, DESENHO ORIGINAL DA V4,
# script 27 secao 27.1) e Table S13 (regra de decisao pre-registrada,
# script 28, regra inalterada) na base nova.
#  S12: DD(0.05) no corte real e nas datas falsas 01/06/2021, 03/01/2022,
#       01/06/2022, 02/01/2023 com pre = [inicio, d), pos = [d, fim da amostra]
#       (inclui o periodo pos-ETF -> superado pela S24 / V7-5.8).
#  S13: (i) DD real fora de [min, max] dos 4 placebos (S12 nova); ao lado,
#       o placebo S24 (V7-5.8), apenas informativo;
#       (ii) quebra BP ou QA a <= 60 dias do ETF com ago/2024 E sem ago/2024
#       (V7-1f) -> Sim; so com -> Parcial; senao Nao;
#       (iii) cesta B: |DD tratado| > |DD cesta| E IC do gap (Delta tratado -
#       Delta cesta) exclui zero, em alguma cauda (V7-5.6 / V7-S8).
#  Linguagem causal so se os tres forem "Sim".
# =====================================================================
source(file.path(Sys.getenv("CVE_ROOT", "."), "code/functions/funcoes_pipeline.R"))
t0 <- Sys.time(); log_pkgs7("v7_42_s12_s13.R")
D <- carregar_v4(); CSV7 <- file.path(OUT7, "csv")
dd_lo <- function(Mp, Mq) { np <- npdc_taus(Mp, c(0.05, 0.50)); nq <- npdc_taus(Mq, c(0.05, 0.50)); unname((nq["0.05"] - np["0.05"]) - (nq["0.50"] - np["0.50"])) }
datas <- as.Date(c("2021-06-01","2022-01-03","2022-06-01","2023-01-02"))
s12 <- do.call(rbind, lapply(c("btc","eth"), function(par) { Zt <- D$amostras[[par]]$total
  rbind(data.frame(pair = toupper(par), split_date = CUTS[[par]], type = "real", n_pre = nrow(D$amostras[[par]]$pre), n_post = nrow(D$amostras[[par]]$post),
                   DD_0.05 = dd_lo(coredata(D$amostras[[par]]$pre), coredata(D$amostras[[par]]$post))),
        do.call(rbind, lapply(datas, function(d) { Mp <- coredata(Zt[index(Zt) < d]); Mq <- coredata(Zt[index(Zt) >= d])
          data.frame(pair = toupper(par), split_date = d, type = "placebo", n_pre = nrow(Mp), n_post = nrow(Mq), DD_0.05 = dd_lo(Mp, Mq)) }))) }))
print(s12); write.csv(s12, file.path(CSV7, "V7-S12_placebo_lower_tail_V4_design.csv"), row.names = FALSE)
progresso("V7b item 5: Table S12 (placebo cauda inferior, desenho V4) reestimada.")

## ---- S13 ------------------------------------------------------------------
c1 <- do.call(rbind, lapply(c("BTC","ETH"), function(p) { s <- s12[s12$pair == p, ]; real <- s$DD_0.05[s$type == "real"]; f <- s$DD_0.05[s$type == "placebo"]
  fora <- real < min(f) || real > max(f)
  data.frame(pair = p, dd_real = real, placebo_min = min(f), placebo_max = max(f), criterion1 = ifelse(fora, "Sim", "Nao")) }))
p24 <- read.csv(file.path(CSV7, "V7-5.8_placebo_summary.csv")); p24 <- p24[p24$statistic == "DD_0.95", ]
c1$S24_DD095_real <- p24$real[match(c1$pair, p24$pair)]; c1$S24_p_incl_real <- p24$p_value_incl_real[match(c1$pair, p24$pair)]
c1$S24_real_below_p5 <- p24$real_below_p5[match(c1$pair, p24$pair)]
br <- read.csv(file.path(CSV7, "V7-1_breaks_TVP_TCI.csv"))
c2 <- do.call(rbind, lapply(c("BTC","ETH"), function(p) { com <- br[br$pair == p & br$sample == "with Aug 2024", ]; sem <- br[br$pair == p & br$sample != "with Aug 2024", ]
  pc <- any(com$days_to_ETF <= 60, na.rm = TRUE); ps <- any(sem$days_to_ETF <= 60, na.rm = TRUE)
  data.frame(pair = p, nearest_BP_with = com$date[1], days_BP_with = com$days_to_ETF[1], QA_with = com$date[2], days_QA_with = com$days_to_ETF[2],
             nearest_BP_without = sem$date[1], days_BP_without = sem$days_to_ETF[1], QA_without = sem$date[2], days_QA_without = sem$days_to_ETF[2],
             break_within_60d_with = pc, break_within_60d_without = ps, criterion2 = if (pc && ps) "Sim" else if (pc && !ps) "Parcial" else "Nao") }))
ic <- read.csv(file.path(CSV7, "V7-S8_counterfactual_DD_all.csv"))
gi <- function(cn, st, col = "point") ic[[col]][ic$cut == cn & ic$statistic == st]
c3 <- do.call(rbind, lapply(c("BTC","ETH"), function(p) {
  r <- sapply(c(lo = "0.05", hi = "0.95"), function(t) { exc <- abs(gi(p, paste0("TREATED_DD_", t))) > abs(gi(p, paste0("BasketB_DD_", t)))
    sig <- gi(p, paste0("TREATEDminusBasketB_Delta_", t), "ci95_lo") > 0 | gi(p, paste0("TREATEDminusBasketB_Delta_", t), "ci95_hi") < 0
    c(exceeds = exc, sig = sig, support = exc & sig) })
  data.frame(pair = p, DD_treated_lo = gi(p, "TREATED_DD_0.05"), DD_basketB_lo = gi(p, "BasketB_DD_0.05"), gap_lo = gi(p, "TREATEDminusBasketB_Delta_0.05"),
             gap_lo_ci = sprintf("[%.2f; %.2f]", gi(p, "TREATEDminusBasketB_Delta_0.05", "ci95_lo"), gi(p, "TREATEDminusBasketB_Delta_0.05", "ci95_hi")),
             DD_treated_hi = gi(p, "TREATED_DD_0.95"), DD_basketB_hi = gi(p, "BasketB_DD_0.95"), gap_hi = gi(p, "TREATEDminusBasketB_Delta_0.95"),
             gap_hi_ci = sprintf("[%.2f; %.2f]", gi(p, "TREATEDminusBasketB_Delta_0.95", "ci95_lo"), gi(p, "TREATEDminusBasketB_Delta_0.95", "ci95_hi")),
             H4_support_lo = ifelse(r["support","lo"], "Sim", "Nao"), H4_support_hi = ifelse(r["support","hi"], "Sim", "Nao"),
             criterion3 = ifelse(r["support","lo"] | r["support","hi"], "Sim", "Nao")) }))
dec <- merge(merge(c1[, c("pair","criterion1")], c2[, c("pair","criterion2")]), c3[, c("pair","criterion3")])
dec$causal_language_admissible <- ifelse(dec$criterion1 == "Sim" & dec$criterion2 == "Sim" & dec$criterion3 == "Sim", "Sim", "Nao")
print(c1); print(c2); print(c3); print(dec)
write.csv(c1, file.path(CSV7, "V7-S13_criterion1_detail.csv"), row.names = FALSE)
write.csv(c2, file.path(CSV7, "V7-S13_criterion2_detail.csv"), row.names = FALSE)
write.csv(c3, file.path(CSV7, "V7-S13_criterion3_detail.csv"), row.names = FALSE)
write.csv(dec, file.path(CSV7, "V7-S13_decision_rule.csv"), row.names = FALSE)
saveRDS(list(s12 = s12, c1 = c1, c2 = c2, c3 = c3, dec = dec), file.path(OUT7, "rds", "v7b_s12_s13.rds"))
log_v7(sprintf("V7b S13: BTC (i) %s (ii) %s (iii) %s -> causal %s; ETH (i) %s (ii) %s (iii) %s -> causal %s",
               dec$criterion1[1], dec$criterion2[1], dec$criterion3[1], dec$causal_language_admissible[1], dec$criterion1[2], dec$criterion2[2], dec$criterion3[2], dec$causal_language_admissible[2]))
progresso(sprintf("V7b item 6: Table S13 reavaliada: BTC %s/%s/%s -> causal %s; ETH %s/%s/%s -> causal %s.",
                  dec$criterion1[1], dec$criterion2[1], dec$criterion3[1], dec$causal_language_admissible[1], dec$criterion1[2], dec$criterion2[2], dec$criterion3[2], dec$causal_language_admissible[2]))
log_v7(sprintf("v7_42 concluido em %.1f s", as.numeric(Sys.time() - t0, units = "secs")))
