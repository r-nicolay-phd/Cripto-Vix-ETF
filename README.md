# Is it the ETF? Transmission of risk aversion to Bitcoin and Ethereum after spot ETF approvals

Replication code and data for the article (Finance Research Letters, submitted).
Authors: Rodolfo Tomás da Fonseca Nicolay, Nelson Felippe Pinheiro, Marcelo Cabús Klötzle.

The repository contains the code and data that generated the current manuscript (Table 1,
Figures 1–5) and its Supplementary Material (Tables S1–S33, Figures S1–S2). The pipeline has
three layers, all run by `run_all.R`:

* `code/analysis/01–20` (prefix `V7-` in the output files): data, point estimates, quantile-on-quantile
  surfaces, placebo, break tests, flow channel, price-source and lag-order robustness.
* `code/analysis/21–27` (prefix `V9-`): QVAR stability, NPDC by horizon, alternative
  specifications, VIX level at the conditional quantiles, scale effect, signed responses,
  block influence, flow-channel power, alternative uncertainty measures.
* `code/analysis/28–32` (prefix `V10-`, plus `Fig4_*.png`): validity diagnostics of the
  bootstrap schemes and all bootstrap-based inference of the current version (Table 1,
  counterfactual, exclusions, alternative samples and specifications).

Earlier development versions are not included. Where a later layer re-estimates a quantity of
an earlier one with the final bootstrap, the earlier output is still written by the earlier
script but is not cited in the article (see "Outputs not used in the article" below).

## Reproducing the results

Requirements: R ≥ 4.5 with `ConnectednessApproach` (1.0.4), `quantreg`, `rugarch`, `zoo`, `xts`,
`vars`, `urca`, `tseries`, `strucchange`, `sandwich`, `lmtest`, `readxl`, `ggplot2`, `patchwork`,
`scales`. Collection scripts also use `httr2`, `jsonlite`, `rvest`, `quantmod`, `lubridate`,
`dplyr`, `readr`, `openxlsx`, `stringr`, `tibble`, `timeDate`.
The exact package versions used are recorded at the start of each script in `results/log.txt`
(and `results/sessionInfo.txt`).

From the repository root:

```
Rscript --vanilla --encoding=UTF-8 run_all.R
```

`run_all.R` runs `code/analysis/01_*.R` … `32_*.R` in order, each in its own R process, and
writes CSV tables to `results/csv/`, figures to `results/fig/` and bootstrap replicates and
intermediate objects to `results/rds/`. Scripts that find their `.rds` block already in
`results/rds/` load it instead of re-estimating it, so a clean re-run requires an empty
`results/` folder. All bootstraps use fixed seeds (V7: 20260922 for the row-block bootstrap of
`03`, 20260930 + k for the other V7 tasks; V9: 20261015 + k; V10: 20261016 + k, with k stated
in each script), so the output is reproducible. Running time of a clean run is about
one hour with 14 parallel workers (20-core machine): about 40 min for scripts 01–20, 2 min
for 21–27 and 18 min for 28–32. The number of workers is set by `N_WORKERS` in
`code/functions/funcoes_contrafactual.R` and `code/functions/funcoes_v10.R`; it does not affect the
results, because all resampling indices and perturbations are drawn before the parallel step.

The `results/` folder in this repository is the output of a clean run of `run_all.R` on the
data in `data/clean/`.

## Specification and inference

Bivariate QVAR (VIX in levels, asset return), p = 2 (BIC), H = 10, τ ∈ {0.05, 0.10, 0.25,
0.50, 0.75, 0.90, 0.95}; sample 2018-01-01 to 2026-09-18 (2,223 daily observations per pair).
Splits: BTC 2024-01-11, ETH 2024-07-23. NPDC(τ) < 0: the asset is a net receiver of VIX shocks.
Control baskets: A = {ADA, XLM}; B = A + {XRP, LTC, TRX}; B′ = B + SOL, each truncated on the eve
of its own US spot ETF.

**Final inference (V10).** Percentile intervals come from a moving-block bootstrap of the rows of
the regression design: z_t = (y_t, y_t−1, y_t−2), y = (asset return, VIX), is built within each
subsample (and within each segment when a window is excluded, so that no lag crosses a gap);
blocks of 20 consecutive design rows, each carrying its own observed lags, are drawn within the
pre- and post-ETF subsamples and the QVAR is re-estimated on the resampled rows (B = 500; B =
2,000 for BC/BCa). The VIX is perturbed by U(−0.005, 0.005), drawn once per calendar date and
added wherever that date appears (dependent variable or lag). The counterfactual uses the same
design rows with a joint-by-date bootstrap (blocks of 20 common dates applied to every series).
The stationary bootstrap of design rows (mean block 20) is the robustness scheme. Both are the
only schemes that pass the validity diagnostics fixed in advance (Table S7: VIX range,
splice ratio, share of valid replicates). The earlier schemes — concatenated blocks of
(return, VIX level) rows ("concatenated level blocks", earlier version) and blocks of
(Δlog VIX, return) with the level rebuilt by cumulation ("rebuilt from log changes",
pre-registered in an earlier revision) — and the sieve VAR(2) bootstrap are reported only as
diagnostics (Table S7) or reference intervals (Table S8, Panel C). Two supplementary tables still
use row-block intervals of the V7 layer, as stated in their notes: Table S14, Panel C (VIX in log
differences with p = 1, where row resampling creates no level jumps) and Table S6, Panel A
(share of valid replicates).

## Repository layout

```
run_all.R                  runs the full analysis
code/functions/            function libraries, chained via CVE_ROOT:
                             funcoes_qvar_bootstrap.R -> funcoes_contrafactual.R ->
                             funcoes_pipeline.R (V7) -> funcoes_v9.R -> funcoes_v10.R
code/analysis/01–20_*.R    V7 layer: estimations, tables (CSV) and figures
code/analysis/21–27_*.R    V9 layer (point estimates, diagnostics, LP/regression bootstraps)
code/analysis/28–32_*.R    V10 layer (design-row bootstrap: all interval inference)
code/collection/           data collection and construction (documentation; not run by run_all.R)
data/raw/                  CoinMarketCap exports and Binance daily candles used to build the base
data/clean/                analysis-ready series read by code/analysis
results/                   output of run_all.R (csv, fig, rds, log.txt, progresso.md)
```

### V9 and V10 scripts

| Script (`code/analysis/`) | Origin | Content | Main outputs |
|---|---|---|---|
| `21_v9_diagnostico_emendas.R` | `v9_01_tarefa0.R` | Re-implemented QVAR = package; splice diagnostic of the concatenated level blocks | `V9-0a_*`, `V9-0b_*` |
| `22_v9_pontuais_jackknife.R` | `v9_02_tarefa1.R` (point estimates and jackknife only) | Point estimates; delete-one-block jackknife with the gap rule | `rds/v9_1_points.rds`, `rds/v9_1_jackknife.rds` |
| `23_v9_estabilidade_especificacoes.R` | `v9_03_tarefa2.R` (bootstrap of 2c removed) | Companion-matrix moduli; NPDC by H (Fig. 2); GARCH standardization; NPDC of the alternative specifications; Rule A | `V9-2a_*`, `V9-2b_*`, `V9-F2_*.png`, `V9-2c_garch_parameters.csv`, `V9-2c_alt_specs_npdc_raw.csv`, `V9-RuleA_*`, `rds/v9_2_garch.rds` |
| `24_v9_vix_quantis_projecoes.R` | `v9_04_tarefa3.R` | VIX at the conditional quantiles; local projections with a fixed VIX state | `V9-3a_*`, `V9-3b_*` |
| `25_v9_escala_estatistica_sinal.R` | `v9_05_tarefa4.R` (item 4a removed) | Scale effect; signed responses on high-VIX days | `V9-4b_*`, `V9-4c_*` |
| `26_v9_influencia_blocos.R` | `v9_06_tarefa5.R` (items 5b, 5c and the post-only figure removed) | Influence of 20-day blocks and sliding windows (Fig. 3) | `V9-5a_*`, `V9-F5_*.png` |
| `27_v9_fluxo_construtos_p_H.R` | `v9_07_tarefa6.R` (items 6d, 6e removed) | Flow-channel MDE and joint tests; alternative uncertainty measures; p × H grid | `V9-6a_*`, `V9-6b_*`, `V9-6c_*` |
| `28_v10_validacao_diagnostico.R` | `v10_01_validacao_diagnostico.R` | Point estimates on design rows; sieve roots; diagnostics of the earlier schemes | `V10-0a_*`, `V10-0b_*`, `V10-0c_*`, `V10-0d_*`, `rds/pairs_main.rds` |
| `29_v10_tabela1.R` | `v10_02_tabela1.R` | Design-row MBB (main), stationary and sieve bootstraps; BC/BCa; validity diagnostics; θ components; H = 5 and H = 1; Rule B | `V10-1_*`, `V10-7_*`, `V10-10_H5_table1_part_raw.csv`, `V10-8_variant_c_H1_raw.csv`, `V10-RuleB_verdict.csv` |
| `30_v10_contrafactual.R` | `v10_03_contrafactual.R` | Counterfactual (baskets A, B, B′), power/TOST, orthogonalized controls, Fig. 4 | `V10-2_*`, `V10-3_*`, `V10-4_*`, `V10-10_H5_counterfactual_raw.csv`, `Fig4_*.png` |
| `31_v10_exclusoes_amostras.R` | `v10_04_exclusoes_amostras.R` | Exclusions (gap rule), ETH pre-sample to 10 Jan 2024, regime from 17 Mar 2023 | `V10-5a_*`, `V10-5b_*`, `V10-5c_*`, `V10-6a_*`, `V10-6b_*` |
| `32_v10_especificacoes.R` | `v10_05_especificacoes.R` | Alternative specifications with the design-row bootstrap; Table 1 (baseline, 16:00 ET, Δlog VIX) | `V10-8_*`, `V10-9_*` |

Changes with respect to the development scripts are limited to paths, the removal of blocks
whose outputs are not used in the article (the V9 bootstraps with the rebuilt VIX, superseded by
the V10 design-row bootstrap) and the removal of checks that only compared point estimates with
files of the earlier versions. No estimation code, seed, number of replicates or specification
was changed. The checks against the V7 point estimates of this repository (`V9-0a`, `V10-0a`)
are kept. Not included: the comparison scripts (`v9_08`, `v10_08`, which only tabulate V7 × V9 ×
V10 differences), the document-consolidation scripts (`v9_10`, `v9_10b`, `v10_10`, which only
round the CSV files into Word tables; all figures they include are produced by the estimation
scripts above) and the V9 bootstrap of `v9_02` (rebuilt-VIX scheme, rejected in Table S7).

### Data

| File (`data/`) | Content | Source | Built by (`code/collection/`) |
|---|---|---|---|
| `raw/manual/{BTC,ETH}_cmc.xlsx` | Daily OHLC and market cap | CoinMarketCap (manual export) | — |
| `raw/binance/{BTC,ETH}USDT_binance_1d_raw.csv` | Daily candles (date check of the CMC export) | Binance public API | 10 |
| `clean/base_estendida_v7.csv` | BTC and ETH log returns (%) between consecutive VIX trading days; VIX level | CMC export relabelled to the 24:00 UTC close of its own day; FRED | 25 |
| `clean/verificacao_datas_cmc_v7.csv` | Year-by-year check of the CMC dates against Binance | — | 25 |
| `clean/vix_fred.csv` | VIX close (VIXCLS) | FRED | 10 |
| `clean/{btc,eth}_binance.csv` | Daily close and volume | Binance public API | 10 |
| `clean/cesta_{ada,xlm,xrp,ltc,trx}_binance.csv` | Control assets, daily candles (24:00 UTC) | Binance public API | 13 |
| `clean/cesta_sol_binance.csv` | Solana, daily candles | Binance public API | 26 |
| `clean/etf_listing_dates.csv` | First US spot ETF listing dates of control/candidate assets, with sources | SEC filings, issuers, press | manual (documented in the file) |
| `clean/etf_flow_{btc,eth}_farside[_limpo].csv` | Daily spot-ETF net flows (raw and cleaned to the NYSE calendar) | Farside Investors | 11, 22 |
| `clean/etf_{ibit,etha}_yahoo.csv` | IBIT and ETHA prices and volume (flow normalization) | Yahoo Finance | 12 |
| `clean/gspc_yahoo.csv` | S&P 500 (realized-volatility construct) | Yahoo Finance | 14 |
| `clean/epu_daily.csv` | US daily economic policy uncertainty | policyuncertainty.com | 16 |
| `clean/dvol_{btc,eth}_deribit.csv` | Deribit implied-volatility indices | Deribit public API | 21 |
| `clean/px16et_<asset>_<coinbase,binance>.csv` | Prices at 16:00 ET from hourly candles | Coinbase Exchange and Binance public APIs | 27 |
| `raw/hourly_collection_log.csv` | Coverage of the hourly collection | — | 27 |

The V9 and V10 layers read only these files (and the outputs of earlier scripts in `results/`).

The CoinMarketCap export labels each row D with the close of UTC day D−1.
`25_correcao_datas_cmc.R` relabels it to the close of its own UTC day, the convention of the
Binance controls, before computing returns.

No API keys are needed. Optional keys are read only from environment variables
(`FRED_API_KEY`, `COINGECKO_API_KEY`, `SOSOVALUE_API_KEY`). Re-running the collection scripts
downloads current data into new dated folders under `data/raw/` and overwrites `data/clean/`.
To reproduce the article, run only `run_all.R`.

Third-party data are redistributed solely to allow replication and remain subject to the
terms of use of their providers (CoinMarketCap, Binance, Coinbase, FRED, Yahoo Finance, Farside
Investors, Deribit, policyuncertainty.com).

## Map: article items → scripts and output files

Files are in `results/csv/` and `results/fig/`; scripts in `code/analysis/`. The attribution
was obtained by matching the numbers of every table row with the rows of the output files.
Numbers in the article are these values rounded (usually to one decimal).

### Manuscript

| Item | Script | Output |
|---|---|---|
| Table 1 | 32 (baseline column from 29) | `V10-9_new_table1_raw.csv` (columns `Baseline (24:00 UTC)`, `16:00 ET prices`, `Dlog VIX, p = 2`) |
| Fig. 1 | 02, 17 | `V7-Fig1_npdc_by_quantile.png` (values: `V7-1_npdc_net_tci_by_quantile.csv`) |
| Fig. 2 | 23 | `V9-F2_npdc_by_horizon.png` (values: `V9-2b_npdc_by_horizon_raw.csv`) |
| Fig. 3 | 22, 26 | `V9-F5_influence_pre_post.png` (values: `V9-5a_influence_all_blocks_raw.csv`, `V9-5a_influence_sliding_step5_raw.csv`) |
| Fig. 4 | 30 | `Fig4_counterfactual_median_upper_tail.png` (values: `V10-2_counterfactual_raw.csv`) |
| Fig. 5 | 06, 20 | `V7-F5.11_flow_differential_state_tminus1.png` (values: `V7-5.11_flow_LP_by_horizon.csv`) |

### Supplementary Material

| Item | Script | Output |
|---|---|---|
| S1 Control assets, ETF listings, coverage | 12, 04 | A: `V7-6.3c_listing_dates.csv` (`data/clean/etf_listing_dates.csv`); B: `V7-5.7_counterfactual_coverage.csv` |
| S2 Date alignment | 08 | `V7-6.0b_cmc_relabelled_vs_binance_label_test.csv` |
| S3 Descriptive statistics | 01 | `V7-1_descriptives.csv` |
| S4 Unit-root and stationarity tests | 01, 15 | A: `V7-1_adf.csv`; B: `V7-6.7a_unit_root_full.csv` |
| S5 Lag-order selection | 02 | `V7-1_lag_selection_summary.csv` (`V7-1_lag_selection_full.csv`) |
| S6 Bootstrap validity, VIX perturbation | 03, 28 | A: `V7-5.1_bootstrap_validity.csv`; B: `V7-5.3_noise_check_point.csv`; C: `V10-0a_validation_V7-5.4.csv` |
| S7 Validity diagnostics of the bootstrap schemes | 28, 29, 21 | A: `V10-1_validity_diagnostics.csv` (earlier schemes from `V10-0d_validity_reference_schemes.csv`; sieve moduli in `V10-0c_sieve_VAR_roots.csv`); B: `V9-0b_splice_jump_distribution.csv` (rows "Observed" and "Old scheme") |
| S8 Stationary, sieve and BC/BCa intervals | 29 | A: `V10-1_table1_SB_raw.csv`; B: `V10-1_BC_BCa_B2000_raw.csv`; C: `V10-1_table1_SIEVE_raw.csv` |
| S9 Decision rule and interpretation rules | 19, 05, 02, 23, 29 | A: `V7-S13_decision_rule.csv`, `V7-S13_criterion{1,2,3}_detail.csv`, `V7-5.8_placebo_summary.csv`, `V7-1_breaks_TVP_TCI.csv` (see note 1); B: `V7-S12_placebo_lower_tail_V4_design.csv`; C: `V9-RuleA_verdict.csv`, `V9-RuleA_pattern_by_variant.csv`, `V9-2a_roots_raw.csv`; D: `V10-RuleB_verdict.csv` |
| S10 NPDC by quantile | 02, 17 | A: `V7-1_npdc_net_tci_by_quantile.csv`, `V7-1_delta_all_quantiles_point.csv`; B: `V7-S18_alt_quantile_grid.csv` |
| S11 QVAR stability and NPDC by horizon | 23 | A: `V9-2a_roots_raw.csv`; B: `V9-2b_npdc_by_horizon_raw.csv` |
| S12 VIX on days beyond the conditional quantiles | 24 | `V9-3a_vix_at_conditional_quantiles_raw.csv` |
| S13 Quantile-on-quantile surfaces (and Fig. S1) | 17 | `V7-S5_qq_surfaces.csv` (`V7-S5_qq_surfaces_long.csv`); Fig. S1: `V7-Fig2_qq_surfaces.png` |
| S14 Alternative specifications | 23, 32, 29, 15 | A: `V9-2c_alt_specs_npdc_raw.csv` (variants a, b, d also in `V10-8_alt_specs_npdc_raw.csv`); B: `V10-8_alt_specs_boot_raw.csv` (variant c from `V10-8_variant_c_H1_raw.csv`); C: `V7-6.7b_table1_dlog.csv`, `V7-6.7b_counterfactual_dlog.csv` (row-block bootstrap, see note 2) |
| S15 NPDC components and scale effect | 29, 25 | A: `V10-7_components_raw.csv`; B: `V9-4b_scale_effect_raw.csv` |
| S16 Influence of 20-day blocks | 22, 26 | `V9-5a_influence_top5_raw.csv` (all blocks: `V9-5a_influence_all_blocks_raw.csv`; sliding windows: `V9-5a_influence_sliding_*`) |
| S17 Exclusion of stress episodes | 31, 02 | A: `V10-5a_exclusion_Aug2024_raw.csv`, `V10-5b_eth_short_exclusion_raw.csv`, `V10-5c_joint_exclusion_boot_p2_raw.csv` (sizes: `V10-5*_sizes.csv`); B: `V7-1_exclusion_Aug2024_delta_point.csv`, `V7-1_exclusion_Aug2024_DD_point.csv`; C: `V10-5c_joint_exclusion_points_raw.csv` |
| S18 Signed responses on high-VIX days | 25, 24 | A: `V9-4c_signed_statistic_raw.csv`, `V9-4c_sample_sizes.csv`; B: `V9-3b_LP_fixed_threshold_raw.csv`, `V9-3b_thresholds.csv` |
| S19 Counterfactual at τ = 0.25–0.95 and power | 30 | A: `V10-2_counterfactual_raw.csv`; B: `V10-3_power_TOST_raw.csv` |
| S20 Counterfactual, each control | 30 | `V10-2_counterfactual_raw.csv` |
| S21 Controls orthogonalized to Bitcoin | 12, 30 | A: `V7-6.3a_orthogonalization_betas.csv` (rows with the Binance regressor also in `V10-4_orthogonalization_betas.csv`); B: `V10-4_orthogonalized_raw.csv` |
| S22 Baskets with Solana | 30 | `V10-2_counterfactual_raw.csv` (SOL and B′ rows) |
| S23 Upper-tail placebo (and Fig. S2) | 05, 20 | A: `V7-5.8_placebo_summary.csv`; B: `V7-5.9_placebo_all_dates.csv`; Fig. S2: `V7-F5.8_placebo_DD095_histogram.png` |
| S24 Placebo at the median and τ = 0.75 | 09 | `V7-6.1c_placebo_summary_full.csv` (`V7-6.1c_placebo_all_dates.csv`) |
| S25 Structural breaks | 02 | `V7-1_breaks_TVP_TCI.csv` |
| S26 Flow channel, state at t − 1 | 06, 02, 27 | A: `V7-5.11_flow_LP_by_horizon.csv` (columns S_t from `V7-1_flow_LP_state_t.csv`); B: `V9-6a_flow_MDE_raw.csv` |
| S27 Flow channel, state at t; quantile regression | 02, 08 | A: `V7-1_flow_LP_state_t.csv`; B: `V7-6.0d_S15_flow_RQ_state_t_vs_tminus1.csv` |
| S28 Price source and closing time | 16 | A: `V7-6.4a_series_comparison_full.csv`; B: `V7-6.4b_npdc_by_quantile_combos.csv` |
| S29 Lag order and forecast horizon | 27, 07 | A: `V9-6c_p_H_raw.csv`; B: `V7-5.14_upper_tail_excl_basketB_by_p.csv` |
| S30 Horizon H = 5 | 29, 30 | `V10-10_H5_table1_part_raw.csv`, `V10-10_H5_counterfactual_raw.csv` |
| S31 ETH pre-sample ending 10 Jan 2024 | 31 | `V10-6a_eth_clean_pre_raw.csv`, `V10-6a_eth_clean_pre_counterfactual_raw.csv` |
| S32 Pre-sample from 17 Mar 2023 | 31, 14 | A: `V10-6b_regime_raw.csv`; B: `V7-6.6_recent_regime_tails_point.csv` (n from `V7-6.6_recent_regime_boot.csv`) |
| S33 Alternative uncertainty measures | 27 | `V9-6b_alt_uncertainty_raw.csv` |

Notes. (1) In Table S9, Panel A, criterion (iii), the treated-minus-basket-B intervals of the
supplement (BTC [−1.9, 2.2] and [−14.5, 25.8]; ETH [−1.9, 3.4] and [−21.4, 20.4]) come from the
rebuilt-VIX bootstrap of the earlier revision, which is not part of this repository; the
corresponding design-row intervals, used in the manuscript and in Table S19, are in
`V10-2_counterfactual_raw.csv` (BTC [−2.8, 3.0] and [−21.1, 43.0]; ETH [−2.0, 3.7] and
[−31.0, 21.4]). The point estimates and the verdict are the same. (2) Table S14, Panel C uses the
V7 row-block bootstrap (blocks of data rows), as its notes state.

### Outputs not used in the article

The V7 layer also writes the results of the earlier row-block bootstrap, re-estimated in the
V10 layer and not cited in the current version: `V7-5.4_stats_MBB.csv` (point estimates used as
reference in Table S6, Panel C), `V7-5.5_*`, `V7-5.6_*`, `V7-6.1a_*`, `V7-6.1b_*`, `V7-6.2_*`,
`V7-6.3_counterfactual_orth_all.csv`, `V7-6.4b_table1_combos.csv`, `V7-6.4b_counterfactual_combos.csv`,
`V7-6.5_*`, `V7-6.6_recent_regime_boot.csv` (except n), `V7-6.8a_*`, `V7-6.8b_*`, `V7-S8_*` and the
figures `V7-F3_counterfactual_median_and_upper_tail.png` and `V7-F8_influence_blocks.png`
(earlier versions of Figs. 4 and 3). The V9 and V10 layers also write intermediate and
diagnostic files that are not tabulated in the article (e.g. `V9-0a_*`,
`V9-0b_splices_by_replicate.csv`, `V9-5a_figure_axis_limits.csv`, `V10-0b_*`) and
`V10-1_table1_MBB_raw.csv`, the full main-scheme table whose Δ and DD(0.95) rows form the
baseline column of `V10-9_new_table1_raw.csv`.
