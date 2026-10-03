# Is it the ETF? Tail transmission of risk aversion to Bitcoin and Ethereum after spot ETF approvals

Replication code and data for the article (Finance Research Letters, submitted).
Authors: Rodolfo Tomás da Fonseca Nicolay, Nelson Felippe Pinheiro, Marcelo Cabús Klötzle.

The repository contains only the code and data that generated the final manuscript and its
Supplementary Material (Tables 1, S1–S30; Figures 1–5, S1–S2). Earlier development versions of
the pipeline are not included.

## Reproducing the results

Requirements: R ≥ 4.5 with `ConnectednessApproach` (1.0.4), `quantreg`, `zoo`, `xts`, `vars`,
`urca`, `tseries`, `strucchange`, `sandwich`, `lmtest`, `readxl`, `ggplot2`, `patchwork`,
`scales`. Collection scripts also use `httr2`, `jsonlite`, `rvest`, `quantmod`, `lubridate`,
`dplyr`, `readr`, `openxlsx`, `stringr`, `tibble`, `timeDate`.
The exact package versions used are recorded at the top of each run in `results/log.txt`.

From the repository root:

```
Rscript --vanilla --encoding=UTF-8 run_all.R
```

`run_all.R` runs `code/analysis/01_*.R` … `20_*.R` in order, each in its own R process, and
writes CSV tables to `results/csv/`, figures to `results/fig/` and bootstrap replicates to
`results/rds/`. All bootstraps use fixed seeds (20260922 for the main moving-block bootstrap;
20260930 + k for the remaining tasks), so the output is reproducible. Running time is about
one hour with 14 cores. The number of parallel workers is set by `N_WORKERS` in
`code/functions/funcoes_contrafactual.R`.

The `results/` folder in this repository is the output of a clean run of `run_all.R` on the
data in `data/clean/`.

## Specification

Bivariate QVAR (VIX in levels, asset return), p = 2 (BIC), H = 10, τ ∈ {0.05, 0.10, 0.25,
0.50, 0.75, 0.90, 0.95}; sample 2018-01-01 to 2026-09-18 (2,223 daily observations per pair).
Splits: BTC 2024-01-11, ETH 2024-07-23. NPDC(τ) < 0: the asset is a net receiver of VIX shocks.
Moving-block bootstrap (block 20, B = 500) within each subsample, with U(−0.005, 0.005) noise
on the VIX; a joint-by-date bootstrap for the counterfactual. Control baskets: A = {ADA, XLM};
B = A + {XRP, LTC, TRX}, each truncated on the eve of its own US spot ETF.

## Repository layout

```
run_all.R                  runs the full analysis
code/functions/            function libraries (QVAR/NPDC, bootstraps, parallel executor)
code/analysis/01–20_*.R    estimations, tables (CSV) and figures
code/collection/           data collection and construction (documentation; not run by run_all.R)
data/raw/                  CoinMarketCap exports and Binance daily candles used to build the base
data/clean/                analysis-ready series read by code/analysis
results/                   output of run_all.R (csv, fig, rds, log.txt)
```

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

Files are in `results/csv/` and `results/fig/`. The `V7-` prefix of the output files
identifies the final version of the pipeline.

| Item | Script (`code/analysis/`) | Output |
|---|---|---|
| Table 1 | 03, 11 | `V7-5.4_stats_MBB.csv`; BCa: `V7-6.8a_BC_BCa_full.csv` |
| Fig. 1 | 17 | `V7-Fig1_npdc_by_quantile.png` (values: `V7-1_npdc_net_tci_by_quantile.csv`) |
| Fig. 2 | 17 | `V7-Fig2_qq_surfaces.png` |
| Fig. 3 | 11 | `V7-F8_influence_blocks.png` |
| Fig. 4 | 09, 20 | `V7-F3_counterfactual_median_and_upper_tail.png` |
| Fig. 5 / Fig. S2 | 06, 20 | `V7-F5.11_flow_differential_state_tminus1.png` |
| Fig. S1 | 05, 20 | `V7-F5.8_placebo_DD095_histogram.png` |
| S1 Control assets, ETF listings, coverage | 04, 12 | `V7-6.3c_listing_dates.csv`, `V7-5.7_counterfactual_coverage.csv` |
| S2 Date alignment and weekend returns | 08 | `V7-6.0a_*.csv`, `V7-6.0b_*.csv` |
| S3 Descriptive statistics | 01 | `V7-1_descriptives.csv` |
| S4 Unit-root and stationarity tests | 01, 15 | `V7-6.7a_unit_root_full.csv`, `V7-1_adf.csv` |
| S5 Lag-order selection | 02 | `V7-1_lag_selection_*.csv` |
| S6 Bootstrap validity, VIX perturbation | 03 | `V7-5.1_bootstrap_validity.csv`, `V7-5.3_noise_*.csv` |
| S7 Decision rule | 19 | `V7-S13_decision_rule.csv`, `V7-S13_criterion*_detail.csv` |
| S8 NPDC by quantile (incl. finer grid) | 02, 17 | `V7-1_npdc_net_tci_by_quantile.csv`, `V7-S18_alt_quantile_grid.csv` |
| S9 Quantile-on-quantile surfaces | 17 | `V7-S5_qq_surfaces*.csv` |
| S10 Rolling-window estimates | 02 | `V7-1_robustness_rolling_window.csv` |
| S11 Stationary bootstrap | 03 | `V7-5.5_stats_stationary.csv` |
| S12 Influence of post-ETF blocks | 11 | `V7-6.8b_influence_*.csv` |
| S13 Excluding August 2024 | 02, 09 | `V7-6.1b_exclusion_all.csv` |
| S14 Percentile, BC and BCa intervals | 11 | `V7-6.8a_BC_BCa_full.csv` |
| S15 Counterfactual at 0.25–0.95 and power | 09, 10 | `V7-6.1a_counterfactual_all.csv`, `V7-6.2_power_equivalence_full.csv` |
| S16 Counterfactual in the tails, each control | 04, 18 | `V7-5.6_counterfactual_CI.csv`, `V7-S8_counterfactual_*.csv` |
| S17 Controls orthogonalized to Bitcoin | 12 | `V7-6.3_counterfactual_orth_all.csv`, `V7-6.3a_orthogonalization_betas.csv` |
| S18 Baskets with Solana | 12 | `V7-6.3_counterfactual_orth_all.csv` (B′ rows) |
| S19 Upper-tail placebo | 05 | `V7-5.8_placebo_*.csv`, `V7-5.9_placebo_all_dates.csv` |
| S20 Placebo at the median and τ = 0.75 | 09 | `V7-6.1c_placebo_*.csv` |
| S21 Structural breaks | 02 | `V7-1_breaks_TVP_TCI.csv` |
| S22 Flow channel, state at t − 1 | 06 | `V7-5.11_flow_LP_by_horizon.csv`, `V7-5.12_flow_joint_tests.csv` |
| S23 Flow channel, state at t, quantile regression | 02, 08 | `V7-1_flow_LP_state_t.csv`, `V7-1_flow_RQ_state_t.csv`, `V7-6.0d_S15_flow_RQ_state_t_vs_tminus1.csv` |
| S24 Price source and closing time | 16 | `V7-6.4a_*.csv`, `V7-6.4b_*.csv` |
| S25 VIX in log differences | 15 | `V7-6.7b_*.csv` |
| S26 Lag order and forecast horizon | 07 | `V7-5.13_upper_tail_by_p_H.csv`, `V7-5.14_*.csv` |
| S27 ETH pre-sample ending 10 Jan 2024 | 13 | `V7-6.5_*.csv` |
| S28 Pre-sample from 17 Mar 2023 | 14 | `V7-6.6_*.csv` |
| S29 Access-vehicle regimes (BTC) | 02 | `V7-1_robustness_vehicle_BTC.csv` |
| S30 Alternative uncertainty measures | 02 | `V7-1_robustness_alt_constructs.csv` |
