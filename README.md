# Simulation Study: Latency-Risk Curve Estimation for Weighted Cumulative Exposure

This repository contains the simulation code and figures for a study of methods that
estimate the **latency-risk curve** — the weight function α(*l*) describing how past
exposures at time lag *l* contribute to current disease risk — within a weighted
cumulative exposure (WCE) Cox proportional-hazards model fitted on person-time
(counting-process) data.

The study compares several estimators:

- **RCspline (df = k)** — restricted (natural) cubic spline of the latency window with a
  fixed number of degrees of freedom.
- **Knot-search RCspline** — restricted cubic spline whose interior knots are selected by
  a grid search.
- **Polynomial (Michels)** — the polynomial-basis approach as a comparator.

Estimators are evaluated across exposure/weight scenarios on accuracy (mean integrated
squared error), interval coverage, runtime, and robustness to misspecification and
missing data.

## Requirements

- **R ≥ 4.5** (the Docker image pins `rocker/r-ver:4.5.3`).
- The custom package **[`LatencyA`](https://github.com/fyGuo/LatencyA)**, which implements
  the estimation methods:

  ```r
  # install.packages("pak")
  pak::pak("fyGuo/LatencyA")
  ```

- CRAN packages used across the scripts:
  `tidyverse`, `survival`, `splines`, `Hmisc`, `future`, `furrr`, `parallelly`,
  `ggsci`, `ggpubr`, `patchwork`, `ggh4x`, `tictoc`, `lme4`, `mice`, `ranger`.

### Reproducible environment (Docker)

`Dockerfile` builds a self-contained image with R, all system libraries, the CRAN
packages, and `LatencyA` preinstalled. `Dockerfile.collab` is a lighter variant for
collaborative use.

```bash
docker build -t latency-sim .
```

## Repository layout

### Simulation scenarios

Each `simulation_*` directory is one scenario and follows the same structure:

| File | Role |
|------|------|
| `simulate_personetime.R` | Data-generating functions (ARIMA(1,1,2) exposure, true weight α(*l*), person-time dataset). |
| `simulation_GridSearchKnot.R` | Runs the knot-search RCspline estimator over the replications. |
| `simulation_TermSearch.R` | Runs the term/df-search estimator. |
| `simulation_CoxNCSpline.R` | Runs the fixed-df natural cubic spline estimator. |
| `simulation_PolyCox.R` | Runs the polynomial (Michels) comparator. |
| `simulation_output.R` / `analysis_functions.R` | Summarises fitted results. |
| `make_figure2.R` | Per-scenario plotting. |
| `run_sim.sh` | Driver script that runs the simulation R scripts in batch. |

The main scenario is **`simulation_Persontime_ARIMA_112_decayed`** (exponentially
decaying weight function). The other folders vary one aspect of the design:

| Folder | Scenario |
|--------|----------|
| `simulation_Persontime_ARIMA_112` | Base ARIMA(1,1,2) scenario. |
| `simulation_Persontime_ARIMA_112_decayed` | Exponentially decayed weight (main). |
| `simulation_Persontime_ARIMA_112_bimodal` | Bimodal weight function. |
| `simulation_Persontime_ARIMA_112_constant` | Constant weight function. |
| `simulation_Persontime_ARIMA_112_coverage` | Bootstrap confidence-interval coverage. |
| `simulation_Persontime_ARIMA_112_varying_size` | Varying sample size *n*. |
| `simulation_Persontime_ARIMA_112_varying_size_knotsearch` | Varying *n*, knot-search estimator. |
| `simulation_Persontime_ARIMA_112_varying_intercept` | Varying baseline intercept. |
| `simulation_Persontime_ARIMA_112_timing_knotSearch` | Runtime / timing benchmark. |
| `simulation_Persontime_ARIMA_112_polynomial_tune` | Polynomial-degree tuning. |
| `simulation_Persontime_ARIMA_112_unimodal_term_search_tune` | Term-search tuning (AIC). |
| `simulation_Persontime_ARIMA_112_unimodal_term_search_tune_BIC` | Term-search tuning (BIC). |
| `simulation_Persontime_ARIMA_112_permutation_test` | Permutation test. |
| `simulation_Persontime_ARIMA_112_exposure_trajectory` | Exposure-trajectory illustration. |
| `simulation_PersonTime_Coarsened_ARIMA_112` | Coarsened (interval-summarised) exposure. |
| `simulation_PersonTime_Coarsened_2period_ARIMA_112` | Coarsened exposure, two periods. |
| `simulation_PersonTime_misspecified_Latency_ARIMA_112_decayed` | Misspecified latency window, decayed weight. |
| `simulation_PersonTime_misspecified_Latency_ARIMA_112_bimodal` | Misspecified latency window, bimodal weight. |
| `simulation_PersonTime_misspecified_Latency_age_ARIMA_112` | Misspecified latency window with age effect. |
| `simulation_missing_Persontime_mixedModel_nsamples_ARIMA_112` | Missing exposure data, mixed-model imputation. |
| `simulation_missing_Persontime_mixedModel_nsamples_2stage_ARIMA_112` | Missing data, two-stage imputation. |

### Top-level scripts and outputs

The `make_*.R` scripts read the per-scenario results and produce the paper figures and
metrics tables:

| Script | Output |
|--------|--------|
| `compute_fit_metrics.R` | `fit_metrics_*.csv`, `fit_metrics_table.tex/pdf` |
| `make_knotsearch_vs_ncs_figure.R` | `figure_knotsearch_vs_ncs.png` |
| `make_combined_figure.R` | `figure_combined.png`, `figure_2x2.png` |
| `make_misspecified_combined_figure.R` | `figure_misspecified_combined.png` |
| `make_misspecified_latency_figure.R` | `figure_misspecified_latency.png` |
| `make_normality_figure.R` | `figure_normality_hist.png`, `figure_normality_qq.png` |
| `make_termsearch_tune_figure.R` | `figure_termsearch_tune.png` |
| `make_termsearch_tune_normality.R` | `figure_termsearch_tune_normality.png` |
| `make_varying_size_figure.R` | `figure_varying_size.png` |
| `make_varying_intercept_figure.R` | `figure_varying_intercept.png` |
| `figure_missingness_combined.R` | `figure_missingness_combined.png` |

## Running the simulations

Simulation result objects (`*.rds`) are **not** stored in the repository — they are
regenerated by running the scenario scripts. To reproduce a scenario:

```bash
cd simulation_Persontime_ARIMA_112_decayed
bash run_sim.sh          # writes the *.rds result objects for this scenario
```

Then build the figures/tables from the repository root, e.g.:

```bash
Rscript compute_fit_metrics.R
Rscript make_knotsearch_vs_ncs_figure.R
```

The simulations use `future`/`furrr` for parallelism; each scenario runs 500
replications and can take substantial compute time, which is why the raw result objects
are excluded rather than checked in. The pre-rendered figures (`figure_*.png`) and metric
tables (`fit_metrics_*.csv`, `.tex`, `.pdf`) are included for reference.

## Notes

- Bootstrap coverage results were produced after a fix to the `LatencyA` bootstrap
  routine (2026-06-12); earlier coverage numbers used a buggy resample and should not be
  used.
