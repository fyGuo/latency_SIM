# Simulation Study: Latency-Risk Curve Estimation
## Scenario: Person-Time Data, ARIMA(1,1,2) Exposure, Decayed Weight Function

---

## Overview

This simulation study evaluates methods for estimating the **latency-risk curve** — the weight function α(l) that describes how past exposures at different time lags l contribute to current disease risk. The outcome is modeled as a Cox proportional hazards model on person-time (counting process) data.

**Folder:** `simulation_Persontime_ARIMA_112_decayed`

**Scenario characteristics:**
- Exposure process: ARIMA(1,1,2) with AR coefficient 0.67, MA coefficient 0.18
- True weight function: Exponential decay α(l) = A · exp(−B · l), starting high and decaying to near 0 after lag 10
- True parameters: A = 0.3 (amplitude), B ∈ {0.4, 0.2, 0.1} (decay rate — shape condition)
- Shape parameter: B (replaces old `knot`); B=0.4 fast decay, B=0.2 medium, B=0.1 slow
- Latency window: lags 0–15 (16 terms)
- Sample size: n = 5000 per simulation
- Replications: 500 per condition

---

## File Details

### 1. `simulate_personetime.R` — Data Generation

**Purpose:** Defines functions to generate one simulated dataset.

**Functions:**

| Function | Description |
|----------|-------------|
| `exposure_function(age, ...)` | Generates ARIMA(1,1,2) exposure trajectory over a person's life. Mean trend = (age − 70); random component follows ε_i = 0.67·ε_{i−1} + η_i + 0.18·η_{i−1} with η ~ N(0, 100). |
| `alpha_function(l, A, B)` | Computes the **true weight** at lag l: α(l) = A · exp(−B · l). A=0.3 (amplitude); B controls decay rate. |
| `generate_data(sample_size, ...)` | Generates a full person-time dataset. Entry age drawn from Uniform{1,…,50}. For each person: 30 years of past + 20 years of future exposure generated; lags 0–30 computed; WCE = Σ α(l)·lag_l for l=0:15; hazard λ = λ_0·exp(γ·WCE); failure times from Exp(λ); person followed until first event or end of follow-up. Returns a person-time data frame with columns: `id`, `age_start`, `age_end`, `failure`, `lag0`–`lag30`, `cumExposure`, `lambda`. |

**Key parameters passed through `generate_data`:**

| Parameter | Default | Meaning |
|-----------|---------|---------|
| `sample_size` | — | Number of individuals |
| `A` | 0.3 | Amplitude of true weight function α(l) = A·exp(−B·l) |
| `B` | 0.2 | Decay rate; shape conditions: 0.4 (fast), 0.2 (medium), 0.1 (slow) |
| `lambda_0` | 0.1 | Baseline hazard |
| `gamma` | 0.1 | Effect size (log-HR per unit WCE) |

---

### 2. `analysis_functions.R` — Standalone Analysis Utilities

**Purpose:** Contains lower-level fitting functions used outside the `LatencyA` package wrapper, kept for reference/debugging.

**Functions:**

| Function | Description |
|----------|-------------|
| `poly_fun(sim_data, K, latency_length)` | Fits the Michels polynomial model directly. Constructs cumulative sum and K polynomial moment terms; fits a Cox model. Returns coefficients. |
| `spline_function_GridSearch(sim_data, J, knots, latency_length)` | Fits an RCS grid-search model directly. For each number of knots j ∈ J, enumerates candidate knot positions (based on decile-defined grid), fits Cox model, selects minimum-AIC solution. Returns best knots and coefficients. |

**Note:** These are standalone versions. The main simulations use the `LatencyA` package equivalents (`CoxPoly`, `CoxKnotsearch`, etc.).

---

### 3. `simulation_output.R` — Master Simulation Function

**Purpose:** Defines `simulation_output()`, which runs 500 parallel replicates for a given method and parameter setting, and returns a data frame of estimated vs. true log-HR per lag per replicate.

**Depends on:** `simulate_personetime.R`, `LatencyA`, `furrr`/`future`

**Function signature:**
```r
simulation_output(sample_size, knot, degree, method, ...)
```

**Supported methods and the `LatencyA` functions they call:**

| `method` | Fitting function | Extraction function | Notes |
|----------|-----------------|--------------------|----|
| `"Polynomial"` | `CoxPoly(..., degree = degree-1)` | `extract_CoxPoly(fit, lag)` | `degree` = 3, 4, or 5 |
| `"NCSpline"` | `CoxNCSpline(..., knots_number = degree)` | `extract_CoxNCSpline(fit, lag, latency)` | `degree` = 3, 4, or 5 (df) |
| `"GridSearchKnot"` | `CoxKnotsearch(..., knots_number = c(3,4,5))` | `extract_CoxKnotsearch(fit, lag, latency)` | No `degree` parameter |
| `"TermSearch"` | `CoxTermsearch(..., knots = term_knots)` | `extract_CoxTermsearch(fit, lag, latency, knots)` | `term_knots = 0:(latency-1)` |

**Output columns per replicate × lag:**

| Column | Description |
|--------|-------------|
| `sim_id` | Replicate index (1–500) |
| `l` | Lag index (0–15) |
| `log_HR_true` | True log-HR at lag l = γ · α(l) |
| `log_HR_estimate` | Model-estimated log-HR at lag l |

**Parallelism:** Uses `plan("multicore")` + `future_map_dfr` with `furrr_options(seed = TRUE)`.

---

### 4. `simulation_CoxNCSpline.R` — Run NCSpline Simulations

**Purpose:** Loops over all (knot, degree) combinations for the NCSpline method and saves results.

**Grid:**
- `B` ∈ {0.4, 0.2, 0.1}
- `degree` ∈ {3, 4, 5}
- Total: 9 conditions × 500 replicates = 4,500 simulation runs

**Output:** `simulation_NCSpline.rds` — data frame with columns `sim_id`, `l`, `log_HR_true`, `log_HR_estimate`, `B`, `degree`, `method = "NCSpline"`.

---

### 5. `simulation_PolyCox.R` — Run Polynomial Simulations

**Purpose:** Loops over all (knot, degree) combinations for the Polynomial method and saves results.

**Grid:**
- `B` ∈ {0.4, 0.2, 0.1}
- `degree` ∈ {3, 4, 5}
- Total: 9 conditions × 500 replicates = 4,500 simulation runs

**Output:** `simulation_Polynomial.rds` — same structure as NCSpline output with `method = "Polynomial"`.

---

### 6. `simulation_GridSearchKnot.R` — Run Grid-Search Knot Simulations

**Purpose:** Loops over knot values for the GridSearchKnot method (no degree parameter) and saves results.

**Grid:**
- `B` ∈ {0.4, 0.2, 0.1}
- Total: 3 conditions × 500 replicates = 1,500 simulation runs

**Output:** `simulation_KnotGridSearch.rds` — same structure with `method = "GridSearchKnot"`.

---

### 7. `simulation_TermSearch.R` — Run Term-Search Simulations

**Purpose:** Loops over knot values for the TermSearch method and saves results.

**Grid:**
- `B` ∈ {0.4, 0.2, 0.1}
- Total: 3 conditions × 500 replicates = 1,500 simulation runs

**Output:** `simulation_TermSearch.rds` — same structure with `method = "TermSearch"`.

---

### 8. `make_figure2.R` — Produce Figure 2

**Purpose:** Reads all four `.rds` result files, computes summary statistics across replicates, and produces Figure 2 for the paper.

**Steps:**
1. Load each `.rds` file; filter NCSpline and Polynomial to `degree == 5` only.
2. Summarise per (method, knot, l): mean of true log-HR, mean and SD of estimated log-HR.
3. Append a "True latency-risk curve" series (estimated = true, SE = 0).
4. Save combined summary to `data_figure2_02.rds`.
5. Build two ggplot panels:
   - **Panel A (`whole_plot`):** All lags (0–15), faceted by ς ∈ {5, 10, 15}, no legend.
   - **Panel B (`tail_plot`):** Zoomed to lags 10–15, y ∈ [−0.002, 0.005], with legend.
6. Combine panels with `ggarrange`, save as `figure2.png` (23 × 18.4 cm, 300 dpi).

**Methods shown in Figure 2:**

| Series | Style |
|--------|-------|
| True latency-risk curve | Black dotted |
| Knot-search RCspline | Blue solid |
| Term-search RCspline | Orange dashed |
| Polynomial (df=5) | Red twodash |

**Note:** NCSpline with df=5 is loaded but not included in `temp` passed to `ggplot` (it is excluded from `rbind`). Can be added back if needed.

---

### 9. `run_sim.sh` — Shell Runner

**Purpose:** Runs simulation scripts via `R CMD BATCH` on a cluster or local machine.

**Current state:** Only `simulation_TermSearch.R` is active; the other three scripts are commented out.

```bash
# To run all simulations, uncomment:
R CMD BATCH --quiet --no-restore --no-save simulation_CoxNCSpline.R simulation_CoxNCSpline.out
R CMD BATCH --quiet --no-restore --no-save simulation_PolyCox.R simulation_PolyCox.out
R CMD BATCH --quiet --no-restore --no-save simulation_GridSearchKnot.R simulation_GridSearchKnot.out
R CMD BATCH --quiet --no-restore --no-save simulation_TermSearch.R simulation_TermSearch.out
```

**Note:** The `setwd` path in each R script and in this shell script points to a local machine path and must be updated before running.

---

### 10. `data_figure2_02.rds` — Pre-computed Figure 2 Data

**Purpose:** Cached output of `make_figure2.R` containing the summarised data for all methods. Can be loaded directly to regenerate the figure without re-running simulations.

---

## Execution Order

```
1. simulate_personetime.R        (sourced automatically — no direct execution)
2. simulation_output.R           (sourced automatically — no direct execution)
3. simulation_CoxNCSpline.R  }
   simulation_PolyCox.R       }  Run independently (parallel OK) via run_sim.sh
   simulation_GridSearchKnot.R}  Each produces one .rds file
   simulation_TermSearch.R    }
4. make_figure2.R                (reads the four .rds files, outputs figure2.png)
```

---

## Known Issues / To-Do

- [ ] Update `setwd()` paths in all scripts from hardcoded local paths to relative or parameterised paths.
- [ ] Update `run_sim.sh` working directory and uncomment all four simulation lines for a full run.
- [ ] `make_figure2.R` silently drops `result_NCS` (NCSpline) from the figure — decide whether to include it.
- [ ] `simulation_output.R` does not return `log_HR_var_estimate` for the `GridSearchKnot` and `TermSearch` methods — confidence interval construction is asymmetric across methods.
- [ ] Verify `entry_age <- sample(50, ...)` is intentional (gives values 1–50, not 50–70 as commented).
- [ ] Existing `.rds` result files (`simulation_NCSpline.rds`, etc.) were generated with the old B-spline weight function and must be regenerated with the new exponential decay α(l) = A·exp(−B·l) before running `make_figure2.R`.
