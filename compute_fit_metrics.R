# Goodness-of-fit metrics for the WCE log-HR estimators.
#
# Scenario selection, labels, method names and the 300-replicate subsample are
# aligned with make_combined_figure.R, so each row of the main table corresponds
# to one panel of figure_combined.png.
#
# For one fitted curve, with observed (true) values y_l = log_HR_true and
# fitted values yhat_l = log_HR_estimate over the lags l = 0..15,
# the residual is e_l = y_l - yhat_l and
#
#   RMSE = sqrt(mean(e_l^2))     MAE = mean(|e_l|)
#
# These are computed per simulated curve (per sim_id), then averaged over the
# 300 simulations.  Two tables are written: the six combined-figure scenarios,
# and the Term-Search tuning study.

library(tidyverse)

# 300-id subsample drawn exactly as in make_combined_figure.R
set.seed(42)
ids_300 <- sort(sample(1:500, 300))

base <- "/Users/fuyuguo/Molin/Molin_spline/paper/SIM_submission/proj_spline/github"
setwd(base)

# ── scenarios (one configuration each, mirroring make_combined_figure.R) ──────
PANEL_ORDER <- c("Unimodal", "Damped exponential", "Bimodal 1",
                 "Bimodal 2", "Null effects", "Constant positive effects")

scenarios <- tribble(
  ~dir,                                         ~scenario_col, ~keep_val, ~label,                       ~subsample,
  "simulation_Persontime_ARIMA_112",            "knot",        "10",      "Unimodal",                   TRUE,
  "simulation_Persontime_ARIMA_112_decayed",    "B",           "0.4",     "Damped exponential",         TRUE,
  "simulation_Persontime_ARIMA_112_bimodal",    "B",           "-1",      "Bimodal 1",                  FALSE,
  "simulation_Persontime_ARIMA_112_bimodal",    "B",           "1",       "Bimodal 2",                  FALSE,
  "simulation_Persontime_ARIMA_112_constant",   "positive",    "FALSE",   "Null effects",               FALSE,
  "simulation_Persontime_ARIMA_112_constant",   "positive",    "TRUE",    "Constant positive effects",  FALSE
)

METHOD_LEVELS <- c("Knot-search RCspline", "Term-search RCspline", "Polynomial (df=5)")
methods <- tribble(
  ~file,                            ~method,
  "simulation_KnotGridSearch.rds",  "Knot-search RCspline",
  "simulation_TermSearch.rds",      "Term-search RCspline",
  "simulation_Polynomial.rds",      "Polynomial (df=5)"
)

# Read one method file, keep df = 5 (polynomial), apply the 300-id subsample for
# the 500-replicate folders, and select the single requested configuration.
load_one <- function(dir, file, method_label, scenario_col, keep_val, subsample) {
  res <- readRDS(file.path(dir, file))
  if ("degree" %in% names(res))             res <- filter(res, degree == 5)
  if (subsample)                            res <- filter(res, sim_id %in% ids_300)
  res <- res[as.character(res[[scenario_col]]) == keep_val, , drop = FALSE]
  transmute(res, method = method_label, sim_id, l, log_HR_true, log_HR_estimate)
}

raw <- scenarios %>%
  cross_join(methods) %>%
  pmap_dfr(function(dir, scenario_col, keep_val, label, subsample, file, method)
    load_one(dir, file, method, scenario_col, keep_val, subsample) %>%
      mutate(scenario = label))

# Per-curve RMSE / MAE, then averaged over simulations.
summarise_metrics <- function(d, scenario_levels = NULL) {
  out <- d %>%
    mutate(e = log_HR_true - log_HR_estimate) %>%
    group_by(scenario, method, sim_id) %>%
    summarise(MSE = mean(e^2, na.rm = TRUE),
              MAE = mean(abs(e), na.rm = TRUE), .groups = "drop") %>%
    group_by(scenario, method) %>%
    summarise(n_sims = n(),
              RMSE   = mean(sqrt(MSE)),
              MAE    = mean(MAE), .groups = "drop")
  if (!is.null(scenario_levels))
    out <- out %>% mutate(scenario = factor(scenario, levels = scenario_levels))
  out %>%
    mutate(method = factor(method, levels = METHOD_LEVELS)) %>%
    arrange(scenario, method)
}

main_tbl <- summarise_metrics(raw, PANEL_ORDER) %>%
  mutate(across(c(RMSE, MAE), ~ signif(.x, 3)))

write_csv(main_tbl, "fit_metrics_main.csv")
cat("== Goodness-of-fit (combined-figure scenarios) ==\n")
print(as.data.frame(main_tbl), row.names = FALSE)

# ── Term-Search tuning study ──────────────────────────────────────────────────
tune_dir <- file.path(base, "simulation_Persontime_ARIMA_112_unimodal_term_search_tune")
tune_raw <- readRDS(file.path(tune_dir, "simulation_TermSearch.rds")) %>%
  transmute(scenario = term_knots_label, method = "Term-search RCspline",
            sim_id, l, log_HR_true, log_HR_estimate)

tune_tbl <- tune_raw %>%
  mutate(e = log_HR_true - log_HR_estimate) %>%
  group_by(scenario, sim_id) %>%
  summarise(MSE = mean(e^2), MAE = mean(abs(e)), .groups = "drop") %>%
  group_by(scenario) %>%
  summarise(n_sims = n(), RMSE = mean(sqrt(MSE)), MAE = mean(MAE), .groups = "drop") %>%
  mutate(across(c(RMSE, MAE), ~ signif(.x, 3)))

write_csv(tune_tbl, "fit_metrics_termsearch_tune.csv")
cat("\n== Goodness-of-fit (Term-Search tuning) ==\n")
print(as.data.frame(tune_tbl), row.names = FALSE)

cat("\nSaved fit_metrics_main.csv and fit_metrics_termsearch_tune.csv\n")

# ══════════════════════════════════════════════════════════════════════════════
# Publication-ready LaTeX table (booktabs).  One row per combined-figure panel;
# RMSE / MAE are reported on a 10^{-3} scale, smallest value in each row in bold.
# ══════════════════════════════════════════════════════════════════════════════
SCALE <- 1000

w2 <- main_tbl %>%
  mutate(RMSE = RMSE * SCALE, MAE = MAE * SCALE) %>%
  pivot_wider(id_cols = scenario, names_from = method,
              values_from = c(RMSE, MAE)) %>%
  arrange(scenario)

fmt_metric <- function(vals) {                 # bold the row minimum
  out <- sprintf("%.2f", vals)
  out[which.min(vals)] <- paste0("\\textbf{", out[which.min(vals)], "}")
  out
}

body <- character(0)
for (i in seq_len(nrow(w2))) {
  row  <- w2[i, ]
  rmse <- fmt_metric(vapply(METHOD_LEVELS, function(m) row[[paste0("RMSE_", m)]], numeric(1)))
  mae  <- fmt_metric(vapply(METHOD_LEVELS, function(m) row[[paste0("MAE_",  m)]], numeric(1)))
  cells <- as.vector(rbind(rmse, mae))         # RMSE,MAE interleaved per method
  body <- c(body, sprintf("%s & %s\\\\",
                          as.character(row$scenario), paste(cells, collapse = " & ")))
}

tune_body <- tune_tbl %>%
  mutate(RMSE = sprintf("%.2f", RMSE * SCALE),
         MAE  = sprintf("%.2f", MAE * SCALE)) %>%
  transmute(line = sprintf("%s & %s & %s\\\\", scenario, RMSE, MAE)) %>%
  pull(line)

tex <- c(
  "\\documentclass{article}",
  "\\usepackage{booktabs}",
  "\\usepackage[margin=1in]{geometry}",
  "\\begin{document}",
  "",
  "\\begin{table}[!ht]",
  "\\centering",
  "\\caption{Goodness of fit of the weighted-cumulative-exposure log-hazard-ratio",
  "  estimators across the six simulation scenarios of Figure~\\ref{fig:combined}.",
  "  Entries are the root-mean-square error (RMSE) and mean absolute error (MAE)",
  "  of $\\hat f(l)$ relative to the true curve over lags $l=0,\\dots,15$, averaged",
  "  over 300 simulations; the smallest value in each row is in bold. All entries",
  "  are multiplied by $10^{3}$.}",
  "\\label{tab:fit-metrics}",
  "\\begin{tabular}{l cc cc cc}",
  "\\toprule",
  sprintf("& \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s}\\\\",
          METHOD_LEVELS[1], METHOD_LEVELS[2], METHOD_LEVELS[3]),
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
  "Scenario & RMSE & MAE & RMSE & MAE & RMSE & MAE\\\\",
  "\\midrule",
  body,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}",
  "",
  "\\begin{table}[!ht]",
  "\\centering",
  "\\caption{Goodness of fit of the term-search RCspline estimator under different",
  "  candidate knot grids (unimodal scenario, 100 simulations). RMSE and MAE are",
  "  multiplied by $10^{3}$.}",
  "\\label{tab:fit-tune}",
  "\\begin{tabular}{l cc}",
  "\\toprule",
  "Candidate knot grid & RMSE & MAE\\\\",
  "\\midrule",
  tune_body,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}",
  "",
  "\\end{document}"
)

writeLines(tex, "fit_metrics_table.tex")
cat("Saved fit_metrics_table.tex\n")
