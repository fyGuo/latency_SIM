# ── Self-locating working directory ───────────────────────────────────────────
local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (!length(f)) { i <- which(a == "-f"); if (length(i)) f <- a[i + 1L] }
  if (!length(f) && requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable())
    f <- rstudioapi::getSourceEditorContext()$path
  if (length(f) && nzchar(f)) setwd(dirname(normalizePath(f)))
})

library(tidyverse)

# Combine all coverage_<method>_<scenario>_part<k>.rds chunks and summarise, for
# each (method, scenario, lag):
#   coverage   : fraction of per-replicate bootstrap intervals covering the truth
#   true 95% percentile by simulation        : 2.5 / 97.5 percentiles of the
#       per-replicate POINT estimates (the empirical/Monte-Carlo sampling interval)
#   average 95% percentile by estimation     : mean lower / upper bound of the
#       per-replicate bootstrap percentile intervals
# Legacy single-lag files (without the scenario / log_HR_estimate columns) are
# skipped automatically.
fs  <- Sys.glob("coverage_*_part*.rds")
req <- c("scenario", "method", "lag", "log_HR_true", "log_HR_estimate",
         "lo", "hi", "covered")
raw <- bind_rows(lapply(fs, function(f) {
  d <- readRDS(f)
  if (!all(req %in% names(d))) {
    message("skip (legacy format): ", f); return(NULL)
  }
  d
}))

if (nrow(raw) == 0)
  stop("No new-format coverage_*_part*.rds found - run coverage_simulation.R first.")

shape_levels <- c("unimodal", "bimodal_pos", "bimodal_neg",
                  "decayed", "constant_low", "constant_high")

coverage <- raw %>%
  mutate(scenario = factor(scenario, levels = shape_levels)) %>%
  group_by(method, scenario, lag) %>%
  summarise(
    n_sims        = sum(!is.na(covered)),
    log_HR_true   = mean(log_HR_true),
    coverage      = mean(covered, na.rm = TRUE),
    mc_se         = sqrt(coverage * (1 - coverage) / n_sims),         # Monte-Carlo SE
    med_est       = median(log_HR_estimate, na.rm = TRUE),            # median point estimate
    # true 95% interval by simulation (empirical sampling distribution of point est.)
    true_pct_lo   = quantile(log_HR_estimate, 0.025, na.rm = TRUE),
    true_pct_hi   = quantile(log_HR_estimate, 0.975, na.rm = TRUE),
    true_width    = true_pct_hi - true_pct_lo,
    # average 95% interval by estimation (mean bootstrap percentile bounds)
    est_pct_lo    = mean(lo, na.rm = TRUE),
    est_pct_hi    = mean(hi, na.rm = TRUE),
    est_width     = mean(hi - lo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(method = recode(method, Knotsearch = "Knot-search", Termsearch = "Term-search")) %>%
  arrange(method, scenario, lag)

write_csv(coverage, "coverage_summary.csv")

cat("== 95% bootstrap-percentile-interval coverage (target 0.95) ==\n")
print(as.data.frame(coverage %>%
        mutate(across(c(log_HR_true, coverage, mc_se,
                        true_pct_lo, true_pct_hi, true_width,
                        est_pct_lo, est_pct_hi, est_width),
                      ~ signif(.x, 4)))),
      row.names = FALSE)
cat("\nSaved coverage_summary.csv\n")

# ── Combined LaTeX table (knot-search + term-search), generated from `coverage` ─
# Scenario order and labels follow the manuscript panels.
shape_order  <- c("unimodal", "decayed", "bimodal_neg", "bimodal_pos",
                  "constant_low", "constant_high")
shape_label  <- c(unimodal      = "Unimodal",
                  decayed       = "Damped exponential",
                  bimodal_neg   = "Bimodal 1",
                  bimodal_pos   = "Bimodal 2",
                  constant_low  = "Constant null effects",
                  constant_high = "Constant positive effects")
method_order <- c("Knot-search", "Term-search")

# format to 4 decimals, displaying a rounded negative zero (-0.0000) as 0.0000
fnum <- function(x) sub("^-(0\\.0+)$", "\\1", sprintf("%.4f", x))
ci   <- function(lo, hi) sprintf("[%s, %s]", fnum(lo), fnum(hi))

rows <- character(0)
for (sc in shape_order) {
  rows <- c(rows, sprintf("\\multicolumn{7}{l}{\\emph{%s}}\\\\", shape_label[[sc]]))
  for (m in method_order) {
    ds <- coverage %>% filter(scenario == sc, method == m) %>% arrange(lag)
    for (i in seq_len(nrow(ds))) {
      r <- ds[i, ]
      mcell <- if (i == 1) m else ""    # method name once per 3-lag block
      rows <- c(rows, sprintf("%s & %d & %s & %s & %s & %s & %s \\\\",
                              mcell, r$lag, fnum(r$log_HR_true), fnum(r$med_est),
                              fnum(r$coverage),
                              ci(r$true_pct_lo, r$true_pct_hi),
                              ci(r$est_pct_lo, r$est_pct_hi)))
    }
  }
  rows <- c(rows, "\\addlinespace")
}

tex <- c(
  "\\documentclass{article}",
  "\\usepackage{booktabs}",
  "\\usepackage{amsmath}",
  "\\usepackage[margin=1in]{geometry}",
  "",
  "\\begin{document}",
  "",
  "\\begin{table}[ht]",
  "\\centering",
  paste0("\\caption{Bootstrap-percentile interval coverage of $\\log\\mathrm{HR}_p(l)$ ",
         "for the knot-search and term-search RCsplines, by true weighting shape and ",
         "lag $l$. For each of $300$ simulated data sets, the ",
         "$\\widehat{\\log\\mathrm{HR}}_p(l)$ is estimated and a $95\\%$ percentile ",
         "interval is obtained from $300$ cluster bootstrap resamples.}"),
  "\\label{tab:coverage}",
  "\\begin{tabular}{l c c c c c c}",
  "\\toprule",
  "Method & Lag $l$ & $\\log\\mathrm{HR}_p(l)$ & Median $\\widehat{\\log\\mathrm{HR}}_p(l)$ & Coverage & Empirical 95\\% interval & Mean bootstrap 95\\% interval \\\\",
  "\\midrule",
  head(rows, -1),   # drop trailing \addlinespace
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}",
  "",
  "\\end{document}")

writeLines(tex, "coverage_results.tex")
cat("Saved coverage_results.tex\n")
