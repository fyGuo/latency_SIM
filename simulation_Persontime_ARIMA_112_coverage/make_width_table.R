# ── Compact coverage table: method, scenario, lag, coverage, true_width, est_width
# Reads coverage_summary.csv (produced by make_coverage_table.R) and emits a
# compilable LaTeX table with just those six columns.
local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (!length(f)) { i <- which(a == "-f"); if (length(i)) f <- a[i + 1L] }
  if (length(f) && nzchar(f)) setwd(dirname(normalizePath(f)))
})

library(tidyverse)

shape_levels <- c("unimodal", "bimodal_pos", "bimodal_neg",
                  "decayed", "constant_low", "constant_high")
shape_label  <- c(unimodal      = "Unimodal",
                  bimodal_pos   = "Bimodal (both peaks $+$)",
                  bimodal_neg   = "Bimodal (2nd peak $-$)",
                  decayed       = "Exponential decay",
                  constant_low  = "Constant, $c=0$",
                  constant_high = "Constant, $c=0.10$")

d <- read_csv("coverage_summary.csv", show_col_types = FALSE) %>%
  mutate(scenario = factor(scenario, levels = shape_levels),
         method   = recode(method, Knotsearch = "Knot-search", Termsearch = "Term-search"),
         method   = factor(method, levels = c("Knot-search", "Term-search"))) %>%
  arrange(method, scenario, lag) %>%
  mutate(method = as.character(method)) %>%
  select(method, scenario, lag, med_est, coverage, true_width, est_width)

methods <- d$method
scen    <- as.character(d$scenario)
# format to 4 decimals, displaying a rounded negative zero (-0.0000) as 0.0000
f4 <- function(x) sub("^-(0\\.0+)$", "\\1", sprintf("%.4f", x))
body <- character(0)
for (i in seq_len(nrow(d))) {
  if (i > 1 && methods[i] != methods[i - 1]) body <- c(body, "\\midrule")
  body <- c(body, sprintf("%s & %s & %d & %s & %.3f & %s & %s \\\\",
                          methods[i], shape_label[[scen[i]]], d$lag[i],
                          f4(d$med_est[i]), d$coverage[i],
                          f4(d$true_width[i]), f4(d$est_width[i])))
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
  paste0("\\caption{Median estimated $\\log\\mathrm{HR}_p(l)$ over $300$ replicates, ",
         "bootstrap-percentile interval coverage (target $0.95$), the width of the ",
         "empirical sampling interval (\\emph{true width}, the $2.5/97.5$ percentile ",
         "range of the point estimates over the replicates) and the average width of ",
         "the per-replicate bootstrap percentile interval (\\emph{est.\\ width}), by ",
         "estimator, true weighting shape, and lag $l$.}"),
  "\\label{tab:coverage-widths}",
  "\\begin{tabular}{l l c c c c c}",
  "\\toprule",
  "Method & Scenario & Lag $l$ & Median $\\widehat{\\log\\mathrm{HR}}_p(l)$ & Coverage & True width & Est.\\ width \\\\",
  "\\midrule",
  body,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}",
  "",
  "\\end{document}")

writeLines(tex, "coverage_width_table.tex")
cat("Saved coverage_width_table.tex\n")
