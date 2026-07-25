# ── Summarise the permutation-test study and emit a figure + LaTeX table ──────
# Combines all permtest_<scenario>_part<k>.rds chunks (6 parts x 50 sims = 300
# replicates per scenario) and reports, per true weighting shape, the rejection
# rate of H0 ("weights constant across lags"). For the constant shapes this is the
# type-I error; for the lag-varying shapes it is the power.
local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (!length(f)) { i <- which(a == "-f"); if (length(i)) f <- a[i + 1L] }
  if (length(f) && nzchar(f)) setwd(dirname(normalizePath(f)))
})

library(tidyverse)
source("weight_curves.R")

# ── Aggregate ─────────────────────────────────────────────────────────────────
ord    <- c("unimodal", "bimodal_pos", "bimodal_neg", "decayed",
            "constant_low", "constant_high")
labels <- c(unimodal      = "Unimodal",
            bimodal_pos   = "Bimodal (both peaks +)",
            bimodal_neg   = "Bimodal (2nd peak −)",
            decayed       = "Exponential decay",
            constant_low  = "Constant, c = 0 (no effect)",
            constant_high = "Constant, c = 0.10")

parts <- list.files(".", pattern = "_part[0-9]+\\.rds$")
raw   <- bind_rows(lapply(parts, readRDS))

summ <- raw %>%
  group_by(scenario) %>%
  summarise(n_sims = sum(!is.na(reject)),
            n_rej  = sum(reject, na.rm = TRUE),
            mean_p = mean(p_value, na.rm = TRUE),
            med_events = median(n_events, na.rm = TRUE),
            .groups = "drop") %>%
  rowwise() %>%
  mutate(rate = n_rej / n_sims,
         ci_lo = binom.test(n_rej, n_sims)$conf.int[1],
         ci_hi = binom.test(n_rej, n_sims)$conf.int[2],
         is_null = WEIGHT_IS_NULL[[scenario]],
         type = if (is_null) "Null (type-I error)" else "Alternative (power)") %>%
  ungroup() %>%
  mutate(scenario = factor(scenario, levels = ord),
         label    = factor(labels[as.character(scenario)], levels = labels[ord])) %>%
  arrange(scenario)

write_csv(summ %>% select(scenario, type, n_sims, n_rej, rate, ci_lo, ci_hi,
                          mean_p, med_events),
          "permutation_test_summary.csv")

# ── Figure: rejection rate +/- 95% Clopper-Pearson CI, nominal level marked ────
fig <- ggplot(summ, aes(x = label, y = rate, colour = type)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", colour = "grey40") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.15, linewidth = 0.6) +
  geom_point(size = 2.6) +
  scale_y_continuous(limits = c(0, 1), breaks = sort(c(0.05, seq(0, 1, 0.2)))) +
  scale_colour_manual(values = c("Null (type-I error)" = "#3B6FB6",
                                 "Alternative (power)"  = "#C5402B"),
                      name = NULL) +
  coord_flip() +
  labs(x = NULL, y = "Rejection rate of H0 (weights constant across lags)",
       title = "Permutation test: type-I error and power across true weighting shapes",
       caption = "Dashed line: nominal level 0.05.") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 11, face = "bold"),
        plot.caption = element_text(hjust = 0))

ggsave("permutation_test_rejection.png", fig,
       width = 20, height = 11, units = "cm", dpi = 300)
message("Saved permutation_test_rejection.png")

# ── LaTeX table (numbers pulled straight from `summ` so it stays in sync) ──────
# Scenario order and labels follow the manuscript model-selection panels.
ord_tbl <- c("unimodal", "decayed", "bimodal_neg", "bimodal_pos",
             "constant_low", "constant_high")
lab_tbl  <- c(unimodal      = "Unimodal",
              decayed       = "Damped exponential",
              bimodal_neg   = "Bimodal 1",
              bimodal_pos   = "Bimodal 2",
              constant_low  = "Null effects",
              constant_high = "Constant positive effects")

srow <- summ[match(ord_tbl, as.character(summ$scenario)), ]
body <- sprintf("%s & %s & %d & %.3f \\\\",
                lab_tbl[ord_tbl], ifelse(srow$is_null, "True", "False"),
                srow$n_sims, srow$rate)

tex <- c(
"\\documentclass{article}",
"\\usepackage{booktabs}",
"\\usepackage{siunitx}",
"\\usepackage[margin=1in]{geometry}",
"\\sisetup{round-mode=places, round-precision=3, table-format=1.3}",
"",
"\\begin{document}",
"",
"\\begin{table}[ht]",
"\\centering",
"\\caption{Permutation test for constant weights across true weighting shapes.",
"The null hypothesis is that the latency weight is constant across lags",
"($\\alpha(0)=\\cdots=\\alpha(\\tau)$). Working model: knot-search restricted cubic",
"spline selected by AIC; for each replicate the observed AIC is compared with the",
"null distribution from $B=300$ permutations of the lag columns, and $H_0$ is",
"rejected when the observed AIC falls below the lower $\\alpha=0.05$ percentile of",
"that distribution. The rejection rate is taken over $300$ simulated data sets",
"($n=5000$, $\\gamma=0.1$, $\\lambda_0=0.5$, true latency $16$); for the constant",
"shapes it estimates the type-I error and for the lag-varying shapes the power.}",
"\\label{tab:permtest}",
"\\begin{tabular}{l l c S}",
"\\toprule",
"True weight shape & $H_0$ status & Replicates & {Rejection rate} \\\\",
"\\midrule",
body,
"\\bottomrule",
"\\end{tabular}",
"\\end{table}",
"",
"\\end{document}")

writeLines(tex, "permutation_test_results.tex")
message("Saved permutation_test_results.tex")

cat("\n== Permutation-test rejection rates ==\n")
print(as.data.frame(summ %>% mutate(across(c(rate, ci_lo, ci_hi, mean_p), ~ signif(.x, 4))) %>%
        select(scenario, type, n_sims, n_rej, rate, ci_lo, ci_hi, mean_p, med_events)),
      row.names = FALSE)
