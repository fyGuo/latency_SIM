# Normality assessment of the WCE log-HR estimators (Knot-search vs Term-Search)
# across four data-generating scenarios, at three selected lags (l = 2, 8, 14).
#
# For folders with 500 simulated samples we draw 300 at random; folders that
# already have 300 are kept in full.  For each lag, within every
# (dataset x method x group) cell the estimates are standardised
# (z = (est - mean)/sd) so they are comparable, then pooled per
# dataset x method and shown as a histogram with an overlaid N(0,1) density.
# One sub-figure is produced per lag.

library(tidyverse)
library(ggpubr)

set.seed(20240608)

base <- "/Users/fuyuguo/Molin/Molin_spline/paper/SIM_submission/proj_spline/github"
setwd(base)

target_lags    <- c(2, 8, 14)
# the Constant folder mixes a true-null (positive = FALSE) and a true-nonzero
# (positive = TRUE) scenario; keep them as separate rows.
dataset_levels <- c("Unimodal", "Bimodal",
                    "null effect", "constant positive effect", "Decayed")

datasets <- tribble(
  ~dir,                                         ~dataset,
  "simulation_Persontime_ARIMA_112",            "Unimodal",
  "simulation_Persontime_ARIMA_112_bimodal",    "Bimodal",
  "simulation_Persontime_ARIMA_112_constant",   "Constant",
  "simulation_Persontime_ARIMA_112_decayed",    "Decayed"
)

method_levels <- c("Knot-search", "Term-Search", "polynomial df = 5")
methods <- tribble(
  ~file,                            ~method,
  "simulation_KnotGridSearch.rds",  "Knot-search",
  "simulation_TermSearch.rds",      "Term-Search",
  "simulation_Polynomial.rds",      "polynomial df = 5"
)

# Per file: read, identify the grouping column(s), optionally down-sample to 300
# simulations, and standardise the estimates within (group cols) x lag.
load_one <- function(dir, dataset_label, file, method_label) {
  res <- readRDS(file.path(dir, file))

  # polynomial files may carry several degrees; keep df = 5 only
  if ("degree" %in% names(res)) res <- filter(res, degree == 5)

  # grouping columns vary by folder/method (knot / B / positive / degree);
  # standardise within every distinct configuration so they are comparable.
  grp_cols <- intersect(c("knot", "B", "positive", "degree"), names(res))

  # draw 300 simulations (keep all if there are 300 or fewer)
  ids <- unique(res$sim_id)
  if (length(ids) > 300) ids <- sample(ids, 300)
  res <- filter(res, sim_id %in% ids)

  # split the Constant scenario into its true-null vs true-nonzero rows
  if ("positive" %in% names(res)) {
    dataset_vec <- ifelse(res$positive, "constant positive effect", "null effect")
  } else {
    dataset_vec <- dataset_label
  }

  # use the *labels* passed in (the file also has a `method` column of its own)
  res %>%
    mutate(dataset = dataset_vec, method = method_label) %>%
    group_by(across(all_of(c("dataset", "method", "l", grp_cols)))) %>%
    mutate(z = (log_HR_estimate - mean(log_HR_estimate, na.rm = TRUE)) /
               sd(log_HR_estimate, na.rm = TRUE)) %>%
    ungroup() %>%
    select(dataset, method, l, z)
}

dat <- datasets %>%
  cross_join(methods) %>%
  pmap_dfr(function(dir, dataset, file, method)
    load_one(dir, dataset, file, method)) %>%   # named args -> load_one params
  filter(is.finite(z)) %>%                      # drop cells with sd = 0 / NA
  mutate(dataset = factor(dataset, levels = dataset_levels),
         method  = factor(method,  levels = method_levels))

# ── One sub-panel per lag: histogram of standardised estimates + N(0,1) ───────
# (z was standardised within dataset x method x group x lag, so simply
#  selecting a lag gives the per-lag standardised estimates.)
make_lag_panel <- function(lag_value) {
  ggplot(filter(dat, l == lag_value), aes(x = z)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 40, fill = "grey70", colour = "white", linewidth = 0.2) +
    geom_density(colour = "firebrick", linewidth = 0.7) +     # KDE of the method
    stat_function(fun = dnorm, args = list(mean = 0, sd = 1), # N(0,1) reference
                  colour = "black", linewidth = 0.7, linetype = "dotted") +
    facet_grid(dataset ~ method) +
    coord_cartesian(xlim = c(-4, 4)) +
    labs(x = "Standardised log-HR estimate", y = "Density",
         title = bquote(lag ~ l == .(lag_value))) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "grey95"),
          plot.title = element_text(hjust = 0.5))
}

# ── Companion QQ-plot sub-panel per lag (normal Q-Q of the standardised z) ────
make_lag_qq <- function(lag_value) {
  ggplot(filter(dat, l == lag_value), aes(sample = z)) +
    stat_qq(size = 0.3, alpha = 0.25, colour = "firebrick") +
    stat_qq_line(colour = "black", linewidth = 0.6, linetype = "dotted") +
    facet_grid(dataset ~ method) +
    coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) +
    labs(x = "Theoretical N(0,1) quantiles", y = "Sample quantiles",
         title = bquote(lag ~ l == .(lag_value))) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "grey95"),
          plot.title = element_text(hjust = 0.5))
}

hist_panels <- map(target_lags, make_lag_panel)
qq_panels   <- map(target_lags, make_lag_qq)

# ── Two separate figures: histograms and matching Q-Q plots ───────────────────
panel_hist <- ggarrange(plotlist = hist_panels, ncol = length(hist_panels), nrow = 1,
                        labels = LETTERS[seq_along(hist_panels)])
panel_hist <- annotate_figure(
  panel_hist,
  top = text_grob(
    "Normality of WCE log-HR estimators: histogram + red KDE vs black-dotted N(0,1) (standardised within dataset x method x group)",
    size = 11))
ggsave("figure_normality_hist.png", plot = panel_hist,
       units = "cm", width = 60, height = 26, dpi = 300)
cat("Saved figure_normality_hist.png\n")

panel_qq <- ggarrange(plotlist = qq_panels, ncol = length(qq_panels), nrow = 1,
                      labels = LETTERS[seq_along(qq_panels)])
panel_qq <- annotate_figure(
  panel_qq,
  top = text_grob(
    "Normality of WCE log-HR estimators: normal Q-Q plots (black-dotted = ideal; standardised within dataset x method x group)",
    size = 11))
ggsave("figure_normality_qq.png", plot = panel_qq,
       units = "cm", width = 60, height = 26, dpi = 300)
cat("Saved figure_normality_qq.png\n")

print(count(filter(dat, l %in% target_lags), dataset, method, l))
