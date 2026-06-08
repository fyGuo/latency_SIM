# Normality of the Term-Search log-HR estimator under different tunings
# (candidate knot sets), unimodal ARIMA(1,1,2) scenario.
#
# Folder: simulation_Persontime_ARIMA_112_unimodal_term_search_tune
#   simulation_TermSearch.rds : 100 simulations, lags 0-15, knot = 10,
#   term_knots_label = the candidate knot grid handed to the term search:
#     "0 to 15"           - a knot at every lag (densest)
#     "0, 2, 4, ..., 14"  - every other lag
#     "0, 4, 8, 12, 15"   - sparse
#
# Within each (tuning x lag) cell the estimates are standardised
# (z = (est - mean)/sd); we then compare each tuning's sampling distribution
# to N(0,1) via histogram + KDE and a normal Q-Q plot, at lags 2 / 8 / 14.

library(tidyverse)
library(ggpubr)

base <- "/Users/fuyuguo/Molin/Molin_spline/paper/SIM_submission/proj_spline/github"
dir  <- file.path(base, "simulation_Persontime_ARIMA_112_unimodal_term_search_tune")
setwd(base)

target_lags <- c(2, 8, 14)

dat <- readRDS(file.path(dir, "simulation_TermSearch.rds")) %>%
  filter(l %in% target_lags) %>%
  group_by(term_knots_label, l) %>%                       # standardise per cell
  mutate(z = (log_HR_estimate - mean(log_HR_estimate, na.rm = TRUE)) /
             sd(log_HR_estimate, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(is.finite(z)) %>%
  mutate(tuning  = fct_inorder(term_knots_label),         # dense -> sparse order
         lag_lab = factor(paste0("l = ", l),
                          levels = paste0("l = ", target_lags)))

# ── Histogram panel: rows = tuning, cols = lag ────────────────────────────────
panel_hist <- ggplot(dat, aes(x = z)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, fill = "grey70", colour = "white", linewidth = 0.2) +
  geom_density(colour = "firebrick", linewidth = 0.7) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                colour = "black", linewidth = 0.7, linetype = "dotted") +
  facet_grid(tuning ~ lag_lab) +
  coord_cartesian(xlim = c(-4, 4)) +
  labs(x = "Standardised log-HR estimate", y = "Density", title = "Histogram + KDE") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "grey95"),
        strip.text.y = element_text(size = 7),
        plot.title = element_text(hjust = 0.5))

# ── Q-Q panel: rows = tuning, cols = lag ──────────────────────────────────────
panel_qq <- ggplot(dat, aes(sample = z)) +
  stat_qq(size = 0.4, alpha = 0.4, colour = "firebrick") +
  stat_qq_line(colour = "black", linewidth = 0.6, linetype = "dotted") +
  facet_grid(tuning ~ lag_lab) +
  coord_cartesian(xlim = c(-3.5, 3.5), ylim = c(-4, 4)) +
  labs(x = "Theoretical N(0,1) quantiles", y = "Sample quantiles",
       title = "Normal Q-Q") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "grey95"),
        strip.text.y = element_text(size = 7),
        plot.title = element_text(hjust = 0.5))

fig <- ggarrange(panel_hist, panel_qq, ncol = 2, nrow = 1, labels = c("A", "B"))
fig <- annotate_figure(
  fig,
  top = text_grob(
    paste("Asymptotic normality of differently tuned Term-Search estimators",
          "(rows = candidate knot grid; red = method density/points, black dotted = N(0,1))"),
    size = 11))

ggsave("figure_termsearch_tune_normality.png", plot = fig,
       units = "cm", width = 38, height = 22, dpi = 300)

cat("Saved figure_termsearch_tune_normality.png\n")
print(count(dat, tuning, lag_lab))
