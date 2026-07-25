setwd(tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
))

library(tidyverse)
library(patchwork)

# ── Knot-search vs. CoxNCSpline, varying sample size (unimodal, knot = 10) ─────
#   top    = median +/- (2.5%, 97.5%) interval, both methods overlaid, one facet
#            per sample size
#   bottom = 10 randomly selected fitted curves (method rows x size columns)
# CoxNCSpline is shown at df = 3 / 4 / 5; Knot-search picks its knot count by AIC.
dir <- "simulation_Persontime_ARIMA_112_varying_size_knotsearch"

SIZE_ORDER <- c(300, 500, 1000, 3000, 5000)

knot <- readRDS(file.path(dir, "simulation_KnotGridSearch.rds")) %>%
  transmute(method = "Knot-search RCspline", sample_size, sim_id, l,
            log_HR_true, log_HR_estimate)

ncs <- readRDS(file.path(dir, "simulation_NCSpline.rds")) %>%
  transmute(method = sprintf("RCspline (df=%d)", degree),
            sample_size, sim_id, l, log_HR_true, log_HR_estimate)

raw <- bind_rows(knot, ncs)

# Average number of uncensored events per sample size (constant within replicate).
events_tbl <- readRDS(file.path(dir, "simulation_KnotGridSearch.rds")) %>%
  distinct(sample_size, sim_id, n_events) %>%
  group_by(sample_size) %>%
  summarise(avg_events = mean(n_events, na.rm = TRUE), .groups = "drop")
avg_events  <- setNames(events_tbl$avg_events, events_tbl$sample_size)
SIZE_LABELS <- setNames(
  sprintf("n = %d\nevents ≈ %d", SIZE_ORDER,
          round(avg_events[as.character(SIZE_ORDER)])),
  as.character(SIZE_ORDER))
size_label <- function(n) factor(SIZE_LABELS[as.character(n)], levels = SIZE_LABELS)

# Order: true curve, then the fixed-df RCspline fits, with Knot-search last.
# Each method keeps its own colour/linetype/shape (so the vectors are reordered
# in lock-step with METHOD_LEVELS).
METHOD_LEVELS <- c("True latency-risk curve",
                   "RCspline (df=3)", "RCspline (df=4)", "RCspline (df=5)",
                   "Knot-search RCspline")
COL_VALUES <- c("black",  "#3B4992FF", "#008B45FF", "#631879FF", "#BB0021FF")
LT_VALUES  <- c("dotted", "dashed",    "twodash",   "longdash",  "solid")
SHP_VALUES <- c(32L, 17L, 15L, 18L, 16L)

# ── top: median + 95% replicate interval ──────────────────────────────────────
summ <- raw %>%
  group_by(method, sample_size, l) %>%
  summarise(
    log_HR_true         = mean(log_HR_true),
    log_HR_estimate_med = median(log_HR_estimate, na.rm = TRUE),
    log_HR_estimate_lo  = quantile(log_HR_estimate, 0.025, na.rm = TRUE),
    log_HR_estimate_hi  = quantile(log_HR_estimate, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(size_label = size_label(sample_size))

true_rows <- summ %>%
  distinct(size_label, l, log_HR_true) %>%
  mutate(method = "True latency-risk curve",
         log_HR_estimate_med = log_HR_true,
         log_HR_estimate_lo  = log_HR_true,
         log_HR_estimate_hi  = log_HR_true)

plot_data <- bind_rows(summ, true_rows) %>%
  mutate(method = factor(method, levels = METHOD_LEVELS))

p_mean <- ggplot(plot_data, aes(
  x = l, y = log_HR_estimate_med,
  colour = method, linetype = method, shape = method, fill = method
)) +
  geom_ribbon(
    aes(ymin = log_HR_estimate_lo, ymax = log_HR_estimate_hi),
    alpha = 0.12, colour = NA, show.legend = FALSE
  ) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.1) +
  facet_wrap(~size_label, nrow = 1, scales = "free_y") +
  scale_colour_manual(  values = COL_VALUES, name = "Method", drop = FALSE) +
  scale_fill_manual(    values = COL_VALUES, name = "Method", drop = FALSE) +
  scale_linetype_manual(values = LT_VALUES,  name = "Method", drop = FALSE) +
  scale_shape_manual(   values = SHP_VALUES, name = "Method", drop = FALSE) +
  theme_bw(base_size = 9) +
  labs(x = expression(Time ~ lag ~ (l)),
       y = expression(logHR[p](l))) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    strip.text       = element_text(size = 9, face = "bold")
  )

# ── goodness-of-fit metrics by sample size x method ───────────────────────────
metrics <- raw %>%
  mutate(e = log_HR_true - log_HR_estimate) %>%
  group_by(sample_size, method, sim_id) %>%
  summarise(MSE = mean(e^2, na.rm = TRUE),
            MAE = mean(abs(e), na.rm = TRUE), .groups = "drop") %>%
  group_by(sample_size, method) %>%
  summarise(n_sims = sum(!is.na(MSE)),
            MSE  = mean(MSE,  na.rm = TRUE),
            RMSE = mean(sqrt(MSE)),
            MAE  = mean(MAE,  na.rm = TRUE), .groups = "drop") %>%
  mutate(method = factor(method, levels = METHOD_LEVELS[-1])) %>%
  arrange(sample_size, method)

# ── bottom: MSE bar plot (one facet per sample size, exact value on each bar) ──
# Express MSE in units of 1e-5 (factored into the axis title) so each bar carries
# only a short mantissa label, leaving more room between the closely spaced bars.
MSE_UNIT <- 1e-5
mse_data <- metrics %>%
  mutate(size_label = size_label(sample_size),
         MSE_scaled = MSE / MSE_UNIT,
         MSE_label  = formatC(MSE_scaled, format = "g", digits = 3))

p_mse <- ggplot(mse_data, aes(x = method, y = MSE_scaled, fill = method)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = MSE_label), vjust = -0.4, size = 2.6) +
  facet_wrap(~size_label, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = COL_VALUES[-1], drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  theme_bw(base_size = 9) +
  labs(x = NULL, y = expression("Mean integrated squared error (×" * 10^-5 * ")")) +
  theme(
    strip.background = element_rect(fill = "grey92"),
    strip.text       = element_text(size = 9, face = "bold"),
    axis.text.x      = element_text(angle = 35, hjust = 1)
  )

# ── panel C: mean run-time per fit vs sample size (all methods, one panel) ────
# All four methods are overlaid on a single panel: run time on a log10 y-axis
# (run times span ~100-300x across methods) against sample size. Timings come
# from the dedicated timing study (one fixed dataset, 300 repeated fits per
# method x sample size).
timing_dir <- "simulation_Persontime_ARIMA_112_timing_knotSearch"
timing <- bind_rows(
  readRDS(file.path(timing_dir, "timing_GridSearchKnot.rds")) %>% mutate(method = "Knot-search RCspline"),
  readRDS(file.path(timing_dir, "timing_NCSpline_df3.rds"))   %>% mutate(method = "RCspline (df=3)"),
  readRDS(file.path(timing_dir, "timing_NCSpline_df4.rds"))   %>% mutate(method = "RCspline (df=4)"),
  readRDS(file.path(timing_dir, "timing_NCSpline_df5.rds"))   %>% mutate(method = "RCspline (df=5)")
)

time_tbl <- timing %>%
  group_by(sample_size, method) %>%
  summarise(mean_time = mean(elapsed, na.rm = TRUE), .groups = "drop") %>%
  mutate(method     = factor(method, levels = METHOD_LEVELS[-1]),
         size_label = size_label(sample_size),
         time_label = paste0(formatC(mean_time, format = "g", digits = 2), " s")) %>%
  arrange(sample_size, method)

p_time <- ggplot(time_tbl, aes(x = sample_size, y = mean_time,
                               colour = method, shape = method, linetype = method)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  # Label only the Knot-search curve; the fixed-df RCspline curves run too close
  # together for per-point labels to stay legible.
  geom_text(data = ~ filter(.x, method == "Knot-search RCspline"),
            aes(label = time_label), vjust = -0.9, size = 2.4, show.legend = FALSE) +
  # No own legend: the shared Method legend from panel A is reused for the whole
  # figure.
  scale_colour_manual(values = COL_VALUES[-1], name = "Method", drop = FALSE, guide = "none") +
  scale_shape_manual(values = SHP_VALUES[-1], name = "Method", drop = FALSE, guide = "none") +
  scale_linetype_manual(values = LT_VALUES[-1], name = "Method", drop = FALSE, guide = "none") +
  scale_x_log10(breaks = SIZE_ORDER) +
  scale_y_log10(expand = expansion(mult = c(0.05, 0.13))) +
  theme_bw(base_size = 9) +
  labs(x = "Sample size (n, log scale)", y = "Mean run time per fit (s, log scale)") +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1)
  )

# Layout: A stacked over B on the left, C filling the full-height right column,
# with a single shared Method legend collected at the bottom.
combined <- ((p_mean / p_mse) | p_time) +
  plot_layout(widths = c(1.7, 1), guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 12, face = "bold"),
        legend.position = "bottom")

ggsave("figure_knotsearch_vs_ncs.png", combined,
       units = "cm", width = 36, height = 17, dpi = 300)
message("Saved figure_knotsearch_vs_ncs.png")

write_csv(time_tbl %>% select(sample_size, method, mean_time),
          "timing_knotsearch_vs_ncs.csv")
write_csv(metrics, "fit_metrics_knotsearch_vs_ncs.csv")
cat("== Goodness-of-fit by sample size: Knot-search vs CoxNCSpline ==\n")
print(as.data.frame(metrics %>% mutate(across(c(MSE, RMSE, MAE), ~ signif(.x, 3)))),
      row.names = FALSE)
cat("\n== Mean run time per fit (seconds) ==\n")
print(time_tbl %>% select(sample_size, method, mean_time) %>%
        mutate(mean_time = signif(mean_time, 3)) %>% as.data.frame(), row.names = FALSE)
cat("\nSaved figure_knotsearch_vs_ncs.png, fit_metrics_knotsearch_vs_ncs.csv and timing_knotsearch_vs_ncs.csv\n")
