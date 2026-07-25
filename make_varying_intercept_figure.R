setwd(tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
))

library(tidyverse)
library(patchwork)
library(ggh4x)

# ── Varying-intercept study (unimodal scenario, knot = 10, n = 5000) ──────────
# Same three methods as make_varying_size_figure.R. Here the latency-risk curve
# (gamma) is held FIXED and the baseline log-hazard `intercept` is varied
# (-4 / -2.5 / -1.5 / 0): a larger intercept gives a larger lambda and hence MORE events,
# while the true curve is identical across facets. This isolates how estimation
# quality depends on the number of events.
#   top    = median +/- (2.5%, 97.5%) interval (all methods overlaid, one facet
#            per intercept)
#   bottom = 10 randomly selected fitted curves (method rows x intercept columns)
# Facet labels carry the intercept, the event count and its ratio to n = 5000.
dir <- "simulation_Persontime_ARIMA_112_varying_intercept"

N         <- 5000
INT_ORDER <- c(-4, -2.5, -1.5, 0)

# Average number of uncensored events per intercept, pooled over the three
# methods. n_events is constant within a replicate, so we keep one value per
# (method, intercept, sim_id) before averaging.
events_tbl <- bind_rows(
  readRDS(file.path(dir, "simulation_KnotGridSearch.rds")) %>%
    transmute(method = "Knot", intercept, sim_id, n_events),
  readRDS(file.path(dir, "simulation_TermSearch.rds")) %>%
    transmute(method = "Term", intercept, sim_id, n_events),
  readRDS(file.path(dir, "simulation_Polynomial.rds")) %>%
    filter(degree == 5) %>%
    transmute(method = "Poly", intercept, sim_id, n_events)
) %>%
  distinct(method, intercept, sim_id, n_events) %>%
  group_by(intercept) %>%
  summarise(avg_events = mean(n_events, na.rm = TRUE), .groups = "drop")

# Facet labels: intercept, number of events and its ratio to the sample size.
avg_events <- setNames(events_tbl$avg_events, events_tbl$intercept)
INT_LABELS <- setNames(
  sprintf("events ≈ %d (%.0f%% of n = %d)",
          round(avg_events[as.character(INT_ORDER)]),
          100 * avg_events[as.character(INT_ORDER)] / N, N),
  as.character(INT_ORDER))
int_label <- function(g) factor(INT_LABELS[as.character(g)], levels = INT_LABELS)

# Per-(intercept, lag) summary across replicates for one method.
load_method <- function(file, method_label, degree_filter = NULL) {
  dat <- readRDS(file.path(dir, file))
  if (!is.null(degree_filter)) dat <- filter(dat, degree == degree_filter)
  dat %>%
    group_by(intercept, l) %>%
    summarise(
      log_HR_true         = mean(log_HR_true),
      log_HR_estimate_med = median(log_HR_estimate, na.rm = TRUE),
      log_HR_estimate_lo  = quantile(log_HR_estimate, 0.025, na.rm = TRUE),
      log_HR_estimate_hi  = quantile(log_HR_estimate, 0.975, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(method = method_label)
}

all_data <- bind_rows(
  load_method("simulation_KnotGridSearch.rds", "Knot-search RCspline"),
  load_method("simulation_TermSearch.rds",     "Term-search RCspline"),
  load_method("simulation_Polynomial.rds",     "Polynomial (df=5)", degree_filter = 5)
) %>%
  mutate(int_label = int_label(intercept))

# ── method levels / aesthetics (matching make_varying_size_figure.R) ─────────
METHOD_LEVELS <- c("True latency-risk curve", "Knot-search RCspline",
                   "Term-search RCspline", "Polynomial (df=5)")

true_rows <- all_data %>%
  distinct(int_label, l, log_HR_true) %>%
  mutate(method = "True latency-risk curve",
         log_HR_estimate_med = log_HR_true,
         log_HR_estimate_lo  = log_HR_true,
         log_HR_estimate_hi  = log_HR_true)

plot_data <- bind_rows(all_data, true_rows) %>%
  mutate(method = factor(method, levels = METHOD_LEVELS))

COL_VALUES <- c("black",  "#3B4992FF", "#DF8F44FF", "#BB0021FF")
LT_VALUES  <- c("dotted", "solid",     "dashed",    "twodash")
SHP_VALUES <- c(32L, 16L, 17L, 15L)

p_mean <- ggplot(plot_data, aes(
  x = l, y = log_HR_estimate_med,
  colour = method, linetype = method, shape = method, fill = method
)) +
  geom_ribbon(
    aes(ymin = log_HR_estimate_lo, ymax = log_HR_estimate_hi),
    alpha = 0.15, colour = NA, show.legend = FALSE
  ) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.1) +
  facet_wrap(~int_label, nrow = 1, scales = "free_y") +
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

# ── raw replicate-level data (shared by variability panel and metrics) ───────
load_raw <- function(file, method_label, degree_filter = NULL) {
  dat <- readRDS(file.path(dir, file))
  if (!is.null(degree_filter)) dat <- filter(dat, degree == degree_filter)
  transmute(dat, method = method_label, intercept, sim_id, l,
            log_HR_true, log_HR_estimate)
}

raw <- bind_rows(
  load_raw("simulation_KnotGridSearch.rds", "Knot-search RCspline"),
  load_raw("simulation_TermSearch.rds",     "Term-search RCspline"),
  load_raw("simulation_Polynomial.rds",     "Polynomial (df=5)", degree_filter = 5)
)

# ── bottom: 10 randomly selected fitted curves (method rows x intercept cols) ─
# Same 10 sim_ids shown in every panel so the same replications are compared.
set.seed(123)
sim10 <- sample(unique(raw$sim_id), 10)

raw_data <- raw %>%
  filter(sim_id %in% sim10) %>%
  mutate(method    = factor(method, levels = METHOD_LEVELS[-1]),
         int_label = int_label(intercept))

true_raw <- raw_data %>%
  distinct(method, int_label, l, log_HR_true)

p_var <- ggplot(raw_data, aes(x = l, y = log_HR_estimate,
                              group = factor(sim_id), colour = method)) +
  geom_line(linewidth = 0.3, alpha = 0.6) +
  geom_line(
    aes(x = l, y = log_HR_true),
    data        = true_raw,
    colour      = "black",
    linetype    = "dashed",
    linewidth   = 0.7,
    inherit.aes = FALSE
  ) +
  facet_grid2(method ~ int_label, scales = "free_y", independent = "y") +
  scale_colour_manual(values = COL_VALUES[-1], guide = "none") +
  theme_bw(base_size = 9) +
  labs(
    x       = expression(Time ~ lag ~ (l)),
    y       = expression(logHR[p](l)),
    caption = "Dashed line: true latency-risk curve"
  ) +
  theme(
    strip.background = element_rect(fill = "grey92"),
    strip.text       = element_text(size = 9, face = "bold"),
    plot.caption     = element_text(hjust = 0)
  )

# ── combine: mean (top) over variability (bottom) ────────────────────────────
combined <- (p_mean / p_var) +
  plot_layout(heights = c(1, 2)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 12, face = "bold"))

width  <- 32
height <- width * 0.7
ggsave("figure_varying_intercept.png", combined,
       units = "cm", width = width, height = height, dpi = 300)
message("Saved figure_varying_intercept.png")

metrics <- raw %>%
  mutate(e = log_HR_true - log_HR_estimate) %>%
  group_by(intercept, method, sim_id) %>%
  summarise(MSE = mean(e^2, na.rm = TRUE),
            MAE = mean(abs(e), na.rm = TRUE), .groups = "drop") %>%
  group_by(intercept, method) %>%
  summarise(n_sims = sum(!is.na(MSE)),
            MSE  = mean(MSE,  na.rm = TRUE),
            RMSE = mean(sqrt(MSE)),
            MAE  = mean(MAE,  na.rm = TRUE), .groups = "drop") %>%
  mutate(method = factor(method, levels = METHOD_LEVELS[-1])) %>%
  # Label rows by event count and proportion of n instead of the intercept.
  left_join(events_tbl, by = "intercept") %>%
  mutate(events     = round(avg_events),
         proportion = sprintf("%.0f%%", 100 * avg_events / N)) %>%
  arrange(intercept, method) %>%
  select(events, proportion, method, n_sims, MSE, RMSE, MAE)

write_csv(metrics, "fit_metrics_varying_intercept.csv")
cat("== Goodness-of-fit by event count (unimodal, knot = 10, n = 5000) ==\n")
print(as.data.frame(metrics %>% mutate(across(c(MSE, RMSE, MAE), ~ signif(.x, 3)))),
      row.names = FALSE)
cat("\nSaved figure_varying_intercept.png and fit_metrics_varying_intercept.csv\n")
