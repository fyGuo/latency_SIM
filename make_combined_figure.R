setwd(tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
))

library(tidyverse)
library(patchwork)

# ── data loader (summarised) ──────────────────────────────────────────────────
# Reads one simulation RDS, renames the scenario column to scenario_val,
# and returns a per-(scenario_val, lag) summary across replicates.
# subsample_ids: integer vector of sim_id values to keep (NULL = keep all).
load_method <- function(file, scenario_col, method_label,
                        degree_filter = NULL, subsample_ids = NULL) {
  dat <- readRDS(file)
  if (!is.null(degree_filter))  dat <- filter(dat, degree == degree_filter)
  if (!is.null(subsample_ids))  dat <- filter(dat, sim_id %in% subsample_ids)
  dat$scenario_val <- dat[[scenario_col]]
  dat %>%
    group_by(scenario_val, l) %>%
    summarise(
      log_HR_true         = mean(log_HR_true),
      log_HR_estimate_med = median(log_HR_estimate),
      log_HR_estimate_lo  = quantile(log_HR_estimate, 0.025),
      log_HR_estimate_hi  = quantile(log_HR_estimate, 0.975),
      .groups = "drop"
    ) %>%
    mutate(method = method_label)
}

# Helper: load all three methods for one folder and keep only one scenario.
# subsample_ids: passed through to load_method for folders with > 300 reps.
load_scenario <- function(dir, scenario_col, keep_expr, label,
                          degree_filter = 5, subsample_ids = NULL) {
  bind_rows(
    load_method(file.path(dir, "simulation_KnotGridSearch.rds"), scenario_col,
                "Knot-search RCspline", subsample_ids = subsample_ids),
    load_method(file.path(dir, "simulation_TermSearch.rds"),     scenario_col,
                "Term-search RCspline", subsample_ids = subsample_ids),
    load_method(file.path(dir, "simulation_Polynomial.rds"),     scenario_col,
                "Polynomial (df=5)", degree_filter = degree_filter,
                subsample_ids = subsample_ids)
  ) %>%
    filter(!!keep_expr) %>%
    transmute(l, log_HR_true, log_HR_estimate_med, log_HR_estimate_lo,
              log_HR_estimate_hi, method, scenario_label = label)
}

# ── raw loader (for variability figures) ─────────────────────────────────────
# Returns individual simulation rows (no summarisation).
# sample_sims: integer vector of sim_id values to display (10 per panel).
load_method_raw <- function(file, scenario_col, method_label,
                            degree_filter = NULL, sample_sims) {
  dat <- readRDS(file)
  if (!is.null(degree_filter)) dat <- filter(dat, degree == degree_filter)
  dat$scenario_val <- dat[[scenario_col]]
  dat %>%
    filter(sim_id %in% sample_sims) %>%
    select(sim_id, scenario_val, l, log_HR_true, log_HR_estimate) %>%
    mutate(method = method_label)
}

load_scenario_raw <- function(dir, scenario_col, keep_expr, label,
                              degree_filter = 5, sample_sims) {
  bind_rows(
    load_method_raw(file.path(dir, "simulation_KnotGridSearch.rds"), scenario_col,
                    "Knot-search RCspline", sample_sims = sample_sims),
    load_method_raw(file.path(dir, "simulation_TermSearch.rds"),     scenario_col,
                    "Term-search RCspline", sample_sims = sample_sims),
    load_method_raw(file.path(dir, "simulation_Polynomial.rds"),     scenario_col,
                    "Polynomial (df=5)", degree_filter = degree_filter,
                    sample_sims = sample_sims)
  ) %>%
    filter(!!keep_expr) %>%
    transmute(sim_id, l, log_HR_true, log_HR_estimate, method,
              scenario_label = label)
}

# ── subsample IDs ─────────────────────────────────────────────────────────────
# ARIMA_112 and ARIMA_112_decayed ran 500 replicates; the other two ran 300.
# Draw 300 ids once so all six panels are compared on the same number of reps.
set.seed(42)
ids_300 <- sort(sample(1:500, 300))

# ── load the 6 selected scenarios (summarised) ───────────────────────────────

# 1. Unimodal: B-spline shape with knot = 10  (500 reps → subsample to 300)
d1 <- load_scenario(
  dir           = "simulation_Persontime_ARIMA_112",
  scenario_col  = "knot",
  keep_expr     = quote(scenario_val == 10),
  label         = "Unimodal",
  subsample_ids = ids_300
)

# 2. Damped exponential: decay rate kappa = 0.4  (500 reps → subsample to 300)
d2 <- load_scenario(
  dir           = "simulation_Persontime_ARIMA_112_decayed",
  scenario_col  = "B",
  keep_expr     = quote(scenario_val == 0.4),
  label         = "Damped exponential",
  subsample_ids = ids_300
)

# 3. Bimodal 1: bimodal shape with B = -1 (second mode negative)
d3 <- load_scenario(
  dir          = "simulation_Persontime_ARIMA_112_bimodal",
  scenario_col = "B",
  keep_expr    = quote(scenario_val == -1),
  label        = "Bimodal 1"
)

# 4. Bimodal 2: bimodal shape with B = 1 (both modes positive)
d4 <- load_scenario(
  dir          = "simulation_Persontime_ARIMA_112_bimodal",
  scenario_col = "B",
  keep_expr    = quote(scenario_val == 1),
  label        = "Bimodal 2"
)

# 5. Null effects: constant weights all = 0
d5 <- load_scenario(
  dir          = "simulation_Persontime_ARIMA_112_constant",
  scenario_col = "positive",
  keep_expr    = quote(scenario_val == FALSE),
  label        = "Null effects"
)

# 6. Constant positive effects: constant weights all = A
d6 <- load_scenario(
  dir          = "simulation_Persontime_ARIMA_112_constant",
  scenario_col = "positive",
  keep_expr    = quote(scenario_val == TRUE),
  label        = "Constant positive effects"
)

# ── combine ───────────────────────────────────────────────────────────────────
PANEL_ORDER <- c(
  "Unimodal", "Damped exponential", "Bimodal 1",
  "Bimodal 2", "Null effects", "Constant positive effects"
)

METHOD_LEVELS <- c(
  "True latency-risk curve",
  "Knot-search RCspline",
  "Term-search RCspline",
  "Polynomial (df=5)"
)

all_data <- bind_rows(d1, d2, d3, d4, d5, d6) %>%
  mutate(scenario_label = factor(scenario_label, levels = PANEL_ORDER))

# Append one true-curve row per (scenario_label, l).
true_rows <- all_data %>%
  distinct(scenario_label, l, log_HR_true) %>%
  mutate(
    method              = "True latency-risk curve",
    log_HR_estimate_med = log_HR_true,
    log_HR_estimate_lo  = log_HR_true,
    log_HR_estimate_hi  = log_HR_true
  )

plot_data <- bind_rows(all_data, true_rows) %>%
  mutate(method = factor(method, levels = METHOD_LEVELS))

# ── shared aesthetics ─────────────────────────────────────────────────────────
METHOD_COLORS <- c("black", "#3B4992FF", "#DF8F44FF", "#BB0021FF")

# Per-method metadata drives both figure loops.
METHOD_INFO <- data.frame(
  method      = METHOD_LEVELS[-1],
  color       = METHOD_COLORS[-1],
  file_suffix = c("knotsearch", "termsearch", "polynomial"),
  stringsAsFactors = FALSE
)

width  <- 23
height <- width * 0.75

# ── Figure 1 (Image #1): all three methods, mean ± 1.96 SE ───────────────────
COL_VALUES <- c("black",  "#3B4992FF", "#DF8F44FF", "#BB0021FF")
LT_VALUES  <- c("dotted", "solid",     "dashed",    "twodash")
SHP_VALUES <- c(32L, 16L, 17L, 15L)

combined <- ggplot(plot_data, aes(
  x        = l,
  y        = log_HR_estimate_med,
  colour   = method,
  linetype = method,
  shape    = method,
  fill     = method
)) +
  geom_ribbon(
    aes(ymin = log_HR_estimate_lo, ymax = log_HR_estimate_hi),
    alpha = 0.15, colour = NA, show.legend = FALSE
  ) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.1) +
  facet_wrap(~scenario_label, nrow = 2, ncol = 3, scales = "free_y") +
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

ggsave("figure_combined.png", combined,
       units = "cm", width = width, height = height, dpi = 300)
message("Saved figure_combined.png")

# ── Figures per method: mean ± 1.96 SE ───────────────────────────────────────
for (i in seq_len(nrow(METHOD_INFO))) {
  m_name   <- METHOD_INFO$method[i]
  m_color  <- METHOD_INFO$color[i]
  m_suffix <- METHOD_INFO$file_suffix[i]

  pd_m <- plot_data %>%
    filter(method %in% c("True latency-risk curve", m_name)) %>%
    mutate(method = factor(method, levels = c("True latency-risk curve", m_name)))

  p <- ggplot(pd_m, aes(
    x        = l,
    y        = log_HR_estimate_med,
    colour   = method,
    linetype = method,
    shape    = method,
    fill     = method
  )) +
    geom_ribbon(
      aes(ymin = log_HR_estimate_lo, ymax = log_HR_estimate_hi),
      alpha = 0.15, colour = NA, show.legend = FALSE
    ) +
    geom_line(linewidth = 0.55) +
    geom_point(size = 1.1) +
    facet_wrap(~scenario_label, nrow = 2, ncol = 3, scales = "free_y") +
    scale_colour_manual(  values = c("black", m_color), name = "Method") +
    scale_fill_manual(    values = c("black", m_color), name = "Method") +
    scale_linetype_manual(values = c("dotted", "solid"), name = "Method") +
    scale_shape_manual(   values = c(32L, 16L),          name = "Method") +
    theme_bw(base_size = 9) +
    labs(x = expression(Time ~ lag ~ (l)),
         y = expression(logHR[p](l))) +
    theme(
      legend.position  = "bottom",
      strip.background = element_rect(fill = "grey92"),
      strip.text       = element_text(size = 9, face = "bold")
    )

  ggsave(paste0("figure_", m_suffix, ".png"), p,
         units = "cm", width = width, height = height, dpi = 300)
  message("Saved figure_", m_suffix, ".png")
}

# ── load raw data for variability figures ─────────────────────────────────────
# Each scenario from the same folder shares the same 10 sim_ids so the same
# replications appear in both bimodal panels and both constant panels.
set.seed(123)
sim10_bspline  <- sample(ids_300, 10)
sim10_decayed  <- sample(ids_300, 10)
sim10_bimodal  <- sample(1:300,   10)
sim10_constant <- sample(1:300,   10)

r1 <- load_scenario_raw("simulation_Persontime_ARIMA_112",          "knot",     quote(scenario_val == 10),    "Unimodal",                  sample_sims = sim10_bspline)
r2 <- load_scenario_raw("simulation_Persontime_ARIMA_112_decayed",  "B",        quote(scenario_val == 0.4),   "Damped exponential",        sample_sims = sim10_decayed)
r3 <- load_scenario_raw("simulation_Persontime_ARIMA_112_bimodal",  "B",        quote(scenario_val == -1),    "Bimodal 1",                 sample_sims = sim10_bimodal)
r4 <- load_scenario_raw("simulation_Persontime_ARIMA_112_bimodal",  "B",        quote(scenario_val == 1),     "Bimodal 2",                 sample_sims = sim10_bimodal)
r5 <- load_scenario_raw("simulation_Persontime_ARIMA_112_constant", "positive", quote(scenario_val == FALSE), "Null effects",              sample_sims = sim10_constant)
r6 <- load_scenario_raw("simulation_Persontime_ARIMA_112_constant", "positive", quote(scenario_val == TRUE),  "Constant positive effects", sample_sims = sim10_constant)

raw_data <- bind_rows(r1, r2, r3, r4, r5, r6) %>%
  mutate(
    scenario_label = factor(scenario_label, levels = PANEL_ORDER),
    method         = factor(method, levels = METHOD_LEVELS[-1])
  )

# One true-curve row per (scenario_label, l) — deterministic, same for all sims.
true_raw <- raw_data %>%
  distinct(scenario_label, l, log_HR_true)

# ── Figures 4–6: 10 individual curves, one method per figure ─────────────────
var_plots <- vector("list", nrow(METHOD_INFO))
for (i in seq_len(nrow(METHOD_INFO))) {
  m_name   <- METHOD_INFO$method[i]
  m_color  <- METHOD_INFO$color[i]
  m_suffix <- METHOD_INFO$file_suffix[i]

  rd_m <- raw_data %>%
    filter(method == m_name) %>%
    mutate(curve_id = factor(sim_id))

  v <- ggplot(rd_m, aes(x = l, y = log_HR_estimate, group = curve_id)) +
    geom_line(colour = m_color, linewidth = 0.3, alpha = 0.6) +
    geom_line(
      aes(x = l, y = log_HR_true, group = scenario_label),
      data        = true_raw,
      colour      = "black",
      linetype    = "dashed",
      linewidth   = 0.7,
      inherit.aes = FALSE
    ) +
    facet_wrap(~scenario_label, nrow = 2, ncol = 3, scales = "free_y") +
    theme_bw(base_size = 9) +
    labs(
      x       = expression(Time ~ lag ~ (l)),
      y       = expression(logHR[p](l))
    ) +
    theme(
      strip.background = element_rect(fill = "grey92"),
      strip.text       = element_text(size = 9, face = "bold")
    )

  var_plots[[i]] <- v
  ggsave(paste0("figure_variability_", m_suffix, ".png"), v,
         units = "cm", width = width, height = height, dpi = 300)
  message("Saved figure_variability_", m_suffix, ".png")
}

# ── 2×2 combined figure ───────────────────────────────────────────────────────
# Layout:  [Image #1: all-methods mean±SE]  [Image #2: knot variability]
#          [Image #3: term variability   ]  [Image #4: poly variability]
big_figure <- (combined | var_plots[[1]]) /
              (var_plots[[2]] | var_plots[[3]]) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    text            = element_text(size = 14),
    axis.title      = element_text(size = 13),
    axis.text       = element_text(size = 11),
    strip.text      = element_text(size = 13, face = "bold"),
    legend.text     = element_text(size = 11),
    legend.title    = element_text(size = 13, face = "bold"),
    legend.position = "bottom",
    plot.tag        = element_text(size = 16, face = "bold")
  )

ggsave("figure_2x2.png", big_figure,
       units = "cm", width = width * 2, height = height * 2, dpi = 300)
message("Saved figure_2x2.png")
