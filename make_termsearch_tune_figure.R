setwd(tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
))

library(tidyverse)
library(patchwork)

# ── Term-search tuning study (unimodal scenario) ──────────────────────────────
# Compares the tuned term-search RCspline estimator under different candidate
# knot grids (5 / 8 / 16 knots) AND under the two model-selection criteria used
# inside the term search: AIC vs BIC. Layout follows make_combined_figure.R:
#   top row    = mean +/- 1.96 SE across replicates (AIC vs BIC overlaid)
#   bottom row = 10 randomly selected fitted curves (AIC rows vs BIC rows)
tune_dirs <- c(
  AIC = "simulation_Persontime_ARIMA_112_unimodal_term_search_tune",
  BIC = "simulation_Persontime_ARIMA_112_unimodal_term_search_tune_BIC"
)

PANEL_ORDER     <- c("5 knots", "8 knots", "16 knots")
CRITERION_ORDER <- c("AIC", "BIC")

# Map the stored "<n> terms" labels onto the "<n> knots" panel labels.
relabel <- function(x) factor(str_replace(x, "terms", "knots"), levels = PANEL_ORDER)

# Read both criteria, tagging each with its selection criterion.
raw <- imap_dfr(tune_dirs, function(dir, crit) {
  readRDS(file.path(dir, "simulation_TermSearch.rds")) %>%
    mutate(criterion = crit)
}) %>%
  mutate(criterion = factor(criterion, levels = CRITERION_ORDER))

# ── top: mean +/- 1.96 SE, AIC vs BIC overlaid ───────────────────────────────
tune <- raw %>%
  group_by(criterion, term_knots_label, l) %>%
  summarise(
    log_HR_true         = mean(log_HR_true),
    log_HR_estimate_med = median(log_HR_estimate),
    log_HR_estimate_lo  = quantile(log_HR_estimate, 0.025),
    log_HR_estimate_hi  = quantile(log_HR_estimate, 0.975),
    .groups = "drop"
  ) %>%
  mutate(scenario_label = relabel(term_knots_label))

METHOD_LEVELS <- c("True latency-risk curve", "Term-search (AIC)", "Term-search (BIC)")

est_rows <- tune %>%
  transmute(scenario_label, l, log_HR_estimate_med, log_HR_estimate_lo, log_HR_estimate_hi,
            method = paste0("Term-search (", criterion, ")"))

true_rows <- tune %>%
  distinct(scenario_label, l, log_HR_true) %>%
  transmute(scenario_label, l,
            log_HR_estimate_med = log_HR_true,
            log_HR_estimate_lo  = log_HR_true,
            log_HR_estimate_hi  = log_HR_true,
            method              = "True latency-risk curve")

plot_data <- bind_rows(est_rows, true_rows) %>%
  mutate(method = factor(method, levels = METHOD_LEVELS))

p_mean <- ggplot(plot_data, aes(
  x        = l,
  y        = log_HR_estimate_med,
  colour   = method,
  linetype = method,
  shape    = method,
  fill     = method
)) +
  geom_ribbon(
    aes(ymin = log_HR_estimate_lo, ymax = log_HR_estimate_hi),
    alpha = 0.12, colour = NA, show.legend = FALSE
  ) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.1) +
  facet_wrap(~scenario_label, nrow = 1, ncol = 3, scales = "free_y") +
  scale_colour_manual(  values = c("black", "#DF8F44FF", "#3B4992FF"), name = "Method") +
  scale_fill_manual(    values = c("black", "#DF8F44FF", "#3B4992FF"), name = "Method") +
  scale_linetype_manual(values = c("dotted", "solid", "dashed"),       name = "Method") +
  scale_shape_manual(   values = c(32L, 17L, 16L),                     name = "Method") +
  theme_bw(base_size = 9) +
  labs(x = expression(Time ~ lag ~ (l)),
       y = expression(logHR[p](l))) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    strip.text       = element_text(size = 9, face = "bold")
  )

# ── bottom: 10 randomly selected fitted curves per grid, AIC vs BIC rows ──────
# Same 10 sim_ids shown in every panel so the same replications are compared.
set.seed(123)
sim10 <- sample(unique(raw$sim_id), 10)

raw_data <- raw %>%
  filter(sim_id %in% sim10) %>%
  transmute(criterion, sim_id, l, log_HR_true, log_HR_estimate,
            scenario_label = relabel(term_knots_label))

true_raw <- raw_data %>%
  distinct(criterion, scenario_label, l, log_HR_true)

CRIT_COLORS <- c(AIC = "#DF8F44FF", BIC = "#3B4992FF")

p_var <- ggplot(raw_data, aes(x = l, y = log_HR_estimate,
                              group = factor(sim_id), colour = criterion)) +
  geom_line(linewidth = 0.3, alpha = 0.6) +
  geom_line(
    aes(x = l, y = log_HR_true, group = scenario_label),
    data        = true_raw,
    colour      = "black",
    linetype    = "dashed",
    linewidth   = 0.7,
    inherit.aes = FALSE
  ) +
  facet_grid(criterion ~ scenario_label, scales = "free_y") +
  scale_colour_manual(values = CRIT_COLORS, guide = "none") +
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
  plot_layout(heights = c(1, 1.6)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 12, face = "bold"))

width  <- 23
height <- width * 0.9
ggsave("figure_termsearch_tune.png", combined,
       units = "cm", width = width, height = height, dpi = 300)
message("Saved figure_termsearch_tune.png")
