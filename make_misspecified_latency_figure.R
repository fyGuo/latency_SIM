# Misspecified-latency simulations under THREE true weight shapes
#   - Unimodal  : B-spline weight  (simulation_PersonTime_misspecified_Latency_ARIMA_112)
#   - Bimodal 1 : Gaussian mixture with B = -1 (..._bimodal, B = -1)
#   - Bimodal 2 : Gaussian mixture with B =  1 (..._bimodal, B =  1)
#
# True latency is 16 in every case; specified latencies 10/13/16/19/22 are compared.
# Two stacked panels: A = Knot-search, B = Term-Search (rows = case, cols = latency).
# The y-axis is freed per weighting shape (case row) so the magnitude differences
# between the three true shapes are visible, while staying comparable across the
# specified latencies within a shape.

library(tidyverse)
library(splines)
library(ggsci)
library(ggpubr)

# ── Paths ─────────────────────────────────────────────────────────────────────
base    <- "/Users/fuyuguo/Molin/Molin_spline/paper/SIM_submission/proj_spline/github"
dir_uni <- file.path(base, "simulation_PersonTime_misspecified_Latency_ARIMA_112")
dir_bim <- file.path(base, "simulation_PersonTime_misspecified_Latency_ARIMA_112_bimodal")
setwd(base)

case_levels <- c("Unimodal", "Bimodal 1", "Bimodal 2")

# ── True latency-risk weight functions (used for the dashed reference curves) ──
gamma <- 0.1

# Unimodal: cubic B-spline weight used in the original simulation
alpha_unimodal <- function(l, alpha1 = 0.3, alpha2 = -0.01, alpha3 = 0.01,
                           alpha4 = 0, knot = 5) {
  as.numeric(bs(l, knots = knot, Boundary.knots = c(0, 16)) %*%
               c(alpha1, alpha2, alpha3, alpha4))
}

# Bimodal: mixture of two Gaussians, modes at lag 3 and lag 12
alpha_bimodal <- function(l, A = 0.3, B = -1) {
  A * (exp(-0.5 * ((l - 3) / 2)^2) + B * exp(-0.5 * ((l - 12) / 2)^2))
}

full_true <- bind_rows(
  data.frame(case = "Unimodal",  l = 0:15, y = gamma * alpha_unimodal(0:15)),
  data.frame(case = "Bimodal 1", l = 0:15, y = gamma * alpha_bimodal(0:15, B = -1)),
  data.frame(case = "Bimodal 2", l = 0:15, y = gamma * alpha_bimodal(0:15, B =  1))
) %>%
  mutate(case = factor(case, levels = case_levels))

# ── Helper: load one results file and summarise over simulations ──────────────
# The unimodal files have no B column; the bimodal files carry B (-1 / 1).
summarise_results <- function(path, method_label) {
  res <- readRDS(path)
  if (!"B" %in% names(res)) res$B <- NA_real_

  res %>%
    group_by(B, latency, l) %>%
    summarise(log_HR_true          = mean(log_HR_true),
              log_HR_estimate_mean = mean(log_HR_estimate),
              log_HR_estimate_se   = sd(log_HR_estimate),
              .groups = "drop") %>%
    filter(latency != 25) %>%
    mutate(method = method_label)
}

load_case <- function(dir, case_name) {
  bind_rows(
    summarise_results(file.path(dir, "simulation_KnotGridSearch.rds"), "Knot-search"),
    summarise_results(file.path(dir, "simulation_TermSearch.rds"),     "Term-Search")
  ) %>%
    mutate(case = case_name)
}

uni <- load_case(dir_uni, "unimodal")          # B is NA here
bim <- load_case(dir_bim, "bimodal")           # B is -1 / 1 here

# Map each row to one of the three underlying cases
temp <- bind_rows(uni, bim) %>%
  mutate(
    case = case_when(
      case == "unimodal"          ~ "Unimodal",
      case == "bimodal" & B == -1 ~ "Bimodal 1",   # B = -1  -> bimodal 1
      case == "bimodal" & B ==  1 ~ "Bimodal 2",   # B =  1  -> bimodal 2
      TRUE                        ~ NA_character_
    ),
    case    = factor(case, levels = case_levels),
    latency = factor(paste0("latency==", latency)),
    method  = factor(method, levels = c("Knot-search", "Term-Search"))
  )

# ── Panel builder: one method, rows = latency, cols = case ────────────────────
make_panel <- function(method_label) {
  ggplot(filter(temp, method == method_label)) +
    geom_line(aes(x = l, y = y, group = case),
              data        = full_true,
              colour      = "black",
              linetype    = "dashed",
              linewidth   = 0.7,
              inherit.aes = FALSE) +
    geom_ribbon(aes(x = l,
                    ymax  = log_HR_estimate_mean + 1.96 * log_HR_estimate_se,
                    ymin  = log_HR_estimate_mean - 1.96 * log_HR_estimate_se,
                    color = latency, fill = latency),
                alpha = 0.3) +
    geom_line(aes(x = l, y = log_HR_estimate_mean,
                  color = latency, group = latency)) +
    facet_grid(case ~ latency, scales = "free_y",
               labeller = labeller(latency = label_parsed, .default = label_value)) +
    scale_color_aaas() +
    scale_fill_aaas() +
    theme_bw() +
    labs(x = expression(Time ~ lag ~ (l)),
         y = expression(logHR[p](l))) +
    theme(legend.position   = "none",
          legend.background = element_blank())
}

panel_knot <- make_panel("Knot-search")
panel_term <- make_panel("Term-Search")

# ── Stack the two panels (A = Knot-search, B = Term-Search) ───────────────────
fig <- ggarrange(panel_knot, panel_term,
                 labels = c("A", "B"),
                 ncol = 1, nrow = 2)

fig <- annotate_figure(
  fig,
  bottom = text_grob(
    "Dashed line: true latency-risk curve (lags 0-15). True latency = 16.",
    hjust = 0, x = 0.01, size = 9))

width  <- 30
height <- width * 0.8
ggsave("figure_misspecified_latency.png", plot = fig,
       units = "cm", height = height, width = width, dpi = 300)
