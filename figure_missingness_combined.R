# ── Combined figure: figures 5, 6, 7 at shape varsigma = 10 only ──────────────
# Panel A (fig 5): coarsened measurement   (every period / 2-period / 4-period)
# Panel B (fig 6): missing pre-baseline exposure, single-stage imputation
# Panel C (fig 7): missing pre-baseline exposure, two-stage imputation
# Each panel keeps only the facet Shape varsigma == 10, then the three are
# stacked with ggpubr::ggarrange.
library(tidyverse)
library(ggsci)
library(ggpubr)

base <- "/Users/fuyuguo/Molin/Molin_spline/paper/SIM_submission/proj_spline/github"
SHAPE <- 10

# summarise one results file over simulations, label its method
summ <- function(path, method) {
  readRDS(path) %>%
    group_by(knot, l) %>%
    summarise(log_HR_true          = mean(log_HR_true, na.rm = TRUE),
              log_HR_estimate_mean = mean(log_HR_estimate, na.rm = TRUE),
              # shaded band = empirical 95% interval (2.5 / 97.5 percentiles)
              lo                   = quantile(log_HR_estimate, 0.025, na.rm = TRUE),
              hi                   = quantile(log_HR_estimate, 0.975, na.rm = TRUE),
              .groups = "drop") %>%
    filter(knot == SHAPE) %>%
    mutate(method = method)
}

# append the true curve as its own "method" and lock the factor order
add_truth <- function(temp, levels) {
  truth <- temp %>%
    mutate(log_HR_estimate_mean = log_HR_true,
           lo                   = log_HR_true,   # no band for the true curve
           hi                   = log_HR_true,
           method               = "True latency-risk curve")
  bind_rows(temp, truth) %>%
    mutate(knot   = factor(paste0("Shape~varsigma==", knot)),
           method = factor(method, levels = levels))
}

panel <- function(temp, colors, linetypes, title) {
  nr <- 2   # 2 legend rows for every panel so all plot areas stay the same height
  ggplot(temp) +
    geom_line(aes(l, log_HR_estimate_mean, color = method, linetype = method)) +
    geom_ribbon(aes(l, ymin = lo, ymax = hi, fill = method), alpha = 0.3) +
    theme_bw() +
    scale_linetype_manual(values = linetypes) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    labs(x = expression(Time ~ lag ~ (l)), y = expression(logHR[p](l)),
         color = "Models", fill = "Models", linetype = "Models", title = title) +
    guides(color    = guide_legend(nrow = nr, byrow = TRUE),
           fill     = guide_legend(nrow = nr, byrow = TRUE),
           linetype = guide_legend(nrow = nr, byrow = TRUE)) +
    theme(legend.position = "bottom", legend.background = element_blank(),
          legend.text = element_text(size = 7),
          legend.key.width = unit(0.8, "lines"),
          legend.spacing.x = unit(0.2, "lines"),
          plot.title = element_text(face = "bold", size = 11))
}

# ── Panel A: coarsened measurement (fig 5) ───────────────────────────────────
fig5 <- bind_rows(
  summ(file.path(base, "simulation_Persontime_ARIMA_112/simulation_KnotGridSearch.rds"),
       "Measured every period"),
  summ(file.path(base, "simulation_PersonTime_Coarsened_2period_ARIMA_112/simulation_KnotGridSearch.rds"),
       "Measured every two periods"),
  summ(file.path(base, "simulation_PersonTime_Coarsened_ARIMA_112/simulation_KnotGridSearch.rds"),
       "Measured every four periods")) %>%
  add_truth(c("True latency-risk curve", "Measured every period",
              "Measured every two periods", "Measured every four periods"))

pA <- panel(fig5,
            colors    = c("black", "#3B4992FF", "#008B45FF", "#BB0021FF"),
            linetypes = c("solid", "dashed", "dotdash", "twodash"),
            title     = "Inner missingness")

# ── Panel B: missing exposure, single-stage imputation (fig 6) ───────────────
d6 <- "simulation_missing_Persontime_mixedModel_nsamples_ARIMA_112"
fig6 <- bind_rows(
  summ(file.path(base, d6, "simulation_CCA_KnotSearch.rds"),        "Complete-person-time analysis"),
  summ(file.path(base, d6, "simulation_MI.rds"),                    "Multiple imputation (mixed effect models)"),
  summ(file.path(base, d6, "simulation_imputation0_KnotSearch.rds"),"Imputation with 0"),
  summ(file.path(base, d6, "simulation_MI_MCMC.rds"),               "Multiple imputation (random forests)")) %>%
  add_truth(c("True latency-risk curve", "Complete-person-time analysis", "Imputation with 0",
              "Multiple imputation (mixed effect models)", "Multiple imputation (random forests)"))

pB <- panel(fig6,
            colors    = c("black", "#3B4992FF", "#631879FF", "#008280FF", "#BB0021FF"),
            linetypes = c("solid", "dashed", "dotted", "dotdash", "twodash"),
            title     = "Pre-baseline missingness")

# ── Panel C: missing exposure, two-stage imputation (fig 7) ──────────────────
d7 <- "simulation_missing_Persontime_mixedModel_nsamples_2stage_ARIMA_112"
fig7 <- bind_rows(
  summ(file.path(base, d7, "simulation_CCA_KnotSearch.rds"), "Mixed effect models + complete-person-time analysis"),
  summ(file.path(base, d7, "simulation_MI_MCMC.rds"),        "Mixed effect models + random forests")) %>%
  add_truth(c("True latency-risk curve",
              "Mixed effect models + complete-person-time analysis",
              "Mixed effect models + random forests"))

pC <- panel(fig7,
            colors    = c("black", "#3B4992FF", "#BB0021FF"),
            linetypes = c("solid", "dashed", "dotdash"),
            title     = "Co-existence of missingness")

# ── Stack the three panels vertically ────────────────────────────────────────
combined <- ggarrange(pA, pB, pC, ncol = 1, nrow = 3, labels = c("A", "B", "C"))

ggsave(file.path(base, "figure_missingness_combined.png"), combined,
       units = "cm", width = 24, height = 33, dpi = 300, bg = "white")
message("Saved figure_missingness_combined.png")
