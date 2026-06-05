setwd(tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
))

library(tidyverse)
library(ggsci)
library(ggpubr)

CURVE_ORDER  <- c("Unimodal", "Bimodal 1", "Bimodal 2", "Decayed")
DEGREE_ORDER <- c("df = 3", "df = 4", "df = 5")

# ── Panel A: mean ± 1.96 SE ───────────────────────────────────────────────────
result_poly <- readRDS("simulation_Polynomial.rds")

label_remap <- c("Bimodal (B=-1)" = "Bimodal 1", "Bimodal (B=1)" = "Bimodal 2")
result_poly$curve_label <- ifelse(result_poly$curve_label %in% names(label_remap),
                                  label_remap[result_poly$curve_label],
                                  result_poly$curve_label)

summarised <- result_poly %>%
  mutate(
    curve_label  = factor(curve_label, levels = CURVE_ORDER),
    degree_label = factor(paste0("df = ", degree), levels = DEGREE_ORDER)
  ) %>%
  group_by(curve_label, degree_label, l) %>%
  summarise(log_HR_true          = mean(log_HR_true),
            log_HR_estimate_mean = mean(log_HR_estimate),
            log_HR_estimate_se   = sd(log_HR_estimate),
            .groups = "drop") %>%
  mutate(method = "Polynomial")

true_rows <- summarised %>%
  distinct(curve_label, degree_label, l, log_HR_true) %>%
  mutate(method               = "True latency-risk curve",
         log_HR_estimate_mean = log_HR_true,
         log_HR_estimate_se   = 0)

temp <- bind_rows(summarised, true_rows) %>%
  mutate(method = factor(method, levels = c("True latency-risk curve", "Polynomial")))

saveRDS(temp, "data_figure2.rds")

panel_a <- ggplot(temp) +
  geom_ribbon(aes(x    = l,
                  ymin = log_HR_estimate_mean - 1.96 * log_HR_estimate_se,
                  ymax = log_HR_estimate_mean + 1.96 * log_HR_estimate_se,
                  fill = method), alpha = 0.2) +
  geom_line( aes(x = l, y = log_HR_estimate_mean, color = method, linetype = method)) +
  geom_point(aes(x = l, y = log_HR_estimate_mean, shape = method, color = method)) +
  facet_grid(degree_label ~ curve_label) +
  scale_linetype_manual(values = c("dotted", "solid")) +
  scale_color_manual(   values = c("black", "#BB0021FF")) +
  scale_fill_manual(    values = c("black", "#BB0021FF")) +
  theme_bw() +
  labs(x = expression(Time ~ lag ~ (l)),
       y = expression(logHR[p](l)),
       color = "Method", fill = "Method", linetype = "Method", shape = "Method") +
  theme(legend.position   = "bottom",
        legend.background = element_blank())

# ── Save ──────────────────────────────────────────────────────────────────────
width  <- 30
height <- width * 0.6
ggsave("figure2.png", panel_a, units = "cm", width = width, height = height, dpi = 300)
