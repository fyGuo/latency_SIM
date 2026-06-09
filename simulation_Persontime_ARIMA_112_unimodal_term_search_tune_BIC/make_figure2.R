setwd(tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
))

library(tidyverse)
library(ggsci)
library(ggpubr)

SCENARIO_ORDER <- c("16 terms", "8 terms", "5 terms")

# ── Panel A: mean ± 1.96 SE ───────────────────────────────────────────────────
result_term <- readRDS("simulation_TermSearch.rds")

# Normalise labels to the current display names regardless of when the RDS
# was generated (handles "0 to 15", "16 knots", "16 terms", etc.)
label_remap <- c(
  "0 to 15"           = "16 terms",
  "16 knots"          = "16 terms",
  "16 terms"          = "16 terms",
  "0, 2, 4, ..., 14" = "8 terms",
  "8 knots"           = "8 terms",
  "8 terms"           = "8 terms",
  "0, 4, 8, 12, 15"  = "5 terms",
  "5 knots"           = "5 terms",
  "5 terms"           = "5 terms"
)
result_term$term_knots_label <- label_remap[result_term$term_knots_label]

summarised <- result_term %>%
  group_by(term_knots_label, l) %>%
  summarise(log_HR_true          = mean(log_HR_true),
            log_HR_estimate_mean = mean(log_HR_estimate),
            log_HR_estimate_se   = sd(log_HR_estimate),
            .groups = "drop") %>%
  mutate(term_knots_label = factor(term_knots_label, levels = SCENARIO_ORDER),
         method = "Term-search RCspline model")

true_rows <- summarised %>%
  distinct(term_knots_label, l, log_HR_true) %>%
  mutate(method               = "True latency-risk curve",
         log_HR_estimate_mean = log_HR_true,
         log_HR_estimate_se   = 0)

temp <- bind_rows(summarised, true_rows) %>%
  mutate(method = factor(method, levels = c("True latency-risk curve",
                                            "Term-search RCspline model")))

saveRDS(temp, "data_figure2.rds")

panel_a <- ggplot(temp) +
  geom_ribbon(aes(x = l,
                  ymin = log_HR_estimate_mean - 1.96 * log_HR_estimate_se,
                  ymax = log_HR_estimate_mean + 1.96 * log_HR_estimate_se,
                  fill = method), alpha = 0.2) +
  geom_line( aes(x = l, y = log_HR_estimate_mean, color = method, linetype = method)) +
  geom_point(aes(x = l, y = log_HR_estimate_mean, shape = method, color = method)) +
  facet_grid(.~term_knots_label) +
  scale_linetype_manual(values = c("dotted", "solid")) +
  scale_color_manual(   values = c("black", "#3B4992FF")) +
  scale_fill_manual(    values = c("black", "#3B4992FF")) +
  theme_bw() +
  labs(x = expression(Time ~ lag ~ (l)),
       y = expression(logHR[p](l)),
       color = "Method", fill = "Method", linetype = "Method", shape = "Method") +
  theme(legend.position  = "bottom",
        legend.background = element_blank())

# ── Panel B: 10 randomly selected individual curves ──────────────────────────
set.seed(123)
index <- sample(unique(result_term$sim_id), 10)

raw_10 <- result_term %>%
  filter(sim_id %in% index) %>%
  mutate(term_knots_label = factor(term_knots_label, levels = SCENARIO_ORDER))

true_raw <- raw_10 %>%
  distinct(term_knots_label, l, log_HR_true)

panel_b <- ggplot(raw_10,
                  aes(x = l, y = log_HR_estimate,
                      group = sim_id)) +
  geom_line(colour = "#3B4992FF", linewidth = 0.3, alpha = 0.6) +
  geom_line(aes(x= l, y = log_HR_true, group = term_knots_label),
            data        = true_raw,
            colour      = "black",
            linetype    = "dashed",
            linewidth   = 0.7,
            inherit.aes = FALSE) +
  facet_grid(.~term_knots_label) +
  theme_bw() +
  labs(x       = expression(Time ~ lag ~ (l)),
       y       = expression(logHR[p](l)),
       caption = "Dashed line: true latency-risk curve") +
  theme(plot.caption     = element_text(hjust = 0),
        legend.background = element_blank())

# ── Combine and save ──────────────────────────────────────────────────────────
ggarrange(panel_a, panel_b, nrow = 2, labels = c("A", "B"))

width  <- 23
height <- width * 0.8
ggsave("figure2.png", units = "cm", width = width, height = height, dpi = 300)
