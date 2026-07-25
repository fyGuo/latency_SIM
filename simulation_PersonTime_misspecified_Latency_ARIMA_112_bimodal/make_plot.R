setwd(tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
))

library(tidyverse)
library(ggsci)

source("simulate_personetime.R")   # provides alpha_function

# ── Full true latency-risk curves for B = -1 and B = 1 (lags 0–15) ───────────
# Shown as dashed black reference lines so the reader can see what is "cut off"
# when the specified latency is shorter than the true latency of 16.
gamma     <- 0.1
full_true <- bind_rows(
  data.frame(l = 0:15, B_label = "B==-1",
             y = as.numeric(gamma * alpha_function(0:15, A = 0.3, B = -1))),
  data.frame(l = 0:15, B_label = "B==1",
             y = as.numeric(gamma * alpha_function(0:15, A = 0.3, B =  1)))
)

# ── Failure-aware loader ──────────────────────────────────────────────────────
# Some fits fail for certain settings (e.g. CoxPoly at long misspecified
# latencies). A replicate is counted as FAILED when all of its lag estimates are
# NA; failed replicates are dropped and every summary uses na.rm = TRUE, so the
# remaining successful replicates are used. The per-setting failure counts are
# accumulated in `failure_log` and reported below.
failure_log <- list()

load_method <- function(file, method_label, degree_filter = NULL) {
  dat <- readRDS(file)
  if (!is.null(degree_filter)) dat <- filter(dat, degree == degree_filter)

  rep_status <- dat %>%
    group_by(B, latency, sim_id) %>%
    summarise(failed = all(is.na(log_HR_estimate)), .groups = "drop")

  failure_log[[method_label]] <<- rep_status %>%
    group_by(B, latency) %>%
    summarise(n_rep = n(), n_failed = sum(failed),
              .groups = "drop") %>%
    mutate(method = method_label)

  dat %>%
    semi_join(filter(rep_status, !failed), by = c("B", "latency", "sim_id")) %>%
    group_by(B, latency, l) %>%
    summarise(log_HR_true          = mean(log_HR_true,     na.rm = TRUE),
              log_HR_estimate_mean = mean(log_HR_estimate, na.rm = TRUE),
              log_HR_estimate_se   = sd(log_HR_estimate,   na.rm = TRUE),
              n_used               = n_distinct(sim_id),
              .groups = "drop") %>%
    mutate(method = method_label)
}

result_KnotGridSearch <- load_method("simulation_KnotGridSearch.rds", "Knot-search") %>%
  filter(latency != 25)
result_TermSearch <- load_method("simulation_TermSearch.rds", "Term-Search") %>%
  filter(latency != 25)
result_Polynomial <- load_method("simulation_Polynomial.rds", "Polynomial (df=5)",
                                 degree_filter = 5) %>%
  filter(latency != 25)

# ── Report how many simulations failed ────────────────────────────────────────
failure_report <- bind_rows(failure_log) %>%
  arrange(method, B, latency)
write_csv(failure_report, "failure_report.csv")
cat("== Failed simulations per setting (failed = all lag estimates NA) ==\n")
print(as.data.frame(failure_report), row.names = FALSE)
cat(sprintf("\nTotal failed replicates: %d of %d\n",
            sum(failure_report$n_failed), sum(failure_report$n_rep)))

# ── Combine and create facet labels ──────────────────────────────────────────
temp <- bind_rows(result_KnotGridSearch, result_TermSearch, result_Polynomial) %>%
  mutate(
    B_label  = paste0("B==", B),
    B_label  = factor(B_label,  levels = c("B==-1", "B==1")),
    latency  = paste0("latency==", latency),
    latency  = as.factor(latency),
    method   = factor(method, levels = c("Knot-search", "Term-Search", "Polynomial (df=5)"))
  )

full_true <- full_true %>%
  mutate(B_label = factor(B_label, levels = c("B==-1", "B==1")))

# ── Plot ──────────────────────────────────────────────────────────────────────
ggplot(temp) +
  # full true curve per B value, replicated across latency rows automatically
  geom_line(aes(x = l, y = y, group = B_label),
            data        = full_true,
            colour      = "black",
            linetype    = "dashed",
            linewidth   = 0.7,
            inherit.aes = FALSE) +
  # estimated mean ± 1.96 SE
  geom_ribbon(aes(x = l,
                  ymax  = log_HR_estimate_mean + 1.96 * log_HR_estimate_se,
                  ymin  = log_HR_estimate_mean - 1.96 * log_HR_estimate_se,
                  color = latency, fill = latency),
              alpha = 0.3) +
  geom_line(aes(x = l, y = log_HR_estimate_mean,
                color = latency, group = latency)) +
  facet_grid(latency ~ B_label + method, labeller = label_parsed) +
  scale_color_aaas() +
  scale_fill_aaas() +
  theme_bw() +
  labs(x       = expression(Time ~ lag ~ (l)),
       y       = expression(logHR[p](l)),
       caption = "Dashed line: true latency-risk curve (lags 0–15)") +
  theme(legend.position   = "none",
        legend.background = element_blank(),
        plot.caption      = element_text(hjust = 0))

width  <- 40
height <- width * 0.6
ggsave("Figure4.png", units = "cm", height = height, width = width, dpi = 300)
