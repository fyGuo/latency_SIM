setwd(tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
))

library(tidyverse)
library(ggsci)
library(splines)

# ── Combine the three misspecified-latency studies ────────────────────────────
# Scenario 1 (unimodal): simulation_PersonTime_misspecified_Latency_ARIMA_112
#   true curve = gamma * bs(l, knots = 10) %*% c(0.3, -0.01, 0.01, 0)
# Scenario 2 (bimodal):  ..._ARIMA_112_bimodal, with B = -1 and B = 1
#   true curve = gamma * 0.3 * (bump@3 + B * bump@12)
# Scenario 3 (decayed):  ..._ARIMA_112_decayed, damped exponential, B = 0.4
#   true curve = gamma * 0.3 * exp(-0.4 * l)
#   (the folder also holds a B = 0.2 Term-Search run, but only B = 0.4 was run
#   for all three methods, so only B = 0.4 enters the figure)
# Each method estimates the latency-risk curve under several MISSPECIFIED
# latency lengths (true latency = 16). We overlay the estimate (mean +/- 1.96 SE
# over successful replicates) with the full true curve (lags 0-15, dashed).
gamma   <- 0.1
DIR_UNI <- "simulation_PersonTime_misspecified_Latency_age_ARIMA_112"
DIR_BIM <- "simulation_PersonTime_misspecified_Latency_ARIMA_112_bimodal"
DIR_DEC <- "simulation_PersonTime_misspecified_Latency_ARIMA_112_decayed"

SCENARIO_LEVELS <- c("Unimodal", "Decayed", "Bimodal 1", "Bimodal 2")

# ── true curves over lags 0..15, per scenario ────────────────────────────────
true_unimodal <- as.numeric(gamma * bs(0:15, knots = 10, Boundary.knots = c(0, 16)) %*%
                              c(0.3, -0.01, 0.01, 0))
true_bimodal  <- function(B) as.numeric(gamma * 0.3 *
                              (exp(-0.5 * ((0:15 - 3) / 2)^2) + B * exp(-0.5 * ((0:15 - 12) / 2)^2)))
true_decayed  <- as.numeric(gamma * 0.3 * exp(-0.4 * (0:15)))

full_true <- bind_rows(
  data.frame(scenario = "Unimodal",  l = 0:15, y = true_unimodal),
  data.frame(scenario = "Bimodal 1", l = 0:15, y = true_bimodal(-1)),  # B = -1
  data.frame(scenario = "Bimodal 2", l = 0:15, y = true_bimodal( 1)),  # B =  1
  data.frame(scenario = "Decayed",   l = 0:15, y = true_decayed)       # B = 0.4
) %>% mutate(scenario = factor(scenario, levels = SCENARIO_LEVELS))

# ── failure-aware loader (count + drop failed replicates, na.rm summaries) ────
failure_log <- list()

load_one <- function(dir, file, method_label, scenario_from,
                     degree_filter = NULL, B_filter = NULL) {
  path <- file.path(dir, file)
  if (!file.exists(path)) return(NULL)
  dat <- readRDS(path)
  if (!is.null(degree_filter)) dat <- filter(dat, degree == degree_filter)
  if (!is.null(B_filter))      dat <- filter(dat, B == B_filter)

  # Scenario column: unimodal folder has no B; bimodal folder has B in {-1, 1};
  # decayed folder has B in {0.2, 0.4} (decay rate, kept to B_filter above).
  dat <- dat %>% mutate(scenario = scenario_from(.))

  grp <- c("scenario", "latency", "sim_id")
  rep_status <- dat %>% group_by(across(all_of(grp))) %>%
    summarise(failed = all(is.na(log_HR_estimate)), .groups = "drop")

  key <- paste(dir, method_label,
               if (is.null(degree_filter)) "" else paste0("df", degree_filter))
  failure_log[[key]] <<- rep_status %>%
    group_by(scenario, latency) %>%
    summarise(n_rep = n(), n_failed = sum(failed), .groups = "drop") %>%
    mutate(method = method_label,
           degree = if (is.null(degree_filter)) NA_integer_ else degree_filter)

  dat %>%
    semi_join(filter(rep_status, !failed), by = grp) %>%
    group_by(scenario, latency, l) %>%
    summarise(log_HR_estimate_mean = mean(log_HR_estimate, na.rm = TRUE),
              log_HR_estimate_se   = sd(log_HR_estimate,   na.rm = TRUE),
              n_used               = n_distinct(sim_id),
              .groups = "drop") %>%
    mutate(method = method_label,
           degree = if (is.null(degree_filter)) NA_integer_ else degree_filter)
}

uni_scenario <- function(d) "Unimodal"
# B=-1 -> "Bimodal 1", B=1 -> "Bimodal 2". as.character keeps the column type
# character even for zero-row inputs (ifelse() would otherwise return logical(0)).
bim_scenario <- function(d) as.character(ifelse(d$B == -1, "Bimodal 1", "Bimodal 2"))
dec_scenario <- function(d) "Decayed"

# ── Methods figure: Knot-search, Term-search, Polynomial (df = 5) ─────────────
methods_data <- bind_rows(
  load_one(DIR_UNI, "simulation_KnotGridSearch.rds", "Knot-search",       uni_scenario),
  load_one(DIR_UNI, "simulation_TermSearch.rds",     "Term-Search",       uni_scenario),
  load_one(DIR_UNI, "simulation_Polynomial.rds",     "Polynomial (df=5)", uni_scenario, degree_filter = 5),
  load_one(DIR_BIM, "simulation_KnotGridSearch.rds", "Knot-search",       bim_scenario),
  load_one(DIR_BIM, "simulation_TermSearch.rds",     "Term-Search",       bim_scenario),
  load_one(DIR_BIM, "simulation_Polynomial.rds",     "Polynomial (df=5)", bim_scenario, degree_filter = 5),
  load_one(DIR_DEC, "simulation_KnotGridSearch.rds", "Knot-search",       dec_scenario, B_filter = 0.4),
  load_one(DIR_DEC, "simulation_TermSearch.rds",     "Term-Search",       dec_scenario, B_filter = 0.4),
  load_one(DIR_DEC, "simulation_Polynomial.rds",     "Polynomial (df=5)", dec_scenario, degree_filter = 5, B_filter = 0.4)
)

# ── Polynomial-degree figure: df = 3, 4, 5 (whatever is present) ──────────────
poly_degree_data <- bind_rows(lapply(c(3, 4, 5), function(dg) bind_rows(
  load_one(DIR_UNI, "simulation_Polynomial.rds", sprintf("Polynomial (df=%d)", dg), uni_scenario, degree_filter = dg),
  load_one(DIR_BIM, "simulation_Polynomial.rds", sprintf("Polynomial (df=%d)", dg), bim_scenario, degree_filter = dg),
  load_one(DIR_DEC, "simulation_Polynomial.rds", sprintf("Polynomial (df=%d)", dg), dec_scenario, degree_filter = dg, B_filter = 0.4)
)))

# ── Report failures ───────────────────────────────────────────────────────────
failure_report <- bind_rows(failure_log) %>% arrange(method, degree, scenario, latency)
write_csv(failure_report, "misspecified_failure_report.csv")
cat("== Failed simulations per setting (failed = all lag estimates NA) ==\n")
print(as.data.frame(failure_report %>% filter(n_failed > 0)), row.names = FALSE)
cat(sprintf("Total failed replicates: %d of %d\n\n",
            sum(failure_report$n_failed), sum(failure_report$n_rep)))

# ── Plot helper ───────────────────────────────────────────────────────────────
library(patchwork)

# One sub-figure per true weight curve (scenario): latency rows x method cols,
# overlaid with that scenario's own dashed true curve. The three sub-figures are
# then stacked and combined with a single shared "Specified latency" legend.
scenario_subfig <- function(dat, sc, col_levels, show_x, drop_latency = 25) {
  d <- dat %>%
    filter(scenario == sc, latency != drop_latency) %>%
    mutate(method      = factor(method, levels = col_levels),
           latency_lab = factor(paste0("latency==", latency),
                                levels = paste0("latency==", sort(unique(latency)))))
  tc <- filter(full_true, scenario == sc)

  ggplot(d) +
    geom_line(aes(x = l, y = y), data = tc,
              colour = "black", linetype = "dashed", linewidth = 0.6, inherit.aes = FALSE) +
    geom_ribbon(aes(x = l,
                    ymax = log_HR_estimate_mean + 1.96 * log_HR_estimate_se,
                    ymin = log_HR_estimate_mean - 1.96 * log_HR_estimate_se,
                    group = latency_lab),
                fill = "grey60", colour = NA, alpha = 0.4) +
    geom_line(aes(x = l, y = log_HR_estimate_mean, group = latency_lab),
              colour = "grey25") +
    facet_grid(latency_lab ~ method, labeller = label_parsed) +
    theme_bw(base_size = 9) +
    labs(title = sc,
         x = if (show_x) expression(Time ~ lag ~ (l)) else NULL,
         y = expression(logHR[p](l))) +
    theme(legend.position  = "bottom",
          strip.background = element_rect(fill = "grey92"),
          plot.title       = element_text(face = "bold", size = 11))
}

combine_scenarios <- function(dat, col_levels, file, width = 36, height = 34) {
  # Three scenario blocks arranged in a 2 x 2 grid (the 4th cell is left empty).
  plots <- lapply(SCENARIO_LEVELS, function(sc)
    scenario_subfig(dat, sc, col_levels, show_x = TRUE))
  combined <- wrap_plots(plots, ncol = 2, guides = "collect") +
    plot_annotation(tag_levels = "A",
                    caption = "Dashed line: full true latency-risk curve (lags 0-15)") &
    theme(legend.position = "bottom",
          plot.tag = element_text(size = 12, face = "bold"))
  ggsave(file, combined, units = "cm", width = width, height = height, dpi = 300)
  message("Saved ", file)
}

# Methods figure: Knot-search, Term-search, Polynomial (df = 5)
combine_scenarios(methods_data,
                  c("Knot-search", "Term-Search", "Polynomial (df=5)"),
                  "figure_misspecified_combined.png")

# Polynomial-degree figure: only drawn once df = 3 or 4 are also present
# (otherwise it would duplicate the df = 5 column of the methods figure).
if (n_distinct(poly_degree_data$method) >= 2) {
  combine_scenarios(poly_degree_data,
                    sprintf("Polynomial (df=%d)", c(3, 4, 5)),
                    "figure_misspecified_poly_degrees.png")
}

cat("Polynomial degrees available: ",
    paste(sort(unique(na.omit(failure_report$degree))), collapse = ", "), "\n")
