source("simulate_personetime.R")

library(future)
library(furrr)
library(LatencyA)

simulation_output <- function(sample_size, beta = 0.01, rho = 0.3, lambda = 0.5,
                              alpha1 = 0.3, alpha2 = -0.01, alpha3 = 0.01, alpha4 = 0,
                              knot = 10,
                              lambda_0 = 0.5, gamma = 0.1,
                              latency = 16,
                              term_knots = 0:(latency-1)) {
  plan(if (Sys.info()[["sysname"]] == "Darwin") "multisession" else "multicore")
  future_map_dfr(1:100, .f = function(sim_id){
    sim_data <- generate_data(sample_size, beta = 0.01, rho = 0.3, lambda = 0.5,
                              alpha1 = alpha1, alpha2 = alpha2,
                              alpha3 = alpha3, alpha4 = alpha4, knot = knot,
                              lambda_0 = lambda_0, gamma = gamma)

    fit <- CoxTermsearch(sim_data,
                         time_start = "age_start", time_end = "age_end", status = "failure",
                         exposure   = paste0("lag", 0:(latency-1)),
                         knots      = term_knots,
                         latency    = latency)

    log_HR_true <- log_HR_estimate <- numeric(latency)
    for (l in 1:latency) {
      log_HR_estimate[l] <- extract_CoxTermsearch(fit, lag = l - 1,
                                                   latency = latency, knots = term_knots)
      log_HR_true[l]     <- gamma * alpha_function(l - 1, knot = knot)
    }

    data.frame(sim_id          = sim_id,
               l               = 0:(latency - 1),
               log_HR_true     = log_HR_true,
               log_HR_estimate = log_HR_estimate)
  }, .options = furrr_options(seed = TRUE))
}
