source("simulate_personetime.R")

library(future)
library(furrr)
library(LatencyA)

simulation_output <- function(sample_size,
                              curve  = "unimodal",
                              A      = 0.3,  B      = 0.4, knot   = 10,
                              alpha1 = 0.3,  alpha2 = -0.01,
                              alpha3 = 0.01, alpha4 = 0,
                              lambda_0 = 0.5, gamma = 0.1,
                              degree   = 4,
                              latency  = 16) {
  future_map_dfr(1:300, .f = function(sim_id){
    sim_data <- generate_data(sample_size, rho = 0.3, lambda = 0.5,
                              curve  = curve,
                              A = A, B = B, knot = knot,
                              alpha1 = alpha1, alpha2 = alpha2,
                              alpha3 = alpha3, alpha4 = alpha4,
                              lambda_0 = lambda_0, gamma = gamma)

    fit <- CoxPoly(sim_data,
                   time_start = "age_start", time_end = "age_end", status = "failure",
                   exposure   = paste0("lag", 0:(latency - 1)),
                   degree     = degree - 1,
                   latency    = latency)

    log_HR_true <- log_HR_estimate <- numeric(latency)
    for (l in 1:latency) {
      log_HR_estimate[l] <- extract_CoxPoly(fit, lag = l - 1)$log_HR
      log_HR_true[l]     <- as.numeric(gamma * alpha_function(
        l - 1, curve = curve, A = A, B = B, knot = knot,
        alpha1 = alpha1, alpha2 = alpha2, alpha3 = alpha3, alpha4 = alpha4))
    }

    data.frame(sim_id          = sim_id,
               l               = 0:(latency - 1),
               log_HR_true     = log_HR_true,
               log_HR_estimate = log_HR_estimate)
  }, .options = furrr_options(seed = TRUE))
}
