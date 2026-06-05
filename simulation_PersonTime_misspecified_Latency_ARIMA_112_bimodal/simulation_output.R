source("simulate_personetime.R")

library(future)
library(furrr)
library(LatencyA)

# True underlying curve: bimodal Gaussian mixture with B = -1.
# A = 0.3, B = -1 are fixed throughout this folder.

simulation_output <- function(sample_size, A = 0.3, B = -1,
                              lambda_0 = 0.5, gamma = 0.1, degree = 4,
                              latency = 16,
                              method = c("Polynomial", "Spline",
                                         "GridSearchKnot", "TermSearch"),
                              TermSearch_knots = 0:(latency - 1)) {
  plan("multicore")
  future_map_dfr(1:300, .f = function(sim_id){
    sim_data <- generate_data(sample_size, beta = 0.01, rho = 0.3, lambda = 0.5,
                              A = A, B = B,
                              lambda_0 = lambda_0, gamma = gamma)

    if (method == "Polynomial") {
      fit <- CoxPoly(sim_data, time_start = "age_start", time_end = "age_end",
                     status = "failure",
                     exposure = paste0("lag", 0:(latency - 1)),
                     degree   = degree - 1,
                     latency  = latency)
      log_HR_true <- log_HR_estimate <- numeric(latency)
      for (l in 1:latency) {
        log_HR_estimate[l] <- extract_CoxPoly(fit, lag = l - 1)
        log_HR_true[l]     <- gamma * alpha_function(l - 1, A = A, B = B)
      }

    } else if (method == "NCSpline") {
      fit <- CoxNCSpline(sim_data, time_start = "age_start", time_end = "age_end",
                         status   = "failure",
                         exposure = paste0("lag", 0:(latency - 1)),
                         knots_number = degree,
                         latency  = latency)
      log_HR_true <- log_HR_estimate <- numeric(latency)
      for (l in 1:latency) {
        log_HR_estimate[l] <- extract_CoxNCSpline(fit, lag = l - 1, latency = latency)$log_HR
        log_HR_true[l]     <- gamma * alpha_function(l - 1, A = A, B = B)
      }

    } else if (method == "GridSearchKnot") {
      fit <- CoxKnotsearch(sim_data, time_start = "age_start", time_end = "age_end",
                           status   = "failure",
                           exposure = paste0("lag", 0:(latency - 1)),
                           knots_number = c(3, 4, 5),
                           latency  = latency)
      log_HR_true <- log_HR_estimate <- numeric(latency)
      for (l in 1:latency) {
        log_HR_estimate[l] <- extract_CoxKnotsearch(fit, lag = l - 1, latency = latency)
        log_HR_true[l]     <- gamma * alpha_function(l - 1, A = A, B = B)
      }

    } else if (method == "TermSearch") {
      fit <- CoxTermsearch(sim_data, time_start = "age_start", time_end = "age_end",
                           status   = "failure",
                           exposure = paste0("lag", 0:(latency - 1)),
                           knots    = 0:(latency - 1),
                           latency  = latency)
      log_HR_true <- log_HR_estimate <- numeric(latency)
      for (l in 1:latency) {
        log_HR_estimate[l] <- extract_CoxTermsearch(fit, lag = l - 1,
                                                     latency = latency,
                                                     knots   = 0:(latency - 1))
        log_HR_true[l]     <- gamma * alpha_function(l - 1, A = A, B = B)
      }
    }

    data.frame(sim_id          = sim_id,
               l               = 0:(latency - 1),
               log_HR_true     = log_HR_true,
               log_HR_estimate = log_HR_estimate)
  }, .options = furrr_options(seed = TRUE, scheduling = Inf))
}
