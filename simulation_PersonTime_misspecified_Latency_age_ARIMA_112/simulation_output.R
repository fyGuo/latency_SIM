source("simulate_personetime.R")

library(future)
library(furrr)
library(LatencyA)

simulation_output <- function(sample_size, beta1 = 0.01, alpha1 = 0.3, alpha2 = -0.01,
                              alpha3 = 0.01, alpha4 = 0, knot = 5,
                              lambda_0 = 0.5, gamma = 0.1, degree = 4,
                              latency = 16,
                              method = c("Polynomial",
                                         "Spline",
                                         "GridSearchKnot",
                                         "TermSearch"),
                              TermSearch_knots = 0:(latency - 1)) {
  plan("multicore")
  future_map_dfr(1:300, .f = function(sim_id){
    na_result <- data.frame(sim_id          = sim_id,
                            l               = 0:(latency - 1),
                            log_HR_true     = rep(NA_real_, latency),
                            log_HR_estimate = rep(NA_real_, latency))
    tryCatch({
      sim_data <- generate_data(sample_size, beta = 0.01, rho = 0.3, lambda = 0.5,
                                alpha1 = alpha1, alpha2 = alpha2, alpha3 = alpha3, alpha4 = alpha4,
                                knot = knot, lambda_0 = lambda_0, gamma = gamma)

      if (method == "Polynomial") {
        fit <- CoxPoly(sim_data, time_start = "age_start", time_end = "age_end",
                       status   = "failure",
                       exposure = paste0("lag", 0:(latency - 1)),
                       degree   = degree - 1,
                       latency  = latency)
        log_HR_true <- log_HR_estimate <- numeric(latency)
        for (l in 1:latency) {
          log_HR_estimate[l] <- tryCatch(extract_CoxPoly(fit, lag = l - 1)$log_HR,
                                         error = function(e) NA_real_)
          log_HR_true[l]     <- gamma * alpha_function(l - 1, knot = knot)
        }

      } else if (method == "NCSpline") {
        fit <- CoxNCSpline(sim_data, time_start = "age_start", time_end = "age_end",
                           status       = "failure",
                           exposure     = paste0("lag", 0:(latency - 1)),
                           knots_number = degree,
                           latency      = latency)
        log_HR_true <- log_HR_estimate <- numeric(latency)
        for (l in 1:latency) {
          log_HR_estimate[l] <- tryCatch(
            extract_CoxNCSpline(fit, lag = l - 1, latency = latency)$log_HR,
            error = function(e) NA_real_)
          log_HR_true[l]     <- gamma * alpha_function(l - 1, knot = knot)
        }

      } else if (method == "GridSearchKnot") {
        fit <- CoxKnotsearch(sim_data, time_start = "age_start", time_end = "age_end",
                             status       = "failure",
                             exposure     = paste0("lag", 0:(latency - 1)),
                             knots_number = c(3, 4, 5),
                             latency      = latency)
        log_HR_true <- log_HR_estimate <- numeric(latency)
        for (l in 1:latency) {
          log_HR_estimate[l] <- tryCatch(
            extract_CoxKnotsearch(fit, lag = l - 1, latency = latency),
            error = function(e) NA_real_)
          log_HR_true[l]     <- gamma * alpha_function(l - 1, knot = knot)
        }

      } else if (method == "TermSearch") {
        fit <- CoxTermsearch(sim_data, time_start = "age_start", time_end = "age_end",
                             status   = "failure",
                             exposure = paste0("lag", 0:(latency - 1)),
                             knots    = TermSearch_knots,
                             latency  = latency)
        log_HR_true <- log_HR_estimate <- numeric(latency)
        for (l in 1:latency) {
          log_HR_estimate[l] <- tryCatch(
            extract_CoxTermsearch(fit, lag = l - 1, latency = latency,
                                  knots = TermSearch_knots),
            error = function(e) NA_real_)
          log_HR_true[l]     <- gamma * alpha_function(l - 1, knot = knot)
        }
      }

      data.frame(sim_id          = sim_id,
                 l               = 0:(latency - 1),
                 log_HR_true     = log_HR_true,
                 log_HR_estimate = log_HR_estimate)
    }, error = function(e) na_result)
  }, .options = furrr_options(seed = TRUE, scheduling = Inf))
}





