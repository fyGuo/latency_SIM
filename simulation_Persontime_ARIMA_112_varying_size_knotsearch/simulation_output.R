# please change the following directory to your own directory

source("simulate_personetime.R")

library(future)
library(furrr)
library(LatencyA)
# let's write a function for simulation
# generate data by functions from simulate_persontetime
# then do the analysis using polynomial and GriSearch-Spline.
# Compare the results to the true curve

simulation_output <- function(sample_size, beta = 0.01, rho = 0.3, lambda = 0.5,  alpha1 = 0.3, alpha2 =-0.01, 
                              alpha3 = 0.01, alpha4 = 0,  knot = 10,
                              lambda_0 = 0.5, gamma = 0.1,  degree = 4, 
                              latency = 16,
                              term_knots = 0:(latency-1),
                              method = c("Polynomial", 
                                         "Spline",
                                         "GridSearchKnot",
                                         "TermSearch")) {
  plan("multicore")
  future_map_dfr(1:300, .f = function(sim_id){
    # The true curve is deterministic, so we can always report it even if a fit
    # fails (e.g. with small samples where a single replicate may not converge).
    log_HR_true <- vapply(1:latency, function(l) gamma * alpha_function(l - 1, knot = knot),
                          numeric(1))

    # Generate data once; the number of events (failures) is a property of the
    # data, so we record it regardless of whether the subsequent fit succeeds.
    sim_data <- tryCatch(
      generate_data(sample_size, beta = 0.01, rho = 0.3, lambda = 0.5,
                    alpha1 = alpha1, alpha2 =alpha2, alpha3 = alpha3, alpha4 = alpha4, knot = knot,
                    lambda_0 = lambda_0, gamma = gamma),
      error = function(e) NULL)
    n_events <- if (is.null(sim_data)) NA_real_ else sum(sim_data$failure, na.rm = TRUE)

    # Fit + estimate extraction. Any failure here imputes NA to log_HR_estimate
    # for this replicate so that the simulation is not interrupted and continues.
    log_HR_estimate <- tryCatch({
      if (is.null(sim_data)) stop("data generation failed")
      est <- numeric(latency)
      if (method == "Polynomial") {
        fit <- CoxPoly(sim_data, time_start = "age_start", time_end ="age_end", status = "failure",
                       exposure = paste0("lag", 0:(latency-1)),
                       degree = degree-1,
                       latency = latency)
        for (l in 1:latency) {
          est[l] <- extract_CoxPoly(fit, lag = l - 1)$log_HR
        }

      } else if (method == "NCSpline") {
        fit <- CoxNCSpline(sim_data, time_start = "age_start", time_end ="age_end", status = "failure",
                       exposure = paste0("lag", 0:(latency-1)),
                       knots_number = degree,
                       latency = latency)
        for (l in 1:latency) {
          est[l] <- extract_CoxNCSpline(fit, lag = l - 1, latency = latency)$log_HR
        }

      } else if (method == "GridSearchKnot") {
        fit <- CoxKnotsearch(sim_data, time_start = "age_start", time_end ="age_end", status = "failure",
                             exposure = paste0("lag", 0:(latency-1)),
                             knots_number = c(3,4,5),
                             latency = latency)
        for (l in 1:latency) {
          est[l] <- extract_CoxKnotsearch(fit, lag = l - 1, latency = latency)
        }
      } else if (method == "TermSearch") {
        fit <- CoxTermsearch(sim_data, time_start = "age_start", time_end ="age_end", status = "failure",
                             exposure = paste0("lag", 0:(latency-1)),
                             knots= term_knots,
                             latency = latency,
                             criteria = "BIC")
        for (l in 1:latency) {
          est[l] <- extract_CoxTermsearch(fit, lag = l - 1, latency = latency, knots = term_knots)
        }
      }
      est
    }, error = function(e) {
      message(sprintf("sim_id %d failed (%s): imputing NA", sim_id, conditionMessage(e)))
      rep(NA_real_, latency)
    })


    data.frame(sim_id = sim_id,
                l =  0:(latency -1),
               log_HR_true = log_HR_true,
               log_HR_estimate = log_HR_estimate,
               n_events = n_events) %>% return()
  }, .options = furrr_options(seed = T))
  
}





