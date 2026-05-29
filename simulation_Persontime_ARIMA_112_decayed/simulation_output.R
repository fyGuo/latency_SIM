source("simulate_personetime.R")

library(future)
library(furrr)
library(LatencyA)
# let's write a function for simulation
# generate data by functions from simulate_persontetime
# then do the analysis using polynomial and GriSearch-Spline.
# Compare the results to the true curve

simulation_output <- function(sample_size, beta = 0.01, rho = 0.3, lambda = 0.5, A = 0.3, B = 0.2,
                              lambda_0 = 0.5, gamma = 0.1, degree = 4,
                              latency = 16,
                              term_knots = 0:(latency-1),
                              method = c("Polynomial",
                                         "Spline",
                                         "GridSearchKnot",
                                         "TermSearch")) {
  future_map_dfr(1:500, .f = function(sim_id){
    # Generate data
    sim_data <- generate_data(sample_size, beta = 0.01, rho = 0.3, lambda = 0.5,
                              A = A, B = B,
                              lambda_0 = lambda_0, gamma = gamma)
    if (method == "Polynomial") {
      fit <- CoxPoly(sim_data, time_start = "age_start", time_end ="age_end", status = "failure", 
                     exposure = paste0("lag", 0:(latency-1)),
                     degree = degree-1,
                     latency = latency)
      log_HR_true <- log_HR_estimate <- log_HR_var_estimate <- numeric(latency)
      for (l in 1:latency) {
        log_HR_estimate[l] <- extract_CoxPoly(fit, lag = l - 1)$log_HR
        log_HR_true[l] <- gamma * alpha_function(l-1, A = A, B = B)
        log_HR_var_estimate[l] <- extract_CoxPoly(fit, lag = l - 1)$log_HR_var
      }
      
    } else if (method == "NCSpline") {
      fit <- CoxNCSpline(sim_data, time_start = "age_start", time_end ="age_end", status = "failure", 
                     exposure = paste0("lag", 0:(latency-1)),
                     knots_number = degree,
                     latency = latency)
      log_HR_true <- log_HR_estimate <- log_HR_var_estimate <- numeric(latency)
      for (l in 1:latency) {
        log_HR_estimate[l] <- extract_CoxNCSpline(fit, lag = l - 1, latency = latency)$log_HR
        log_HR_true[l] <- gamma * alpha_function(l-1, A = A, B = B)
        log_HR_var_estimate[l] <- extract_CoxNCSpline(fit, lag = l - 1, latency = latency)$log_HR_var
      }

    } else if (method == "GridSearchKnot") {
      fit <- CoxKnotsearch(sim_data, time_start = "age_start", time_end ="age_end", status = "failure", 
                           exposure = paste0("lag", 0:(latency-1)),
                           knots_number = c(3,4,5),
                           latency = latency)
      log_HR_true <- log_HR_estimate <- numeric((latency-1))
      for (l in 1:latency) {
        log_HR_estimate[l] <- extract_CoxKnotsearch(fit, lag = l - 1, latency = latency) 
        log_HR_true[l] <- gamma * alpha_function(l-1, A = A, B = B)
        }
    } else if (method == "TermSearch") {
        fit <- CoxTermsearch(sim_data, time_start = "age_start", time_end ="age_end", status = "failure", 
                           exposure = paste0("lag", 0:(latency-1)),
                           knots= term_knots,
                           latency = latency)
        log_HR_true <- log_HR_estimate <- numeric(latency)
      for (l in 1:latency) {
        log_HR_estimate[l] <- extract_CoxTermsearch(fit, lag = l - 1, latency = latency, knots = term_knots)
        log_HR_true[l] <- gamma * alpha_function(l-1, A = A, B = B)
      }
    }
    
    
    data.frame(sim_id = sim_id,
                l =  0:(latency -1),
               log_HR_true = log_HR_true,
               log_HR_estimate = log_HR_estimate) %>% return()
  }, .options = furrr_options(seed = TRUE, scheduling = Inf))
  
}





