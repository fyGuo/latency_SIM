setwd("/n/home00/fyguo/proj_spline/simulation_missing_Persontime_mixedModel_nsamples_ARIMA_112")
source("simulate_personetime.R")
source("imputation_glmer.R")
source("imputation_mcmc.R")
source("Inner_imputation.R")

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
                              method = c("Polynomial", 
                                         "Spline",
                                         "GridSearchKnot",
                                         "TermSearch"),
                              TermSearch_knots = 0:(latency - 1),
                              missingness_method) {
  plan("multicore")
  future_map_dfr(1:100, .f = function(sim_id){
    # Generate data
    sim_data <- generate_data(sample_size, beta = 0.01, rho = 0.3, lambda = 0.5,  
                              alpha1 = alpha1, alpha2 =alpha2, alpha3 = alpha3, alpha4 = alpha4, knot = knot,
                              lambda_0 = lambda_0, gamma = gamma)
    
    
# this section will consider missingness method
    if (missingness_method == "imputation_0") {
      sim_data <- sim_data %>%
        dplyr::mutate(lag1 = ifelse(is.na(lag1), 0, lag1),
                      lag2 = ifelse(is.na(lag2), 0, lag2),
                      lag3 = ifelse(is.na(lag3), 0, lag3),
                      lag4 = ifelse(is.na(lag4), 0, lag4),
                      lag5 = ifelse(is.na(lag5), 0, lag5),
                      lag6 = ifelse(is.na(lag6), 0, lag6),
                      lag7 = ifelse(is.na(lag7), 0, lag7),
                      lag8 = ifelse(is.na(lag8), 0, lag8),
                      lag9 = ifelse(is.na(lag9), 0, lag9),
                      lag10 = ifelse(is.na(lag10), 0, lag10),
                      lag11 = ifelse(is.na(lag11), 0, lag11),
                      lag12 = ifelse(is.na(lag12), 0, lag12),
                      lag13 = ifelse(is.na(lag13), 0, lag13),
                      lag14 = ifelse(is.na(lag14), 0, lag14),
                      lag15 = ifelse(is.na(lag15), 0, lag15))
    } else if(missingness_method == "compete-case") {
      sim_data <- sim_data[complete.cases(sim_data),]
    } else if(missingness_method == "inner_imputation_complete-case") {
      log_HR_true <- log_HR_estimate <- matrix(NA, nrow = 10, ncol = latency)
      
      for (i in 1:5) {
        tmp <- inner_imputation(sim_data)
        tmp <- as.data.frame(tmp)
        fit <-  CoxKnotsearch(tmp, time_start = "age_start", time_end ="age_end", status = "failure", 
                              exposure = paste0("lag", 0:(latency - 1)),
                              knots_number = c(3,4,5),
                              latency = latency)
        for (l in 1:latency) {
          log_HR_estimate[i, l] <- extract_CoxKnotsearch(fit, lag = l - 1, latency = latency)
          log_HR_true[i, l] <- gamma * alpha_function(l-1, knot = knot)
        }
      }
      # take the average of ten imputation results
      log_HR_estimate <- colMeans(log_HR_estimate)
      log_HR_true <- colMeans(log_HR_true)
    } else if(missingness_method == "multiple_imputation_KnotSearch") {
      log_HR_true <- log_HR_estimate <- matrix(NA, nrow = 5, ncol = latency)
      
      for (i in 1:5) {
        tmp <- imputation_glmer(sim_data)
        tmp <- as.data.frame(tmp)
        fit <-  CoxKnotsearch(tmp, time_start = "age_start", time_end ="age_end", status = "failure", 
                              exposure = paste0("lag", 0:(latency - 1)),
                              knots_number = c(3,4,5),
                              latency = latency)
        for (l in 1:latency) {
          log_HR_estimate[i, l] <- extract_CoxKnotsearch(fit, lag = l - 1, latency = latency)
          log_HR_true[i, l] <- gamma * alpha_function(l-1, knot = knot)
        }
      }
      # take the average of ten imputation results
      log_HR_estimate <- colMeans(log_HR_estimate)
      log_HR_true <- colMeans(log_HR_true)
    } else if(missingness_method == "multiple_imputation_mcmc_KnotSearch") {
      log_HR_true <- log_HR_estimate <- matrix(NA, nrow = 5, ncol = latency)
      
      for (i in 1:5) {
        tmp <- imputation_mcmc(sim_data)
        tmp <- as.data.frame(tmp)
        fit <-  CoxKnotsearch(tmp, time_start = "age_start", time_end ="age_end", status = "failure", 
                              exposure = paste0("lag", 0:(latency - 1)),
                              knots_number = c(3,4,5),
                              latency = latency)
        for (l in 1:latency) {
          log_HR_estimate[i, l] <- extract_CoxKnotsearch(fit, lag = l - 1, latency = latency)
          log_HR_true[i, l] <- gamma * alpha_function(l-1, knot = knot)
        }
      }
      # take the average of ten imputation results
      log_HR_estimate <- colMeans(log_HR_estimate)
      log_HR_true <- colMeans(log_HR_true)
    }

    
    sim_data <- as.data.frame(sim_data)
    # then consider the analysis options
    
    if (method == "Polynomial") {
      fit <- CoxPoly(sim_data, time_start = "age_start", time_end ="age_end", status = "failure", 
                     exposure = paste0("lag", 0:(latency - 1)),
                     degree = degree-1,
                     latency = latency)
      log_HR_true <- log_HR_estimate <- numeric(latency)
      for (l in 1:latency) {
        log_HR_estimate[l] <- extract_CoxPoly(fit, lag = l - 1)
        log_HR_true[l] <- gamma * alpha_function(l-1, knot = knot)
      }
      
    } else if (method == "NCSpline") {
      fit <- CoxNCSpline(sim_data, time_start = "age_start", time_end ="age_end", status = "failure", 
                         exposure = paste0("lag", 0:(latency - 1)),
                         knots_number = degree,
                         latency = latency)
      log_HR_true <- log_HR_estimate <- numeric(latency)
      for (l in 1:latency) {
        log_HR_estimate[l] <- extract_CoxNCSpline(fit, lag = l - 1, latency = latency)
        log_HR_true[l] <- gamma * alpha_function(l-1, knot = knot)
      }
      
    } else if (method == "GridSearchKnot") {
      fit <- CoxKnotsearch(sim_data, time_start = "age_start", time_end ="age_end", status = "failure", 
                           exposure = paste0("lag", 0:(latency - 1)),
                           knots_number = c(3,4,5),
                           latency = latency)
      log_HR_true <- log_HR_estimate <- numeric(latency)
      for (l in 1:latency) {
        log_HR_estimate[l] <- extract_CoxKnotsearch(fit, lag = l - 1, latency = latency)
        log_HR_true[l] <- gamma * alpha_function(l-1, knot = knot)
      }
    } else if (method == "TermSearch") {
      fit <- CoxTermsearch(sim_data, time_start = "age_start", time_end ="age_end", status = "failure", 
                           exposure = paste0("lag", 0:(latency -1)),
                           knots= TermSearch_knots,
                           latency = latency)
      log_HR_true <- log_HR_estimate <- numeric(latency)
      for (l in 1:latency) {
        log_HR_estimate[l] <- extract_CoxTermsearch(fit, lag = l - 1, latency = latency, knots =TermSearch_knots)
        log_HR_true[l] <- gamma * alpha_function(l-1, knot = knot)
      }
    }
    
    
    data.frame(sim_id = sim_id,
               l = 0:(latency -1),
               log_HR_true = log_HR_true,
               log_HR_estimate = log_HR_estimate) %>% return()
  }, .options = furrr_options(seed = T))
  
}





