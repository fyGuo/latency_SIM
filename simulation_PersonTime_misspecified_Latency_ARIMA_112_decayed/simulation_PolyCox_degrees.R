setwd(tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
))
source("simulate_personetime.R")

library(future)
library(furrr)
library(LatencyA)

parallel_backend <- if (Sys.info()[["sysname"]] == "Darwin") "multisession" else "multicore"
plan(parallel_backend)

# Polynomial CoxPoly at df = 3, 4, 5 under the bimodal true curve. To avoid
# regenerating data three times, each replicate generates ONE data set and fits
# all three degrees on it (so degrees share the same datasets). Failed replicates
# return all-NA rows; failed (B, latency) settings are skipped.
DEGREES <- c(3, 4, 5)

poly_degrees <- function(sample_size = 5000, A = 0.3, B = -1,
                         lambda_0 = 0.5, gamma = 0.1, latency = 16,
                         degrees = DEGREES, n_sims = 300) {
  exposure <- paste0("lag", 0:(latency - 1))
  na_block <- do.call(rbind, lapply(degrees, function(dg)
    data.frame(l = 0:(latency - 1),
               log_HR_true     = gamma * alpha_function(0:(latency - 1), A = A, B = B),
               log_HR_estimate = NA_real_, degree = dg)))

  future_map_dfr(1:n_sims, .f = function(sim_id) {
    res <- tryCatch({
      sim_data <- generate_data(sample_size, A = A, B = B,
                                lambda_0 = lambda_0, gamma = gamma)
      do.call(rbind, lapply(degrees, function(dg) {
        fit <- CoxPoly(sim_data, time_start = "age_start", time_end = "age_end",
                       status = "failure", exposure = exposure,
                       degree = dg - 1, latency = latency)
        est <- vapply(1:latency, function(l)
          tryCatch(extract_CoxPoly(fit, lag = l - 1)$log_HR, error = function(e) NA_real_),
          numeric(1))
        data.frame(l = 0:(latency - 1),
                   log_HR_true     = gamma * alpha_function(0:(latency - 1), A = A, B = B),
                   log_HR_estimate = est, degree = dg)
      }))
    }, error = function(e) na_block)
    cbind(sim_id = sim_id, res)
  }, .options = furrr_options(seed = TRUE, scheduling = Inf))
}

temp <- data.frame()
for (B_val in c(0.2, 0.4)) {
  for (mis_latency in c(10, 13, 16, 19, 22, 25)) {
    result <- tryCatch({
      r         <- poly_degrees(B = B_val, latency = mis_latency)
      r$latency <- mis_latency
      r$B       <- B_val
      r
    }, error = function(e) {
      message("Skipping B=", B_val, ", latency=", mis_latency, ": ", conditionMessage(e))
      NULL
    })
    if (!is.null(result) && nrow(result) > 0) temp <- rbind(temp, result)
    cat(sprintf("done B=%s latency=%d\n", B_val, mis_latency))
  }
}

temp$method <- "Polynomial"
saveRDS(temp, "simulation_Polynomial.rds")
Sys.time()
