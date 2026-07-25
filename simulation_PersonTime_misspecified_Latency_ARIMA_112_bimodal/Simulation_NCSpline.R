setwd(tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
))

source("simulate_personetime.R")
source("simulation_output.R")

library(future)
library(furrr)
library(LatencyA)
library(dplyr)

temp <- data.frame()

for (mis_latency in c(10, 13, 16, 19, 22, 25)) {
  result <- simulation_output(sample_size = 5000, degree = 5, method = "NCSpline",
                              latency = mis_latency)
  if (nrow(result) == 0) {
    result <- data.frame(sim_id = NA, l = NA, log_HR_true = NA, log_HR_estimate = NA)
  }
  result$latency <- mis_latency
  temp <- rbind(temp, result)
}

temp$method  <- "Spline"
saveRDS(temp, "simulation_spline.rds")

Sys.time()
