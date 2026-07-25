# please change the following directory to your own directory
# setwd("/n/home00/fyguo/proj_spline/simulation_PersonTime")
setwd("~/proj_spline/simulation_Persontime_ARIMA_112")
source("simulate_personetime.R")
source("simulation_output.R")

library(future)
library(furrr)
library(LatencyA)

# Parallel backend: set once here at top level. Calling plan() inside a
# function body breaks future's environment walk (NULL env in the chain).
parallel_backend <- if (Sys.info()[["sysname"]] == "Darwin") "multisession" else "multicore"
plan(parallel_backend)

# simulate for polynomials with different knot and degree
temp <- data.frame()

for (knot in c(5, 10, 15)){
  for (degree in c(3, 4, 5)) {
    result <- simulation_output(sample_size = 5000, knot = knot, degree = degree, method = "NCSpline")
    result$knot <- knot
    result$degree <- degree
    temp <- rbind(temp, result)
  }
}
temp$method <- "NCSpline"
saveRDS(temp, "simulation_NCSpline.rds")

Sys.time()
