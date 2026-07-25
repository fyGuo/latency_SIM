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

# simulate for KnotGridSearch with different knot
temp <- data.frame()

for (knot in c(5, 10, 15)){
    result <- simulation_output(sample_size = 5000, knot = knot, method  = "GridSearchKnot")
    result$knot <- knot
    temp <- rbind(temp, result)
}

temp$method <- "GridSearchKnot"
saveRDS(temp, "simulation_KnotGridSearch.rds")

Sys.time()
