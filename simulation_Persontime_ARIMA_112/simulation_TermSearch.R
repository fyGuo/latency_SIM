# please change the following directory to your own directory
# setwd("/n/home00/fyguo/proj_spline/simulation_PersonTime")
setwd("~/proj_spline/simulation_Persontime_ARIMA_112")
source("simulate_personetime.R")
source("simulation_output.R")

library(future)
library(furrr)
library(LatencyA)

# simulate for KnotGridSearch with different knot
temp <- data.frame()

for (knot in c(5, 10, 15)){
    result <- simulation_output(sample_size = 5000, knot = knot, method  = "TermSearch")
    result$knot <- knot
    temp <- rbind(temp, result)
}

temp$method <- "TermSearch"
saveRDS(temp, "simulation_TermSearch.rds")

Sys.time()
