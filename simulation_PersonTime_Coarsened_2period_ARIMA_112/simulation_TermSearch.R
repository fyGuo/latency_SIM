setwd("~/proj_spline/simulation_PersonTime_Coarsened_2period_ARIMA_112")
source("simulate_personetime.R")
source("simulation_output.R")

library(future)
library(furrr)
library(LatencyA)



# simulate for TermSearch
temp <- data.frame()

set.seed(123)

for (knot in c(5, 10, 15)) {
  result <- simulation_output(sample_size = 5000, knot = knot, method  ="TermSearch", latency = 20 )
  result$knot <- knot
  temp <- rbind(temp, result)
}

temp$method <- "TermSearch"
saveRDS(temp, "simulation_TermSearch.rds")

Sys.time()



