setwd("/n/home00/fyguo/proj_spline/simulation_PersonTime_misspecified_Latency_age_ARIMA_112")
source("simulate_personetime.R")
source("simulation_output.R")

library(future)
library(furrr)
library(LatencyA)



# simulate for TermSearch
temp <- data.frame()

knot = 10
for (mis_latency in c(10, 13, 16, 19, 22, 25)) {
  result <- simulation_output(sample_size = 5000, knot = knot, method  ="TermSearch", latency = mis_latency )
  result$latency <- mis_latency
  temp <- rbind(temp, result)
}

temp$method <- "TermSearch"
saveRDS(temp, "simulation_TermSearch.rds")

Sys.time()



