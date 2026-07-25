setwd(tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
))

source("simulate_personetime.R")
source("simulation_output.R")

library(future)
library(furrr)
library(LatencyA)

temp <- data.frame()

for (B_val in c(0.4)) {
  for (mis_latency in c(10, 13, 16, 19, 22, 25)) {
    result         <- simulation_output(sample_size = 5000, B = B_val,
                                        method = "GridSearchKnot", latency = mis_latency)
    result$latency <- mis_latency
    result$B       <- B_val
    temp           <- rbind(temp, result)
  }
}

temp$method <- "GridSearchKnot"
saveRDS(temp, "simulation_KnotGridSearch.rds")

Sys.time()
