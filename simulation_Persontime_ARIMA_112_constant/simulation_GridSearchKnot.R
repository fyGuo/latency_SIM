setwd(tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
))
source("simulate_personetime.R")
source("simulation_output.R")

library(future)
library(furrr)
library(parallelly)
library(LatencyA)
library(tictoc)

local_workers  <- parallel::detectCores() - 1
remote_workers <- 4

cl <- parallelly::makeClusterPSOCK(
  workers = c(
    rep("localhost", local_workers),
    rep("r-worker",  remote_workers)
  ),
  homogeneous = FALSE,
  rscript      = "/usr/local/bin/Rscript",
  verbose      = TRUE
)
plan(cluster, workers = cl)
on.exit(parallel::stopCluster(cl), add = TRUE)

tic()
# simulate for KnotGridSearch with different weight settings
# positive = FALSE: all weights = 0 (null effect)
# positive = TRUE:  all weights = A (constant positive effect)
temp <- data.frame()

for (positive in c(FALSE, TRUE)){
    result <- simulation_output(sample_size = 5000, positive = positive, method = "GridSearchKnot")
    result$positive <- positive
    temp <- rbind(temp, result)
}

temp$method <- "GridSearchKnot"
saveRDS(temp, "simulation_KnotGridSearch.rds")

Sys.time()
toc()
