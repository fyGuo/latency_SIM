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
# simulate for TermSearch with different knot
temp <- data.frame()

for (B in c(-1, 1)){
    result <- simulation_output(sample_size = 5000, B = B, method = "TermSearch")
    result$B <- B
    temp <- rbind(temp, result)
}

temp$method <- "TermSearch"
saveRDS(temp, "simulation_TermSearch.rds")

Sys.time()
toc()
