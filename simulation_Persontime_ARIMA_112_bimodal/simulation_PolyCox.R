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

# Two-machine cluster: 11 local cores + 8 cores in Docker on the other Mac.
# "r-worker" is resolved via ~/.ssh/config (port 2222, user root, key auth).
local_workers  <- parallel::detectCores() - 1   # 11
remote_workers <- 4                              # M1 MacBook Air (T8103)

cl <- parallelly::makeClusterPSOCK(
  workers = c(
    rep("localhost", local_workers),
    rep("r-worker",  remote_workers)
  ),
  homogeneous = FALSE,          # local = macOS R, remote = Linux Docker R
  rscript      = "/usr/local/bin/Rscript",
  verbose      = TRUE
)
plan(cluster, workers = cl)
on.exit(parallel::stopCluster(cl), add = TRUE)
# if in the usual R session, you can just use plan(multisession).


tic()
# simulate for polynomials with different knot and degree
temp <- data.frame()

for (B in c(-1, 1)){
  for (degree in c(5)) {
    result <- simulation_output(sample_size = 5000, B = B, degree = degree, method = "Polynomial")
    result$B <- B
    result$degree <- degree
    temp <- rbind(temp, result)
  }
}
temp$method <- "Polynomial"
saveRDS(temp, "simulation_Polynomial.rds")

Sys.time()
toc()