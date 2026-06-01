setwd(tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
))
source("simulate_personetime.R")
source("simulation_output.R")

library(future)
library(furrr)
library(LatencyA)

# simulate for polynomials with different knot and degree
temp <- data.frame()

for (B in c(-1, 1)){
  for (degree in c(3, 4, 5)) {
    result <- simulation_output(sample_size = 5000, B = B, degree = degree, method = "NCSpline")
    result$B <- B
    result$degree <- degree
    temp <- rbind(temp, result)
  }
}
temp$method <- "NCSpline"
saveRDS(temp, "simulation_NCSpline.rds")

Sys.time()
