setwd(tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
))
source("simulate_personetime.R")
source("simulation_output.R")

library(future)
library(furrr)
library(LatencyA)

# simulate for NCSpline with different weight settings
# positive = FALSE: all weights = 0 (null effect)
# positive = TRUE:  all weights = A (constant positive effect)
temp <- data.frame()

for (positive in c(FALSE, TRUE)){
  for (degree in c(3, 4, 5)) {
    result <- simulation_output(sample_size = 5000, positive = positive, degree = degree, method = "NCSpline")
    result$positive <- positive
    result$degree <- degree
    temp <- rbind(temp, result)
  }
}
temp$method <- "NCSpline"
saveRDS(temp, "simulation_NCSpline.rds")

Sys.time()
