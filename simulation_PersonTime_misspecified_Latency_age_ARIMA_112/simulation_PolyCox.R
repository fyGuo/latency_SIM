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

for (knot_val in 10) {
  for (mis_latency in c(10, 13, 16, 19, 22, 25)) {
    result <- tryCatch({
      r         <- simulation_output(sample_size = 5000, knot = knot_val,
                                     degree = 5, method = "Polynomial",
                                     latency = mis_latency)
      r$latency <- mis_latency
      r$knot    <- knot_val
      r$degree  <- 5
      r
    }, error = function(e) {
      message("Skipping knot=", knot_val, ", latency=", mis_latency,
              " due to error: ", conditionMessage(e))
      NULL
    })
    if (!is.null(result) && nrow(result) > 0) {
      temp <- rbind(temp, result)
    }
  }
}

temp$method <- "Polynomial"
saveRDS(temp, "simulation_Polynomial.rds")

Sys.time()
