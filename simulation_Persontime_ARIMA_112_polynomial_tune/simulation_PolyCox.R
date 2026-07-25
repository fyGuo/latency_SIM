setwd(tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
))

source("simulate_personetime.R")
source("simulation_output.R")

library(future)
library(furrr)
library(LatencyA)

# Parallel backend: set once here at top level. Calling plan() inside a
# function body breaks future's environment walk (NULL env in the chain).
parallel_backend <- if (Sys.info()[["sysname"]] == "Darwin") "multisession" else "multicore"
plan(parallel_backend)

# Four underlying curve shapes x polynomial degrees 3-5
curve_scenarios <- list(
  list(curve = "unimodal", A = 0.3, B = 0.4,  knot = 10, label = "Unimodal"),
  list(curve = "bimodal",  A = 0.3, B = -1,   knot = 10, label = "Bimodal (B=-1)"),
  list(curve = "bimodal",  A = 0.3, B =  1,   knot = 10, label = "Bimodal (B=1)"),
  list(curve = "decayed",  A = 0.3, B = 0.4,  knot = 10, label = "Decayed")
)

temp <- data.frame()

for (s in curve_scenarios) {
  for (degree in 3:5) {
    result <- simulation_output(sample_size = 5000,
                                curve  = s$curve,
                                A      = s$A,
                                B      = s$B,
                                knot   = s$knot,
                                degree = degree)
    result$curve_label <- s$label
    result$degree      <- degree
    temp <- rbind(temp, result)
  }
}

temp$method <- "Polynomial"
saveRDS(temp, "simulation_Polynomial.rds")

Sys.time()
