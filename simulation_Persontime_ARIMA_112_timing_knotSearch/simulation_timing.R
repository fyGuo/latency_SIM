source("simulate_personetime.R")
library(LatencyA)

# Run `method` on ONE fixed dataset n_reps times and return per-rep elapsed
# wall-clock times (seconds).  Keeping the data fixed isolates algorithm cost
# from data-generation cost.
simulation_timing <- function(sample_size,
                               method,
                               degree    = NULL,
                               n_reps    = 300,
                               knot      = 10,
                               latency   = 16) {

  sim_data <- generate_data(sample_size, knot = knot)

  times <- numeric(n_reps)
  for (rep in seq_len(n_reps)) {
    t0 <- proc.time()["elapsed"]
    tryCatch({
      if (method == "GridSearchKnot") {
        CoxKnotsearch(sim_data,
                      time_start   = "age_start",
                      time_end     = "age_end",
                      status       = "failure",
                      exposure     = paste0("lag", 0:(latency - 1)),
                      knots_number = c(3, 4, 5),
                      latency      = latency)
      } else if (method == "NCSpline") {
        CoxNCSpline(sim_data,
                    time_start   = "age_start",
                    time_end     = "age_end",
                    status       = "failure",
                    exposure     = paste0("lag", 0:(latency - 1)),
                    knots_number = degree,
                    latency      = latency)
      }
    }, error = function(e) {
      message(sprintf("rep %d failed: %s", rep, conditionMessage(e)))
    })
    times[rep] <- proc.time()["elapsed"] - t0
  }

  data.frame(
    rep         = seq_len(n_reps),
    elapsed     = times,
    sample_size = sample_size,
    method      = method,
    degree      = if (is.null(degree)) NA_integer_ else as.integer(degree)
  )
}
