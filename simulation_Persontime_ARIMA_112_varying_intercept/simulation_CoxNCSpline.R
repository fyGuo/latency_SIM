# ── Self-locating working directory ───────────────────────────────────────────
# Set the working directory to the folder this script lives in, so the project
# can be copied to another machine without editing any path. Works whether the
# script is run with Rscript, R CMD BATCH, or "Source"d in RStudio.
local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))               # Rscript
  if (!length(f)) { i <- which(a == "-f"); if (length(i)) f <- a[i + 1L] }  # R CMD BATCH
  if (!length(f) && requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable())
    f <- rstudioapi::getSourceEditorContext()$path                          # RStudio
  if (length(f) && nzchar(f)) setwd(dirname(normalizePath(f)))
})
source("simulate_personetime.R")
source("simulation_output.R")

library(future)
library(furrr)
library(LatencyA)

plan("multicore")

# simulate for NCSpline with sample_size = 5000, knot = 10, varying intercept and degree
temp <- data.frame()

for (intercept in c(-4, -2.5, -1.5, 0)){
  for (degree in c(3, 4, 5)) {
    result <- simulation_output(sample_size = 5000, knot = 10, intercept = intercept, degree = degree, method = "NCSpline")
    result$intercept <- intercept
    result$degree <- degree
    n_fail <- sum(result$fit_failed[!duplicated(result$sim_id)], na.rm = TRUE)
    cat(sprintf("intercept = %g, degree = %d: average number of events = %.1f | failed fits = %d/300\n",
                intercept, degree, mean(result$n_events, na.rm = TRUE), n_fail))
    temp <- rbind(temp, result)
  }
}
temp$method <- "NCSpline"
saveRDS(temp, "simulation_NCSpline.rds")

Sys.time()
