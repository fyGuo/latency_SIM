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

# simulate for NCSpline with sample_size = 5000, knot = 10, varying gamma and degree
temp <- data.frame()

for (gamma in c(0.01, 0.05, 0.1)){
  for (degree in c(3, 4, 5)) {
    result <- simulation_output(sample_size = 5000, knot = 10, gamma = gamma, degree = degree, method = "NCSpline")
    result$gamma <- gamma
    result$degree <- degree
    cat(sprintf("gamma = %.2f, degree = %d: average number of events = %.1f\n",
                gamma, degree, mean(result$n_events, na.rm = TRUE)))
    temp <- rbind(temp, result)
  }
}
temp$method <- "NCSpline"
saveRDS(temp, "simulation_NCSpline.rds")

Sys.time()
