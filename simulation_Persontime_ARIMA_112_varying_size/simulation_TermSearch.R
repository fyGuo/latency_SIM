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

# Parallel backend: set once here at top level. Calling plan() inside a
# function body breaks future's environment walk (NULL env in the chain).
parallel_backend <- if (Sys.info()[["sysname"]] == "Darwin") "multisession" else "multicore"
plan(parallel_backend)

# simulate for TermSearch with knot = 10, varying sample size
temp <- data.frame()

for (sample_size in c(300, 500, 1000, 3000, 5000)){
    result <- simulation_output(sample_size = sample_size, knot = 10, method  = "TermSearch")
    result$sample_size <- sample_size
    cat(sprintf("sample_size = %d: average number of events = %.1f\n",
                sample_size, mean(result$n_events, na.rm = TRUE)))
    temp <- rbind(temp, result)
    sprintf("Finished sample size = %d\n", sample_size)

}

temp$method <- "TermSearch"
saveRDS(temp, "simulation_TermSearch.rds")

Sys.time()
