# ── Self-locating working directory ───────────────────────────────────────────
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
source("coverage_output.R")

library(future)
library(furrr)
library(LatencyA)

# ── Part selection ─────────────────────────────────────────────────────────────
# The 300 replicates are split into 5 independent jobs of 60 sims each, so a
# crashed job only loses its own part. Submit with `sbatch --array=1-5`, or set
# PART=k manually; defaults to part 1.
part          <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID",
                                       Sys.getenv("PART", "1")))
n_parts       <- 5
sims_per_part <- 60
stopifnot(part >= 1, part <= n_parts)

# 95% bootstrap-percentile coverage of Knot-search at lag 8
# (n = 5000, knot = 10, gamma = 0.1).
set.seed(100 + part)   # distinct seed per part (and per method)
result <- coverage_output(method      = "Knotsearch",
                          sample_size = 5000,
                          knot        = 10,
                          gamma       = 0.1,
                          lags        = 8,
                          n_sims      = sims_per_part,
                          boot_iter   = 300)

# Make sim_id unique across parts (part 1 -> 1:60, part 2 -> 61:120, ...)
result$sim_id <- result$sim_id + (part - 1L) * sims_per_part

saveRDS(result, sprintf("coverage_Knotsearch_part%d.rds", part))

cat(sprintf("== Knot-search part %d / %d: 95%% percentile-interval coverage ==\n",
            part, n_parts))
print(aggregate(covered ~ lag, result, function(x) mean(x, na.rm = TRUE)))

Sys.time()
