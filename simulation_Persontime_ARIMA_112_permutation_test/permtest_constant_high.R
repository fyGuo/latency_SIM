# ── Self-locating working directory ───────────────────────────────────────────
local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (!length(f)) { i <- which(a == "-f"); if (length(i)) f <- a[i + 1L] }
  if (!length(f) && requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable())
    f <- rstudioapi::getSourceEditorContext()$path
  if (length(f) && nzchar(f)) setwd(dirname(normalizePath(f)))
})
source("weight_curves.R")
source("permutation_study.R")

# ── Part selection ─────────────────────────────────────────────────────────────
# The 300 replicates are split into 6 independent jobs of 50 sims each, so a
# crashed job only loses its own part. Submit with `sbatch --array=1-6`, or set
# PART=k manually; defaults to part 1.
part          <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID",
                                       Sys.getenv("PART", "1")))
n_parts       <- 6
sims_per_part <- 50
stopifnot(part >= 1, part <= n_parts)

# Permutation test of H0 (weights constant across lags) under the constant (high level, NULL) curve.
# n = 5000, gamma = 0.1, knot-search RCspline, 300 permutations per replicate.
# Tune sample_size / n_sims / n_perm / intercept for your compute budget.
set.seed(600 + part)   # distinct seed per part (and per scenario)
result <- permutation_study(WEIGHT_CURVES$constant_high, "constant_high",
                            sample_size = 5000,
                            gamma       = 0.1,
                            lambda_0    = 0.5,
                            intercept   = 0,
                            n_sims      = sims_per_part,
                            n_perm      = 300,
                            alpha       = 0.05,
                            parallel    = TRUE)

# Make sim_id unique across parts (part 1 -> 1:50, part 2 -> 51:100, ...)
result$sim_id <- result$sim_id + (part - 1L) * sims_per_part

saveRDS(result, sprintf("permtest_constant_high_part%d.rds", part))

cat(sprintf("constant_high part %d / %d: rejection rate = %.3f  (mean events = %.1f)\n",
            part, n_parts,
            mean(result$reject, na.rm = TRUE), mean(result$n_events, na.rm = TRUE)))
Sys.time()
