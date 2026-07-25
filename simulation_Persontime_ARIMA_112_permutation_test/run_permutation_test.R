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
source("simulate_personetime.R")
source("weight_curves.R")
source("permutation_test.R")

library(future)
library(furrr)
library(LatencyA)

# Demo: run the permutation test once per scenario on a single data set.
# (Full type-I-error / power study over many replicates to follow.)
exposure <- paste0("lag", 0:(LATENCY - 1))

set.seed(1)
for (scenario in names(WEIGHT_CURVES)) {
  dat <- generate_data(2000, weights = WEIGHT_CURVES[[scenario]],
                       gamma = 0.1, lambda_0 = 0.5, intercept = 0)
  res <- permutation_test(dat, exposure = exposure, latency = LATENCY,
                          n_perm = 300, alpha = 0.05, parallel = TRUE)
  cat(sprintf("%-14s (null=%-5s): AIC_obs = %8.1f | 5%% thresh = %8.1f | reject = %-5s | p = %.3f\n",
              scenario, WEIGHT_IS_NULL[scenario],
              res$aic_obs, res$threshold, res$reject, res$p_value))
}

Sys.time()
