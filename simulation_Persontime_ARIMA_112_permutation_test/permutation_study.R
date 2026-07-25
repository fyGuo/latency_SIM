# ── Replicate driver for the permutation test ─────────────────────────────────
# For one true weighting curve, generate `n_sims` data sets, run the AIC
# permutation test on each, and return a per-replicate data.frame. The rejection
# rate over replicates estimates the type-I error (for the constant/null curves)
# or the power (for the lag-varying curves).
source("simulate_personetime.R")
source("permutation_test.R")

library(future)
library(furrr)
library(LatencyA)

permutation_study <- function(weights, scenario,
                              sample_size  = 5000,
                              gamma        = 0.1,
                              lambda_0     = 0.5,
                              intercept    = 0,
                              latency      = 16,
                              n_sims       = 300,
                              n_perm       = 300,
                              alpha        = 0.05,
                              knots_number = c(3, 4, 5),
                              criteria     = "AIC",
                              parallel     = TRUE) {

  exposure <- paste0("lag", 0:(latency - 1))
  out <- vector("list", n_sims)

  for (s in seq_len(n_sims)) {
    dat <- generate_data(sample_size, weights = weights, gamma = gamma,
                         lambda_0 = lambda_0, intercept = intercept, latency = latency)

    res <- tryCatch(
      permutation_test(dat, exposure = exposure, latency = latency,
                       knots_number = knots_number, criteria = criteria,
                       n_perm = n_perm, alpha = alpha, parallel = parallel),
      error = function(e) {
        message(sprintf("[%s] sim %d failed: %s", scenario, s, conditionMessage(e)))
        NULL
      })

    out[[s]] <- data.frame(
      scenario  = scenario,
      sim_id    = s,
      n_events  = sum(dat$failure),
      aic_obs   = if (is.null(res)) NA_real_ else res$aic_obs,
      threshold = if (is.null(res)) NA_real_ else res$threshold,
      p_value   = if (is.null(res)) NA_real_ else res$p_value,
      reject    = if (is.null(res)) NA      else res$reject)

    if (s %% 5 == 0)
      cat(sprintf("[%s] %d / %d done | running reject rate = %.3f\n",
                  scenario, s, n_sims,
                  mean(vapply(out[seq_len(s)], `[[`, logical(1), "reject"), na.rm = TRUE)))
  }

  do.call(rbind, out)
}
