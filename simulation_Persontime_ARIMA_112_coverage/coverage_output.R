# ── Bootstrap-percentile coverage of the latency-risk curve ───────────────────
# Generalised over (a) the true weighting SHAPE (any curve from weight_curves.R)
# and (b) several time lags assessed at once.
#
# For each replicate we:
#   1. generate one data set under the supplied true weighting curve;
#   2. fit the chosen estimator ONCE on the original data to read the point
#      estimate at each requested lag (log_HR_estimate);
#   3. cluster-bootstrap (ids WITH replacement) `boot_iter` times, extracting
#      ALL requested lags from each resampled fit so the lags share one set of
#      resamples (≈ |lags|× cheaper than one bootstrap per lag).
#
# Per replicate × lag we record:
#   log_HR_true     truth gamma * weight(lag)
#   log_HR_estimate point estimate from the original-data fit  (NEW: stored so the
#                   empirical/"true" sampling interval can be computed exactly as
#                   the 2.5/97.5 percentiles of these across replicates)
#   boot_median     median of the bootstrap estimates
#   lo, hi          2.5 / 97.5 bootstrap percentiles (the estimated interval)
#   covered         whether [lo, hi] contains the truth
source("simulate_personetime.R")
source("weight_curves.R")

library(future)
library(furrr)
library(LatencyA)

# Corrected cluster (id-level) bootstrap: draw ids WITH replacement and give each
# drawn copy a fresh unique id so the Cox partial likelihood treats the duplicates
# as independent clusters. (Same logic as LatencyA's fixed resample_clusters; kept
# local so this script does not depend on a package internal.)
resample_clusters <- function(data, id = "id") {
  ids   <- unique(data[[id]])
  draw  <- sample(ids, length(ids), replace = TRUE)
  by_id <- split(seq_len(nrow(data)), data[[id]])
  parts <- vector("list", length(draw))
  for (j in seq_along(draw)) {
    rows       <- data[by_id[[as.character(draw[j])]], , drop = FALSE]
    rows[[id]] <- j
    parts[[j]] <- rows
  }
  do.call(rbind, parts)
}

coverage_output <- function(method       = c("Knotsearch", "Termsearch"),
                            weights,
                            scenario     = NA_character_,
                            sample_size  = 5000,
                            gamma        = 0.1,
                            lambda_0     = 0.5,
                            intercept    = 0,
                            latency      = 16,
                            lags         = c(1, 8, 15),
                            knots_number = c(3, 4, 5),
                            term_knots   = 0:(latency - 1),
                            n_sims       = 300,
                            boot_iter    = 300,
                            id           = "id",
                            criteria     = "AIC",
                            parallel     = TRUE) {
  method <- match.arg(method)
  stopifnot(length(weights) == latency, all(lags >= 0), all(lags < latency))

  exposure   <- paste0("lag", 0:(latency - 1))
  time_start <- "age_start"; time_end <- "age_end"; status <- "failure"
  true_lag   <- as.numeric(gamma * weights[lags + 1L])   # truth at each lag

  # Fit `method` once and return the log-HR at every lag in `lags`.
  fit_lags <- function(data) {
    if (method == "Knotsearch") {
      fit <- CoxKnotsearch(data, time_start, time_end, status, exposure,
                           knots_number = knots_number, latency = latency,
                           criteria = criteria)
      vapply(lags, function(l) as.numeric(extract_CoxKnotsearch(fit, l, latency)),
             numeric(1))
    } else {
      fit <- CoxTermsearch(data, time_start, time_end, status, exposure,
                           knots = term_knots, latency = latency,
                           criteria = criteria)
      vapply(lags, function(l) as.numeric(extract_CoxTermsearch(fit, l, latency, term_knots)),
             numeric(1))
    }
  }

  out <- vector("list", n_sims)
  for (s in seq_len(n_sims)) {
    dat <- generate_data(sample_size, weights = weights, gamma = gamma,
                         lambda_0 = lambda_0, intercept = intercept, latency = latency)

    # Point estimate on the original data (one fit, all lags).
    est_obs <- tryCatch(fit_lags(dat),
                        error = function(e) rep(NA_real_, length(lags)))

    # Cluster bootstrap: one resample -> one fit -> all lags.
    one_boot <- function(i)
      tryCatch(fit_lags(resample_clusters(dat, id)),
               error = function(e) rep(NA_real_, length(lags)))

    boot <- if (parallel) {
      future::plan("multicore")
      furrr::future_map(seq_len(boot_iter), one_boot,
                        .options = furrr::furrr_options(seed = TRUE))
    } else {
      lapply(seq_len(boot_iter), one_boot)
    }
    boot_mat <- do.call(rbind, boot)                 # boot_iter x length(lags)

    lo  <- apply(boot_mat, 2, quantile, probs = 0.025, na.rm = TRUE)
    hi  <- apply(boot_mat, 2, quantile, probs = 0.975, na.rm = TRUE)
    bmd <- apply(boot_mat, 2, median, na.rm = TRUE)

    out[[s]] <- data.frame(
      scenario        = scenario,
      method          = method,
      sim_id          = s,
      lag             = lags,
      n_events        = sum(dat$failure),
      log_HR_true     = true_lag,
      log_HR_estimate = est_obs,
      boot_median     = as.numeric(bmd),
      lo              = as.numeric(lo),
      hi              = as.numeric(hi),
      covered         = true_lag >= lo & true_lag <= hi)

    if (s %% 10 == 0)
      cat(sprintf("[%s/%s] finished %d / %d replicates\n", method, scenario, s, n_sims))
  }

  do.call(rbind, out)
}
