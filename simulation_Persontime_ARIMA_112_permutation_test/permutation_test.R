# ── Permutation test: are the exposure weights the same across lags? ──────────
# H0: the latency weighting curve is CONSTANT across lags (no lag structure).
#
# Procedure (knot-search RCspline as the working model):
#   1. Fit CoxKnotsearch on the observed data and read its AIC (= AIC_obs).
#   2. Permute the lag columns of the exposure matrix `n_perm` times; refit each
#      time and collect the AIC -> null distribution.
#   3. Reject H0 if AIC_obs is below the lower `alpha` percentile of the null
#      distribution (a genuine lag structure lets the un-permuted fit reach a
#      lower AIC than the scrambled ones).

library(LatencyA)

# CoxKnotsearch returns list(knots_best, fit_best); read the fit's AIC/BIC.
knotsearch_aic <- function(fit, criteria = "AIC", n = NULL) {
  m <- fit[[2]]
  if (criteria == "BIC") extractAIC(m, k = log(n))[2] else extractAIC(m)[2]
}

permutation_test <- function(data,
                             exposure,
                             time_start   = "age_start",
                             time_end     = "age_end",
                             status       = "failure",
                             latency      = length(exposure),
                             knots_number = c(3, 4, 5),
                             criteria     = "AIC",
                             n_perm       = 300,
                             alpha        = 0.05,
                             parallel     = FALSE) {

  n_obs <- nrow(data)

  # 1. Observed fit + AIC.
  fit_obs <- CoxKnotsearch(data, time_start, time_end, status, exposure,
                           knots_number = knots_number, latency = latency,
                           criteria = criteria)
  aic_obs <- knotsearch_aic(fit_obs, criteria, n_obs)

  # 2. One permutation: shuffle which physical exposure series is treated as
  #    each lag, then refit and return the AIC.
  one_perm <- function(i) {
    perm <- sample(seq_along(exposure))
    perm_data <- data
    perm_data[, exposure] <- data[, exposure[perm]]
    fit <- tryCatch(
      CoxKnotsearch(perm_data, time_start, time_end, status, exposure,
                    knots_number = knots_number, latency = latency,
                    criteria = criteria),
      error = function(e) NULL)
    if (is.null(fit)) NA_real_ else knotsearch_aic(fit, criteria, n_obs)
  }

  if (parallel) {
    future::plan("multicore")
    aic_null <- furrr::future_map_dbl(seq_len(n_perm), one_perm,
                                      .options = furrr::furrr_options(seed = TRUE))
  } else {
    aic_null <- vapply(seq_len(n_perm), one_perm, numeric(1))
  }

  # 3. Decision: reject if observed AIC < lower-alpha percentile of the null.
  threshold <- as.numeric(quantile(aic_null, alpha, na.rm = TRUE))
  p_value   <- mean(aic_null <= aic_obs, na.rm = TRUE)   # P(null AIC <= observed)

  list(
    aic_obs   = aic_obs,
    aic_null  = aic_null,
    threshold = threshold,
    reject    = aic_obs < threshold,
    p_value   = p_value,
    n_perm    = sum(!is.na(aic_null)),
    alpha     = alpha
  )
}
