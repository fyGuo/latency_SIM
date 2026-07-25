# ── True latency weighting curves used in the permutation-test study ──────────
# Each curve gives the exposure weight as a function of the time lag l = 0..15.
# The permutation test asks whether these weights are the SAME across lags
# (null = constant curve) or vary with lag (unimodal / bimodal / decayed).
library(splines)

LATENCY <- 16
LAGS    <- 0:(LATENCY - 1)

# 1. Unimodal natural-spline weight, knot = 10 (the main scenario).
w_unimodal <- function(l, knot = 10)
  as.numeric(bs(l, knots = knot, Boundary.knots = c(0, 16)) %*% c(0.3, -0.01, 0.01, 0))

# 2. Bimodal: Gaussian bumps at lag 3 and lag 12; B scales the second bump.
#    B = 0.2 -> both bumps positive; B = -1 -> second bump negative.
w_bimodal <- function(l, A = 0.3, B = 0.2)
  A * (exp(-0.5 * ((l - 3) / 2)^2) + B * exp(-0.5 * ((l - 12) / 2)^2))

# 3. Exponentially decayed weight.
w_decayed <- function(l, A = 0.3, B = 0.2) A * exp(-B * l)

# 4. Constant weight (NULL: identical weight at every lag).
w_constant <- function(l, c = 0.1) rep(c, length(l))

# Named list of the six true curves evaluated at lags 0..15.
WEIGHT_CURVES <- list(
  unimodal      = w_unimodal(LAGS, knot = 10),
  bimodal_pos   = w_bimodal(LAGS, A = 0.3, B = 0.2),   # second bump positive
  bimodal_neg   = w_bimodal(LAGS, A = 0.3, B = -1),    # second bump negative
  decayed       = w_decayed(LAGS, A = 0.3, B = 0.2),
  constant_low  = w_constant(LAGS, c = 0),             # null curve, no exposure effect at any lag
  constant_high = w_constant(LAGS, c = 0.10)           # null curve, constant nonzero effect
)

# Whether each scenario is a null (constant) or an alternative (lag-varying).
WEIGHT_IS_NULL <- c(unimodal = FALSE, bimodal_pos = FALSE, bimodal_neg = FALSE,
                    decayed = FALSE, constant_low = TRUE, constant_high = TRUE)
