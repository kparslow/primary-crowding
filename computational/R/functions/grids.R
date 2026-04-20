# computational/R/functions/grids.R
# Helpers for constructing simulation grids.
# No file I/O.

# ------------------------
# 1D grids
# ------------------------

# For calibration / economically relevant parameter scaling, assume intrinsic valence
# v ~ N(0, sigma^2), where sigma is the standard deviation of candidate valence.
# Since v and the general-election penalty delta enter the GE index in the same units,
# we report/grids delta in "sigma units" (e.g., delta = 1*sigma).
sigma <- 1

grid_v <- function(v_min = -4*sigma, v_max = 4*sigma, n = 1201) {
  if (n < 2) stop("n must be >= 2")
  seq(v_min, v_max, length.out = n)
}

# Grid over logS rather than S because:
#   (i) S = sum_{j!=i} exp(v_j + a_j) > 0 can vary over orders of magnitude, so log-scale
#       gives more even coverage of opponent-field strength.
#   (ii) Several reduced-form objects (e.g., G(v;S)) depend on (v,S) primarily through
#       v - log(S), so logS is the natural state variable for comparative statics.
# Note: logS can be negative (this corresponds to S in (0,1)).
grid_logS <- function(logS_min = -4, logS_max = 4, n = 81) {
  if (n < 2) stop("n must be >= 2.")
  if (!is.finite(logS_min) || !is.finite(logS_max)) stop("logS_min/logS_max must be finite.")
  if (logS_max <= logS_min) stop("logS_max must be > logS_min.")
  seq(logS_min, logS_max, length.out = n)
}

grid_delta <- function(delta_min = 0.1*sigma, delta_max = 2.5*sigma, n = 61) {
  if (n <2) stop("n must be >= 2.")
  if (delta_min <= 0) stop("delta_min must be > 0.")
  if (delta_max <= delta_min) stop("delta_max must be > delta_min.")
  seq(delta_min, delta_max, length.out = n)
}

# ----------------------------
# Data frames
# ----------------------------

expand_grid_df <- function(...) {
  expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
}

# Build a standard parameter grid for (logS, delta)
grid_logS_delta <- function(
    logS_min = -4, logS_max = 4, n_logS = 41,
    delta_min = 0.05, delta_max = 3, n_delta = 41
) {
  logS_vec  <- grid_logS(logS_min, logS_max, n_logS)
  delta_vec <- grid_delta(delta_min, delta_max, n_delta)
  
  df <- expand_grid_df(logS = logS_vec, delta = delta_vec)
  
  # Convert from log scale to level scale used in model.R: S = exp(logS)
  df$S <- exp(df$logS)
  df
}
