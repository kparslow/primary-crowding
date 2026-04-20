# computational/R/runners/01_single_agent_grid.R
# Build a grid over (v, logS, delta) and compute incentives + best response.
# Writes simulated output to computational/output/ (model-generated, not empirical).

source("computational/R/functions/model.R")
source("computational/R/functions/grids.R")

# For calibration / economically relevant parameter scaling, we assume intrinsic valence
# v ~ N(0, sigma^2), where sigma is the standard deviation of candidate valence.
# Parameters in the same units as v (notably the GE penalty delta) are expressed in "sigma units".
sigma <- 1

# Grids
v_vec <- grid_v(v_min = -4 * sigma, v_max = 4 * sigma, n = 1201)

param_df <- grid_logS_delta(
  logS_min = -4 * sigma, logS_max = 4 * sigma, n_logS = 41,
  delta_min = 0.1 * sigma, delta_max = 2.5 * sigma, n_delta = 41
)

# Evaluate model objects on the grid
out_list <- vector("list", nrow(param_df))

for (k in seq_len(nrow(param_df))) {
  logS  <- param_df$logS[k]
  S     <- param_df$S[k]
  delta <- param_df$delta[k]
  
  Delta_vec <- Delta_incentive(v_vec, S = S, delta = delta)
  
  out_list[[k]] <- data.frame(
    v = v_vec,
    sigma = sigma,
    logS = logS,
    S = S,
    delta = delta,
    u0 = u0(v_vec, S = S),
    u1 = u1(v_vec, S = S, delta = delta),
    Delta = Delta_vec,
    a_star = ifelse(Delta_vec >= 0, 1L, 0L)
  )
}

out_df <- do.call(rbind, out_list)

# Save (simulated/model-generated output)
out_dir <- file.path("computational", "output", "datasets")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

outfile <- file.path(out_dir, "single_agent_grid.csv")
utils::write.csv(out_df, outfile, row.names = FALSE)

message("Wrote: ", outfile)
