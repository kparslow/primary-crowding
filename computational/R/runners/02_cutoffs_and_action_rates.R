# computational/R/runners/02_cutoffs_and_action_rates.R
# Reads single_agent_grid.csv (large, uncommitted) and produces a small summary:
#  (1) cutoff v_star(logS, delta) where Delta crosses 0
#  (2) Pr(a_star = 1 | logS, delta) under v ~ N(0, sigma^2)
#
# This script also diagnoses whether a_star(v) behaves like a cutoff (0->1 at most once)
# within each (logS, delta) cell. If not, it flags the cell and still computes the
# model-implied action probability from the actual (possibly non-monotone) a_star pattern.

infile <- file.path("computational", "output", "datasets", "single_agent_grid.csv")
if (!file.exists(infile)) stop("Missing input file: ", infile)

# Prefer data.table for speed/memory; fall back to base read.csv if unavailable.
use_dt <- requireNamespace("data.table", quietly = TRUE)

if (use_dt) {
  DT <- data.table::fread(infile)
} else {
  warning("Package 'data.table' not installed; using read.csv() (may be slower).")
  DT <- utils::read.csv(infile, check.names = TRUE)
}

# Basic checks
required_cols <- c("v", "sigma", "logS", "delta", "Delta", "a_star")
missing <- setdiff(required_cols, names(DT))
if (length(missing) > 0) {
  stop("Input missing required columns: ", paste(missing, collapse = ", "))
}

# Normal density (for v ~ N(0, sigma^2))
normal_weight <- function(v, sigma) stats::dnorm(v, mean = 0, sd = sigma)

if (use_dt) {
  data.table::setDT(DT)
  
  # Ensure ordering within groups so "first crossing" and switch counting make sense
  data.table::setorderv(DT, c("logS", "delta", "v"))
  
  summary_dt <- DT[, {
    sig <- sigma[1L]
    
    v_vec <- v
    dv <- if (length(v_vec) >= 2) (v_vec[2L] - v_vec[1L]) else NA_real_
    
    # Switch diagnostic for a_star along v
    a <- as.integer(a_star)
    n_switches <- if (length(a) >= 2) sum(a[-1L] != a[-length(a)]) else 0L
    switches_ok <- (n_switches <= 1L)
    
    # Weights for numerical integration under N(0, sig^2)
    w <- normal_weight(v_vec, sig)
    
    # Model-implied Pr(action): integrate the actual (possibly non-monotone) a_star(v)
    action_rate <- sum((a == 1L) * w) * dv
    
    # Cutoff: first v where Delta >= 0 (matches tie-break used in a_star construction)
    idx <- which(Delta >= 0)
    v_star <- if (length(idx) == 0) Inf else v_vec[min(idx)]
    
    # If behavior is a valid cutoff, you can also compute Pr(action) from the cutoff via CDF.
    # If not a cutoff, set to NA.
    action_rate_cutoff <- if (switches_ok && is.finite(v_star)) {
      1 - stats::pnorm(v_star, mean = 0, sd = sig)
    } else if (switches_ok && is.infinite(v_star)) {
      0
    } else {
      NA_real_
    }
    
    always0 <- all(Delta < 0)
    always1 <- all(Delta >= 0)
    
    list(
      sigma = sig,
      v_star = v_star,
      action_rate = action_rate,
      action_rate_cutoff = action_rate_cutoff,
      n_switches_a_star = n_switches,
      switches_ok = switches_ok,
      always0 = always0,
      always1 = always1,
      n_v = length(v_vec),
      dv = dv
    )
  }, by = .(logS, delta)]
  
  out_dir <- file.path("computational", "output", "derived")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  outfile <- file.path(out_dir, "cutoffs_and_action_rates.csv")
  data.table::fwrite(summary_dt, outfile)
  
} else {
  # Base R fallback (slower on large files)
  DT <- DT[order(DT$logS, DT$delta, DT$v), , drop = FALSE]
  
  groups <- unique(DT[, c("logS", "delta")])
  out <- vector("list", nrow(groups))
  
  for (g in seq_len(nrow(groups))) {
    logS_g <- groups$logS[g]
    delta_g <- groups$delta[g]
    sub <- DT[DT$logS == logS_g & DT$delta == delta_g, , drop = FALSE]
    
    sig <- sub$sigma[1]
    v_vec <- sub$v
    dv <- if (length(v_vec) >= 2) (v_vec[2] - v_vec[1]) else NA_real_
    w <- normal_weight(v_vec, sig)
    
    a <- as.integer(sub$a_star)
    n_switches <- if (length(a) >= 2) sum(a[-1] != a[-length(a)]) else 0L
    switches_ok <- (n_switches <= 1L)
    
    action_rate <- sum((a == 1L) * w) * dv
    
    idx <- which(sub$Delta >= 0)
    v_star <- if (length(idx) == 0) Inf else v_vec[min(idx)]
    
    action_rate_cutoff <- if (switches_ok && is.finite(v_star)) {
      1 - stats::pnorm(v_star, mean = 0, sd = sig)
    } else if (switches_ok && is.infinite(v_star)) {
      0
    } else {
      NA_real_
    }
    
    out[[g]] <- data.frame(
      logS = logS_g,
      delta = delta_g,
      sigma = sig,
      v_star = v_star,
      action_rate = action_rate,
      action_rate_cutoff = action_rate_cutoff,
      n_switches_a_star = n_switches,
      switches_ok = switches_ok,
      always0 = all(sub$Delta < 0),
      always1 = all(sub$Delta >= 0),
      n_v = length(v_vec),
      dv = dv
    )
  }
  
  out_df <- do.call(rbind, out)
  
  out_dir <- file.path("computational", "output", "derived")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  outfile <- file.path(out_dir, "cutoffs_and_action_rates.csv")
  utils::write.csv(out_df, outfile, row.names = FALSE)
}

message("Wrote: ", outfile)