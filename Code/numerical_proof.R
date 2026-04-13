################################################################################
################## Numerical Proof for Proposition 5 ##########################
################################################################################

# Last updated: 4/13/2026, 7:30 AM
# Author: Katherine Parslow
#
# From paper [Appendix]: "For Proposition 5 I supply a proof sketch...
# a full, detailed classification... requires additional technical lemmas...
# and is ongoing work."
#
# PURPOSE: Characterize action set structures A(S) = {v : H(v) ≥ 0}
#          across comprehensive parameter space
################################################################################

cat("\n")
cat(paste(rep("=", 78), collapse = ""), "\n")
cat("NUMERICAL CHARACTERIZATION: ACTION SET STRUCTURES\n")
cat(paste(rep("=", 78), collapse = ""), "\n\n")

# ==============================================================================
# SETUP: CREATE OUTPUT DIRECTORIES
# ==============================================================================

if (!dir.exists("results")) dir.create("results")
if (!dir.exists("figures")) dir.create("figures")

cat("Output directories ready:\n")
cat("  - results/ (for data files)\n")
cat("  - figures/ (for plots)\n\n")

# ==============================================================================
# PART 1: DEFINE CORE FUNCTIONS
# ==============================================================================

cat("Part 1: Defining model primitives...\n\n")

# G(v;S) = 1 + (e-1)S/(e^(v+1) + S)
# From paper [Lemma 2]: "1 < G(v; S) < e for all v ∈ R"
G <- function(v, S) {
  numerator <- (exp(1) - 1) * S
  denominator <- exp(v + 1) + S
  return(1 + numerator / denominator)
}

# C(v;delta) = Phi(v)/Phi(v-delta) where Phi is standard normal CDF
# From paper [Lemma 3]: "C(v; δ) ≡ Φ(v)/Φ(v − δ)"
# Direct computation (numerically stable for v ≥ -10)
C <- function(v, delta) {
  numerator <- pnorm(v)
  denominator <- pnorm(v - delta)
  return(numerator / denominator)
}

# H(v) = G(v;S) - C(v;delta)
# From paper [Proposition 1]: "ai = 1 ⇐⇒ G(vi; S∗i(v)) ≥ C(vi; δ)"
# Equivalently: H(v) ≥ 0
H <- function(v, S, delta) {
  return(G(v, S) - C(v, delta))
}

# Verify functions
cat("Testing functions:\n")
cat(sprintf("  G(0, 3) = %.4f (should be in (1, e))\n", G(0, 3)))
cat(sprintf("  C(0, 0.15) = %.4f (should be > 1)\n", C(0, 0.15)))
cat(sprintf("  H(0, 3, 0.15) = %.4f\n\n", H(0, 3, 0.15)))

# ==============================================================================
# PART 2: COMPUTATIONAL BOUNDS
# ==============================================================================

cat("Part 2: Establishing computational bounds...\n\n")

# Fixed range: [-10, 10]
# From paper [Section 2]: "Candidate valence vi is drawn independently for 
# each candidate i from a distribution F with strictly positive density f on R"
# 
# We test v ∈ [-10, 10], covering >99.999% under F=N(0,4)
v_min <- -10
v_max <- 10

cat("Computational bounds:\n")
cat(sprintf("  v ∈ [%d, %d]\n", v_min, v_max))
cat("  Covers >99.999%% of candidates under F=N(0,4) benchmark\n")
cat("  From paper [Lemma 3]: 'limv→−∞ C(v; δ) = +∞'\n")
cat("  Therefore H(v) < 0 guaranteed for sufficiently negative v\n\n")

# ==============================================================================
# PART 3: STRUCTURE ANALYSIS FUNCTION
# ==============================================================================

analyze_structure <- function(delta, S, step = 0.1, v_range = c(v_min, v_max)) {
  
  v_grid <- seq(v_range[1], v_range[2], by = step)
  H_vals <- sapply(v_grid, function(v) H(v, S, delta))
  
  # Find zero crossings
  sign_changes <- which(diff(sign(H_vals)) != 0)
  n_crossings <- length(sign_changes)
  
  # Interpolate crossing points
  crossing_points <- numeric(n_crossings)
  if (n_crossings > 0) {
    for (i in 1:n_crossings) {
      idx <- sign_changes[i]
      v_before <- v_grid[idx]
      v_after <- v_grid[idx + 1]
      H_before <- H_vals[idx]
      H_after <- H_vals[idx + 1]
      crossing_points[i] <- v_before - H_before * (v_after - v_before) / 
        (H_after - H_before)
    }
  }
  
  H_left <- H_vals[1]
  H_right <- H_vals[length(H_vals)]
  
  # Classify structure
  # From paper [Definition 4]: "The action region is A(S) ≡ {v ∈ R : G(v; S) ≥ C(v; δ)}"
  structure <- "Unknown"
  action_set <- "Unknown"
  regime <- "Unknown"
  
  if (n_crossings == 0) {
    if (all(H_vals > 0)) {
      structure <- "All action"
      action_set <- sprintf("[%d,%d]", v_range[1], v_range[2])
      regime <- "Low penalty"
    } else {
      structure <- "No action"
      action_set <- "∅"
      regime <- "Extreme penalty"
    }
    
  } else if (n_crossings == 1) {
    if (H_left < 0 && H_right > 0) {
      structure <- "Single upper tail"
      action_set <- sprintf("[%.2f, ∞)", crossing_points[1])
      regime <- "Moderate/high penalty"
    } else if (H_left > 0 && H_right < 0) {
      structure <- "Single lower tail"
      action_set <- sprintf("(-∞, %.2f]", crossing_points[1])
      regime <- "Atypical"
    } else {
      structure <- "Atypical 1-crossing"
      action_set <- sprintf("v ≈ %.2f", crossing_points[1])
      regime <- "Atypical"
    }
    
  } else if (n_crossings == 2) {
    if (H_left < 0 && H_right < 0) {
      structure <- "Bounded interval"
      action_set <- sprintf("[%.2f, %.2f]", crossing_points[1], crossing_points[2])
      regime <- "Right tail negative"
    } else if (H_left > 0 && H_right > 0) {
      structure <- "Two disjoint tails"
      action_set <- sprintf("(-∞, %.2f] ∪ [%.2f, ∞)", 
                            crossing_points[1], crossing_points[2])
      regime <- "Atypical"
    } else {
      structure <- "Atypical 2-crossing"
      action_set <- sprintf("[%.2f, %.2f]", crossing_points[1], crossing_points[2])
      regime <- "Atypical"
    }
    
  } else if (n_crossings == 3) {
    if (H_left < 0 && H_right > 0) {
      structure <- "Union"
      action_set <- sprintf("[%.2f, %.2f] ∪ [%.2f, ∞)",
                            crossing_points[1], crossing_points[2], crossing_points[3])
      regime <- "Three cutoffs"
    } else {
      structure <- "Atypical 3-crossing"
      action_set <- "Complex"
      regime <- "Atypical"
    }
    
  } else {
    structure <- sprintf("%d crossings", n_crossings)
    action_set <- "Complex"
    regime <- "Complex"
  }
  
  return(list(
    delta = delta,
    S = S,
    v_range = v_range,
    n_crossings = n_crossings,
    crossings = crossing_points,
    structure = structure,
    action_set = action_set,
    regime = regime,
    H_left = H_left,
    H_right = H_right,
    v_grid = v_grid,
    H_vals = H_vals
  ))
}

# ==============================================================================
# PART 4: SYSTEMATIC PARAMETER EXPLORATION
# ==============================================================================

cat("Part 3: Exploring parameter space...\n\n")


# From paper [Section 2]: "δ > 0 is the general-election penalty"
# From paper [Section 3]: "S ≡ ∑j≠i e^(vj+aj)" (aggregate opponent strength)

# From paper: "increasing primary crowding — summarized by a larger opponents' 
# strength scalar S" [Primary_Competition_(9).pdf]
# 
# With v ∈ [-10,10], feasible S depends on number of candidates N:
#   N=3 (duopoly): S ≈ 2-40 (both opponents with v ∈ [-6,6])
#   N=10: S ≈ 9-100 (typical valence spread)
#   N=20 (crowded): S ≈ 19-500 (extreme crowding, e.g., 2016 GOP primary)

delta_values <- seq(0.01, 0.50, by = 0.01)  # 50 values
S_values <- seq(2, 200, by = 1)              # 39 values
total_tests <- length(delta_values) * length(S_values)

cat(sprintf("Testing %d parameter combinations\n", total_tests))
cat(sprintf("  δ ∈ [%.2f, %.2f]: %d values (step = 0.01)\n", 
            min(delta_values), max(delta_values), length(delta_values)))
cat(sprintf("  S ∈ [%.0f, %.0f]: %d values (step = 1)\n", 
            min(S_values), max(S_values), length(S_values)))
cat(sprintf("\nEstimated runtime: ~3-5 minutes\n\n"))

results_list <- list()
idx <- 1

pb_start <- Sys.time()

for (delta in delta_values) {
  for (S in S_values) {
    
    result <- analyze_structure(delta, S, step = 0.1)
    
    results_list[[idx]] <- data.frame(
      delta = delta,
      S = S,
      n_crossings = result$n_crossings,
      structure = result$structure,
      regime = result$regime,
      action_set = result$action_set,
      v_min = result$v_range[1],
      v_max = result$v_range[2],
      H_left = round(result$H_left, 4),
      H_right = round(result$H_right, 4),
      stringsAsFactors = FALSE
    )
    
    if (idx %% 100 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), pb_start, units = "secs"))
      est_total <- elapsed * total_tests / idx
      est_remaining <- est_total - elapsed
      cat(sprintf("  Progress: %d/%d (%.0f%%) - Est. remaining: %.0f sec\r", 
                  idx, total_tests, 100*idx/total_tests, est_remaining))
    }
    
    idx <- idx + 1
  }
}

results <- do.call(rbind, results_list)
runtime <- as.numeric(difftime(Sys.time(), pb_start, units = "secs"))
cat(sprintf("\n\n✓ Analysis complete: %d cases in %.1f seconds\n\n", 
            nrow(results), runtime))

# ==============================================================================
# PART 5: SUMMARY OF FINDINGS
# ==============================================================================

cat(paste(rep("=", 78), collapse = ""), "\n")
cat("SUMMARY: ACTION SET STRUCTURES\n")
cat(paste(rep("=", 78), collapse = ""), "\n\n")

cat("Distribution by structure type:\n\n")
structure_table <- table(results$structure)
for (struct in names(sort(structure_table, decreasing = TRUE))) {
  count <- structure_table[struct]
  pct <- 100 * count / nrow(results)
  cat(sprintf("  %-25s: %4d cases (%.1f%%)\n", struct, count, pct))
}

cat("\n")
cat("Regime classification:\n\n")
regime_table <- table(results$regime)
for (reg in names(sort(regime_table, decreasing = TRUE))) {
  count <- regime_table[reg]
  pct <- 100 * count / nrow(results)
  cat(sprintf("  %-25s: %4d cases (%.1f%%)\n", reg, count, pct))
}

cat("\n")
cat("Main findings:\n")
all_action <- sum(results$structure == "All action")
single_tail <- sum(results$structure == "Single upper tail")
bounded <- sum(results$structure == "Bounded interval")
union <- sum(results$structure == "Union")

cat(sprintf("  All action:           %4d cases (%.1f%%)\n", 
            all_action, 100*all_action/nrow(results)))
cat(sprintf("  Single upper tail:    %4d cases (%.1f%%)\n",
            single_tail, 100*single_tail/nrow(results)))
cat(sprintf("  Bounded interval:     %4d cases (%.1f%%)\n",
            bounded, 100*bounded/nrow(results)))
cat(sprintf("  Union:                %4d cases (%.1f%%)\n\n",
            union, 100*union/nrow(results)))

# Parameter dependence
# From paper [Proposition 5]: "Then there exists v(S) ∈ R such that a*(v; S) = 0 
# for all v < v(S)"
cat("Parameter dependence:\n\n")
low_delta <- results[results$delta <= 0.05, ]
mid_delta <- results[results$delta > 0.05 & results$delta <= 0.20, ]
high_delta <- results[results$delta > 0.20, ]

cat(sprintf("Low penalty (δ ≤ 0.05): %d cases\n", nrow(low_delta)))
if (nrow(low_delta) > 0) {
  top <- names(sort(table(low_delta$structure), decreasing=TRUE))[1]
  cat(sprintf("  Most common: %s (%.0f%%)\n", 
              top, 100*mean(low_delta$structure == top)))
}

cat(sprintf("\nModerate penalty (0.05 < δ ≤ 0.20): %d cases\n", nrow(mid_delta)))
if (nrow(mid_delta) > 0) {
  top <- names(sort(table(mid_delta$structure), decreasing=TRUE))[1]
  cat(sprintf("  Most common: %s (%.0f%%)\n",
              top, 100*mean(mid_delta$structure == top)))
}

cat(sprintf("\nHigh penalty (δ > 0.20): %d cases\n", nrow(high_delta)))
if (nrow(high_delta) > 0) {
  top <- names(sort(table(high_delta$structure), decreasing=TRUE))[1]
  cat(sprintf("  Most common: %s (%.0f%%)\n",
              top, 100*mean(high_delta$structure == top)))
}

# ==============================================================================
# PART 6: REPRESENTATIVE EXAMPLES
# ==============================================================================

cat("\n")
cat(paste(rep("=", 78), collapse = ""), "\n")
cat("REPRESENTATIVE EXAMPLES\n")
cat(paste(rep("=", 78), collapse = ""), "\n\n")

example_cases <- list()

if (all_action > 0) {
  ex <- results[results$structure == "All action", ][1, ]
  example_cases[["all_action"]] <- list(
    delta = ex$delta, 
    S = ex$S, 
    action_set = ex$action_set,
    label = "All action (low penalty)"
  )
}

if (single_tail > 0) {
  ex <- results[results$structure == "Single upper tail", ][1, ]
  example_cases[["single_tail"]] <- list(
    delta = ex$delta,
    S = ex$S,
    action_set = ex$action_set,
    label = "Single upper tail"
  )
}

if (bounded > 0) {
  ex <- results[results$structure == "Bounded interval", ][1, ]
  example_cases[["bounded"]] <- list(
    delta = ex$delta,
    S = ex$S,
    action_set = ex$action_set,
    label = "Bounded interval"
  )
}

if (union > 0) {
  ex <- results[results$structure == "Union", ][1, ]
  example_cases[["union"]] <- list(
    delta = ex$delta,
    S = ex$S,
    action_set = ex$action_set,
    label = "Union"
  )
}

for (name in names(example_cases)) {
  ex <- example_cases[[name]]
  cat(sprintf("%s:\n", ex$label))
  cat(sprintf("  Parameters: δ = %.2f, S = %.0f\n", ex$delta, ex$S))
  cat(sprintf("  Action set: %s\n\n", ex$action_set))
}

# ==============================================================================
# PART 7: SAVE RESULTS
# ==============================================================================

cat(paste(rep("=", 78), collapse = ""), "\n")
cat("SAVING RESULTS\n")
cat(paste(rep("=", 78), collapse = ""), "\n\n")

save(results, v_min, v_max, example_cases,
     file = "results/numerical_proof_results.RData")

write.csv(results, "results/numerical_proof_table.csv", row.names = FALSE)

summary_stats <- data.frame(
  metric = c("Total cases", "All action", "Single upper tail", 
             "Bounded interval", "Union"),
  value = c(nrow(results), all_action, single_tail, bounded, union),
  percentage = sprintf("%.1f%%", 100 * c(1, all_action, single_tail, bounded, union) / 
                         c(nrow(results), rep(nrow(results), 4)))
)
write.csv(summary_stats, "results/summary_statistics.csv", row.names = FALSE)

cat("✓ Results saved:\n")
cat("  - results/numerical_proof_results.RData\n")
cat("  - results/numerical_proof_table.csv\n")
cat("  - results/summary_statistics.csv\n\n")

cat("✓ Numerical characterization complete\n\n")

cat("Sample results (first 15 cases):\n")
print(results[1:15, c("delta", "S", "n_crossings", "structure", "regime")])

cat("\n")
cat(paste(rep("=", 78), collapse = ""), "\n")
cat("Completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(paste(rep("=", 78), collapse = ""), "\n\n")