# ============================================================================
# SIGN STRUCTURE ANALYSIS: Determine action regions explicitly
# ============================================================================

library(tidyverse)
library(ggplot2)

# Core functions
G <- function(v, S) {
  return(1 + ((exp(1) - 1) * S) / (exp(v + 1) + S))
}

C <- function(v, delta) {
  return(pnorm(v) / pnorm(v - delta))
}

H <- function(v, S, delta) {
  return(G(v, S) - C(v, delta))
}

# ============================================================================
# PARAMETER GRID
# ============================================================================

v_grid <- seq(-10, 10, by = 0.05)
delta_grid <- seq(0.01, 0.50, by = 0.01)
S_grid <- seq(2, 200, by = 2)

cat(sprintf("Computing sign structure for %d delta × %d S combinations\n\n",
            length(delta_grid), length(S_grid)))

# ============================================================================
# MAIN ANALYSIS: For each (delta, S), extract all zero-crossings and signs
# ============================================================================

sign_structure_results <- tibble()

for (i_delta in seq_along(delta_grid)) {
  delta <- delta_grid[i_delta]
  
  if (i_delta %% 10 == 0) {
    cat(sprintf("  delta = %.2f (%d/%d)\n", delta, i_delta, length(delta_grid)))
  }
  
  for (i_S in seq_along(S_grid)) {
    S <- S_grid[i_S]
    
    # Evaluate H on the grid
    H_vec <- sapply(v_grid, function(v) H(v, S, delta))
    
    # Find sign changes
    sign_vec <- sign(H_vec)
    sign_changes_idx <- which(diff(sign_vec) != 0)
    
    # For each zero-crossing, determine the sign on left and right
    if (length(sign_changes_idx) == 0) {
      # No zero-crossing in domain
      if (all(H_vec > 0, na.rm = TRUE)) {
        action_structure <- "no_zero_all_positive"
        n_zeros <- 0
        zero_list <- NA
        sign_left <- NA
        sign_right <- NA
      } else if (all(H_vec < 0, na.rm = TRUE)) {
        action_structure <- "no_zero_all_negative"
        n_zeros <- 0
        zero_list <- NA
        sign_left <- NA
        sign_right <- NA
      } else {
        action_structure <- "unknown"
        n_zeros <- 0
        zero_list <- NA
        sign_left <- NA
        sign_right <- NA
      }
    } else {
      n_zeros <- length(sign_changes_idx)
      
      # Extract zero locations and signs
      zero_details <- list()
      
      for (k in seq_along(sign_changes_idx)) {
        idx <- sign_changes_idx[k]
        
        # Get values on either side
        v_left <- v_grid[idx]
        v_right <- v_grid[idx + 1]
        H_left <- H_vec[idx]
        H_right <- H_vec[idx + 1]
        
        # Interpolate zero location
        v_zero <- v_left - H_left * (v_right - v_left) / (H_right - H_left)
        
        # Determine sign transition
        sign_left <- sign(H_left)
        sign_right <- sign(H_right)
        
        transition <- case_when(
          sign_left < 0 & sign_right > 0 ~ "neg_to_pos",
          sign_left > 0 & sign_right < 0 ~ "pos_to_neg",
          TRUE ~ "other"
        )
        
        zero_details[[k]] <- list(
          zero_loc = v_zero,
          sign_left = sign_left,
          sign_right = sign_right,
          transition = transition
        )
      }
      
      # Determine overall action structure from transition pattern
      transitions <- sapply(zero_details, function(x) x$transition)
      zero_locs <- sapply(zero_details, function(x) x$zero_loc)
      
      if (n_zeros == 1) {
        if (transitions[1] == "neg_to_pos") {
          action_structure <- "upper_tail_only"
        } else {
          action_structure <- "lower_tail_only"
        }
      } else if (n_zeros == 2) {
        if (all(transitions == c("neg_to_pos", "pos_to_neg"))) {
          action_structure <- "interior_interval"
        } else {
          action_structure <- "other_2cross"
        }
      } else if (n_zeros == 3) {
        if (all(transitions == c("neg_to_pos", "pos_to_neg", "neg_to_pos"))) {
          action_structure <- "interior_plus_upper_tail"
        } else {
          action_structure <- "other_3cross"
        }
      } else {
        action_structure <- paste0("multi_cross_", n_zeros)
      }
      
      zero_list <- paste(
        sapply(seq_along(zero_locs), function(k) 
          sprintf("%.2f(%s)", zero_locs[k], transitions[k])),
        collapse = "; "
      )
      sign_left <- zero_details[[1]]$sign_left
      sign_right <- tail(zero_details, 1)[[1]]$sign_right
    }
    
    sign_structure_results <- bind_rows(
      sign_structure_results,
      tibble(
        delta = delta,
        S = S,
        n_zero_crossings = n_zeros,
        action_structure = action_structure,
        zero_crossings = zero_list,
        H_at_left_boundary = H_vec[1],
        H_at_right_boundary = H_vec[length(H_vec)]
      )
    )
  }
}

cat("\n✓ Analysis complete\n\n")

# ============================================================================
# SUMMARY: Distribution of action structures
# ============================================================================

cat(strrep("=", 70), "\n")
cat("ACTION STRUCTURE DISTRIBUTION\n")
cat(strrep("=", 70), "\n\n")

structure_summary <- sign_structure_results %>%
  group_by(action_structure) %>%
  summarize(
    n_cases = n(),
    pct = 100 * n() / nrow(sign_structure_results),
    .groups = 'drop'
  ) %>%
  arrange(desc(n_cases))

print(structure_summary)

cat("\n")

# ============================================================================
# KEY FINDING: How many cases show EACH structure type?
# ============================================================================

cat(strrep("=", 70), "\n")
cat("CRITICAL: MONOTONE vs. NON-MONOTONE STRUCTURES\n")
cat(strrep("=", 70), "\n\n")

monotone_cases <- sign_structure_results %>%
  filter(action_structure %in% c("upper_tail_only", "no_zero_all_positive", "no_zero_all_negative"))

non_monotone_cases <- sign_structure_results %>%
  filter(action_structure %in% c("interior_interval", "interior_plus_upper_tail", "other_2cross", "other_3cross"))

cat(sprintf("Monotone structures (single contiguous action region):\n"))
cat(sprintf("  Count: %d / %d (%.1f%%)\n\n", 
            nrow(monotone_cases), nrow(sign_structure_results),
            100 * nrow(monotone_cases) / nrow(sign_structure_results)))

cat(sprintf("Non-monotone structures (multiple disjoint action regions):\n"))
cat(sprintf("  Count: %d / %d (%.1f%%)\n\n",
            nrow(non_monotone_cases), nrow(sign_structure_results),
            100 * nrow(non_monotone_cases) / nrow(sign_structure_results)))

if (nrow(non_monotone_cases) > 0) {
  cat("Non-monotone cases breakdown:\n")
  non_monotone_cases %>%
    group_by(action_structure) %>%
    summarize(n = n(), .groups = 'drop') %>%
    print()
  
  cat("\n\nExamples of non-monotone cases (first 10):\n")
  print(head(non_monotone_cases %>% select(delta, S, action_structure, zero_crossings), 10))
}

cat("\n")

# ============================================================================
# DETAILED: For each structure, what are the sign patterns?
# ============================================================================

cat(strrep("=", 70), "\n")
cat("SIGN PATTERNS BY STRUCTURE\n")
cat(strrep("=", 70), "\n\n")

pattern_summary <- sign_structure_results %>%
  group_by(action_structure) %>%
  summarize(
    n_cases = n(),
    mean_n_zeros = mean(n_zero_crossings),
    H_left_mean = mean(H_at_left_boundary, na.rm = TRUE),
    H_right_mean = mean(H_at_right_boundary, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    left_sign = case_when(H_left_mean > 0 ~ "positive", H_left_mean < 0 ~ "negative", TRUE ~ "zero"),
    right_sign = case_when(H_right_mean > 0 ~ "positive", H_right_mean < 0 ~ "negative", TRUE ~ "zero")
  )

print(pattern_summary %>% select(action_structure, n_cases, left_sign, right_sign, mean_n_zeros))

cat("\n")

# ============================================================================
# SPECIFIC QUESTION: Upper-tail vs interior structure
# ============================================================================

cat(strrep("=", 70), "\n")
cat("UPPER-TAIL vs. INTERIOR ACTION REGIONS\n")
cat(strrep("=", 70), "\n\n")

upper_tail <- sign_structure_results %>%
  filter(action_structure == "upper_tail_only")

interior <- sign_structure_results %>%
  filter(action_structure %in% c("interior_interval", "interior_plus_upper_tail"))

cat(sprintf("Upper-tail only: %d cases (%.1f%%)\n",
            nrow(upper_tail), 100 * nrow(upper_tail) / nrow(sign_structure_results)))
cat(sprintf("Interior intervals: %d cases (%.1f%%)\n",
            nrow(interior), 100 * nrow(interior) / nrow(sign_structure_results)))

if (nrow(interior) > 0) {
  cat("\n⚠️  INTERIOR INTERVAL CASES DETECTED:\n")
  cat(sprintf("Parameter ranges with interior structure:\n"))
  print(interior %>%
          summarize(
            delta_min = min(delta),
            delta_max = max(delta),
            S_min = min(S),
            S_max = max(S),
            .groups = 'drop'
          ))
}

# ============================================================================
# EXPORT FOR INSPECTION
# ============================================================================

write_csv(sign_structure_results, "results/sign_structure_full.csv")
write_csv(structure_summary, "results/structure_summary.csv")
write_csv(pattern_summary, "results/pattern_summary.csv")

cat("\n✓ Exported: sign_structure_full.csv, structure_summary.csv, pattern_summary.csv\n")

# ============================================================================
# VISUAL: Plot a few examples showing sign transitions
# ============================================================================

cat("\nGenerating example plots...\n")

# Pick one case from each category
examples <- list(
  "upper_tail_only" = sign_structure_results %>% 
    filter(action_structure == "upper_tail_only") %>% slice(1),
  "interior_interval" = sign_structure_results %>% 
    filter(action_structure == "interior_interval") %>% slice(1),
  "no_zero_all_positive" = sign_structure_results %>% 
    filter(action_structure == "no_zero_all_positive") %>% slice(1)
)

plot_list <- list()

for (example_name in names(examples)) {
  ex <- examples[[example_name]]
  
  if (nrow(ex) > 0) {
    delta_val <- ex$delta[1]
    S_val <- ex$S[1]
    
    H_curve <- tibble(
      v = v_grid,
      H = sapply(v_grid, function(v) H(v, S_val, delta_val))
    )
    
    p <- H_curve %>%
      ggplot(aes(x = v, y = H)) +
      geom_line(size = 1, color = "steelblue") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
      geom_vline(xintercept = 0, linetype = "dotted", color = "gray", alpha = 0.5) +
      geom_ribbon(aes(ymin = 0, ymax = pmax(H, 0)), fill = "green", alpha = 0.2, label = "Action region") +
      geom_ribbon(aes(ymin = pmin(H, 0), ymax = 0), fill = "red", alpha = 0.2, label = "No action") +
      labs(
        title = example_name,
        x = "Candidate valence (v)",
        y = "H(v) = G(v;S) - C(v;delta)",
        subtitle = sprintf("delta=%.2f, S=%d", delta_val, S_val)
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 10))
    
    plot_list[[example_name]] <- p
  }
}

if (length(plot_list) > 0) {
  combined <- gridExtra::grid.arrange(grobs = plot_list, ncol = min(3, length(plot_list)))
  ggsave("figures/sign_structure_examples.png", combined, width = 14, height = 5, dpi = 150)
  cat("✓ Saved: sign_structure_examples.png\n")
}