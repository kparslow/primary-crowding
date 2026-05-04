# Produce summary tables + key PNG figures from:
#   computational/output/derived/cutoffs_and_action_rates.csv
#
# Outputs:
#   computational/output/results/tables/*.csv
#   computational/output/results/figures/*.png
#
# Designed to be easy to read and modify.

suppressWarnings(suppressMessages({
  have_dt <- requireNamespace("data.table", quietly = TRUE)
  have_gg <- requireNamespace("ggplot2", quietly = TRUE)
}))

if (!have_gg) stop("Missing package 'ggplot2'. Install with install.packages('ggplot2').")

read_any <- function(path) {
  if (!file.exists(path)) stop("Missing input file: ", path)
  if (have_dt) return(data.table::fread(path))
  utils::read.csv(path, check.names = TRUE)
}

to_logical <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(x != 0)
  if (is.character(x)) return(toupper(x) %in% c("TRUE", "T", "1"))
  as.logical(x)
}

quantile_table <- function(x, probs = c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1)) {
  q <- stats::quantile(x, probs = probs, na.rm = TRUE)
  data.frame(stat = names(q), value = as.numeric(q), row.names = NULL)
}

# ---- Paths ----
infile <- file.path("computational", "output", "derived", "cutoffs_and_action_rates.csv")

out_base <- file.path("computational", "output", "results")
tab_dir  <- file.path(out_base, "tables")
fig_dir  <- file.path(out_base, "figures")

dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Load + validate ----
df <- read_any(infile)
if (have_dt) data.table::setDT(df)

required <- c(
  "logS", "delta", "sigma", "v_star",
  "action_rate", "action_rate_cutoff",
  "n_switches_a_star", "switches_ok", "always0", "always1"
)
miss <- setdiff(required, names(df))
if (length(miss) > 0) stop("Input missing required columns: ", paste(miss, collapse = ", "))

df[, switches_ok := to_logical(switches_ok)]
df[, always0 := to_logical(always0)]
df[, always1 := to_logical(always1)]

# ---- Define regimes (for counts + regime map figure) ----
# Note: nonmonotone overrides other labels (useful diagnostic).
df[, regime := "cutoff_ok"]
df[always0 == TRUE, regime := "always0"]
df[always1 == TRUE, regime := "always1"]
df[switches_ok == FALSE, regime := "nonmonotone"]
df[, regime := factor(regime, levels = c("always0", "always1", "cutoff_ok", "nonmonotone"))]

# -------------------------
# TABLES
# -------------------------
N <- nrow(df)

# 0) Grid info / caveat
# Regime shares (especially always0/always1) are sensitive to the chosen parameter grid.
# Extending delta_max mechanically increases the number of always0 observations, etc.
grid_info <- data.frame(
  n_cells = N,
  logS_min = min(df$logS, na.rm = TRUE),
  logS_max = max(df$logS, na.rm = TRUE),
  delta_min = min(df$delta, na.rm = TRUE),
  delta_max = max(df$delta, na.rm = TRUE),
  logS_step = {
    s <- sort(unique(df$logS))
    if (length(s) >= 2) s[2] - s[1] else NA_real_
  },
  delta_step = {
    d <- sort(unique(df$delta))
    if (length(d) >= 2) d[2] - d[1] else NA_real_
  }
)
utils::write.csv(grid_info, file.path(tab_dir, "grid_info.csv"), row.names = FALSE)

# 1) Regime counts
reg_counts <- as.data.frame(table(df$regime), stringsAsFactors = FALSE)
names(reg_counts) <- c("regime", "count")
reg_counts$share <- reg_counts$count / N
utils::write.csv(reg_counts, file.path(tab_dir, "regime_counts.csv"), row.names = FALSE)

# 2) Switch-count distribution
switch_counts <- as.data.frame(table(df$n_switches_a_star), stringsAsFactors = FALSE)
names(switch_counts) <- c("n_switches_a_star", "count")
switch_counts$share <- switch_counts$count / N
utils::write.csv(switch_counts, file.path(tab_dir, "switch_count_distribution.csv"), row.names = FALSE)

# 3) Quantiles of action_rate
ar_tbl <- quantile_table(df$action_rate)
names(ar_tbl)[2] <- "action_rate"
utils::write.csv(ar_tbl, file.path(tab_dir, "action_rate_quantiles.csv"), row.names = FALSE)

# 4) v_star summary (Inf handling)
is_inf <- is.infinite(df$v_star)
finite_v <- df$v_star[!is_inf]

v_tbl <- data.frame(
  n_total = N,
  n_inf = sum(is_inf),
  share_inf = mean(is_inf),
  min_finite = if (length(finite_v)) min(finite_v) else NA_real_,
  median_finite = if (length(finite_v)) stats::median(finite_v) else NA_real_,
  max_finite = if (length(finite_v)) max(finite_v) else NA_real_
)
utils::write.csv(v_tbl, file.path(tab_dir, "v_star_summary.csv"), row.names = FALSE)

# 5) Cutoff approximation error where available
err <- abs(df$action_rate - df$action_rate_cutoff)

err_summary <- data.frame(
  n_with_action_rate_cutoff = sum(!is.na(df$action_rate_cutoff)),
  median_abs_error = stats::median(err, na.rm = TRUE),
  p90_abs_error = stats::quantile(err, 0.9, na.rm = TRUE),
  max_abs_error = max(err, na.rm = TRUE)
)
utils::write.csv(err_summary, file.path(tab_dir, "cutoff_approx_error_summary.csv"), row.names = FALSE)

# Error by regime
err_by_regime <- do.call(rbind, lapply(levels(df$regime), function(r) {
  e <- err[df$regime == r]
  data.frame(
    regime = r,
    n = sum(!is.na(e)),
    median_abs_error = stats::median(e, na.rm = TRUE),
    p90_abs_error = stats::quantile(e, 0.9, na.rm = TRUE),
    max_abs_error = if (all(is.na(e))) NA_real_ else max(e, na.rm = TRUE)
  )
}))
utils::write.csv(err_by_regime, file.path(tab_dir, "cutoff_approx_error_by_regime.csv"), row.names = FALSE)

# 6) Empirical frontiers by logS (useful for notes)
frontiers <- do.call(rbind, lapply(sort(unique(df$logS)), function(ls) {
  sub <- df[df$logS == ls, ]
  sub <- sub[order(sub$delta), ]
  
  d0 <- sub$delta[sub$always0 == TRUE]
  d1 <- sub$delta[sub$always1 == TRUE]
  
  data.frame(
    logS = ls,
    delta_always0_min = if (length(d0)) min(d0) else NA_real_,
    delta_always1_max = if (length(d1)) max(d1) else NA_real_,
    share_nonmonotone = mean(sub$switches_ok == FALSE)
  )
}))
utils::write.csv(frontiers, file.path(tab_dir, "frontiers_by_logS.csv"), row.names = FALSE)

# 7) Nonmonotone cell summary
nonmono <- df[df$switches_ok == FALSE, ]

nonmono_summary <- data.frame(
  n_nonmonotone = nrow(nonmono),
  share_nonmonotone = nrow(nonmono) / N,
  logS_min = if (nrow(nonmono)) min(nonmono$logS) else NA_real_,
  logS_max = if (nrow(nonmono)) max(nonmono$logS) else NA_real_,
  delta_min = if (nrow(nonmono)) min(nonmono$delta) else NA_real_,
  delta_max = if (nrow(nonmono)) max(nonmono$delta) else NA_real_,
  n_switches_min = if (nrow(nonmono)) min(nonmono$n_switches_a_star) else NA_real_,
  n_switches_max = if (nrow(nonmono)) max(nonmono$n_switches_a_star) else NA_real_,
  action_rate_min = if (nrow(nonmono)) min(nonmono$action_rate) else NA_real_,
  action_rate_median = if (nrow(nonmono)) stats::median(nonmono$action_rate) else NA_real_,
  action_rate_max = if (nrow(nonmono)) max(nonmono$action_rate) else NA_real_,
  n_inf_v_star = if (nrow(nonmono)) sum(is.infinite(nonmono$v_star)) else NA_real_,
  share_inf_v_star = if (nrow(nonmono)) mean(is.infinite(nonmono$v_star)) else NA_real_,
  v_star_min_finite = {
    if (!nrow(nonmono)) NA_real_ else {
      vv <- nonmono$v_star[is.finite(nonmono$v_star)]
      if (length(vv)) min(vv) else NA_real_
    }
  },
  v_star_median_finite = {
    if (!nrow(nonmono)) NA_real_ else {
      vv <- nonmono$v_star[is.finite(nonmono$v_star)]
      if (length(vv)) stats::median(vv) else NA_real_
    }
  },
  v_star_max_finite = {
    if (!nrow(nonmono)) NA_real_ else {
      vv <- nonmono$v_star[is.finite(nonmono$v_star)]
      if (length(vv)) max(vv) else NA_real_
    }
  }
)
utils::write.csv(nonmono_summary,
                 file.path(tab_dir, "nonmonotone_cells_summary.csv"),
                 row.names = FALSE)

# Full list of nonmonotone cells (for selecting examples / plotting)
if (nrow(nonmono) > 0) {
  nonmono_list <- nonmono[, c(
    "logS", "delta", "sigma",
    "n_switches_a_star",
    "v_star", "action_rate",
    "always0", "always1"
  )]
  nonmono_list <- nonmono_list[order(nonmono_list$logS, nonmono_list$delta), ]
  utils::write.csv(nonmono_list,
                   file.path(tab_dir, "nonmonotone_cells_list.csv"),
                   row.names = FALSE)
}

# -------------------------
# FIGURES
# -------------------------
library(ggplot2)

# Heatmap: action_rate
p_ar <- ggplot(df, aes(x = logS, y = delta, fill = action_rate)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C", trans = "sqrt") +
  labs(
    title = "Implied action rate Pr(a*=1 | logS, delta)",
    x = "logS",
    y = "delta",
    fill = "action_rate"
  ) +
  theme_minimal(base_size = 12)

ggplot2::ggsave(
  file.path(fig_dir, "heatmap_action_rate.png"),
  p_ar, width = 8.5, height = 6.5, dpi = 200
)

# Heatmap: v_star (Inf as NA/grey); cap extreme values for readability
df[, v_star_plot := v_star]
df[is.infinite(v_star_plot), v_star_plot := NA_real_]

cap <- stats::quantile(df$v_star_plot, 0.99, na.rm = TRUE)
df[, v_star_cap := pmin(v_star_plot, cap)]

p_vs <- ggplot(df, aes(x = logS, y = delta, fill = v_star_cap)) +
  geom_tile() +
  scale_fill_viridis_c(option = "B", na.value = "grey90") +
  labs(
    title = "Cutoff v* (capped at 99th pct; Inf shown as grey)",
    x = "logS",
    y = "delta",
    fill = "v*_cap"
  ) +
  theme_minimal(base_size = 12)

ggplot2::ggsave(
  file.path(fig_dir, "heatmap_v_star.png"),
  p_vs, width = 8.5, height = 6.5, dpi = 200
)

# Regime map
p_reg <- ggplot(df, aes(x = logS, y = delta, fill = regime)) +
  geom_tile() +
  scale_fill_manual(values = c(
    always0 = "#d73027",
    always1 = "#1a9850",
    cutoff_ok = "#4575b4",
    nonmonotone = "#984ea3"
  ), drop = FALSE) +
  labs(
    title = "Regime map over (logS, delta)",
    x = "logS",
    y = "delta",
    fill = "regime"
  ) +
  theme_minimal(base_size = 12)

ggplot2::ggsave(
  file.path(fig_dir, "regime_map.png"),
  p_reg, width = 8.5, height = 6.5, dpi = 200
)

# ---- Console report ----
cat("\nDone.\n")
cat("Read: ", infile, "\n", sep = "")
cat("Wrote tables to: ", tab_dir, "\n", sep = "")
cat("Wrote figures to: ", fig_dir, "\n\n", sep = "")

cat("Grid info (caveat: regime shares depend on grid bounds; see grid_info.csv):\n")
print(grid_info)

cat("\nRegime counts:\n")
print(reg_counts)

cat("\nNonmonotone summary:\n")
print(nonmono_summary)

cat("\nCutoff approx error summary:\n")
print(err_summary)

cat("\nFrontiers by logS (head):\n")
print(utils::head(frontiers))