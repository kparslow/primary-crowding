# computational/R/tests/test-grids.R
# Tests for computational/R/functions/grids.R

library(testthat)

# Locate the repo root regardless of working directory when tests run.
.find_root <- function() {
  d <- getwd()
  for (i in seq_len(6)) {
    if (file.exists(file.path(d, "computational", "R", "functions", "grids.R"))) return(d)
    d <- dirname(d)
  }
  stop("Cannot find repo root from: ", getwd())
}
.root <- .find_root()

source(file.path(.root, "computational", "R", "functions", "grids.R"))


# ---------------------------------------------------------------------------
# grid_v
# ---------------------------------------------------------------------------
test_that("grid_v returns a numeric vector of length n", {
  g <- grid_v(n = 10)
  expect_type(g, "double")
  expect_length(g, 10)
})

test_that("grid_v spans [v_min, v_max]", {
  g <- grid_v(v_min = -3, v_max = 3, n = 50)
  expect_equal(min(g), -3)
  expect_equal(max(g), 3)
})

test_that("grid_v is equally spaced", {
  g <- grid_v(n = 100)
  diffs <- diff(g)
  expect_true(all(abs(diffs - diffs[1]) < 1e-12))
})

test_that("grid_v with n=2 returns exactly the endpoints", {
  g <- grid_v(v_min = -1, v_max = 1, n = 2)
  expect_equal(g, c(-1, 1))
})

test_that("grid_v raises error for n < 2", {
  expect_error(grid_v(n = 1), "n must be >= 2")
  expect_error(grid_v(n = 0), "n must be >= 2")
})

test_that("grid_v default uses 4*sigma-based range and 1201 points", {
  # sigma is defined as 1 in grids.R; use the literal value here to be explicit.
  g <- grid_v()
  expect_length(g, 1201)
  expect_equal(min(g), -4)
  expect_equal(max(g),  4)
})

# ---------------------------------------------------------------------------
# grid_logS
# ---------------------------------------------------------------------------
test_that("grid_logS returns a numeric vector of length n", {
  g <- grid_logS(n = 20)
  expect_type(g, "double")
  expect_length(g, 20)
})

test_that("grid_logS spans [logS_min, logS_max]", {
  g <- grid_logS(-2, 2, 40)
  expect_equal(min(g), -2)
  expect_equal(max(g),  2)
})

test_that("grid_logS is equally spaced", {
  g <- grid_logS(n = 81)
  diffs <- diff(g)
  expect_true(all(abs(diffs - diffs[1]) < 1e-12))
})

test_that("grid_logS raises error for n < 2", {
  expect_error(grid_logS(n = 1), "n must be >= 2")
})

test_that("grid_logS raises error for non-finite bounds", {
  expect_error(grid_logS(-Inf, 4),  "must be finite")
  expect_error(grid_logS(-4, Inf),  "must be finite")
  expect_error(grid_logS(NaN, 4),   "must be finite")
})

test_that("grid_logS raises error when logS_max <= logS_min", {
  expect_error(grid_logS(0, 0),  "logS_max must be >")
  expect_error(grid_logS(1, -1), "logS_max must be >")
})

test_that("grid_logS allows negative logS values", {
  g <- grid_logS(-4, 0, 10)
  expect_true(all(g <= 0))
  expect_true(all(is.finite(g)))
})

# ---------------------------------------------------------------------------
# grid_delta
# ---------------------------------------------------------------------------
test_that("grid_delta returns a numeric vector of length n", {
  g <- grid_delta(n = 20)
  expect_type(g, "double")
  expect_length(g, 20)
})

test_that("grid_delta spans [delta_min, delta_max]", {
  g <- grid_delta(delta_min = 0.1, delta_max = 2, n = 30)
  expect_equal(min(g), 0.1)
  expect_equal(max(g), 2)
})

test_that("grid_delta is equally spaced", {
  g <- grid_delta(n = 61)
  diffs <- diff(g)
  expect_true(all(abs(diffs - diffs[1]) < 1e-12))
})

test_that("grid_delta raises error for n < 2", {
  expect_error(grid_delta(n = 1), "n must be >= 2")
})

test_that("grid_delta raises error when delta_min <= 0", {
  expect_error(grid_delta(delta_min = 0),  "delta_min must be > 0")
  expect_error(grid_delta(delta_min = -1), "delta_min must be > 0")
})

test_that("grid_delta raises error when delta_max <= delta_min", {
  expect_error(grid_delta(delta_min = 1, delta_max = 1),   "delta_max must be > delta_min")
  expect_error(grid_delta(delta_min = 2, delta_max = 0.5), "delta_max must be > delta_min")
})

test_that("all grid_delta values are strictly positive", {
  g <- grid_delta()
  expect_true(all(g > 0))
})

# ---------------------------------------------------------------------------
# expand_grid_df
# ---------------------------------------------------------------------------
test_that("expand_grid_df produces a data.frame with correct dimensions", {
  df <- expand_grid_df(x = 1:3, y = c("a", "b"))
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 6)
  expect_equal(ncol(df), 2)
})

test_that("expand_grid_df has no KEEP.OUT.ATTRS attribute", {
  df <- expand_grid_df(x = 1:2, y = 1:2)
  # The result should be a plain data.frame without extra attributes
  expect_false("out.attrs" %in% names(attributes(df)))
})

test_that("expand_grid_df does not convert characters to factors", {
  df <- expand_grid_df(x = 1:2, y = c("a", "b"))
  expect_type(df$y, "character")
})

test_that("expand_grid_df handles single vector input", {
  df <- expand_grid_df(x = 1:5)
  expect_equal(nrow(df), 5)
})

# ---------------------------------------------------------------------------
# grid_logS_delta
# ---------------------------------------------------------------------------
test_that("grid_logS_delta returns a data.frame with columns logS, delta, S", {
  df <- grid_logS_delta(n_logS = 5, n_delta = 5)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("logS", "delta", "S") %in% names(df)))
})

test_that("grid_logS_delta has n_logS * n_delta rows", {
  n1 <- 5; n2 <- 7
  df <- grid_logS_delta(n_logS = n1, n_delta = n2)
  expect_equal(nrow(df), n1 * n2)
})

test_that("grid_logS_delta: S = exp(logS)", {
  df <- grid_logS_delta(n_logS = 10, n_delta = 10)
  expect_equal(df$S, exp(df$logS))
})

test_that("grid_logS_delta: all delta > 0", {
  df <- grid_logS_delta(n_logS = 5, n_delta = 5)
  expect_true(all(df$delta > 0))
})

test_that("grid_logS_delta: all S > 0", {
  df <- grid_logS_delta(n_logS = 5, n_delta = 5)
  expect_true(all(df$S > 0))
})

test_that("grid_logS_delta respects custom bounds", {
  df <- grid_logS_delta(
    logS_min = -2, logS_max = 2, n_logS = 5,
    delta_min = 0.1, delta_max = 1, n_delta = 4
  )
  expect_equal(min(df$logS), -2)
  expect_equal(max(df$logS),  2)
  expect_equal(min(df$delta), 0.1)
  expect_equal(max(df$delta), 1)
})

test_that("grid_logS_delta: logS values cover full range", {
  df <- grid_logS_delta(logS_min = -3, logS_max = 3, n_logS = 7, n_delta = 3)
  logS_unique <- sort(unique(df$logS))
  expect_equal(logS_unique[1],  -3)
  expect_equal(logS_unique[7],   3)
})

test_that("grid_logS_delta default call does not error", {
  expect_no_error(grid_logS_delta())
})
