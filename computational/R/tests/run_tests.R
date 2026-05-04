# computational/R/tests/run_tests.R
# Entry point: run all testthat test files in this directory.
#
# Usage (from the repository root):
#   Rscript computational/R/tests/run_tests.R
#
# Exit code: 0 if all tests pass, non-zero otherwise.

library(testthat)

# Determine the directory of this script.
this_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NULL)
test_dir  <- if (!is.null(this_file)) dirname(this_file) else
               normalizePath("computational/R/tests")

result <- testthat::test_dir(
  test_dir,
  reporter        = testthat::default_reporter(),
  stop_on_failure = FALSE
)

# Check for failures across all test results.
failed <- sum(vapply(result, function(x) {
  f <- x$failed
  if (length(f) == 0L) 0L else as.integer(f)
}, integer(1)))

if (failed > 0L) {
  quit(save = "no", status = 1)
}
