# computational/R/tests/test-manifest.R
# Tests for computational/R/functions/manifest.R

library(testthat)

# Locate the repo root regardless of working directory when tests run.
.find_root <- function() {
  d <- getwd()
  for (i in seq_len(6)) {
    if (file.exists(file.path(d, "computational", "R", "functions", "manifest.R"))) return(d)
    d <- dirname(d)
  }
  stop("Cannot find repo root from: ", getwd())
}
.root <- .find_root()

source(file.path(.root, "computational", "R", "functions", "manifest.R"))


# ---------------------------------------------------------------------------
# sha256_file
# ---------------------------------------------------------------------------
test_that("sha256_file returns a non-empty string for an existing file", {
  tmp <- tempfile()
  writeLines("hello world", tmp)
  on.exit(unlink(tmp))

  h <- sha256_file(tmp)
  expect_type(h, "character")
  expect_true(nchar(h) > 0)
})

test_that("sha256_file returns the same hash for identical content", {
  tmp1 <- tempfile(); tmp2 <- tempfile()
  writeLines("same content", tmp1)
  writeLines("same content", tmp2)
  on.exit({ unlink(tmp1); unlink(tmp2) })

  expect_equal(sha256_file(tmp1), sha256_file(tmp2))
})

test_that("sha256_file returns different hashes for different content", {
  tmp1 <- tempfile(); tmp2 <- tempfile()
  writeLines("content A", tmp1)
  writeLines("content B", tmp2)
  on.exit({ unlink(tmp1); unlink(tmp2) })

  expect_false(sha256_file(tmp1) == sha256_file(tmp2))
})

test_that("sha256_file errors when the file does not exist", {
  expect_error(sha256_file("/nonexistent/path/file.csv"), "File not found")
})

test_that("sha256_file handles an empty file", {
  tmp <- tempfile()
  file.create(tmp)
  on.exit(unlink(tmp))

  h <- sha256_file(tmp)
  expect_type(h, "character")
  expect_true(nchar(h) > 0)
})

# ---------------------------------------------------------------------------
# git_commit_hash
# ---------------------------------------------------------------------------
test_that("git_commit_hash returns a character scalar", {
  h <- git_commit_hash()
  expect_type(h, "character")
  expect_length(h, 1)
})

test_that("git_commit_hash is either NA or a 40-char hex string", {
  h <- git_commit_hash()
  if (!is.na(h)) {
    expect_match(h, "^[0-9a-f]{40}$")
  } else {
    expect_true(is.na(h))
  }
})

# ---------------------------------------------------------------------------
# git_is_dirty
# ---------------------------------------------------------------------------
test_that("git_is_dirty returns a single logical value", {
  d <- git_is_dirty()
  expect_type(d, "logical")
  expect_length(d, 1)
})

# ---------------------------------------------------------------------------
# write_manifest_json
# ---------------------------------------------------------------------------
make_manifest_args <- function(out_dir) {
  tmp_file <- tempfile(tmpdir = out_dir, fileext = ".json")
  list(
    manifest_path = tmp_file,
    run_id        = "test_run_001",
    script_path   = "computational/R/runners/01_single_agent_grid.R",
    parameters    = list(sigma = 1, n_v = 100),
    outputs       = list(list(path = "output/test.csv", bytes = 1234L, sha256 = "abc"))
  )
}

test_that("write_manifest_json creates a file and returns the manifest list", {
  out_dir <- tempdir()
  args    <- make_manifest_args(out_dir)
  on.exit(unlink(args$manifest_path))

  result <- do.call(write_manifest_json, args)
  expect_true(file.exists(args$manifest_path))
  expect_type(result, "list")
})

test_that("write_manifest_json result contains required top-level fields", {
  out_dir <- tempdir()
  args    <- make_manifest_args(out_dir)
  on.exit(unlink(args$manifest_path))

  m <- do.call(write_manifest_json, args)
  expect_true("run_id"      %in% names(m))
  expect_true("created_utc" %in% names(m))
  expect_true("git"         %in% names(m))
  expect_true("script"      %in% names(m))
  expect_true("parameters"  %in% names(m))
  expect_true("outputs"     %in% names(m))
  expect_true("environment" %in% names(m))
})

test_that("write_manifest_json records the correct run_id", {
  out_dir <- tempdir()
  args    <- make_manifest_args(out_dir)
  on.exit(unlink(args$manifest_path))

  m <- do.call(write_manifest_json, args)
  expect_equal(m$run_id, "test_run_001")
})

test_that("write_manifest_json records the correct script path", {
  out_dir <- tempdir()
  args    <- make_manifest_args(out_dir)
  on.exit(unlink(args$manifest_path))

  m <- do.call(write_manifest_json, args)
  expect_equal(m$script, args$script_path)
})

test_that("write_manifest_json records the parameters correctly", {
  out_dir <- tempdir()
  args    <- make_manifest_args(out_dir)
  on.exit(unlink(args$manifest_path))

  m <- do.call(write_manifest_json, args)
  expect_equal(m$parameters$sigma, 1)
  expect_equal(m$parameters$n_v, 100)
})

test_that("write_manifest_json environment contains R_version", {
  out_dir <- tempdir()
  args    <- make_manifest_args(out_dir)
  on.exit(unlink(args$manifest_path))

  m <- do.call(write_manifest_json, args)
  expect_true("R_version" %in% names(m$environment))
  expect_true(nchar(m$environment$R_version) > 0)
})

test_that("write_manifest_json writes valid JSON when jsonlite is available", {
  skip_if_not_installed("jsonlite")
  out_dir <- tempdir()
  args    <- make_manifest_args(out_dir)
  on.exit(unlink(args$manifest_path))

  do.call(write_manifest_json, args)
  raw <- readLines(args$manifest_path, warn = FALSE)
  parsed <- jsonlite::fromJSON(paste(raw, collapse = "\n"))
  expect_equal(parsed$run_id, "test_run_001")
})

test_that("write_manifest_json creates parent directories if they don't exist", {
  out_dir <- file.path(tempdir(), "nested_manifest_test_dir")
  on.exit(unlink(out_dir, recursive = TRUE))

  manifest_file <- file.path(out_dir, "subdir", "manifest.json")
  args <- list(
    manifest_path = manifest_file,
    run_id        = "nested_test",
    script_path   = "script.R",
    parameters    = list(),
    outputs       = list()
  )
  expect_no_error(do.call(write_manifest_json, args))
  expect_true(file.exists(manifest_file))
})

test_that("write_manifest_json git field contains commit and repo_dirty", {
  out_dir <- tempdir()
  args    <- make_manifest_args(out_dir)
  on.exit(unlink(args$manifest_path))

  m <- do.call(write_manifest_json, args)
  expect_true("commit"     %in% names(m$git))
  expect_true("repo_dirty" %in% names(m$git))
})

test_that("write_manifest_json created_utc looks like a UTC timestamp", {
  out_dir <- tempdir()
  args    <- make_manifest_args(out_dir)
  on.exit(unlink(args$manifest_path))

  m <- do.call(write_manifest_json, args)
  expect_match(m$created_utc, "UTC")
})
