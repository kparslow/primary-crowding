# computational/R/functions/manifest.R
# Creates small, commit-friendly manifests that describe how large outputs were generated.

sha256_file <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  # tools::md5sum is built-in, but SHA-256 is nicer for verification.
  # Prefer digest if available; fall back to md5 otherwise.
  if (requireNamespace("digest", quietly = TRUE)) {
    return(unname(digest::digest(file = path, algo = "sha256")))
  }
  warning("Package 'digest' not installed; falling back to MD5 via tools::md5sum().")
  return(unname(tools::md5sum(path)))
}

git_commit_hash <- function() {
  out <- tryCatch(
    system("git rev-parse HEAD", intern = TRUE),
    error = function(e) character(0)
  )
  if (length(out) == 0) return(NA_character_)
  trimws(out[[1]])
}

git_is_dirty <- function() {
  out <- tryCatch(
    system("git status --porcelain", intern = TRUE),
    error = function(e) character(0)
  )
  length(out) > 0
}

write_manifest_json <- function(
    manifest_path,
    run_id,
    script_path,
    parameters,
    outputs
) {
  dir.create(dirname(manifest_path), recursive = TRUE, showWarnings = FALSE)
  
  manifest <- list(
    run_id = run_id,
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    git = list(
      commit = git_commit_hash(),
      repo_dirty = git_is_dirty()
    ),
    script = script_path,
    parameters = parameters,
    outputs = outputs,
    environment = list(
      R_version = as.character(getRversion()),
      platform = R.version$platform,
      packages = list(
        digest = if (requireNamespace("digest", quietly = TRUE))
          as.character(utils::packageVersion("digest")) else NA_character_
      )
    )
  )
  
  # Use jsonlite if available; otherwise write a dput() manifest.
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    json <- jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = "null")
    writeLines(json, manifest_path, useBytes = TRUE)
  } else {
    warning("Package 'jsonlite' not installed; writing .R dput manifest instead.")
    alt_path <- sub("\\.json$", ".R", manifest_path)
    dput(manifest, file = alt_path)
  }
  
  invisible(manifest)
}