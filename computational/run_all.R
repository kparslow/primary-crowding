cat > computational/run_all.R <<'EOF'
# Entry point for computational explorations

out_data_dir <- file.path("evidence", "datasets")
out_fig_dir <- file.path("evidence", "figures")
out_meta_dir <- file.path("evidence", "run-metadata")

dir.create(out_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_meta_dir, recursive = TRUE, showWarnings = FALSE)

run_id <- format(Sys.time(), "%Y%m%d-%H%M%S")

# record run metadata
capture.output(sessionInfo(), file = file.path(out_meta_dir, paste0("sessionInfo_", run_id, ".txt")))
writeLines(c(
  paste0("run_id: ", run_id),
  paste0("started: ", Sys.time())
), con = file.path(out_meta_dir, paste0("run_", run_id, ".txt")))

message("Initialized computational run: ", run_id)

EOF
