# Local test runner for the copied RC_gamma_11 experiment.
#
# This script is intended to be sourced manually from RStudio. It runs the copy
# in RC_gamma_11_local_test and leaves the original RC_gamma_11 folder untouched.
# By default it runs experiment_smoke.yaml, a tiny serial smoke test. To run the
# full copied experiment.yaml, explicitly set RUN_FULL_RC_GAMMA_11 <- TRUE before
# sourcing this script.

script_path <- tryCatch(
  normalizePath(sys.frame(1)$ofile, mustWork = TRUE),
  error = function(e) NA_character_
)

experiment_folder <- if (!is.na(script_path)) {
  dirname(script_path)
} else if (dir.exists("RC_gamma_11_local_test")) {
  normalizePath("RC_gamma_11_local_test", mustWork = TRUE)
} else {
  normalizePath(getwd(), mustWork = TRUE)
}

experiment_folder_location <- dirname(experiment_folder)
experiment_name <- basename(experiment_folder)
experiment_design_filename <- "experiment.yaml"
#  "experiment_smoke.yaml"


if (identical(experiment_design_filename, "experiment.yaml")) {
  warning(
    "Running the full copied RC_gamma_11 experiment. This is large and may exhaust memory if parallel workers are enabled.",
    call. = FALSE
  )
} else {
  message("Running smoke-test specification: ", file.path(experiment_folder, experiment_design_filename))
}

if (file.exists(file.path(getwd(), "DESCRIPTION")) &&
    grepl("^Package:\\s+community\\.simulator\\s*$", readLines(file.path(getwd(), "DESCRIPTION"), n = 1))) {
  devtools::load_all(".")
} else {
  library(community.simulator)
}

run_experiment(
  experiment_folder_location = experiment_folder_location,
  experiment_name = experiment_name,
  experiment_design_filename = experiment_design_filename,
  overwrite = TRUE,
  verbose = TRUE,
  confirm_run = TRUE
)
