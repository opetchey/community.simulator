test_that("dynamics timing option defaults off and requires a scalar logical", {
  spec <- read_experiment_spec(system.file(
    "experiment_templates/lv_discrete.yaml", package = "community.simulator"
  ))
  expect_false(community.simulator:::flatten_spec_settings(spec)$record_dynamics_wall_time)
  for (value in list("true", 1, NA, c(TRUE, FALSE))) {
    spec$output$record_dynamics_wall_time <- value
    expect_error(validate_experiment_spec(spec), "output.record_dynamics_wall_time", fixed = TRUE)
  }
  spec$output$record_dynamics_wall_time <- TRUE
  expect_silent(validate_experiment_spec(spec))
  expect_true(community.simulator:::flatten_spec_settings(spec)$record_dynamics_wall_time)
})

test_that("per-case timing preserves results in serial and parallel for every model", {
  skip_on_os("windows")
  timing_column <- "dynamics_wall_time_seconds"
  for (template in c("lv_discrete", "lv_continuous", "consumer_resource")) {
    folder <- tempfile(paste0("timing-", template, "-"))
    dir.create(folder)
    spec <- read_experiment_spec(system.file(
      paste0("experiment_templates/", template, ".yaml"), package = "community.simulator"
    ))
    spec$community$replicates <- 2L
    spec$simulation$burn_in_duration <- 2L
    spec$simulation$experiment_duration <- 4L
    spec$simulation$initial_abundance_seed_base <- 123L
    spec$output <- list(save_dynamics = FALSE, save_resources = FALSE,
                        simulation_progress = FALSE, environment_progress = FALSE)
    spec$parallel <- list(workers = 2L, simulations = FALSE)
    yaml::write_yaml(spec, file.path(folder, "experiment.yaml"))
    outputs <- run_experiment(
      experiment_folder_location = dirname(folder), experiment_name = basename(folder),
      experiment_design_filename = "experiment.yaml", overwrite = TRUE,
      verbose = FALSE, confirm_run = FALSE
    )
    baseline <- readRDS(outputs$simulation_summaries)
    baseline_populations <- readRDS(outputs$population_summaries)
    expect_false(timing_column %in% names(baseline))
    expect_gt(nrow(baseline), 1L)
    # A deliberate wait distinguishes wall time from CPU time, including in workers.
    simulator_name <- switch(template,
      lv_discrete = "simulator_lv_discrete",
      lv_continuous = "simulator_lv_continuous",
      consumer_resource = "simulator_consumer_resource_continuous"
    )
    original_simulator <- getFromNamespace(simulator_name, "community.simulator")
    delayed_simulator <- local({
      original <- original_simulator
      function(...) {
        Sys.sleep(0.03)
        original(...)
      }
    })
    do.call(local_mocked_bindings, c(
      setNames(list(delayed_simulator), simulator_name),
      list(.package = "community.simulator", .env = environment())
    ))
    local_mocked_bindings(execute_stage_chunk = function(payload) {
      name <- switch(payload$settings$dynamics_type, discrete = "simulator_lv_discrete",
                     continuous = "simulator_lv_continuous",
                     consumer_resource_continuous = "simulator_consumer_resource_continuous")
      original <- getFromNamespace(name, "community.simulator")
      delayed <- function(...) { Sys.sleep(0.03); original(...) }
      assignInNamespace(name, delayed, "community.simulator")
      getFromNamespace("execute_stage_chunk", "community.simulator")(payload)
    })
    for (parallel in c(FALSE, TRUE)) {
      spec$output$record_dynamics_wall_time <- TRUE
      spec$parallel$simulations <- parallel
      community.simulator:::simulate_dynamics_from_spec(folder, spec, overwrite = TRUE, verbose = FALSE)
      timed <- readRDS(outputs$simulation_summaries)
      expect_identical(timed$case_id, baseline$case_id)
      expect_equal(anyDuplicated(timed$case_id), 0L)
      expect_type(timed[[timing_column]], "double")
      expect_true(all(is.finite(timed[[timing_column]]) & timed[[timing_column]] >= 0.02))
      expect_equal(timed[setdiff(names(timed), timing_column)], baseline)
      expect_equal(readRDS(outputs$population_summaries), baseline_populations)
      expect_false(file.exists(file.path(folder, "dynamics.db")))
    }
  }
})
