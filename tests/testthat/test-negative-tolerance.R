test_that("LV evaluates negative internal trials at the boundary and records them", {
  spec <- read_experiment_spec(system.file("experiment_templates/lv_continuous.yaml",
                                          package = "community.simulator"))
  community <- create_experiment_table_from_spec(spec)$community_object[[1]]
  trial <- -10
  local_mocked_bindings(run_ode_with_captured_output = function(ode_args, failure_prefix) {
    state <- ode_args$y
    state[] <- trial
    derivative <- ode_args$func(0, state, NULL)[[1]]
    expect_equal(unname(derivative), rep(0.1, community$S))
    cbind(time = c(0, 1, 2), matrix(1, nrow = 3, ncol = community$S,
                                  dimnames = list(NULL, names(state))))
  })
  simulate <- function(tolerance) simulator_lv_continuous(
    community, matrix(c(16, 16), nrow = 1), rep(1, community$S),
    times = c(1, 2), negative_tolerance = tolerance)
  output <- simulate(1e-8)
  expect_equal(attr(output, "ode_negative_trials"), list(
    negative_trial_evaluations = 1L,
    minimum_trial_abundance = -10,
    time_of_minimum_trial = 0))
  expect_no_error(simulate(0))
  trial <- Inf
  expect_error(simulate(1e-8), "Non-finite abundance")
  trial <- 1e13
  expect_error(simulate(1e-8), "blow-up threshold")
  trial <- 0
  expect_null(attr(simulate(0), "ode_negative_trials"))
})

test_that("LV clips tolerated negative output and rejects larger negatives", {
  spec <- read_experiment_spec(system.file("experiment_templates/lv_continuous.yaml",
                                          package = "community.simulator"))
  community <- create_experiment_table_from_spec(spec)$community_object[[1]]
  local_mocked_bindings(run_ode_with_captured_output = function(ode_args, failure_prefix) {
    cbind(time = c(0, 1, 2), matrix(-5e-5, nrow = 3, ncol = community$S,
                                  dimnames = list(NULL, names(ode_args$y))))
  })
  simulate <- function(tolerance) simulator_lv_continuous(
    community, matrix(c(16, 16), nrow = 1), rep(1, community$S),
    times = c(1, 2), negative_tolerance = tolerance)
  expect_true(all(simulate(1e-4) == 0))
  expect_error(simulate(1e-6), "returned negative abundances")
  for (invalid in list(-1, NA_real_, Inf, c(1, 2))) {
    expect_error(simulate(invalid), "negative_tolerance")
  }
  expect_error(simulator_lv_continuous(community, matrix(c(16, 16), nrow = 1),
                                       rep(-1, community$S)), "initial_abundances")
})

test_that("YAML negative_tolerance reaches continuous LV simulation", {
  spec <- read_experiment_spec(system.file("experiment_templates/lv_continuous.yaml",
                                          package = "community.simulator"))
  spec$simulation$negative_tolerance <- 0.0002
  spec$simulation$burn_in_duration <- 0
  spec$simulation$experiment_duration <- 3
  spec$output <- list(simulation_progress = FALSE, environment_progress = FALSE,
                      save_dynamics = FALSE, save_resources = FALSE)
  folder <- tempfile("lv-negative-tolerance-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  yaml::write_yaml(unclass(spec), file.path(folder, "experiment.yaml"))
  spec <- read_experiment_spec(file.path(folder, "experiment.yaml"))
  create_experiment_table_from_spec(spec, file.path(folder, "experiment_table.RDS"),
                                     verbose = FALSE)
  community.simulator:::create_environments_from_spec(folder, spec, verbose = FALSE)
  seen <- numeric()
  local_mocked_bindings(simulator_lv_continuous = function(input_com_params,
                                                          output_times,
                                                          negative_tolerance, ...) {
    seen <<- c(seen, negative_tolerance)
    as.data.frame(matrix(1, nrow = length(output_times), ncol = input_com_params$S,
                         dimnames = list(NULL, paste0("Spp", seq_len(input_com_params$S)))))
  })
  community.simulator:::simulate_dynamics_from_spec(folder, spec, verbose = FALSE)
  expect_equal(seen, rep(0.0002, 2))
})

test_that("parent logs LV negative trial diagnostics in serial and parallel runs", {
  local_mocked_bindings(run_ode_with_captured_output = function(ode_args, failure_prefix) {
    trial <- ode_args$y
    trial[] <- -10
    ode_args$func(0.5, trial, NULL)
    cbind(time = ode_args$times,
          matrix(1, nrow = length(ode_args$times), ncol = length(trial),
                 dimnames = list(NULL, names(trial))))
  })
  local_mocked_bindings(execute_stage_chunk = function(payload) {
    assignInNamespace("run_ode_with_captured_output", function(ode_args, failure_prefix) {
      trial <- ode_args$y
      trial[] <- -10
      ode_args$func(0.5, trial, NULL)
      cbind(time = ode_args$times,
            matrix(1, nrow = length(ode_args$times), ncol = length(trial),
                   dimnames = list(NULL, names(trial))))
    }, "community.simulator")
    getFromNamespace("execute_stage_chunk", "community.simulator")(payload)
  })
  for (parallel_run in c(FALSE, TRUE)) {
    if (parallel_run && .Platform$OS.type == "windows") next
    spec <- read_experiment_spec(system.file("experiment_templates/lv_continuous.yaml",
                                            package = "community.simulator"))
    spec$simulation$burn_in_duration <- 0
    spec$simulation$experiment_duration <- 3
    spec$output <- list(simulation_progress = FALSE, environment_progress = FALSE,
                        save_dynamics = FALSE, save_resources = FALSE)
    spec$parallel$workers <- 2
    spec$parallel$simulations <- parallel_run
    folder <- tempfile("lv-negative-trial-log-")
    dir.create(folder)
    create_experiment_table_from_spec(spec, file.path(folder, "experiment_table.RDS"),
                                       verbose = FALSE)
    community.simulator:::create_environments_from_spec(folder, spec, verbose = FALSE)
    community.simulator:::simulate_dynamics_from_spec(folder, spec, verbose = FALSE)
    events <- lapply(readLines(file.path(folder, "experiment_log.txt")), jsonlite::fromJSON)
    events <- Filter(function(x) identical(x$event, "ode_negative_trials"), events)
    expect_length(events, 2)
    expect_setequal(vapply(events, `[[`, character(1), "case_id"), c("case_1", "case_2"))
    for (event in events) {
      expect_equal(event$event, "ode_negative_trials")
      expect_equal(event$negative_trial_evaluations, 1)
      expect_equal(event$minimum_trial_abundance, -10)
      expect_equal(event$time_of_minimum_trial, 0.5)
    }
    unlink(folder, recursive = TRUE)
  }
})
