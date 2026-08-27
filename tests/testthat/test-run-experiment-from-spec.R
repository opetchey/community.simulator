test_that("YAML LV experiment runs end to end", {
  output_root <- tempdir()
  experiment_name <- paste0("community-simulator-yaml-", Sys.getpid())
  output_dir <- file.path(output_root, experiment_name)
  spec_path <- system.file(
    "experiment_templates/lv_discrete.yaml",
    package = "community.simulator"
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(spec_path, file.path(output_dir, "experiment.yaml"), overwrite = TRUE)

  outputs <- run_experiment(
    experiment_folder_location = output_root,
    experiment_name = experiment_name,
    experiment_design_filename = "experiment.yaml",
    overwrite = TRUE,
    verbose = FALSE,
    confirm_run = FALSE
  )

  expect_true(file.exists(outputs$experiment_table))
  expect_true(file.exists(outputs$environment_table))
  expect_true(file.exists(outputs$simulation_table))
  expect_true(file.exists(outputs$community_objects))
  expect_true(file.exists(outputs$temperatures_db))
  expect_true(file.exists(outputs$simulation_summaries))
  expect_true(file.exists(outputs$population_summaries))
  expect_true(file.exists(outputs$community_measures))

  experiment_table <- readRDS(outputs$experiment_table)
  environment_table <- readRDS(outputs$environment_table)
  simulation_table <- readRDS(outputs$simulation_table)
  community_objects <- readRDS(outputs$community_objects)
  community_measures <- readRDS(outputs$community_measures)

  expect_equal(nrow(experiment_table), 2)
  expect_equal(nrow(environment_table), 1)
  expect_equal(nrow(simulation_table), 2)
  expect_equal(nrow(community_objects), 2)
  expect_equal(nrow(community_measures), 2)
  expect_true("case_spec" %in% names(experiment_table))
  expect_true("community_object" %in% names(experiment_table))
  expect_false("case_spec" %in% names(simulation_table))
  expect_false("community_object" %in% names(simulation_table))
  expect_true("community_object" %in% names(community_objects))
  expect_true("cv_total_abundance" %in% names(community_measures))
  expect_true("realized_mean_performance_optimum" %in% names(community_measures))
  expect_true("performance_optimum_trait" %in% names(community_measures))
  expect_false("CV_totab" %in% names(community_measures))
  expect_false("real_mean_b_opt" %in% names(community_measures))

  preflight <- community.simulator:::estimate_experiment_outputs_from_spec(
    output_dir,
    read_experiment_spec(file.path(output_dir, "experiment.yaml"))
  )
  expect_gt(preflight$estimated_runtime_input_memory_bytes, 0)
  expect_gt(preflight$estimated_simulation_stage_memory_bytes, 0)
  expect_gt(preflight$estimated_environment_stage_memory_bytes, 0)

  temperature_connection <- DBI::dbConnect(RSQLite::SQLite(), outputs$temperatures_db)
  on.exit(DBI::dbDisconnect(temperature_connection), add = TRUE)
  temperature_rows <- DBI::dbGetQuery(
    temperature_connection,
    "select count(*) as n_rows, count(distinct env_series_id) as n_env from temperatures"
  )
  settings <- community.simulator:::flatten_spec_settings(
    read_experiment_spec(file.path(output_dir, "experiment.yaml"))
  )
  expect_equal(
    temperature_rows$n_rows,
    temperature_rows$n_env * settings$experiment_duration
  )
})

test_that("all YAML model templates run end to end", {
  templates <- c(
    "lv_discrete.yaml",
    "lv_continuous.yaml",
    "consumer_resource.yaml"
  )

  for (template in templates) {
    output_root <- tempdir()
    experiment_name <- paste0(
      "community-simulator-yaml-",
      tools::file_path_sans_ext(template),
      "-",
      Sys.getpid()
    )
    output_dir <- file.path(output_root, experiment_name)
    spec_path <- system.file(
      file.path("experiment_templates", template),
      package = "community.simulator"
    )
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(spec_path, file.path(output_dir, "experiment.yaml"), overwrite = TRUE)
    spec <- yaml::read_yaml(file.path(output_dir, "experiment.yaml"))
    spec$simulation$burn_in_duration <- 2
    spec$simulation$experiment_duration <- 4
    yaml::write_yaml(spec, file.path(output_dir, "experiment.yaml"))

    outputs <- run_experiment(
      experiment_folder_location = output_root,
      experiment_name = experiment_name,
      experiment_design_filename = "experiment.yaml",
      overwrite = TRUE,
      verbose = FALSE,
      confirm_run = FALSE
    )

    expect_true(file.exists(outputs$community_measures))
    expect_gt(nrow(readRDS(outputs$community_measures)), 0)
  }
})

test_that("progress reporter prints sparse elapsed-time updates", {
  reporter <- community.simulator:::make_progress_reporter(
    label = "Simulating cases",
    total = 250,
    update_every = 100,
    enabled = TRUE
  )

  expect_silent(reporter(99))
  expect_message(
    reporter(100),
    "Simulating cases: 100 of 250 .* elapsed: .* estimated remaining:"
  )
  expect_message(
    reporter(250),
    "Simulating cases: 250 of 250 .* estimated remaining: 0 sec"
  )
})

write_parallel_regression_spec <- function(template, output_dir, mutate_spec) {
  spec_path <- system.file(
    file.path("experiment_templates", template),
    package = "community.simulator"
  )
  spec <- yaml::read_yaml(spec_path)
  spec <- mutate_spec(spec)
  yaml::write_yaml(spec, file.path(output_dir, "experiment.yaml"))
  invisible(file.path(output_dir, "experiment.yaml"))
}

read_distinct_case_ids <- function(db_path, table) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbGetQuery(
    conn,
    paste0("select distinct case_id from ", table, " order by case_id")
  )$case_id
}

test_that("parallel simulations append all LV dynamics cases", {
  skip_on_os("windows")

  output_root <- tempdir()
  experiment_name <- paste0("parallel-lv-dynamics-", Sys.getpid())
  output_dir <- file.path(output_root, experiment_name)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  write_parallel_regression_spec(
    "lv_continuous.yaml",
    output_dir,
    function(spec) {
      spec$community$replicates <- 2L
      spec$simulation$burn_in_duration <- 2L
      spec$simulation$experiment_duration <- 4L
      spec$output <- list(
        save_dynamics = TRUE,
        dynamics_save_every = 1L,
        runtime_update_every = 100L,
        simulation_progress = FALSE,
        environment_progress = FALSE
      )
      spec$parallel <- list(
        workers = 2L,
        environments = FALSE,
        simulations = TRUE,
        community_measures = FALSE
      )
      spec
    }
  )

  outputs <- run_experiment(
    experiment_folder_location = output_root,
    experiment_name = experiment_name,
    experiment_design_filename = "experiment.yaml",
    overwrite = TRUE,
    verbose = FALSE,
    confirm_run = FALSE
  )

  expected_case_ids <- sort(readRDS(outputs$simulation_table)$case_id)
  written_case_ids <- read_distinct_case_ids(outputs$dynamics_db, "dynamics")

  expect_gt(length(expected_case_ids), 1)
  expect_equal(written_case_ids, expected_case_ids)
})

test_that("parallel simulations append all consumer-resource dynamics and resources cases", {
  skip_on_os("windows")

  output_root <- tempdir()
  experiment_name <- paste0("parallel-cr-dynamics-resources-", Sys.getpid())
  output_dir <- file.path(output_root, experiment_name)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  write_parallel_regression_spec(
    "consumer_resource.yaml",
    output_dir,
    function(spec) {
      spec$community$replicates <- 2L
      spec$simulation$burn_in_duration <- 2L
      spec$simulation$experiment_duration <- 4L
      spec$output <- list(
        save_dynamics = TRUE,
        save_resources = TRUE,
        dynamics_save_every = 1L,
        resources_save_every = 1L,
        runtime_update_every = 100L,
        simulation_progress = FALSE,
        environment_progress = FALSE
      )
      spec$parallel <- list(
        workers = 2L,
        environments = FALSE,
        simulations = TRUE,
        community_measures = FALSE
      )
      spec
    }
  )

  outputs <- run_experiment(
    experiment_folder_location = output_root,
    experiment_name = experiment_name,
    experiment_design_filename = "experiment.yaml",
    overwrite = TRUE,
    verbose = FALSE,
    confirm_run = FALSE
  )

  expected_case_ids <- sort(readRDS(outputs$simulation_table)$case_id)
  dynamics_case_ids <- read_distinct_case_ids(outputs$dynamics_db, "dynamics")
  resources_case_ids <- read_distinct_case_ids(outputs$resources_db, "resources")

  expect_gt(length(expected_case_ids), 1)
  expect_equal(dynamics_case_ids, expected_case_ids)
  expect_equal(resources_case_ids, expected_case_ids)
})
