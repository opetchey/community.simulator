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
  expect_true(file.exists(outputs$temperatures_db))
  expect_true(file.exists(outputs$simulation_summaries))
  expect_true(file.exists(outputs$population_summaries))
  expect_true(file.exists(outputs$community_measures))

  experiment_table <- readRDS(outputs$experiment_table)
  community_measures <- readRDS(outputs$community_measures)

  expect_equal(nrow(experiment_table), 2)
  expect_equal(nrow(community_measures), 2)
  expect_true("case_spec" %in% names(experiment_table))
  expect_true("community_object" %in% names(experiment_table))
  expect_true("cv_total_abundance" %in% names(community_measures))
  expect_true("realized_mean_performance_optimum" %in% names(community_measures))
  expect_true("performance_optimum_trait" %in% names(community_measures))
  expect_false("CV_totab" %in% names(community_measures))
  expect_false("real_mean_b_opt" %in% names(community_measures))
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
