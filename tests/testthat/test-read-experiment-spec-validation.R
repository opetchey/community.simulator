test_that("YAML validation rejects invalid model names", {
  spec <- read_experiment_spec(system.file(
    "experiment_templates/lv_discrete.yaml",
    package = "community.simulator"
  ))
  spec$model$type <- "lv"

  expect_error(
    validate_experiment_spec(spec),
    "`model.type` must be one of:",
    fixed = TRUE
  )
})

test_that("YAML validation rejects invalid LV distributions and widths", {
  spec <- read_experiment_spec(system.file(
    "experiment_templates/lv_discrete.yaml",
    package = "community.simulator"
  ))
  spec$traits$birth_width$distribution <- "normal"

  expect_error(
    validate_experiment_spec(spec),
    "`traits.birth_width.distribution` must be one of:",
    fixed = TRUE
  )

  spec <- read_experiment_spec(system.file(
    "experiment_templates/lv_discrete.yaml",
    package = "community.simulator"
  ))
  spec$traits$birth_width$mean <- 1
  spec$traits$birth_width$range <- 4

  expect_error(
    validate_experiment_spec(spec),
    "`traits.birth_width.mean - 0.5 * traits.birth_width.range` must be positive",
    fixed = TRUE
  )
})

test_that("YAML validation rejects invalid LV interaction specifications", {
  spec <- read_experiment_spec(system.file(
    "experiment_templates/lv_discrete.yaml",
    package = "community.simulator"
  ))
  spec$interactions$treatments[[2]]$type <- "predator_prey"
  spec$interactions$treatments[[2]]$symmetry <- "symmetric"

  expect_error(
    validate_experiment_spec(spec),
    "predator-prey interactions cannot use symmetric symmetry",
    fixed = TRUE
  )

  spec <- read_experiment_spec(system.file(
    "experiment_templates/lv_discrete.yaml",
    package = "community.simulator"
  ))
  spec$interactions$treatments[[2]]$distribution <- "uniform"
  spec$interactions$treatments[[2]]$parameters$min <- 2
  spec$interactions$treatments[[2]]$parameters$max <- 1

  expect_error(
    validate_experiment_spec(spec),
    "max` must be greater than or equal to",
    fixed = TRUE
  )
})

test_that("YAML validation rejects invalid CR resource settings", {
  spec <- read_experiment_spec(system.file(
    "experiment_templates/consumer_resource.yaml",
    package = "community.simulator"
  ))
  spec$resources$use_mode <- "shared"

  expect_error(
    validate_experiment_spec(spec),
    "`resources.use_mode` must be one of:",
    fixed = TRUE
  )

  spec <- read_experiment_spec(system.file(
    "experiment_templates/consumer_resource.yaml",
    package = "community.simulator"
  ))
  spec$resources$private_use$distribution <- "beta"
  spec$resources$private_use$mean <- 1

  expect_error(
    validate_experiment_spec(spec),
    "`resources.private_use.mean` must be greater than 0 and less than 1",
    fixed = TRUE
  )
})

test_that("YAML validation rejects invalid runtime controls", {
  spec <- read_experiment_spec(system.file(
    "experiment_templates/lv_continuous.yaml",
    package = "community.simulator"
  ))
  spec$simulation$experiment_duration <- 0

  expect_error(
    validate_experiment_spec(spec),
    "`simulation.experiment_duration` must be an integer >= 1",
    fixed = TRUE
  )

  spec <- read_experiment_spec(system.file(
    "experiment_templates/lv_continuous.yaml",
    package = "community.simulator"
  ))
  spec$parallel$workers <- 1.5

  expect_error(
    validate_experiment_spec(spec),
    "`parallel.workers` must be an integer",
    fixed = TRUE
  )

  spec$parallel$workers <- "available_cores_minus_1"
  expect_no_error(validate_experiment_spec(spec))

  spec$parallel$workers <- "many"
  expect_error(
    validate_experiment_spec(spec),
    "`parallel.workers` must be one of",
    fixed = TRUE
  )
})
