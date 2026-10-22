test_that("unknown settings are rejected throughout nested specifications", {
  spec <- read_experiment_spec(system.file(
    "experiment_templates/lv_continuous_rich.yaml", package = "community.simulator"
  ))
  paths <- c("unknown", "experiment.unknown", "model.unknown", "community.unknown",
    "traits.unknown", "traits.birth_width.unknown", "traits.death.unknown",
    "environment.unknown", "environment.temperature.unknown", "simulation.unknown",
    "simulation.ode.unknown", "output.unknown", "parallel.unknown", "measures.unknown",
    "interactions.unknown", "interactions.selected.unknown", "treatments.unknown")
  for (path in paths) {
    changed <- community.simulator:::set_dotted_path(spec, path, TRUE)
    expect_error(validate_experiment_spec(changed), path, fixed = TRUE)
  }
  spec$interactions$treatments[[2]]$parameters$sd_log <- 1
  expect_error(validate_experiment_spec(spec),
    "interactions.treatments[[2]].parameters.sd_log", fixed = TRUE)
  expect_error(validate_experiment_spec(spec), "Did you mean", fixed = TRUE)

  cr <- read_experiment_spec(system.file(
    "experiment_templates/consumer_resource.yaml", package = "community.simulator"
  ))
  for (path in c("resources.unknown", "resources.private_use.unknown", "traits.uptake_width.unknown")) {
    expect_error(validate_experiment_spec(community.simulator:::set_dotted_path(cr, path, 1)), path, fixed = TRUE)
  }
})

test_that("YAML errors include the filename, full key and a useful correction", {
  spec <- yaml::read_yaml(system.file(
    "experiment_templates/lv_discrete.yaml", package = "community.simulator"
  ))
  spec$output <- list(record_dynamics_wall_tim = TRUE)
  path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(spec, path)
  message <- tryCatch(read_experiment_spec(path), error = conditionMessage)
  expect_match(message, path, fixed = TRUE)
  expect_match(message, "output.record_dynamics_wall_tim", fixed = TRUE)
  expect_match(message, "Did you mean `output.record_dynamics_wall_time`?", fixed = TRUE)
  expect_match(message, "Allowed settings", fixed = TRUE)
  spec$output <- list(completely_unrelated_option = NULL)
  expect_error(validate_experiment_spec(spec), "output.completely_unrelated_option", fixed = TRUE)
})

test_that("factorial and paired treatment typos fail at read time", {
  spec <- yaml::read_yaml(system.file(
    "experiment_templates/lv_discrete.yaml", package = "community.simulator"
  ))
  for (mode in c("factorial", "paired")) {
    values <- list("environment.temperature.one_over_f_gama" = c(0, 1))
    spec$treatments <- list(mode = mode, values = if (mode == "paired") list(values) else values)
    path <- tempfile(fileext = ".yaml")
    yaml::write_yaml(spec, path)
    expect_error(read_experiment_spec(path), "Did you mean `environment.temperature.one_over_f_gamma`?", fixed = TRUE)
  }
})

test_that("all templates and generated case specifications remain accepted", {
  paths <- list.files(system.file("experiment_templates", package = "community.simulator"),
                      pattern = "[.]yaml$", full.names = TRUE)
  for (path in paths) {
    spec <- read_experiment_spec(path)
    expect_silent(validate_experiment_spec(spec))
    cases <- create_experiment_table_from_spec(spec)
    expect_silent(validate_experiment_spec(cases$case_spec[[1]]))
  }
})
