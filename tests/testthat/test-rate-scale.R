rate_scale_spec <- function(model = "lv_continuous") {
  template <- if (model == "lv_continuous") "lv_continuous.yaml" else "consumer_resource.yaml"
  spec <- read_experiment_spec(system.file("experiment_templates", template,
                                          package = "community.simulator"))
  spec$simulation$rate_scale <- NULL
  spec$community$richness <- 3
  spec$simulation$burn_in_duration <- 0
  spec$simulation$experiment_duration <- 12
  spec$environment$temperature$mean <- 16
  spec$environment$temperature$sd <- 0.2
  if (model == "lv_continuous") {
    spec$interactions$treatments <- spec$interactions$treatments[2]
    spec$traits$birth_maximum$range <- 0.1
    spec$traits$death$intercept <- 0.02
  } else {
    spec$traits$uptake_maximum$range <- 0.05
  }
  spec
}

rate_scale_trajectory <- function(spec, scale, times, initial = NULL,
                                 rtol = 1e-9, atol = 1e-10) {
  spec$simulation$rate_scale <- scale
  scale <- scale %||% 1
  case <- create_experiment_table_from_spec(spec)$case_spec[[1]]
  community <- build_community_from_spec(case)
  if (is.null(initial)) initial <- rep(10, community$S)
  common <- list(input_com_params = community,
                 TcelSeries = matrix(c(16, 16), nrow = 1),
                 times = c(0, max(times)), output_times = times,
                 rtol = rtol, atol = atol, max_step = 1)
  if (spec$model$type == "lv_continuous") {
    return(do.call(simulator_lv_continuous, c(common, list(
      initial_abundances = initial,
      immigration_rate = spec$simulation$immigration_rate * scale))))
  }
  result <- do.call(simulator_consumer_resource_continuous, c(common, list(
    initial_consumer_abundances = initial,
    initial_resource_values = rep(spec$simulation$resource_initial_value, community$R),
    consumer_immigration_rate = spec$simulation$consumer_immigration_rate * scale)))
  cbind(result$consumers, result$resources)
}

test_that("rate_scale is strictly validated and defaults to one", {
  spec <- rate_scale_spec()
  expect_identical(community.simulator:::flatten_spec_settings(spec)$rate_scale, 1)
  for (invalid in list(0, -1, NA_real_, Inf, c(1, 2), "2", "fast", TRUE, NULL)) {
    bad <- spec
    bad$simulation["rate_scale"] <- list(invalid)
    expect_error(create_experiment_table_from_spec(bad), "simulation.rate_scale")
  }
  spec$simulation$rate_scale <- 2
  expect_equal(community.simulator:::flatten_spec_settings(spec)$rate_scale, 2)
  spec$simulation$immigration_mode <- "pulse"
  expect_error(create_experiment_table_from_spec(spec), "pulse")
  spec$model$type <- "lv_discrete"
  expect_error(create_experiment_table_from_spec(spec), "lv_discrete")
  spec <- rate_scale_spec()
  spec$simulation$rate_scal <- 2
  expect_error(create_experiment_table_from_spec(spec), "Unknown simulation option.*rate_scal")
})

test_that("unit scaling preserves communities and random state exactly", {
  for (model in c("lv_continuous", "consumer_resource_continuous")) {
    spec <- rate_scale_spec(model)
    baseline <- create_experiment_table_from_spec(spec)
    baseline_rng <- .Random.seed
    spec$simulation$rate_scale <- 1
    unit <- create_experiment_table_from_spec(spec)
    expect_identical(unit$community_object, baseline$community_object)
    expect_identical(.Random.seed, baseline_rng)
    expect_identical(unit$env_series_id, baseline$env_series_id)
    expect_identical(unit$temperature_seed, baseline$temperature_seed)
    expect_identical(rate_scale_trajectory(spec, NULL, 1:10),
                     rate_scale_trajectory(spec, 1, 1:10))
  }
})

test_that("realised rate fields and aliases scale after matched random draws", {
  for (model in c("lv_continuous", "consumer_resource_continuous")) {
    spec <- rate_scale_spec(model)
    spec$community$replicates <- 2
    spec$simulation$rate_scale <- 1
    spec$treatments <- list(values = list(
      "simulation.rate_scale" = c(0.5, 1, 2),
      "environment.temperature.sd" = c(0.2, 0.4)))
    table <- create_experiment_table_from_spec(spec)
    scaled_fields <- if (model == "lv_continuous") {
      c("a_b_i", "birth_maximum_i", "a_d_i", "death_intercept_i", "alpha_ij")
    } else {
      c("a_u_ij", "uptake_maximum_ij", "u_max_i", "uptake_maximum_i", "d_i",
        "consumer_death_rate", "rho_j", "resource_renewal_rate", "a_b_i", "a_d_i")
    }
    for (rows in split(seq_len(nrow(table)), interaction(table$community_replicate,
                                                        table$temperature_sd))) {
      base <- rows[table$rate_scale[rows] == 1]
      reference <- table$community_object[[base]]
      expect_length(unique(table$initial_abundance_seed[rows]), 1)
      expect_length(unique(table$env_series_id[rows]), 1)
      expect_length(unique(table$temperature_seed[rows]), 1)
      expect_length(unique(vapply(table$case_spec[rows], function(x) x$community$seed,
                                  integer(1))), 1)
      for (i in rows) {
        actual <- table$community_object[[i]]
        for (field in scaled_fields) {
          expect_equal(actual[[field]], reference[[field]] * table$rate_scale[[i]],
                       tolerance = 0, info = field)
        }
        for (field in setdiff(names(reference), scaled_fields)) {
          expect_identical(actual[[field]], reference[[field]], info = field)
        }
      }
    }
    spec$environment$sharing <- "all_different"
    table <- create_experiment_table_from_spec(spec)
    expect_equal(length(unique(table$env_series_id)), 4)
  }
})

test_that("constant-temperature trajectories obey biological time rescaling", {
  for (model in c("lv_continuous", "consumer_resource_continuous")) {
    spec <- rate_scale_spec(model)
    for (scale in c(0.25, 0.5, 2, 4)) {
      actual <- rate_scale_trajectory(spec, scale, seq(0, 20, by = 0.5))
      reference <- rate_scale_trajectory(spec, 1, seq(0, 20, by = 0.5) * scale)
      expect_equal(as.matrix(actual), as.matrix(reference), tolerance = 2e-6,
                   info = paste(model, scale))
    }
  }
})

test_that("equilibrium abundances do not change under rate scaling", {
  for (model in c("lv_continuous", "consumer_resource_continuous")) {
    spec <- rate_scale_spec(model)
    spec$community$richness <- 1
    if (model == "lv_continuous") {
      spec$traits$birth_maximum$range <- 0
      spec$traits$birth_optimum$mean <- 16
      spec$traits$birth_optimum$range <- 0
    } else {
      spec$traits$uptake_maximum$range <- 0
    }
    baseline <- rate_scale_trajectory(spec, 1, c(0, 2000))
    for (scale in c(0.25, 0.5, 2, 4)) {
      actual <- rate_scale_trajectory(spec, scale, c(0, 2000))
      expect_equal(as.numeric(actual[2, ]), as.numeric(baseline[2, ]), tolerance = 1e-6)
    }
  }
})

test_that("rate factorials run through serial and parallel workflows", {
  for (model in c("lv_continuous", "consumer_resource_continuous")) {
    for (parallel_run in c(FALSE, TRUE)) {
      if (parallel_run && .Platform$OS.type == "windows") next
      spec <- rate_scale_spec(model)
      spec$simulation$rate_scale <- 1
      spec$simulation$initial_abundance_seed_base <- 15
      spec$treatments <- list(values = list("simulation.rate_scale" = c(0.5, 1, 2)))
      spec$output <- list(save_dynamics = TRUE, save_resources = TRUE,
                          simulation_progress = FALSE, environment_progress = FALSE)
      spec$parallel <- list(workers = 2, environments = parallel_run,
                            simulations = parallel_run, community_measures = parallel_run)
      folder <- tempfile("rate-factorial-")
      dir.create(folder)
      yaml::write_yaml(unclass(spec), file.path(folder, "experiment.yaml"))
      outputs <- run_experiment(dirname(folder), basename(folder), "experiment.yaml",
                                overwrite = TRUE, verbose = FALSE, confirm_run = FALSE)
      table <- readRDS(outputs$experiment_table)
      expect_equal(vapply(table$treatment_values, `[[`, numeric(1), "simulation.rate_scale"),
                   c(0.5, 1, 2))
      expect_equal(nrow(readRDS(outputs$community_measures)), 3)
      preflight <- community.simulator:::estimate_experiment_outputs_from_spec(folder, spec)
      expect_equal(preflight$rate_scales, c(0.5, 1, 2))
      connection <- DBI::dbConnect(RSQLite::SQLite(), outputs$temperatures_db)
      temperatures <- DBI::dbReadTable(connection, "temperatures")
      DBI::dbDisconnect(connection)
      expect_equal(nrow(temperatures), 12)
      connection <- DBI::dbConnect(RSQLite::SQLite(), file.path(folder, "dynamics.db"))
      dynamics <- DBI::dbReadTable(connection, "dynamics")
      DBI::dbDisconnect(connection)
      expect_setequal(unique(dynamics$case_id), table$case_id)
      for (i in seq_len(nrow(table))) {
        set.seed(table$initial_abundance_seed[[i]])
        initial <- (dirmult::rdirichlet(1, rep(1, 3)) *
                      (spec$simulation$initial_consumer_total_abundance %||% 1000))[1, ]
        community <- table$community_object[[i]]
        args <- list(input_com_params = community,
                      TcelSeries = matrix(temperatures$temperature, nrow = 1),
                      times = temperatures$time, output_times = 1:12,
                      rtol = as.numeric(spec$simulation$ode$rtol),
                      atol = as.numeric(spec$simulation$ode$atol))
        expected <- if (model == "lv_continuous") {
          do.call(simulator_lv_continuous, c(args, list(initial_abundances = initial,
            immigration_rate = spec$simulation$immigration_rate * table$rate_scale[[i]])))
        } else {
          do.call(simulator_consumer_resource_continuous, c(args, list(
            initial_consumer_abundances = initial,
            initial_resource_values = rep(spec$simulation$resource_initial_value, community$R),
            consumer_immigration_rate = spec$simulation$consumer_immigration_rate * table$rate_scale[[i]])))$consumers
        }
        actual <- dynamics[dynamics$case_id == table$case_id[[i]], ]
        expect_equal(actual$Abundance, as.vector(t(as.matrix(expected))), tolerance = 1e-10)
      }
      unlink(folder, recursive = TRUE)
    }
  }
})

test_that("CR lsoda handles rate scale four at requested tolerances", {
  spec <- rate_scale_spec("consumer_resource_continuous")
  spec$community$richness <- 5
  spec$environment$temperature$sd <- 1
  spec$environment$temperature$sample_interval <- 0.01
  spec$simulation$rate_scale <- 4
  spec$simulation$ode$rtol <- 1e-2
  spec$simulation$ode$atol <- 1e-4
  spec$simulation$ode$maxsteps <- 50000
  table <- create_experiment_table_from_spec(spec)
  set.seed(7)
  temperatures <- community.simulator:::generate_one_over_f_temperature(
    n = 200000, mean = 16, sd = 1, gamma = 0.8)
  expect_no_warning(result <- simulator_consumer_resource_continuous(
    table$community_object[[1]], matrix(c(rep(16, 20000), temperatures), nrow = 1),
    rep(6, 5), rep(1000, 6), times = seq(0.01, 2200, by = 0.01),
    output_times = 1:2200, consumer_immigration_rate = 0.04,
    rtol = 1e-2, atol = 1e-4, max_step = 1, maxsteps = 50000))
  expect_equal(nrow(result$consumers), 2200)
  expect_true(all(is.finite(as.matrix(result$consumers))))
})
