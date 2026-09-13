test_that("single-community helper uses configurable performance-width ranges", {
  lv_community <- build_single_community(
    model_type = "lv_discrete",
    richness = 3,
    birth_width_mean = 6,
    birth_width_range = 1
  )

  expect_equal(lv_community$traits$birth_width, c(5.5, 6, 6.5))

  cr_default <- build_single_community(
    model_type = "consumer_resource_continuous",
    richness = 3,
    uptake_width_mean = 1
  )

  expect_equal(cr_default$traits$uptake_width, rep(1, 3))

  cr_ranged <- build_single_community(
    model_type = "consumer_resource_continuous",
    richness = 3,
    uptake_width_mean = 1,
    uptake_width_range = 0.5
  )

  expect_true(length(unique(cr_ranged$traits$uptake_width)) > 1)
})

test_that("single-community helper uses environmental sampling interval for continuous models", {
  lv_continuous <- simulate_single_community(
    model_type = "lv_continuous",
    richness = 2,
    experiment_duration = 5,
    environment_sampling_interval = 0.5
  )
  expect_equal(lv_continuous$temperature$time, seq(0.5, 5, by = 0.5))

  consumer_resource <- simulate_single_community(
    model_type = "consumer_resource_continuous",
    richness = 2,
    experiment_duration = 5,
    environment_sampling_interval = 0.5
  )
  expect_equal(consumer_resource$temperature$time, seq(0.5, 5, by = 0.5))

  lv_discrete <- simulate_single_community(
    model_type = "lv_discrete",
    richness = 2,
    experiment_duration = 5,
    environment_sampling_interval = 0.5
  )
  expect_equal(lv_discrete$temperature$time, seq_len(5))
})

test_that("single-community helper rejects invalid environmental sampling intervals", {
  expect_error(
    simulate_single_community(
      model_type = "lv_discrete",
      experiment_duration = 5,
      environment_sampling_interval = -1
    ),
    "`environment_sampling_interval` must be a positive finite number.",
    fixed = TRUE
  )

  expect_error(
    simulate_single_community(
      model_type = "lv_continuous",
      experiment_duration = 5,
      environment_sampling_interval = 6
    ),
    "`environment_sampling_interval` must be less than or equal to",
    fixed = TRUE
  )
})
