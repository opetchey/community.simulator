test_that("LV case specs build LV community objects", {
  table <- create_experiment_table_from_spec(system.file(
    "experiment_templates/lv_continuous.yaml",
    package = "community.simulator"
  ))

  community <- build_community_from_spec(table$case_spec[[2]])
  community_direct <- build_LV_community_from_spec(table$case_spec[[2]])

  expect_equal(community$S, 2)
  expect_equal(community_direct$S, 2)
  expect_equal(nrow(community$alpha_ij), 2)
  expect_equal(ncol(community$alpha_ij), 2)
  expect_true(all(is.finite(community$birth_maximum_i)))
  expect_true(all(is.finite(community$birth_optimum_i)))
  expect_true(all(community$birth_width_i > 0))
})

test_that("consumer-resource case specs build consumer-resource community objects", {
  table <- create_experiment_table_from_spec(system.file(
    "experiment_templates/consumer_resource.yaml",
    package = "community.simulator"
  ))

  community <- build_community_from_spec(table$case_spec[[1]])
  community_direct <- build_CR_community_from_spec(table$case_spec[[1]])

  expect_equal(community$model_type, "consumer_resource")
  expect_equal(community_direct$model_type, "consumer_resource")
  expect_equal(community$S, 2)
  expect_equal(community$R, 3)
  expect_equal(nrow(community$resource_use_ij), 2)
  expect_equal(ncol(community$resource_use_ij), 3)
  expect_true(all(community$private_resource_use_i >= 0))
  expect_true(all(community$private_resource_use_i <= 1))
})

test_that("rewrite-facing low-level constructors are available", {
  lv_table <- create_experiment_table_from_spec(system.file(
    "experiment_templates/lv_discrete.yaml",
    package = "community.simulator"
  ))
  cr_table <- create_experiment_table_from_spec(system.file(
    "experiment_templates/consumer_resource.yaml",
    package = "community.simulator"
  ))

  lv_community <- build_LV_community(
    S = 2,
    birth_maximum_mean = 0.3,
    birth_maximum_range = 0,
    birth_maximum_distribution = "random_uniform",
    birth_optimum_mean = 20,
    birth_optimum_range = 4,
    birth_optimum_distribution = "random_uniform",
    birth_width_distribution = "random_uniform",
    birth_width_mean = 10,
    birth_width_range = 0,
    community_seed = 1,
    death_intercept = 0,
    death_temperature_slope = 0.05,
    lv_interaction_spec = lv_table$case_spec[[1]]$interactions$selected
  )
  cr_community <- build_CR_community(
    S = 2,
    uptake_maximum_mean = 0.363064,
    uptake_maximum_range = 0,
    uptake_maximum_distribution = "random_uniform",
    uptake_optimum_mean = 16,
    uptake_optimum_range = 0,
    uptake_optimum_distribution = "random_uniform",
    uptake_width_mean = 1,
    uptake_width_range = 0.5,
    uptake_width_distribution = "random_uniform",
    half_saturation_mean = 100,
    half_saturation_range = 0,
    half_saturation_distribution = "random_uniform",
    consumer_death_rate = 0.181532,
    resource_renewal_rate = 6.051066,
    resource_supply = 1000,
    conversion_efficiency = 1,
    resource_use_mode = "shared_to_private",
    active_resource = 1,
    private_resource_use_distribution = "constant",
    private_resource_use_mean = 0,
    private_resource_use_range = 0,
    community_seed = 1
  )

  expect_equal(lv_community$S, 2)
  expect_equal(cr_community$S, cr_table$case_spec[[1]]$community$richness)
  expect_true("birth_maximum_i" %in% names(lv_community))
  expect_true("uptake_maximum_i" %in% names(cr_community))
})

test_that("community objects are included in canonical experiment tables", {
  table <- create_experiment_table_from_spec(system.file(
    "experiment_templates/consumer_resource.yaml",
    package = "community.simulator"
  ))

  expect_true("community_object" %in% names(table))
  expect_equal(table$community_object[[1]]$S, table$case_spec[[1]]$community$richness)
})
