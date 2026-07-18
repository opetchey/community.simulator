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
  expect_true(all(is.finite(community$a_b_i)))
  expect_true(all(is.finite(community$b_opt_i)))
  expect_true(all(community$sd_perf_i > 0))
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
  expect_true(all(community$resource_specialization_i >= 0))
  expect_true(all(community$resource_specialization_i <= 1))
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
    a_b_mean = 0.3,
    a_b_range = 0,
    a_b_distribution = "random_uniform",
    b_opt_mean = 20,
    b_opt_range = 4,
    b_opt_distribution = "random_uniform",
    sd_perf_distribution = "random_uniform",
    sd_perf_mean = 10,
    sd_perf_range = 0,
    community_seed = 1,
    a_d = 0,
    z = 0.05,
    lv_interaction_spec = lv_table$case_spec[[1]]$interactions$selected
  )
  cr_community <- build_CR_community(
    S = 2,
    u_max_mean = 0.06,
    u_max_range = 0,
    u_max_distribution = "random_uniform",
    u_opt_mean = 20,
    u_opt_range = 4,
    u_opt_distribution = "random_uniform",
    sd_u_mean = 5,
    sd_u_range = 0,
    sd_u_distribution = "random_uniform",
    half_saturation_mean = 100,
    half_saturation_range = 0,
    half_saturation_distribution = "random_uniform",
    consumer_death_rate = 0.03,
    resource_renewal_rate = 1,
    resource_supply = 1000,
    conversion_efficiency = 1,
    resource_use_mode = "shared_to_private",
    active_resource = 1,
    resource_specialization_distribution = "beta",
    resource_specialization_mean = 0.7,
    resource_specialization_precision = 10,
    community_seed = 1
  )

  expect_equal(lv_community$S, 2)
  expect_equal(cr_community$S, cr_table$case_spec[[1]]$community$richness)
})

test_that("community objects are included in canonical experiment tables", {
  table <- create_experiment_table_from_spec(system.file(
    "experiment_templates/consumer_resource.yaml",
    package = "community.simulator"
  ))

  expect_true("community_object" %in% names(table))
  expect_equal(table$community_object[[1]]$S, table$case_spec[[1]]$community$richness)
})
