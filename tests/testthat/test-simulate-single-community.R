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
