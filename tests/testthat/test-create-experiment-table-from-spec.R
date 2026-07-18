test_that("baseline specs expand over model treatments and replicates", {
  spec <- read_experiment_spec(system.file(
    "experiment_templates/lv_discrete.yaml",
    package = "community.simulator"
  ))
  table <- create_experiment_table_from_spec(spec)

  expect_s3_class(table, "tbl_df")
  expect_equal(nrow(table), 2)
  expect_equal(
    table$interaction_treatment_label,
    c("no_interactions", "weak_asymmetric_competition")
  )
  expect_equal(table$treatment_label, c("baseline", "baseline"))
  expect_equal(table$case_spec[[1]]$interactions$selected$label, "no_interactions")
  expect_equal(length(table$community_object), 2)
  expect_equal(table$community_object[[1]]$S, 2)
})

test_that("factorial treatments are the default", {
  spec <- read_experiment_spec(system.file(
    "experiment_templates/lv_discrete.yaml",
    package = "community.simulator"
  ))
  spec$treatments <- list(
    values = list(
      "traits.birth_width.mean" = c(5, 10),
      "environment.temperature.sd" = c(0.5, 1)
    )
  )
  table <- create_experiment_table_from_spec(spec)

  expect_equal(nrow(table), 8)
  expect_true(all(table$case_spec[[1]]$treatments$mode == "factorial"))
  expect_equal(
    sort(unique(vapply(
      table$case_spec,
      function(x) x$traits$birth_width$mean,
      numeric(1)
    ))),
    c(5, 10)
  )
})

test_that("paired treatments expand exactly supplied rows", {
  spec <- read_experiment_spec(system.file(
    "experiment_templates/consumer_resource.yaml",
    package = "community.simulator"
  ))
  spec$treatments <- list(
    mode = "paired",
    values = list(
      list(
        "resources.private_use.mean" = 0.4,
        "environment.temperature.sd" = 0.5
      ),
      list(
        "resources.private_use.mean" = 0.8,
        "environment.temperature.sd" = 2
      )
    )
  )
  table <- create_experiment_table_from_spec(spec)

  expect_equal(nrow(table), 2)
  expect_equal(
    vapply(table$case_spec, function(x) x$resources$private_use$mean, numeric(1)),
    c(0.4, 0.8)
  )
  expect_equal(
    vapply(table$case_spec, function(x) x$environment$temperature$sd, numeric(1)),
    c(0.5, 2)
  )
})

test_that("invalid treatment paths fail clearly", {
  spec <- read_experiment_spec(system.file(
    "experiment_templates/lv_discrete.yaml",
    package = "community.simulator"
  ))
  spec$treatments <- list(
    values = list("traits.birth_wdith.mean" = c(5, 10))
  )

  expect_error(
    create_experiment_table_from_spec(spec),
    "does not exist in the baseline specification"
  )
})

test_that("invalid treatment values fail validation after overrides", {
  spec <- read_experiment_spec(system.file(
    "experiment_templates/lv_discrete.yaml",
    package = "community.simulator"
  ))
  spec$treatments <- list(
    values = list("traits.birth_width.mean" = "wide")
  )

  expect_error(
    create_experiment_table_from_spec(spec),
    "`traits.birth_width.mean` must be one finite number",
    fixed = TRUE
  )
})

test_that("all YAML templates build valid community objects", {
  templates <- list.files(
    system.file("experiment_templates", package = "community.simulator"),
    pattern = "yaml$",
    full.names = TRUE
  )

  tables <- lapply(templates, create_experiment_table_from_spec)
  communities <- unlist(lapply(tables, `[[`, "community_object"), recursive = FALSE)

  expect_equal(length(tables), 3)
  expect_true(all(vapply(communities, function(x) is.list(x) && x$S == 2, logical(1))))
  expect_true(any(vapply(communities, function(x) identical(x$model_type, "consumer_resource"), logical(1))))
})
