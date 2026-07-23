test_that("spectral temperature generator preserves requested moments", {
  set.seed(123)
  temperature <- community.simulator:::generate_one_over_f_temperature(
    n = 1001,
    mean = 20,
    sd = 4,
    gamma = 0.8
  )

  expect_length(temperature, 1001)
  expect_equal(mean(temperature), 20, tolerance = 1e-10)
  expect_equal(stats::sd(temperature), 4, tolerance = 1e-10)
  expect_true(all(is.finite(temperature)))
})

test_that("spectral temperature generator is reproducible from the R seed", {
  set.seed(456)
  first <- community.simulator:::generate_one_over_f_temperature(
    n = 501,
    mean = 15,
    sd = 2,
    gamma = 1
  )
  set.seed(456)
  second <- community.simulator:::generate_one_over_f_temperature(
    n = 501,
    mean = 15,
    sd = 2,
    gamma = 1
  )

  expect_equal(first, second)
})

test_that("spectral temperature generator handles constant temperature", {
  temperature <- community.simulator:::generate_one_over_f_temperature(
    n = 100,
    mean = 12,
    sd = 0,
    gamma = 0.5
  )

  expect_equal(temperature, rep(12, 100))
})

test_that("spectral temperature generator avoids quadratic-sized outputs", {
  set.seed(789)
  temperature <- community.simulator:::generate_one_over_f_temperature(
    n = 50001,
    mean = 20,
    sd = 4,
    gamma = 1
  )

  expect_length(temperature, 50001)
  expect_lt(as.numeric(utils::object.size(temperature)), 1e6)
  expect_equal(mean(temperature), 20, tolerance = 1e-10)
  expect_equal(stats::sd(temperature), 4, tolerance = 1e-10)
})
