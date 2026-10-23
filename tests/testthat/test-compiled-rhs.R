test_that("compiled and R trajectories and case summaries agree across the grid", {
  for (model in c("cr", "lv")) for (S in c(4, 8, 12)) {
    for (gamma in c(0, 0.4, 1)) for (scale in c(0.1, 1, 10)) {
      f <- compiled_fixture(model, S, gamma, scale)
      r <- do.call(f$fun, c(f$args, list(rhs = "r")))
      cc <- do.call(f$fun, c(f$args, list(rhs = "compiled")))
      if (model == "cr") {
        expect_equal(as.matrix(cc$resources), as.matrix(r$resources), tolerance = 1e-4)
        r <- r$consumers; cc <- cc$consumers
      }
      # Mixed absolute/relative criterion remains meaningful near extinction.
      expect_lt(max(abs(as.matrix(cc)-as.matrix(r))/(1e-6+1e-4*abs(as.matrix(r)))), 5)
      summary <- function(x) community.simulator:::summarise_case_dynamics(
        "test", x, f$args$output_times, f$args$output_times,
        data.frame(time = f$args$times, temperature = f$args$TcelSeries), 0)
      expect_equal(summary(cc), summary(r), tolerance = 5e-4)
    }
  }
})

test_that("forcing interpolation, endpoints, and pulse immigration agree", {
  for (model in c("cr", "lv")) for (method in c("linear", "constant")) {
    f <- compiled_fixture(model)
    f$args$times <- c(8, 2, 4, 4, 6)
    f$args$TcelSeries <- c(18, 15, 20, 22, 17)
    f$args$temperature_interpolation <- method
    if (model == "lv") f$args$immigration_mode <- "pulse"
    expect_warning(r <- do.call(f$fun, c(f$args, list(rhs = "r"))), "collapsing")
    expect_warning(cc <- do.call(f$fun, c(f$args, list(rhs = "compiled"))), "collapsing")
    expect_equal(cc, r, tolerance = 5e-4)
  }
})

test_that("compiled failures retain the reference messages and recover", {
  for (model in c("cr", "lv")) {
    f <- compiled_fixture(model)
    expect_error(do.call(f$fun, c(f$args, list(rhs="compiled", blowup_threshold=1))),
                 "blow-up threshold")
    f$args$input_com_params[[if(model == "lv") "a_b_i" else "a_u_ij"]][] <- Inf
    expect_error(do.call(f$fun, c(f$args, list(rhs="compiled"))), "Non-finite derivative")
    f <- compiled_fixture(model, 8)
    expect_no_error(do.call(f$fun, c(f$args, list(rhs="compiled"))))
  }
})

test_that("YAML RHS selection validates and defaults to R", {
  spec <- read_experiment_spec(system.file("experiment_templates/lv_continuous.yaml",
                                          package="community.simulator"))
  expect_identical(community.simulator:::flatten_spec_settings(spec)$ode_rhs, "r")
  spec$simulation$ode$rhs <- "compiled"
  expect_no_error(create_experiment_table_from_spec(spec))
  expect_identical(community.simulator:::flatten_spec_settings(spec)$ode_rhs, "compiled")
  spec$simulation$ode$rhs <- "invalid"
  expect_error(create_experiment_table_from_spec(spec), "simulation.ode.rhs")
})

test_that("compiled forcings equal approxfun at knots and held endpoints", {
  for (method in c("linear", "constant")) {
    args <- list(y = c(Spp1=1), times = seq(0, 6, by=0.25), method="lsoda")
    # One LV species with zero interactions, constant positive net growth.
    p <- c(1, 0, 0, 1e12, 1e-6, 0, 16, 1, 0, 0, 0)
    times <- c(1, 2, 4, 5); temp <- c(10, 20, 15, 12)
    args <- community.simulator:::compiled_rhs_args(args, "lv", p, times, temp, method)
    args$nout <- 1
    value <- do.call(deSolve::ode, args)
    expect_equal(unname(value[,3]), stats::approxfun(times, temp, method=method,
                                                   rule=2, f=0)(args$times))
  }
})

test_that("compiled LV records and resets negative trial diagnostics", {
  # Euler deliberately takes a negative trial, then immigration restores it.
  args <- list(y = c(Spp1=1), times = 0:3, method="euler")
  p <- c(1, 0, 2, 1e12, 1e-6, 0, 16, 1, 0, 5, 0)
  args <- community.simulator:::compiled_rhs_args(args,"lv",p,c(0,3),c(16,16),"linear")
  value <- do.call(deSolve::ode,args)
  trials <- .Call("cs_trials", PACKAGE="community.simulator")
  expect_gt(trials[1], 0)
  expect_equal(trials[2], min(value[,2]))
  expect_equal(trials[3], 1)
  p[10] <- 0
  args <- community.simulator:::compiled_rhs_args(args,"lv",p,c(0,3),c(16,16),"linear")
  do.call(deSolve::ode,args)
  expect_equal(.Call("cs_trials",PACKAGE="community.simulator"),c(0,0,NA_real_))
})

test_that("compiled YAML experiments run and record their implementation", {
  for (template in c("lv_continuous.yaml", "consumer_resource.yaml")) {
    folder <- tempfile("compiled-experiment-")
    dir.create(folder)
    spec <- read_experiment_spec(system.file("experiment_templates",template,
                                            package="community.simulator"))
    spec$simulation$burn_in_duration <- 0
    spec$simulation$experiment_duration <- 3
    spec$simulation$ode$rhs <- "compiled"
    spec$parallel <- list(workers=2L, simulations=.Platform$OS.type != "windows")
    spec$output <- list(simulation_progress=FALSE, environment_progress=FALSE)
    yaml::write_yaml(unclass(spec),file.path(folder,"experiment.yaml"))
    out <- run_experiment(dirname(folder),basename(folder),"experiment.yaml",
                          overwrite=TRUE,verbose=FALSE,confirm_run=FALSE)
    log <- paste(readLines(file.path(folder,"experiment_log.txt")),collapse="\n")
    expect_match(log,'"rhs":"compiled"',fixed=TRUE)
    expect_match(log,'"ode_rhs":"compiled"',fixed=TRUE)
    expect_true(file.exists(out$simulation_summaries))
    unlink(folder,recursive=TRUE)
  }
})

test_that("implicit reference RHS is recorded in the specification log", {
  path <- system.file("experiment_templates/lv_continuous.yaml",package="community.simulator")
  logged <- community.simulator:::read_experiment_specification_for_log(path)
  expect_identical(logged$specification$simulation$ode$rhs,"r")
})
