worker_test_events <- function(path) lapply(readLines(path), jsonlite::fromJSON)

expect_workers_gone <- function(events) {
  started <- Filter(function(x) identical(x$event, "worker_started"), events)
  expect_gt(length(started), 0)
  for (event in started) expect_false(tryCatch(ps::ps_is_running(ps::ps_handle(event$pid)),
                                          no_such_process = function(e) FALSE))
}

test_that("worker settings are optional and validated", {
  spec <- read_experiment_spec(system.file("experiment_templates/lv_continuous.yaml",
                                          package = "community.simulator"))
  settings <- flatten_spec_settings(spec)
  expect_equal(settings$worker_timeout_minutes, 120)
  expect_equal(settings$worker_retries, 2)
  expect_equal(settings$chunk_size, 10)
  for (key in c("worker_timeout_minutes", "worker_retries", "chunk_size")) {
    for (bad in list(-1, NA_real_, Inf, "nonsense", TRUE, c(1, 2))) {
      invalid <- spec; invalid$parallel[[key]] <- bad
      expect_error(validate_experiment_spec(invalid), paste0("parallel.", key), fixed = TRUE)
    }
  }
  spec$parallel$worker_retries <- 0
  spec$parallel$worker_timeout_minutes <- 0.05
  spec$parallel$chunk_size <- 1
  expect_no_error(validate_experiment_spec(spec))
})

test_that("hung, killed and erroring chunks recover in fresh workers without lost rows", {
  skip_on_os("windows") # SIGKILL injection is Unix-specific, not the backend.
  for (fault in c("hang", "death", "error", "missing")) {
    dir <- tempfile(); dir.create(dir)
    log <- file.path(dir, "events.jsonl")
    result <- list()
    worker <- function(payload) {
      marker <- file.path(payload$dir, paste0("attempt-", payload$ii[1]))
      if (payload$ii[1] == 1 && !file.exists(marker)) {
        file.create(marker)
        switch(payload$fault,
          hang = repeat { Sys.sleep(1) },
          death = tools::pskill(Sys.getpid(), 9),
          error = stop("Injected chunk error"),
          missing = return(list(NULL)))
      }
      as.list(payload$ii)
    }
    started <- proc.time()[["elapsed"]]
    run_supervised_chunks(1:4, 2,
      function(ii) list(ii=ii, dir=dir, fault=fault),
      function(i, value) result[[i]] <<- value, function(n) NULL,
      step="get_community_measures", case_ids=paste0("case_", 1:4),
      timeout_minutes=0.05, retries=1, chunk_size=2, log_path=log, worker=worker)
    expect_identical(result, as.list(1:4))
    expect_lt(proc.time()[["elapsed"]] - started, 30)
    events <- worker_test_events(log)
    types <- vapply(events, `[[`, character(1), "event")
    expect_true("worker_retry" %in% types)
    expect_true(switch(fault, hang="worker_timeout",death="worker_death",
                       error="worker_error",missing="worker_error") %in% types)
    retry <- events[[which(types == "worker_retry")]]
    expect_equal(retry$case_ids, c("case_1", "case_2"))
    expect_equal(retry$step, "get_community_measures")
    expect_gte(retry$elapsed_seconds, 0)
    expect_workers_gone(events)
    unlink(dir, recursive=TRUE)
  }
})

test_that("exhaustion and parent errors clean up all active workers", {
  for (parent_error in c(FALSE, TRUE)) {
    log <- tempfile()
    worker <- function(payload) {
      if (payload$ii == 1 && payload$parent_error) return(list(1L))
      repeat { Sys.sleep(1) }
    }
    expect_error(run_supervised_chunks(1:2, 2,
      function(ii) list(ii=ii, parent_error=parent_error),
      function(i,x) stop("parent write error"), function(n) NULL,
      step="simulate_dynamics", case_ids=c("case_A","case_B"),
      timeout_minutes=0.05, retries=0, chunk_size=1, log_path=log, worker=worker),
      if (parent_error) "parent write error" else "case_A.*deadline")
    expect_workers_gone(worker_test_events(log))
  }
})

test_that("case validation rejects missing, duplicate and unexpected rows", {
  for (ids in list(c("a","a"), "a", c("a","c"), c("b","a"), c("a",NA))) {
    expect_error(validate_case_rows(data.frame(case_id=ids),c("a","b"),"test"),"case IDs")
  }
})

test_that("large preflight products do not overflow integers", {
  spec <- read_experiment_spec(system.file("experiment_templates/consumer_resource.yaml",
                                          package="community.simulator"))
  spec$simulation$burn_in_duration <- 1000L
  spec$simulation$experiment_duration <- 4000L
  spec$environment$temperature$sample_interval <- 0.01
  local_mocked_bindings(
    read_environment_table_for_runtime = function(...) data.frame(id=seq_len(13122)),
    read_simulation_table_for_runtime = function(...) data.frame(community_id=rep("c",13122)),
    read_community_objects_for_runtime = function(...) list(c=list(S=4,R=5)))
  expect_warning(value <- estimate_experiment_outputs_from_spec(tempdir(),spec), NA)
  expect_true(is.finite(value$estimated_total_seconds))
  expect_gt(value$temperature_rows, .Machine$integer.max)
})

test_that("an interrupt cleans up the worker processes", {
  skip_on_os("windows")
  log <- tempfile()
  parent <- callr::r_bg(function(log) {
    getFromNamespace("run_supervised_chunks","community.simulator")(1:2, 2,
      function(ii) list(ii=ii), function(i,x) NULL, function(n) NULL,
      step="interrupt_test", case_ids=c("a","b"), chunk_size=1, log_path=log,
      worker=function(payload) repeat { Sys.sleep(1) })
  }, list(log), libpath=.libPaths(), supervise=TRUE)
  on.exit(try(parent$kill_tree(), silent=TRUE), add=TRUE)
  for (i in 1:100) {
    if (file.exists(log) && length(readLines(log)) >= 2) break
    Sys.sleep(0.1)
  }
  expect_true(file.exists(log))
  parent$interrupt()
  parent$wait(10000)
  expect_false(parent$is_alive())
  expect_workers_gone(worker_test_events(log))
})
