test_that("240-case stages are identical serially and in independent R workers", {
  folder <- tempfile("parallel-equivalence-"); dir.create(folder)
  on.exit(unlink(folder,recursive=TRUE),add=TRUE)
  spec <- read_experiment_spec(system.file("experiment_templates/lv_continuous.yaml",
                                          package="community.simulator"))
  spec$community$replicates <- 60L
  spec$environment$replicates <- 2L
  spec$simulation$initial_abundance_seed_base <- 123L
  spec$simulation$burn_in_duration <- 0L
  spec$simulation$experiment_duration <- 4L
  spec$output <- list(simulation_progress=FALSE,environment_progress=FALSE,
                      save_dynamics=TRUE,save_resources=FALSE)
  spec$parallel <- list(workers=2L,chunk_size=20L)
  create_experiment_table_from_spec(spec,file.path(folder,"experiment_table.RDS"),verbose=FALSE)
  create_environments_from_spec(folder,spec,verbose=FALSE)
  environment_rng <- .Random.seed
  temperatures <- read_temperature_data(file.path(folder,"temperatures.db"))
  outputs <- simulate_dynamics_from_spec(folder,spec,verbose=FALSE)
  summaries <- readRDS(outputs$simulation_summaries)
  populations <- readRDS(outputs$population_summaries)
  read_dynamics <- function() {
    con <- DBI::dbConnect(RSQLite::SQLite(),file.path(folder,"dynamics.db"),flags=RSQLite::SQLITE_RO)
    on.exit(DBI::dbDisconnect(con))
    DBI::dbReadTable(con,"dynamics")
  }
  dynamics <- read_dynamics()
  path <- get_community_measures_from_spec(folder,spec,verbose=FALSE)
  measures <- readRDS(path)
  expect_equal(nrow(measures),240L)
  spec$parallel$environments <- TRUE
  spec$parallel$chunk_size <- 1L
  create_environments_from_spec(folder,spec,overwrite=TRUE,verbose=FALSE)
  expect_identical(.Random.seed,environment_rng)
  expect_identical(read_temperature_data(file.path(folder,"temperatures.db")),temperatures)
  spec$parallel$simulations <- TRUE
  spec$parallel$community_measures <- TRUE
  spec$parallel$chunk_size <- 20L
  simulate_dynamics_from_spec(folder,spec,overwrite=TRUE,verbose=FALSE)
  expect_identical(readRDS(outputs$simulation_summaries),summaries)
  expect_identical(readRDS(outputs$population_summaries),populations)
  expect_identical(read_dynamics(),dynamics)
  get_community_measures_from_spec(folder,spec,overwrite=TRUE,verbose=FALSE)
  expect_identical(readRDS(path),measures)
  events <- lapply(readLines(file.path(folder,"experiment_log.txt")),jsonlite::fromJSON)
  for (event in events) if (identical(event$event,"worker_started")) {
    expect_false(tryCatch(ps::ps_is_running(ps::ps_handle(event$pid)),
                         no_such_process=function(e) FALSE))
  }
})

test_that("unseeded legacy dynamics reuse serial RNG draws across chunks", {
  folder <- tempfile(); dir.create(folder)
  on.exit(unlink(folder,recursive=TRUE),add=TRUE)
  spec <- read_experiment_spec(system.file("experiment_templates/lv_continuous.yaml",
                                          package="community.simulator"))
  spec$community$replicates <- 2L
  spec$simulation$burn_in_duration <- 0L
  spec$simulation$experiment_duration <- 3L
  spec$output <- list(save_dynamics=TRUE,save_resources=FALSE,
                      simulation_progress=FALSE,environment_progress=FALSE)
  create_experiment_table_from_spec(spec,file.path(folder,"experiment_table.RDS"),verbose=FALSE)
  create_environments_from_spec(folder,spec,verbose=FALSE)
  set.seed(194)
  outputs <- simulate_dynamics_from_spec(folder,spec,verbose=FALSE)
  reference <- readRDS(outputs$simulation_summaries)
  spec$parallel <- list(workers=2L,simulations=TRUE,chunk_size=1L)
  set.seed(194)
  simulate_dynamics_from_spec(folder,spec,overwrite=TRUE,verbose=FALSE)
  expect_identical(readRDS(outputs$simulation_summaries),reference)
})

test_that("CPC stage retries actual hung, killed and erroring calculations", {
  skip_on_os("windows")
  folder <- tempfile(); dir.create(folder)
  on.exit(unlink(folder,recursive=TRUE),add=TRUE)
  spec <- read_experiment_spec(system.file("experiment_templates/lv_continuous.yaml",
                                          package="community.simulator"))
  spec$community$replicates <- 6L
  spec$simulation$burn_in_duration <- 0L
  spec$simulation$experiment_duration <- 4L
  spec$output <- list(save_dynamics=FALSE,save_resources=FALSE,
                      simulation_progress=FALSE,environment_progress=FALSE)
  create_experiment_table_from_spec(spec,file.path(folder,"experiment_table.RDS"),verbose=FALSE)
  create_environments_from_spec(folder,spec,verbose=FALSE)
  simulate_dynamics_from_spec(folder,spec,verbose=FALSE)
  path <- get_community_measures_from_spec(folder,spec,verbose=FALSE)
  reference <- readRDS(path)
  spec$parallel <- list(workers=2L,community_measures=TRUE,chunk_size=6L,
                        worker_timeout_minutes=0.1,worker_retries=1L)
  for (fault in c("hang","death","error")) {
    marker <- file.path(folder,paste0("injected-",fault))
    worker <- eval(substitute(function(payload) {
      if (payload$expt$case_id[1] == "case_1" && !file.exists(MARKER)) {
        file.create(MARKER)
        switch(FAULT,hang=repeat { Sys.sleep(1) },
               death=tools::pskill(Sys.getpid(),9),error=stop("Injected CPC failure"))
      }
      getFromNamespace("execute_stage_chunk","community.simulator")(payload)
    },list(MARKER=marker,FAULT=fault)))
    local_mocked_bindings(execute_stage_chunk=worker)
    get_community_measures_from_spec(folder,spec,overwrite=TRUE,verbose=FALSE)
    expect_identical(readRDS(path),reference)
  }
  events <- lapply(readLines(file.path(folder,"experiment_log.txt")),jsonlite::fromJSON)
  types <- vapply(events,`[[`,character(1),"event")
  expect_true(all(c("worker_timeout","worker_death","worker_error","worker_retry") %in% types))
  for (event in events) if (identical(event$event,"worker_started")) {
    expect_false(tryCatch(ps::ps_is_running(ps::ps_handle(event$pid)),
                         no_such_process=function(e) FALSE))
  }
})
