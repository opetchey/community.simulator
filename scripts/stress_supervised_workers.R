# Run against an installed package: Rscript scripts/stress_supervised_workers.R [output-dir]
# 15,000 synthetic cases, 14 independent workers, 3 runs. Short series isolate
# worker scheduling/collection; this is not the original 4,000-unit server workload.
library(community.simulator)
args <- commandArgs(TRUE)
outdir <- if (length(args)) args[1] else tempfile("supervised-stress-")
dir.create(outdir,recursive=TRUE,showWarnings=FALSE)
spec <- read_experiment_spec(system.file("experiment_templates/consumer_resource.yaml",
                                        package="community.simulator"))
spec$simulation$burn_in_duration <- 0L
spec$simulation$experiment_duration <- 4L
spec$output <- list(save_dynamics=FALSE,save_resources=FALSE,
                    simulation_progress=FALSE,environment_progress=FALSE)
create_experiment_table_from_spec(spec,file.path(outdir,"experiment_table.RDS"),verbose=FALSE)
create_environments_from_spec(outdir,spec,overwrite=TRUE,verbose=FALSE)
simulate_dynamics_from_spec(outdir,spec,overwrite=TRUE,verbose=FALSE)
base <- readRDS(file.path(outdir,"experiment_table.RDS"))
summaries <- readRDS(file.path(outdir,"simulation_summaries.RDS"))
ii <- rep(seq_len(nrow(base)),length.out=15000)
expanded <- base[ii,]
expanded$case_id <- sprintf("stress_%05d",seq_along(ii))
expanded_summaries <- summaries[ii,]
expanded_summaries$case_id <- expanded$case_id
saveRDS(expanded,file.path(outdir,"experiment_table.RDS"))
saveRDS(expanded_summaries,file.path(outdir,"simulation_summaries.RDS"))
spec$parallel <- list(workers=14L,community_measures=TRUE,chunk_size=100L,
                      worker_timeout_minutes=10,worker_retries=1L)
reference <- NULL
results <- list()
for (repeat_id in 1:3) {
  started <- proc.time()[["elapsed"]]
  path <- get_community_measures_from_spec(outdir,spec,overwrite=TRUE,verbose=FALSE)
  value <- readRDS(path)
  stopifnot(nrow(value)==15000,identical(value$case_id,expanded$case_id))
  if (is.null(reference)) reference <- value else stopifnot(identical(value,reference))
  results[[repeat_id]] <- list(repeat_id=repeat_id,cases=nrow(value),workers=14,
    chunk_size=100,seconds=proc.time()[["elapsed"]]-started,identical=TRUE)
  cat(jsonlite::toJSON(results[[repeat_id]],auto_unbox=TRUE),"\n")
}
events <- lapply(readLines(file.path(outdir,"experiment_log.txt")),jsonlite::fromJSON)
started <- Filter(function(x) identical(x$event,"worker_started"),events)
alive <- vapply(started,function(x) tryCatch(ps::ps_is_running(ps::ps_handle(x$pid)),
                                           no_such_process=function(e) FALSE),logical(1))
stopifnot(!any(alive))
jsonlite::write_json(list(runs=results,started_workers=length(started),live_workers=sum(alive)),
                    file.path(outdir,"stress_results.json"),pretty=TRUE,auto_unbox=TRUE)
