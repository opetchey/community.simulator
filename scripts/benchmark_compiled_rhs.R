# Run from the package root after R CMD INSTALL --preclean (not load_all's debug build).
# Optional first argument is the output directory. These are reproducible synthetic
# cases matching the requested sizes, not the original pilot's saved states.
library(community.simulator)
source("tests/testthat/helper-compiled-rhs.R")
outdir <- commandArgs(TRUE)
if (!length(outdir)) outdir <- tempdir()
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
trace("ode", where = asNamespace("deSolve"), print = FALSE,
      exit = quote(assign("last_ode", returnValue(), envir = .GlobalEnv)))
run <- function(f, rhs) {
  elapsed <- system.time(value <- do.call(f$fun, c(f$args, list(rhs = rhs))))[["elapsed"]]
  stats <- attr(last_ode, "istate")
  list(value = value, seconds = elapsed, steps = stats[2], evaluations = stats[3])
}
rows <- list()
for (S in c(4, 12)) {
  gamma <- if (S == 4) 0 else 1
  f <- compiled_fixture("cr", S, gamma, duration = 500)
  for (rhs in c("r", "compiled")) for (repeat_id in 1:3) {
    x <- run(f, rhs)
    rows[[length(rows)+1]] <- data.frame(S, R=S+1, gamma, rhs, repeat_id,
      seconds=x$seconds, steps=x$steps, rhs_evals=x$evaluations,
      us_per_rhs=x$seconds*1e6/x$evaluations)
  }
}
benchmark <- do.call(rbind, rows)
write.csv(benchmark, file.path(outdir, "benchmark.csv"), row.names=FALSE)
print(benchmark)
rows <- list()
for (model in c("cr", "lv")) for (S in c(4,8,12)) {
  for (gamma in c(0,0.4,1)) for (scale in c(0.1,1,10)) {
    f <- compiled_fixture(model, S, gamma, scale, duration=50)
    r <- run(f, "r"); cc <- run(f, "compiled")
    mat <- function(x) if(model=="cr") as.matrix(cbind(x$consumers,x$resources)) else as.matrix(x)
    a <- mat(r$value); b <- mat(cc$value)
    summary <- function(x) {
      if(model=="cr") x <- x$consumers
      y <- community.simulator:::summarise_case_dynamics("test", x,
        f$args$output_times, f$args$output_times,
        data.frame(time=f$args$times, temperature=f$args$TcelSeries), 0)$case_summary
      unlist(y[vapply(y, is.numeric, logical(1))])
    }
    sa <- summary(r$value); sb <- summary(cc$value)
    rows[[length(rows)+1]] <- data.frame(model,S,gamma,scale,
      max_relative=max(abs(a-b)/pmax(abs(a),1e-6)),
      max_tolerance_units=max(abs(a-b)/(1e-6+1e-4*abs(a))),
      summary_max_relative=max(abs(sa-sb)/pmax(abs(sa),1e-6),na.rm=TRUE),
      r_steps=r$steps,c_steps=cc$steps,r_evals=r$evaluations,c_evals=cc$evaluations)
  }
}
parity <- do.call(rbind, rows)
write.csv(parity, file.path(outdir,"parity.csv"), row.names=FALSE)
print(aggregate(cbind(max_relative,max_tolerance_units,summary_max_relative)~model,parity,max))
untrace("ode", where=asNamespace("deSolve"))
