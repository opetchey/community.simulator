# Supervised parallel stages — development version 0.9.0.9002

## Audit and diagnosis

| Entry point in 0.9.0.9001 | Previous backend | Open package-owned handles when forking | New backend |
|---|---|---|---|
| `create_environments_from_spec()` → `run_indexed_jobs()` | One `mcparallel()` fork per environment | Writable `temperatures.db` connection | Supervised independent callr R sessions |
| `simulate_dynamics_from_spec()` → `run_indexed_jobs()` | One `mcparallel()` fork per case | Read `temperatures.db`; optional writable `dynamics.db` and `resources.db` | Supervised independent callr R sessions |
| `get_community_measures_from_spec()` → `get_community_CPC_measures()` | One `mcparallel()` fork per case | Read `temperatures.db`, even after collecting temperatures | Supervised independent callr R sessions |

Both former scheduling loops polled `mccollect(wait = FALSE)` indefinitely, with
no deadline, no termination/cleanup guard, and no bounded recovery. The CPC loop
would rerun a NULL result serially, also without a deadline. The package's only
other `parallel::` use is `detectCores()` for worker-count selection; it launches
no workers. Plotting database access is serial.

The code and supplied process evidence are consistent with fork-after-threads
lock inheritance. They do not identify which native lock caused the observed
futex wait. Replacing fork is the primary fix; deadlines also protect against
unrelated worker hangs/crashes.

## Implementation

All three stages now use `run_supervised_chunks()`. It starts fresh R executables
through `callr::r_bg(..., supervise = TRUE)`, not forked copies of the calling R
session. callr is now an Imports dependency. The public stage signatures and
existing YAMLs remain compatible. The C/R ODE implementations and solver settings
are untouched.

A namespace worker function receives only the current chunk's environments or
case rows, required community objects, required temperature series and settings.
No closure over the parent workspace is serialized. Workers do not use a database.
Parent reads use read-only SQLite connections; every read/write has `on.exit()`
cleanup and closes before dispatch resumes. The performance-optimum SQL
aggregation remains in the parent, preserving its original numerical behavior.

Output and diagnostics are written only by the parent. Chunks are committed in
input order; a bounded dispatch window limits accumulated results when an early
chunk is slow. Worker stdout/stderr use private temporary files rather than
undrained shared pipes. Progress uses the existing reporter in the parent.

An elapsed-time watchdog includes worker startup, calculation and result
serialization. Dead processes and transported R errors are detected separately.
Timeouts, deaths, errors and missing results trigger cleanup and bounded retries
in fresh sessions. Exhaustion stops with the stage, chunk and case IDs. There is
no unbounded serial fallback. Normal completion, exceptions and interrupts clean
up active processes; cleanup defers further R interrupts. callr supervision also
terminates workers if their parent is killed abruptly.

Explicit case/environment seeds and the RNG kind are retained. Parallel environment
results return their final RNG state so the parent continues as the serial path
would. For legacy dynamics without explicit initial-abundance seeds, the parent
makes initial draws once in serial order and reuses them on retries. When those
legacy runs save database output, SQLite's RNG use for internal names requires
one supervised case at a time to preserve the serial sequence. This is logged
as `parallel_rng_serialized` and reflected in the preflight runtime estimate.
Explicit `simulation.initial_abundance_seed_base` or per-case seeds enable full
concurrency. Process-management RNG draws are isolated from the scientific stream.
Scientific
formulas are unchanged; wall-time measurements naturally need not match.

Before final output, validation checks row count, case IDs, uniqueness and order.
The measures-stage joins also validate all input case sets, preventing missing
cases from becoming silent NA rows. Valid undefined scientific statistics are
retained. Earlier committed database chunks may remain after a failed stage;
rerun with `overwrite = TRUE`. No incomplete final measures file is saved.

Preflight case-count and environment-count products now use doubles, fixing
integer overflow for 13,122 × 401,000 and large temperature-row estimates.
Preflight also records the effective backend and supervision settings.

## New optional YAML settings

```yaml
parallel:
  worker_timeout_minutes: 120
  worker_retries: 2
  chunk_size: 10
```

These are the defaults. The timeout is **per whole chunk attempt**, in minutes;
2 retries permit 3 attempts. Use smaller chunks or a longer deadline for expensive
R-RHS simulations. Existing `parallel.workers`, `environments`, `simulations` and
`community_measures` flags retain their meanings. Explicit serial runs remain
serial and do not use worker deadlines.

`experiment_log.txt` remains newline-delimited JSON. Events include
`worker_started` (PID), `worker_timeout`, `worker_death`, `worker_error`, and
`worker_retry`, with stage, chunk ID, case/environment IDs, attempt, elapsed
seconds and reason. Progress and logs are parent-owned.

## Validation (macOS arm64, R 4.6.1, Rscript)

- A 240-case LV experiment produced **identical** serial/parallel temperatures,
  dynamics rows, population summaries, simulation summaries and final community
  measures, in the same order. Separate tests cover unseeded legacy initial draws
  and the parent's environmental RNG continuation.
- Existing CR/LV/discrete, rate-scale, wall-time and compiled-RHS regressions pass.
- Fault injection: indefinite sleep, SIGKILL, an R exception and a missing chunk
  result all triggered their expected events and successful fresh-worker retries.
  The real CPC stage also recovered from sleep/SIGKILL/error injections and
  returned an identical measures table.
- Retry exhaustion reported the affected cases; a parent write error and an
  interrupt stopped active workers. All recorded worker PIDs were gone afterward.
- A separate isolation check intentionally kept an unrelated parent SQLite
  connection open: the worker inherited neither its file descriptor nor a parent
  global variable. The caller's connection remained usable. An abrupt parent
  kill also terminated both supervised workers.
- Stress: three 15,000-case CPC runs with 14 workers and chunks of 100, using short
  four-unit synthetic CR series. Final fixed-installation results: **64.259, 65.295 and 70.321 seconds**.
  All three outputs were identical; all 450 worker attempts succeeded without
  retries, and none of the worker PIDs remained alive. The final RNG-only changes
  affect environment/dynamics orchestration, not the CPC stress calculations.
- Full final `R CMD check`, including vignette rebuilds and PDF manual: pending.

An earlier stress batch completed three times with identical output, but one
chunk hit a transient package-load error while the shared test library was being
reinstalled. The supervisor logged and recovered it. The final stress batch uses
a separate fixed library to remove that test-environment interference.

The large stress fixture tests process scheduling and collection rather than the
original 4,000-unit ecological workload. Test source is in
`tests/testthat/test-supervised-workers.R` and
`tests/testthat/test-parallel-stage-equivalence.R`; the reproducible stress driver
is `scripts/stress_supervised_workers.R`. Install the current source before running
process-isolation tests: child sessions deliberately load the installed package,
not an in-memory `load_all()` namespace.

## Not verified here

There is no connection to the user's Linux/Docker RStudio Server, so that host
and UI session were not tested. Windows behavior is supported by the chosen
backend but was not executed here. The original full experiment was not rerun.
These changes are local and versioned **0.9.0.9002**, ready to commit and install;
no commit was pushed to GitHub.
