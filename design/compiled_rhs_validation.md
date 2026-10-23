# Optional compiled ODE RHS: implementation and validation

Date: 23 October 2026.

## Delivered design

The R RHS remains the default and reference. Both continuous simulators accept
`rhs = "r" | "compiled"`; YAML uses `simulation.ode.rhs`. Selection passes through
`flatten_spec_settings()` and `simulate_one_dynamics_case()`. The experiment log
records the effective value in `experiment_specification` and the preflight
summary, including `r` when omitted. Compiled requests fail clearly if the DLL
is unavailable.

Registered C callbacks use dynamically allocated, per-solve parameter storage.
The vector layout and ownership are documented in `vignettes/yaml_templates.Rmd`
and implemented in `R/compiled_rhs.R`, the two simulator files, and `src/rhs.c`.
No solver defaults, tolerance settings, events, output grids, or postprocessing
were changed. Rate preparation remains in R; the LV growth offset is packed
already scaled. There is no analytic Jacobian.

C reproduces the internal state-finiteness, blow-up and derivative-finiteness
checks with the reference error text. No check was moved to post-solve. Existing
returned-state checks and clipping remain shared R code. LV negative trials are
recorded in C and returned through the existing attribute. Parameter and uptake
scratch storage use `R_alloc` inside deSolve's native solve, so errors also release
storage; independent processes have independent callback state.

Forcings use deSolve `initforc`, `rule = 2`, `f = 0`, and the chosen linear/constant
method. Tests directly compare the temperature exposed by the C callback against
`approxfun()` at interior points, exact knots, and outside both ends. Additional
trajectory tests exercise unsorted/duplicate times and LV pulse immigration.

## Regression measurements

Optimized installed package; R 4.6.1, macOS arm64, Apple clang 21. Each of CR and
LV was tested across S = 4, 8, 12; gamma = 0, 0.4, 1; rate_scale = 0.1, 1, 10
(54 model/case combinations total). Measured trajectories span 50 time units at
integer output times, with rtol = 1e-4, atol = 1e-6, hmax = 0.01 and maxsteps =
500000. Community parameters come from the package templates, with rate scaling
applied by the standard community builder. Temperature seed is 724.

| Quantity (maximum across cases) | CR | LV |
|---|---:|---:|
| Relative trajectory difference | 1.820782e-6 | 3.875969e-6 |
| Difference / (atol + rtol × abs(reference)) | 0.01726192 | 0.03875683 |
| Relative numeric case-summary difference | 4.951691e-7 | 9.972120e-5 |
| Relative step-count difference | 0 | 0 |
| Relative RHS-evaluation-count difference | 0.0003633721 | 0.02808329 |

Relative differences use abs(reference) with a floor of 1e-6. Summaries include
abundance means, variability, temperature sensitivity and extinction metrics;
all numeric summary fields are included. Near-zero summaries can have larger
relative error despite small trajectory differences. Automated tests additionally
compare complete case and population summaries, and run the 54-case grid over
10 time units so routine package checks remain quick.

## Benchmark measurements

These are reproducible substitute fixtures matching the requested richness and
gamma, **not the original diagnostic cases 1 and 324**. Their saved communities,
forcing series and starting states were not identified in the workspace. In
particular, these fixtures need fewer steps than the supplied case-1 diagnostic.
The user's 5.8 s and 2.9 s baselines must not be divided by these C timings.

500 time units; rtol = 1e-4, atol = 1e-6, max_step = 0.01, maxsteps = 500000.
No burn-in; initial consumers total 30, resources 1000; immigration 0.01;
rate_scale = 1. Medians of three runs of the full simulator wrapper:

| S / R / gamma | RHS | Seconds | Steps | RHS evaluations | µs / RHS evaluation | Speed-up |
|---|---|---:|---:|---:|---:|---:|
| 4 / 5 / 0 | R | 1.845 | 50,001 | 88,953 | 20.7413 | 1× |
| 4 / 5 / 0 | C | 0.022 | 50,001 | 88,953 | 0.2473 | 83.9× |
| 12 / 13 / 1 | R | 2.126 | 50,001 | 88,878 | 23.9204 | 1× |
| 12 / 13 / 1 | C | 0.086 | 50,001 | 88,881 | 0.9676 | 24.7× |

µs/evaluation divides total wrapper elapsed time by the solver's RHS evaluation
count; it is an amortized cost, not an isolated callback microbenchmark. These
short compiled timings are timer-sensitive. Three raw repeats are retained.
The R costs per evaluation are consistent with the supplied 22–26 µs evidence.
The measurements support a substantial reduction in solver callback overhead,
but do not establish that either entire server experiment will finish in a day.
Exact pilot reruns and a representative end-to-end server sample remain needed.

Reproduce from the repository root using an optimized installed package:

```sh
R CMD INSTALL --preclean .
Rscript scripts/benchmark_compiled_rhs.R /tmp/community-rhs-results
```

Raw results: `design/compiled_rhs_results/benchmark.csv` and `parity.csv`.
The script captures deSolve diagnostics without changing public return values.

## Package validation

A clean optimized source installation succeeded with the normal C toolchain.
The original 1,101 assertions passed before adding compiled coverage. New tests
cover the parity grid, interpolation, pulse events, internal errors and recovery,
negative-trial reset, YAML validation, and logged end-to-end parallel experiments.

Build housekeeping excludes the two existing `RC_gamma_11` experiment-output
folders from the package tarball (their database/report files remain untouched).
Two existing `tail()` calls were qualified as `utils::tail()` to remove a check
note. No external compiled library or additional R dependency is required.

Full final `R CMD check` (including vignette rebuilds and the PDF manual):
**Status: OK**, with zero errors, warnings or notes. All **1,271 test assertions**
passed with zero failures, warnings or skips. The check log is retained at
`design/compiled_rhs_results/00check.log`. Repository-index lookups were blocked
by this environment's network restrictions, but installed dependencies were
sufficient and the dependency check passed. A GitHub remote installation and
execution on the RStudio server were not performed here.
