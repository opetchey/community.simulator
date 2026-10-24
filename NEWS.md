# community.simulator 0.9.0.9002

- Replaced fork workers in environment generation, dynamics and CPC measures
  with supervised independent R sessions using callr. Database connections are
  closed before dispatch; only each chunk's inputs are sent to workers.
- Added optional `parallel.worker_timeout_minutes` (120), `worker_retries` (2),
  and `chunk_size` (10). Hung, dead and erroring workers are terminated and
  retried in fresh sessions, with case IDs and timings in the JSON-line log.
  All workers are cleaned up on completion, error or interrupt.
- Parallel results are committed in input order, and case coverage is validated
  before final output. Explicit seeds are preserved; legacy unseeded dynamics
  draw initial abundances once in the parent in serial order for retry stability.
  Unseeded legacy runs saving database output use one supervised case at a time
  to preserve SQLite-related RNG consumption; explicit abundance seeds retain
  full concurrency.
- Fixed integer overflow in large preflight runtime and temperature-row estimates.
- The compiled ODE RHS and its numerical settings are unchanged.

# community.simulator 0.9.0.9001

- Added an optional compiled C RHS for continuous consumer-resource and LV
  models, selected with `simulation.ode.rhs: compiled`. The reference R RHS
  remains the default.

# community.simulator 0.9.0.9000

- Experiment YAML validation now rejects unknown settings at every mapping level,
  including interaction parameters and treatment paths, with full setting paths,
  allowed settings and suggestions for likely typos.

- Added `output.record_dynamics_wall_time` (default `false`) to record per-case
  dynamics elapsed seconds in `simulation_summaries.RDS`.

- Continuous LV now evaluates negative internal solver trials at zero, as CR
  already does, and logs their count, minimum and time per case. Returned
  abundances still honour `simulation.negative_tolerance` (default `1e-8`);
  non-finite values and blow-ups remain errors.

- Added `simulation.rate_scale` as a continuous-model treatment. Biological
  rates are scaled after draws, including CPC aliases; matched levels share
  trait, interaction, initial-abundance and environment draws.
- Preflight summaries display the biological rate multipliers. Non-unit scales
  reject discrete LV and fixed-time LV immigration pulses.

# community.simulator 0.9.0

- Added `simulation.ode.maxsteps` for continuous LV and consumer-resource simulations, allowing dense environmental forcing runs to raise the `lsoda` internal work limit from YAML.
- Added `environment.temperature.sample_interval` for continuous LV and
  consumer-resource experiments, so generated environmental series can be
  sampled more finely than one model-time unit and written in full to
  `temperatures.db`.
- Made YAML experiment specifications the main workflow for reading,
  expanding, running, and measuring simulation experiments.
- Added schema validation, richer YAML examples, and clearer error messages for
  experiment specifications.
- Standardised user-facing parameter names across LV and consumer-resource
  model specifications.
- Cleaned the community-builder API around `build_LV_community()`,
  `build_CR_community()`, `build_community_from_spec()`,
  `build_LV_community_from_spec()`, and `build_CR_community_from_spec()`.
- Renamed the discrete-time LV simulator to `simulator_lv_discrete()`.
- Made discrete-time LV immigration rate an explicit required YAML simulation
  parameter.
- Improved experiment progress reporting, runtime estimates, experiment logs,
  and parallel workflow controls.
- Added and documented single-community walkthroughs, reviewer/user/developer
  entry points, richer pkgdown navigation, and the Shiny simulation explorer.
- Expanded the User Guide with full model equations, symbol definitions,
  output-measure tables, workflow diagrams, and CR resource-use notation.
- Cleaned generated documentation and removed obsolete JSON-era helpers and
  stale documentation artifacts where no longer needed.

# community.simulator 0.8.0

- Added workflows for discrete-time LV, continuous-time LV, and continuous-time consumer-resource simulations.
- Added experiment logging with step timings, experiment specifications, output paths, and workflow status.
- Added preflight estimates for output sizes and total runtime before launching experiments.
- Added parallel options for environmental time-series creation, simulation runs, and community performance curve measures.
- Added single-community walkthroughs, an experiment getting-started vignette, a user guide, and a pkgdown site.
- Added a Shiny simulation explorer for building one community, inspecting its structure and performance curves, and then running dynamics.
- Improved user-facing parameter names while preserving backward compatibility
  with older names.

Known compatibility note: some output datasets and internal objects still retain
older internal parameter names for backward compatibility. A future release will
standardise output and reporting names around the user-facing YAML terminology.
