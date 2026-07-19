# Changelog

## community.simulator 0.9.0

- Made YAML experiment specifications the main workflow for reading,
  expanding, running, and measuring simulation experiments.
- Added schema validation, richer YAML examples, and clearer error
  messages for experiment specifications.
- Standardised user-facing parameter names across LV and
  consumer-resource model specifications.
- Cleaned the community-builder API around
  [`build_LV_community()`](https://opetchey.github.io/community.simulator/reference/build_community_constructors.md),
  [`build_CR_community()`](https://opetchey.github.io/community.simulator/reference/build_community_constructors.md),
  [`build_community_from_spec()`](https://opetchey.github.io/community.simulator/reference/build_community_from_spec.md),
  [`build_LV_community_from_spec()`](https://opetchey.github.io/community.simulator/reference/build_community_from_spec.md),
  and
  [`build_CR_community_from_spec()`](https://opetchey.github.io/community.simulator/reference/build_community_from_spec.md).
- Renamed the discrete-time LV simulator to
  [`simulator_lv_discrete()`](https://opetchey.github.io/community.simulator/reference/simulator_lv_discrete.md).
- Made discrete-time LV immigration rate an explicit required YAML
  simulation parameter.
- Improved experiment progress reporting, runtime estimates, experiment
  logs, and parallel workflow controls.
- Added and documented single-community walkthroughs,
  reviewer/user/developer entry points, richer pkgdown navigation, and
  the Shiny simulation explorer.
- Expanded the User Guide with full model equations, symbol definitions,
  output-measure tables, workflow diagrams, and CR resource-use
  notation.
- Cleaned generated documentation and removed obsolete JSON-era helpers
  and stale documentation artifacts where no longer needed.

## community.simulator 0.8.0

- Added workflows for discrete-time LV, continuous-time LV, and
  continuous-time consumer-resource simulations.
- Added experiment logging with step timings, experiment specifications,
  output paths, and workflow status.
- Added preflight estimates for output sizes and total runtime before
  launching experiments.
- Added parallel options for environmental time-series creation,
  simulation runs, and community performance curve measures.
- Added single-community walkthroughs, an experiment getting-started
  vignette, a user guide, and a pkgdown site.
- Added a Shiny simulation explorer for building one community,
  inspecting its structure and performance curves, and then running
  dynamics.
- Improved user-facing parameter names while preserving backward
  compatibility with older names.

Known compatibility note: some output datasets and internal objects
still retain older internal parameter names for backward compatibility.
A future release will standardise output and reporting names around the
user-facing YAML terminology.
