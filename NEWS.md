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
