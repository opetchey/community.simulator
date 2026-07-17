# Run a complete experiment workflow

This is a convenience wrapper for the standard experiment workflow. It
creates the experiment folder, builds the experiment table, generates
environmental time series, simulates dynamics, and calculates
community-level summary measures.

## Usage

``` r
run_experiment(
  experiment_folder_location,
  experiment_name,
  experiment_design_filename,
  overwrite = FALSE,
  verbose = TRUE,
  confirm_run = interactive()
)
```

## Arguments

- experiment_folder_location:

  Location where the experiment folder should be created.

- experiment_name:

  Name of the experiment folder.

- experiment_design_filename:

  Name of the experiment definition file. This file must already be
  present inside the experiment folder.

- overwrite:

  Logical. If `TRUE`, overwrite existing workflow outputs.

- verbose:

  Logical. If `TRUE`, print progress messages during the workflow.

- confirm_run:

  Logical. If `TRUE`, show a preflight summary and ask whether to
  continue before creating environments and simulating dynamics.
  Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

Invisibly returns a named list containing the experiment folder and the
main output file paths.

## Details

The preflight summary estimates output rows, database sizes, and runtime
from the experiment table and design settings, including output controls
such as `save_dynamics`, `save_resources`, `dynamics_save_every`,
`resources_save_every`, `summary_checkpoint_every`, and
`runtime_update_every`. Confirmation happens before environment
generation, dynamics simulation, and analysis. These estimates are
intended as rough guidance before launching large experiments. Compact
simulation summaries are checkpointed during simulation and are used to
calculate `community_measures.RDS` even when `save_dynamics = FALSE`.
Each run also writes `experiment_log.txt`, a plain-text
newline-delimited JSON log containing the experiment specification,
preflight summary, output paths, workflow status, and elapsed time for
each workflow step.

## Examples

``` r
NULL
#> NULL
```
