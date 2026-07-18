# Run a complete experiment workflow

This is the standard YAML experiment workflow. It creates or locates the
experiment folder, reads the YAML experiment specification from that
folder, builds the canonical experiment table, generates environmental
time series, simulates dynamics, and calculates community-level summary
measures.

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

  Name of the experiment definition file. This must be a `.yaml` or
  `.yml` file already present inside the experiment folder.

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

The preflight summary estimates output rows, database sizes, and total
runtime from the canonical experiment table and YAML settings.
Confirmation happens before environment generation, dynamics simulation,
and analysis. Each run also writes `experiment_log.txt`, a plain-text
newline-delimited JSON log containing the parsed YAML specification,
preflight summary, output paths, workflow status, and elapsed time for
each workflow step.

## Examples

``` r
NULL
#> NULL
```
