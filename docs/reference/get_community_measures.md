# Get various community level measures, e.g., community stability, response diversity, position of optimal temperature, etc.

Get various community level measures, e.g., community stability,
response diversity, position of optimal temperature, etc.

## Usage

``` r
get_community_measures(
  experiment_folder,
  experiment_design_filename,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- experiment_folder:

  The folder containing the experiment data

- experiment_design_filename:

  The name of the experiment design file

- overwrite:

  Logical. If `TRUE`, overwrite an existing community-measures file.

- verbose:

  Logical. If `TRUE`, print messages about written outputs.

## Value

Nothing. Saves data to a file.

## Details

If `simulation_summaries.RDS` is present, dynamic abundance summaries
are read from that compact file. Otherwise, the function falls back to
calculating those summaries from `dynamics.db`.

## Examples

``` r
NULL
#> NULL
```
