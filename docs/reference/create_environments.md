# Create temperature time series.

Environmental series sharing is controlled in the experiment table by
the JSON field `environment_sharing`. With `"same_per_replicate"`, cases
with the same environment treatment and replicate number share an
environmental time series. With `"all_different"`, each simulation case
gets its own environmental time series. The legacy field
`temperature_series_control` is still accepted as an alias.

## Usage

``` r
create_environments(
  experiment_folder,
  experiment_design_filename,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- experiment_folder:

  The folder where all information about the experiment is stored

- experiment_design_filename:

  The filename of the experiment design file

- overwrite:

  Logical. If `TRUE`, overwrite an existing temperatures database.

- verbose:

  Logical. If `TRUE`, print messages about written outputs.

## Value

Returns nothing. Saves to SQLite databases the temperature time series.

## Details

Experiment JSON files can optionally include `parallel_environments`,
`parallel_simulations`, `parallel_workers`, `environment_progress`, and
`runtime_update_every`. When `parallel_environments` evaluates to
`TRUE`, or when it is absent and `parallel_simulations` evaluates to
`TRUE`, environmental time series are generated in parallel and SQLite
tables are still written serially by the parent process. Parallel
processing is intended for macOS/Linux.

## Examples

``` r
NULL
#> NULL
```
