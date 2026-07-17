# Create temperature times series. Currently three options for how times series vary among cases.

1.  `same_per_replicate` replicates of the same number share the same
    environmental time series. E.g., `case1_rep1` and `case2_rep1` share
    the same time series.

2.  `all_different` all environmental time series are different

Create temperature times series. Currently three options for how times
series vary among cases.

1.  `same_per_replicate` replicates of the same number share the same
    environmental time series. E.g., `case1_rep1` and `case2_rep1` share
    the same time series.

2.  `all_different` all environmental time series are different

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
