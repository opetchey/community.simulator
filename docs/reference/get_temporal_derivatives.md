# Calculate the derivatives of the growth rate - temperature relationship at the temperatures in the temperature times series of each case in the experiment.

Calculate the derivatives of the growth rate - temperature relationship
at the temperatures in the temperature times series of each case in the
experiment.

## Usage

``` r
get_temporal_derivatives(
  experiment_folder,
  experiment_design_filename,
  every_t = 10,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- experiment_folder:

  The folder where the experiment is stored

- experiment_design_filename:

  The name of the experiment design file

- every_t:

  The time interval at which to calculate the derivatives

- overwrite:

  Logical. If `TRUE`, overwrite an existing temporal-derivatives
  database.

- verbose:

  Logical. If `TRUE`, print progress and output messages.

## Value

Nothing. Saves data to a file.

## Examples

``` r
NULL
#> NULL
```
