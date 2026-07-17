# Calculate the difference in growth rate from one time point to the next

Calculate the difference in growth rate from one time point to the next

## Usage

``` r
get_delta_igr(
  experiment_folder,
  experiment_design_filename,
  every_t = 1,
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

  The time interval at which to calculate the difference

- overwrite:

  Logical. If `TRUE`, overwrite an existing delta-IGR database.

- verbose:

  Logical. If `TRUE`, print progress and output messages.

## Value

Nothing. Saves data to a file.

## Examples

``` r
NULL
#> NULL
```
