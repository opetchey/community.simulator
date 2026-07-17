# Plot the temperature time series for one simulation case

Plot the temperature time series for one simulation case

## Usage

``` r
plot_case_temperature(experiment_folder, case_id, quiet = FALSE)
```

## Arguments

- experiment_folder:

  Folder where the experiment outputs are stored.

- case_id:

  Case identifier to plot, such as `"case_id_1"`.

- quiet:

  Logical. If `FALSE`, explain why a plot cannot be made when required
  files are missing.

## Value

A `ggplot` object, or `NULL` if the required standard outputs are not
available.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_case_temperature("path/to/experiment", "case_id_1")
} # }
```
