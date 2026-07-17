# Plot total abundance through time for one simulation case

Plot total abundance through time for one simulation case

## Usage

``` r
plot_case_total_abundance(experiment_folder, case_id, quiet = FALSE)
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

A `ggplot` object, or `NULL` if `dynamics.db` is unavailable.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_case_total_abundance("path/to/experiment", "case_id_1")
} # }
```
