# Plot resource dynamics for one consumer-resource simulation case

Plot resource dynamics for one consumer-resource simulation case

## Usage

``` r
plot_resource_dynamics(experiment_folder, case_id, quiet = FALSE)
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

A `ggplot` object, or `NULL` if `resources.db` is unavailable.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_resource_dynamics("path/to/experiment", "case_id_1")
} # }
```
