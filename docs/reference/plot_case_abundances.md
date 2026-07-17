# Plot species abundances for one simulation case

Plot species abundances for one simulation case

## Usage

``` r
plot_case_abundances(
  experiment_folder,
  case_id,
  log10_y = FALSE,
  quiet = FALSE
)
```

## Arguments

- experiment_folder:

  Folder where the experiment outputs are stored.

- case_id:

  Case identifier to plot, such as `"case_id_1"`.

- log10_y:

  Logical. If `TRUE`, plot `log10(abundance)` on the y-axis.

- quiet:

  Logical. If `FALSE`, explain why a plot cannot be made when required
  files are missing.

## Value

A `ggplot` object, or `NULL` if `dynamics.db` is unavailable.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_case_abundances("path/to/experiment", "case_id_1")
} # }
```
