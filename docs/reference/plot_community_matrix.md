# Plot the community matrix for one simulation case

Plot the community matrix for one simulation case

## Usage

``` r
plot_community_matrix(
  experiment_folder,
  case_id,
  matrix = c("auto", "interaction", "resource_use", "half_saturation"),
  quiet = FALSE
)
```

## Arguments

- experiment_folder:

  Folder where the experiment outputs are stored.

- case_id:

  Case identifier to plot, such as `"case_id_1"`.

- matrix:

  Type of community matrix to plot. `"auto"` plots the LV interaction
  matrix for LV cases and the resource-use matrix for consumer-resource
  cases.

- quiet:

  Logical. If `FALSE`, explain why a plot cannot be made when required
  files are missing.

## Value

A `ggplot` object, or `NULL` if no supported matrix is available.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_community_matrix("path/to/experiment", "case_id_1")
} # }
```
