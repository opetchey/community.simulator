# Make standard diagnostic plots for one simulation case

`make_plots_for_one_community()` is a convenience wrapper around the
smaller plotting helpers. It only uses standard workflow outputs and
returns the plots that can be made from files currently present in the
experiment folder. Missing optional outputs, such as `dynamics.db` or
`resources.db`, are skipped rather than treated as errors.

## Usage

``` r
make_plots_for_one_community(experiment_folder, case_id_oi, quiet = FALSE)
```

## Arguments

- experiment_folder:

  Folder where the experiment outputs are stored.

- case_id_oi:

  Case identifier to plot, such as `"case_id_1"`.

- quiet:

  Logical. If `FALSE`, explain why individual plots cannot be made.

## Value

Named list of available `ggplot` objects. Entries whose source data are
unavailable are omitted.

## Examples

``` r
if (FALSE) { # \dontrun{
plots <- make_plots_for_one_community("path/to/experiment", "case_id_1")
names(plots)
} # }
```
