# This function takes the experimental design from the specified JSON experiment design file and creates a table of all the simulations that will be run.

This function takes the experimental design from the specified JSON
experiment design file and creates a table of all the simulations that
will be run.

## Usage

``` r
create_experiment_table(
  experiment_folder,
  experiment_design_filename,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- experiment_folder:

  Folder where the experiment data will be saved

- experiment_design_filename:

  Name of the experiment definition file

- overwrite:

  Logical. If `TRUE`, overwrite an existing experiment table.

- verbose:

  Logical. If `TRUE`, print messages about written outputs.

## Value

Returns the number of cases in the experiment. Also saves to RDS the
experiment design, for later use.

## Details

LV experiment definitions can use the preferred `lv_interactions` field
to specify one or more named interaction treatments. Each treatment can
set `type`, `symmetry`, `distribution`, `parameters`, and `diagonal`.
Legacy `alpha_ij_*` fields are still supported and are converted to
interaction specifications internally.

## Examples

``` r
NULL
#> NULL
```
