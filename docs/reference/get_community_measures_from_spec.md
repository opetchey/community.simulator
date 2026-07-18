# Calculate community measures from a YAML experiment specification

Calculate community measures from a YAML experiment specification

## Usage

``` r
get_community_measures_from_spec(
  experiment_folder,
  spec,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- experiment_folder:

  Folder containing `experiment_table.RDS` and where `temperatures.db`
  should be written.

- spec:

  A YAML experiment specification path or specification object.

- overwrite:

  Logical. If `TRUE`, overwrite existing outputs.

- verbose:

  Logical. If `TRUE`, print progress messages.

## Value

Invisibly returns the path to `community_measures.RDS`.
