# Create environments from a YAML experiment specification

Create environments from a YAML experiment specification

## Usage

``` r
create_environments_from_spec(
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

Invisibly returns the path to `temperatures.db`.
