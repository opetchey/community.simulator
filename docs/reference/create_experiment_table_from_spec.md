# Create a canonical experiment table from a YAML experiment specification

Expands a validated YAML experiment specification into one row per
simulation case. Global treatments are applied as overrides to the
baseline specification, LV interaction treatments are expanded as model
treatments, and community/environment replicates are crossed with those
treatments.

## Usage

``` r
create_experiment_table_from_spec(
  spec,
  output_path = NULL,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- spec:

  An experiment specification returned by
  [`read_experiment_spec()`](https://opetchey.github.io/community.simulator/reference/read_experiment_spec.md),
  a path to a YAML experiment specification, or a compatible named list.

- output_path:

  Optional path to save the resulting table as an RDS file.

- overwrite:

  Logical. If `TRUE`, overwrite an existing `output_path`.

- verbose:

  Logical. If `TRUE`, print a message when `output_path` is written.

## Value

A tibble with one row per simulation case. The `case_spec` list-column
contains the fully resolved nested specification for that case. When
`output_path` is supplied, lean runtime tables are also written next to
the full table: `environment_table.RDS`, `simulation_table.RDS`, and
`community_objects.RDS`.

## Examples

``` r
template <- system.file(
  "experiment_templates/lv_discrete.yaml",
  package = "community.simulator"
)
if (nzchar(template)) {
  experiment_table <- create_experiment_table_from_spec(template)
}
```
