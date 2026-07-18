# Build a community object from a resolved case specification

Builds the model-specific community parameter object for one resolved
`case_spec` from the canonical YAML experiment-table workflow.

## Usage

``` r
build_community_from_spec(case_spec, community_seed = NULL)

build_LV_community_from_spec(case_spec, community_seed = NULL)

build_CR_community_from_spec(case_spec, community_seed = NULL)
```

## Arguments

- case_spec:

  A resolved case specification, usually from the `case_spec`
  list-column produced by
  [`create_experiment_table_from_spec()`](https://opetchey.github.io/community.simulator/reference/create_experiment_table_from_spec.md).

- community_seed:

  Optional random seed for community construction. When omitted,
  `case_spec$community$seed` is used, falling back to
  `case_spec$experiment$random_seed`.

## Value

A model-specific community parameter object.

## Examples

``` r
template <- system.file(
  "experiment_templates/lv_discrete.yaml",
  package = "community.simulator"
)
if (nzchar(template)) {
  table <- create_experiment_table_from_spec(template)
  community <- build_community_from_spec(table$case_spec[[1]])
}
```
