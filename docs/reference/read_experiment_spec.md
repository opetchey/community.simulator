# Read an experiment specification

Reads a YAML experiment specification that uses the new nested,
declarative experiment format. The specification is data only: it does
not parse or evaluate R expressions from the specification file.

## Usage

``` r
read_experiment_spec(path)
```

## Arguments

- path:

  Path to a `.yaml` or `.yml` experiment specification.

## Value

A validated experiment specification list with class
`community_simulator_experiment_spec`.

## Examples

``` r
template <- system.file(
  "experiment_templates/lv_discrete.yaml",
  package = "community.simulator"
)
if (nzchar(template)) {
  spec <- read_experiment_spec(template)
}
```
