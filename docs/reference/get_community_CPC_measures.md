# Get measures of community performance curves

Get measures of community performance curves

## Usage

``` r
get_community_CPC_measures(
  temperatures,
  expt,
  expt_def,
  every_t = 1,
  soft_viability_scale = 0.01,
  parallel_community_measures = FALSE,
  parallel_workers = 1,
  verbose = TRUE
)
```

## Arguments

- temperatures:

  - temperature time series used in the simulations

- expt:

  - experiment table with community parameters

- expt_def:

  - experiment design information

- every_t:

  - how often to sample the temperature time series (e.g., every 1 time
    step, every 10 time steps, etc.)

- soft_viability_scale:

  Positive scale parameter for the soft viability transform
  `plogis(g_i(T) / soft_viability_scale)`.

- parallel_community_measures:

  Logical. If `TRUE`, calculate per-case community performance curve
  measures in parallel where supported.

- parallel_workers:

  Number of worker processes to use when `parallel_community_measures`
  is `TRUE`.

- verbose:

  Logical. If `TRUE`, print progress messages.

## Value

A dataset containing measures of community performance curves, including
CV of community performance, synchrony of performance curves, and
average CV of species performance curves weighted by their mean
contribution to community performance.

## Examples

``` r
NULL
#> NULL
```
