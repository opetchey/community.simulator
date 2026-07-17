# Get measures of community performance curves

Get measures of community performance curves

## Usage

``` r
get_community_CPC_measures(
  temperatures,
  expt,
  expt_def,
  every_t = 1,
  soft_viability_scale = 0.01
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
