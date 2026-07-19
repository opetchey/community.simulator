# Simulate the population dynamics of a community of species using the Lotka-Volterra competition model with temperature-dependent vital rates.

The LV carrying-capacity scaling constants are fixed internally at
`bet = delt = 0.001`, and net growth includes a small offset of `1e-6`.

## Usage

``` r
simulator_lv_discrete(
  input_com_params,
  TcelSeries,
  initial_abundances,
  immigration_rate
)
```

## Arguments

- input_com_params:

  Community object, containing all species and community parameters

- TcelSeries:

  Time series of temperature values

- initial_abundances:

  Initial abundances of each species

- immigration_rate:

  Immigration rate added to each species per time step.

## Value

Time series of population abundances for each species

## Examples

``` r
NULL
#> NULL
```
