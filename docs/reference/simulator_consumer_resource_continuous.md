# Simulate consumer-resource dynamics in continuous time

Simulate consumer-resource dynamics in continuous time

## Usage

``` r
simulator_consumer_resource_continuous(
  input_com_params,
  TcelSeries,
  initial_consumer_abundances,
  initial_resource_values,
  times = seq_len(ncol(TcelSeries)),
  output_times = times,
  temperature_interpolation = "linear",
  consumer_immigration_rate = 0.1,
  ode_method = "lsoda",
  rtol = 1e-06,
  atol = 1e-08,
  max_step = 1,
  blowup_threshold = 1e+12,
  negative_tolerance = 1e-08
)
```

## Arguments

- input_com_params:

  Consumer-resource community object.

- TcelSeries:

  Time series of temperature values.

- initial_consumer_abundances:

  Initial consumer abundances.

- initial_resource_values:

  Initial resource values.

- times:

  Times at which temperatures are defined.

- output_times:

  Times at which states should be returned.

- temperature_interpolation:

  Temperature interpolation method.

- consumer_immigration_rate:

  Consumer immigration rate per unit time.

- ode_method:

  ODE solver method passed to
  [`deSolve::ode()`](https://rdrr.io/pkg/deSolve/man/ode.html).

- rtol:

  Relative tolerance passed to
  [`deSolve::ode()`](https://rdrr.io/pkg/deSolve/man/ode.html).

- atol:

  Absolute tolerance passed to
  [`deSolve::ode()`](https://rdrr.io/pkg/deSolve/man/ode.html).

- max_step:

  Maximum solver step size.

- blowup_threshold:

  State-value threshold above which the run stops.

- negative_tolerance:

  Negative tolerance for numerical error.

## Value

A named list with `consumers` and `resources` data frames.

## Examples

``` r
NULL
#> NULL
```
