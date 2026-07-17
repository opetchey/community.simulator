# Simulate Lotka-Volterra community dynamics in continuous time

Simulate Lotka-Volterra community dynamics in continuous time

## Usage

``` r
simulator_lv_continuous(
  input_com_params,
  TcelSeries,
  initial_abundances,
  times = seq_len(ncol(TcelSeries)),
  output_times = times,
  temperature_interpolation = "linear",
  immigration_rate = 0.1,
  immigration_mode = "continuous",
  ode_method = "lsoda",
  rtol = 1e-06,
  atol = 1e-08,
  max_step = 1,
  blowup_threshold = 1e+12
)
```

## Arguments

- input_com_params:

  Community object, containing all species and community parameters.

- TcelSeries:

  Time series of temperature values.

- initial_abundances:

  Initial abundances of each species.

- times:

  Times at which temperatures are defined. Defaults to one time unit per
  temperature value.

- output_times:

  Times at which abundances should be returned.

- temperature_interpolation:

  How temperature should be interpolated between supplied time points.
  One of `"linear"` or `"constant"`.

- immigration_rate:

  Immigration rate per species.

- immigration_mode:

  One of `"continuous"` or `"pulse"`.

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

  Abundance threshold above which the run stops.

## Value

Time series of population abundances for each species.

## Examples

``` r
NULL
#> NULL
```
