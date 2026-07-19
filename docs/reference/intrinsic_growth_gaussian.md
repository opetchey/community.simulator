# Calculate intrinsic growth rate from species parameters and temperature

Assumes a Gaussian birth-rate temperature response curve and an
exponential death-rate temperature response curve.

## Usage

``` r
intrinsic_growth_gaussian(
  birth_maximum,
  birth_optimum,
  birth_width,
  death_intercept,
  death_temperature_slope,
  temperature
)
```

## Arguments

- birth_maximum:

  Value of birth rate at the birth optimum.

- birth_optimum:

  Temperature at which birth rate is maximized.

- birth_width:

  Standard-deviation width of the Gaussian birth-rate curve.

- death_intercept:

  Value of death rate when temperature is equal to 0.

- death_temperature_slope:

  Slope of the exponential death-rate curve.

- temperature:

  Temperature at which to calculate the intrinsic growth rate.

## Value

Returns the intrinsic growth rate at the given temperature

## Examples

``` r
NULL
#> NULL
```
