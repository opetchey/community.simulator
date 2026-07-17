# Calculate intrinsic growth rate from species parameters and a temperature, assuming a Gaussian birth rate - temperature response curve and an exponential death rate - temperature response curve.

Calculate intrinsic growth rate from species parameters and a
temperature, assuming a Gaussian birth rate - temperature response curve
and an exponential death rate - temperature response curve.

## Usage

``` r
intrinsic_growth_gaussian(a_b_i, b_opt_i, s_i, a_d_i, z_i, temperature)
```

## Arguments

- a_b_i:

  Value of birth rate when temperature is equal to b_opt_i

- b_opt_i:

  Temperature at which birth rate is maximized

- s_i:

  Standard-deviation width of the Gaussian birth-rate curve.

- a_d_i:

  Value of death rate when temperature is equal to 0

- z_i:

  Slope of the exponential curve

- temperature:

  Temperature at which to calculate the intrinsic growth rate

## Value

Returns the intrinsic growth rate at the given temperature

## Examples

``` r
NULL
#> NULL
```
