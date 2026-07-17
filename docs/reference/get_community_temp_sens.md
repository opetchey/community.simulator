# Get the sensitivity of total biomass to temperature variation. Currently measured as the slope of a linear regression of total biomass on temperature.

Get the sensitivity of total biomass to temperature variation. Currently
measured as the slope of a linear regression of total biomass on
temperature.

## Usage

``` r
get_community_temp_sens(dynamics, temperatures, rollsumr_window = 50, expt)
```

## Arguments

- dynamics:

  Connection to database containing dynamics data

- temperatures:

  Connection to database containing temperature data

- rollsumr_window:

  The window size for the rolling sum of temperature. Default is 50

- expt:

  Dataset containing experiment information

## Value

A dataset containing the sensitivity of total biomass to temperature
variation

## Examples

``` r
NULL
#> NULL
```
