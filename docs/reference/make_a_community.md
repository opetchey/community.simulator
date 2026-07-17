# Make a community parameter object

This function makes a community object containing the parameters for all
species in the community. At the moment, only the temperature optima
`b_opt_i` vary among species; the remaining parameters are shared or are
generated from common distributions.

## Usage

``` r
make_a_community(
  S,
  a_b_mean,
  a_b_range,
  a_b_distribution,
  b_opt_mean,
  b_opt_range,
  b_opt_distribution,
  sd_perf_distribution,
  sd_perf_mean,
  sd_perf_range,
  alpha_ij_mean = NULL,
  alpha_ij_sd = NULL,
  community_seed,
  a_d,
  z,
  alpha_jj = 1,
  alpha_ij_distribution = NULL,
  lv_interaction_spec = NULL
)
```

## Arguments

- S:

  Number of species in the community

- a_b_mean:

  Mean of the distribution from which a_b values are drawn.

- a_b_range:

  Range of the distribution from which a_b values are drawn.

- a_b_distribution:

  Distribution used to generate `a_b_i` values.

- b_opt_mean:

  Mean of the distribution from which `b_opt_i` values are drawn.

- b_opt_range:

  Range of the distribution from which `b_opt_i` values are drawn.

- b_opt_distribution:

  Distribution used to generate `b_opt_i` values.

- sd_perf_distribution:

  Distribution used to generate Gaussian performance-curve widths.

- sd_perf_mean:

  Mean of the distribution from which Gaussian performance-curve widths
  are drawn.

- sd_perf_range:

  Range of the distribution from which Gaussian performance-curve widths
  are drawn.

- alpha_ij_mean:

  Deprecated. Mean used by the legacy interaction generator.

- alpha_ij_sd:

  Deprecated. Spread used by the legacy interaction generator.

- community_seed:

  Random seed used when generating community traits.

- a_d:

  Death rate when temperature is equal to 0; same for all species.

- z:

  Exponential rate of increase in death rate with temperature; same for
  all species.

- alpha_jj:

  Value of the diagonal of the community matrix, shared across species.

- alpha_ij_distribution:

  Deprecated. Distribution used by the legacy interaction generator.

- lv_interaction_spec:

  Optional named list specifying LV interactions. Preferred over legacy
  `alpha_ij_*` arguments. Fields include `type` (`"none"`,
  `"competition"`, `"any"`, or `"predator_prey"`), `symmetry`
  (`"asymmetric"`, `"symmetric"`, or `"antisymmetric"`), `distribution`
  (`"constant"`, `"uniform"`, `"normal"`, `"lognormal"`, or `"gamma"`),
  `parameters`, and `diagonal`.

## Value

Returns a list containing the community object.

## Examples

``` r
NULL
#> NULL
```
