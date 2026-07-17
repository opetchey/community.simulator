# Make a consumer-resource community parameter object

Make a consumer-resource community parameter object

## Usage

``` r
make_a_consumer_resource_community(
  S,
  u_max_mean,
  u_max_range,
  u_max_distribution,
  u_opt_mean,
  u_opt_range,
  u_opt_distribution,
  sd_u_mean,
  sd_u_range,
  sd_u_distribution,
  half_saturation_mean,
  half_saturation_range,
  half_saturation_distribution,
  consumer_death_rate,
  resource_renewal_rate,
  resource_supply,
  conversion_efficiency,
  resource_use_mode = "one_resource_all_consumers",
  active_resource = 1,
  resource_specialization = 1,
  resource_specialization_distribution = "constant",
  resource_specialization_mean = resource_specialization,
  resource_specialization_range = 0,
  resource_specialization_precision = 10,
  community_seed
)
```

## Arguments

- S:

  Number of consumer species. The number of resources is set equal to
  `S`, except when `resource_use_mode = "shared_to_private"`, where the
  number of resources is `S + 1`.

- u_max_mean:

  Mean maximum uptake height.

- u_max_range:

  Range of maximum uptake heights.

- u_max_distribution:

  Distribution for maximum uptake heights.

- u_opt_mean:

  Mean thermal optimum for maximum uptake.

- u_opt_range:

  Range of thermal optima.

- u_opt_distribution:

  Distribution for thermal optima.

- sd_u_mean:

  Mean standard-deviation width of the Gaussian uptake curve.

- sd_u_range:

  Range of standard-deviation widths for Gaussian uptake curves.

- sd_u_distribution:

  Distribution for Gaussian uptake-curve widths.

- half_saturation_mean:

  Mean Monod half-saturation constant.

- half_saturation_range:

  Range of half-saturation constants.

- half_saturation_distribution:

  Distribution for half-saturation constants.

- consumer_death_rate:

  Consumer death rate.

- resource_renewal_rate:

  Chemostat resource renewal rate.

- resource_supply:

  Resource supply concentration.

- conversion_efficiency:

  Conversion efficiency from uptake to consumer growth.

- resource_use_mode:

  Resource-use mode. One of `"one_resource_all_consumers"`,
  `"diagonal"`, or `"shared_to_private"`.

- active_resource:

  Active resource index for `"one_resource_all_consumers"` or shared
  resource index for `"shared_to_private"`.

- resource_specialization:

  Backwards-compatible scalar value between 0 and 1 controlling the
  transition from shared-resource use to private- resource use when
  `resource_use_mode = "shared_to_private"`.

- resource_specialization_distribution:

  Distribution used to generate species-level shared-private partition
  values. One of `"constant"`, `"regular"`, `"random_uniform"`, or
  `"beta"`.

- resource_specialization_mean:

  Mean species-level private-resource specialization.

- resource_specialization_range:

  Range for `"regular"` and `"random_uniform"` species-level
  private-resource specialization.

- resource_specialization_precision:

  Precision for beta-distributed species-level private-resource
  specialization. Larger values produce less among-species variation
  around `resource_specialization_mean`.

- community_seed:

  Random seed used when generating community traits.

## Value

A consumer-resource community parameter object.

## Examples

``` r
NULL
#> NULL
```
