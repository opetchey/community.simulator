# Build an LV community object

This is the rewrite-facing LV community constructor. It uses the same
parameter names as the YAML experiment schema and delegates to the
existing LV constructor at the adapter boundary.

This is the rewrite-facing consumer-resource community constructor. It
uses the same parameter names as the YAML experiment schema and
delegates to the existing CR constructor at the adapter boundary.

## Usage

``` r
build_LV_community(
  S,
  birth_maximum_mean,
  birth_maximum_range,
  birth_maximum_distribution,
  birth_optimum_mean,
  birth_optimum_range,
  birth_optimum_distribution,
  birth_width_distribution,
  birth_width_mean,
  birth_width_range,
  community_seed,
  death_intercept,
  death_temperature_slope,
  lv_interaction_spec = NULL
)

build_CR_community(
  S,
  uptake_maximum_mean,
  uptake_maximum_range,
  uptake_maximum_distribution,
  uptake_optimum_mean,
  uptake_optimum_range,
  uptake_optimum_distribution,
  uptake_width_mean,
  uptake_width_range,
  uptake_width_distribution,
  half_saturation_mean,
  half_saturation_range,
  half_saturation_distribution,
  consumer_death_rate,
  resource_renewal_rate,
  resource_supply,
  conversion_efficiency,
  resource_use_mode = "one_resource_all_consumers",
  active_resource = 1,
  private_resource_use = 1,
  private_resource_use_distribution = "constant",
  private_resource_use_mean = private_resource_use,
  private_resource_use_range = 0,
  private_resource_use_precision = 10,
  community_seed
)
```

## Arguments

- S:

  Number of consumer species.

- birth_maximum_mean:

  Mean maximum birth rate.

- birth_maximum_range:

  Range of maximum birth rates.

- birth_maximum_distribution:

  Distribution for maximum birth rates.

- birth_optimum_mean:

  Mean birth-rate thermal optimum.

- birth_optimum_range:

  Range of birth-rate thermal optima.

- birth_optimum_distribution:

  Distribution for birth-rate thermal optima.

- birth_width_distribution:

  Distribution for Gaussian birth-curve widths.

- birth_width_mean:

  Mean Gaussian birth-curve width.

- birth_width_range:

  Range of Gaussian birth-curve widths.

- community_seed:

  Random seed used when generating community traits.

- death_intercept:

  Death rate when temperature is equal to 0.

- death_temperature_slope:

  Exponential temperature sensitivity of death.

- lv_interaction_spec:

  Optional named list specifying LV interactions.

- uptake_maximum_mean:

  Mean maximum uptake height.

- uptake_maximum_range:

  Range of maximum uptake heights.

- uptake_maximum_distribution:

  Distribution for maximum uptake heights.

- uptake_optimum_mean:

  Mean uptake thermal optimum.

- uptake_optimum_range:

  Range of uptake thermal optima.

- uptake_optimum_distribution:

  Distribution for uptake thermal optima.

- uptake_width_mean:

  Mean Gaussian uptake-curve width.

- uptake_width_range:

  Range of Gaussian uptake-curve widths.

- uptake_width_distribution:

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

  Resource-use mode.

- active_resource:

  Active shared resource index.

- private_resource_use:

  Mean private-resource use when a scalar default is needed.

- private_resource_use_distribution:

  Distribution used to generate species-level private-resource use.

- private_resource_use_mean:

  Mean species-level private-resource use.

- private_resource_use_range:

  Range for regular or uniform private-resource use.

- private_resource_use_precision:

  Precision for beta-distributed private-resource use.

## Value

An LV community parameter object.

A CR community parameter object.
