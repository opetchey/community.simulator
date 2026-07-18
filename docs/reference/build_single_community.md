# Build a single exploratory community

Build a single exploratory community

## Usage

``` r
build_single_community(
  model_type = c("lv_discrete", "lv_continuous", "consumer_resource_continuous"),
  richness = 4,
  random_seed = 1,
  lv_interaction = c("none", "weak_asymmetric_competition", "weak_symmetric_competition",
    "predator_prey"),
  lv_interaction_type = NULL,
  lv_interaction_symmetry = "asymmetric",
  lv_interaction_distribution = "uniform",
  lv_interaction_min = 0,
  lv_interaction_max = 0.2,
  lv_interaction_value = 0,
  lv_interaction_diagonal = 1,
  private_resource_use_mean = 0.7,
  private_resource_use_distribution = "beta",
  private_resource_use_range = 0,
  private_resource_use_precision = 12,
  resource_use_mode = "shared_to_private",
  active_resource = 1,
  half_saturation_mean = 100,
  consumer_death_rate = 0.03,
  resource_renewal_rate = 1,
  resource_supply = 1000,
  conversion_efficiency = 1,
  birth_maximum_mean = 0.3,
  birth_optimum_mean = 20,
  birth_optimum_range = 6,
  birth_width_mean = 8,
  uptake_maximum_mean = 0.06,
  uptake_optimum_mean = 20,
  uptake_optimum_range = 6,
  uptake_width_mean = 5
)
```

## Arguments

- model_type:

  Population dynamic model. One of `"lv_discrete"`, `"lv_continuous"`,
  or `"consumer_resource_continuous"`.

- richness:

  Number of species or consumers.

- random_seed:

  Random seed for community generation.

- lv_interaction:

  LV interaction preset. Used for LV models.

- lv_interaction_type:

  Optional detailed LV interaction type. If supplied, this overrides
  `lv_interaction`.

- lv_interaction_symmetry:

  Detailed LV interaction symmetry.

- lv_interaction_distribution:

  Detailed LV interaction distribution.

- lv_interaction_min:

  Minimum off-diagonal interaction value for detailed uniform LV
  interactions.

- lv_interaction_max:

  Maximum off-diagonal interaction value for detailed uniform LV
  interactions.

- lv_interaction_value:

  Constant off-diagonal interaction value for detailed constant LV
  interactions.

- lv_interaction_diagonal:

  Diagonal value of the detailed LV interaction matrix.

- private_resource_use_mean:

  Mean private-resource use fraction for the consumer-resource model
  when using the shared-to-private resource mode.

- private_resource_use_distribution:

  Distribution used to generate private-resource use fractions.

- private_resource_use_range:

  Range of private-resource use fractions for regular and random-uniform
  distributions.

- private_resource_use_precision:

  Precision for beta-distributed private-resource use fractions.

- resource_use_mode:

  Consumer-resource resource-use mode.

- active_resource:

  Active or shared resource index.

- half_saturation_mean:

  Mean Monod half-saturation constant.

- consumer_death_rate:

  Consumer death rate.

- resource_renewal_rate:

  Resource renewal rate.

- resource_supply:

  Resource supply concentration.

- conversion_efficiency:

  Conversion efficiency from uptake to growth.

- birth_maximum_mean:

  Mean maximum birth rate for LV models.

- birth_optimum_mean:

  Mean birth thermal optimum for LV models.

- birth_optimum_range:

  Range of birth thermal optima for LV models.

- birth_width_mean:

  Mean birth performance-curve width for LV models.

- uptake_maximum_mean:

  Mean maximum uptake rate for the CR model.

- uptake_optimum_mean:

  Mean uptake thermal optimum for the CR model.

- uptake_optimum_range:

  Range of uptake thermal optima for the CR model.

- uptake_width_mean:

  Mean uptake performance-curve width for the CR model.

## Value

A list with the community object, model type, trait table,
community-structure matrix data, species performance curves, and a
summed community performance curve.

## Examples

``` r
build_single_community(model_type = "lv_discrete", richness = 3)
#> $model_type
#> [1] "lv_discrete"
#> 
#> $community
#> $community$S
#> [1] 3
#> 
#> $community$birth_maximum_i
#> [1] 0.3 0.3 0.3
#> 
#> $community$birth_optimum_i
#> [1] 17 20 23
#> 
#> $community$birth_width_i
#> [1] 8 8 8
#> 
#> $community$death_intercept_i
#> [1] 0 0 0
#> 
#> $community$death_temperature_slope_i
#> [1] 0.05 0.05 0.05
#> 
#> $community$a_b_i
#> [1] 0.3 0.3 0.3
#> 
#> $community$b_opt_i
#> [1] 17 20 23
#> 
#> $community$sd_perf_i
#> [1] 8 8 8
#> 
#> $community$s_i
#> [1] 8 8 8
#> 
#> $community$a_d_i
#> [1] 0 0 0
#> 
#> $community$z_i
#> [1] 0.05 0.05 0.05
#> 
#> $community$alpha_ij
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    0    1    0
#> [3,]    0    0    1
#> 
#> $community$lv_interaction_spec
#> $community$lv_interaction_spec$label
#> [1] "no_interactions"
#> 
#> $community$lv_interaction_spec$type
#> [1] "none"
#> 
#> $community$lv_interaction_spec$diagonal
#> [1] 1
#> 
#> 
#> 
#> $traits
#> # A tibble: 3 × 6
#>   species birth_maximum birth_optimum birth_width death_intercept
#>   <chr>           <dbl>         <dbl>       <dbl>           <dbl>
#> 1 Spp1              0.3            17           8               0
#> 2 Spp2              0.3            20           8               0
#> 3 Spp3              0.3            23           8               0
#> # ℹ 1 more variable: death_temperature_slope <dbl>
#> 
#> $structure_matrix
#>   row_index column_index value       row    column    matrix_type
#> 1         1            1     1 Species 1 Species 1 LV interaction
#> 2         2            1     0 Species 2 Species 1 LV interaction
#> 3         3            1     0 Species 3 Species 1 LV interaction
#> 4         1            2     0 Species 1 Species 2 LV interaction
#> 5         2            2     1 Species 2 Species 2 LV interaction
#> 6         3            2     0 Species 3 Species 2 LV interaction
#> 7         1            3     0 Species 1 Species 3 LV interaction
#> 8         2            3     0 Species 2 Species 3 LV interaction
#> 9         3            3     1 Species 3 Species 3 LV interaction
#> 
#> $performance_curves
#> # A tibble: 600 × 6
#>    species_index temperature species performance viable model_type 
#>            <int>       <dbl> <chr>         <dbl> <lgl>  <chr>      
#>  1             1       -7    Spp1       0.00333  TRUE   lv_discrete
#>  2             2       -7    Spp2       0.00101  TRUE   lv_discrete
#>  3             3       -7    Spp3       0.000265 TRUE   lv_discrete
#>  4             1       -6.73 Spp1       0.00369  TRUE   lv_discrete
#>  5             2       -6.73 Spp2       0.00113  TRUE   lv_discrete
#>  6             3       -6.73 Spp3       0.000301 TRUE   lv_discrete
#>  7             1       -6.46 Spp1       0.00408  TRUE   lv_discrete
#>  8             2       -6.46 Spp2       0.00127  TRUE   lv_discrete
#>  9             3       -6.46 Spp3       0.000341 TRUE   lv_discrete
#> 10             1       -6.19 Spp1       0.00450  TRUE   lv_discrete
#> # ℹ 590 more rows
#> 
#> $community_performance_curve
#> # A tibble: 200 × 4
#>    temperature community_performance performance_type   y_label                 
#>          <dbl>                 <dbl> <chr>              <chr>                   
#>  1       -7                  0.00461 summed_performance Summed community perfor…
#>  2       -6.73               0.00512 summed_performance Summed community perfor…
#>  3       -6.46               0.00568 summed_performance Summed community perfor…
#>  4       -6.19               0.00630 summed_performance Summed community perfor…
#>  5       -5.91               0.00698 summed_performance Summed community perfor…
#>  6       -5.64               0.00772 summed_performance Summed community perfor…
#>  7       -5.37               0.00853 summed_performance Summed community perfor…
#>  8       -5.10               0.00942 summed_performance Summed community perfor…
#>  9       -4.83               0.0104  summed_performance Summed community perfor…
#> 10       -4.56               0.0114  summed_performance Summed community perfor…
#> # ℹ 190 more rows
#> 
```
